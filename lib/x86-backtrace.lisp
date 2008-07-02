;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2006 Clozure Associates and contributors
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html


(in-package "CCL")


;;; Returns two values:
;;;  [nil, nil] if it can be reliably determined that function uses no registers at PC
;;;  [mask, saved-location]  if it can be reliably determined that the registers specified by "mask"
;;;      were saved at "saved-location" in the function's stack frame
;;;  [mask, nil] if registers in "mask" MAY have been saved, but we don't know how to restore them
;;;      (perhaps because the "at-pc" argument wasn't specified.


(defun registers-used-by (function &optional at-pc)
  (multiple-value-bind (mask stack-location rpc)
      (%function-register-usage function)
    (if (null mask)
      (values nil nil)
      (values (canonicalize-register-mask mask) (if (and at-pc rpc (> at-pc rpc)) stack-location)))))

(defun canonicalize-register-mask (mask)
  (dpb (ldb (byte 2 14) mask) (byte 2 2) (ldb (byte 2 11) mask)))

(defun xcf-p (p)
  (eql 0 (%fixnum-ref p x8664::lisp-frame.return-address)))

(defun %current-xcf ()
  (do* ((q (%get-frame-ptr) (%%frame-backlink q)))
       ((zerop q))
    (declare (fixnum q))
    (when (xcf-p q) (return q))))

;;; Try to determine the program counter value, relative to an xcf's nominal function.
(defun pc-from-xcf (xcf)
  (let* ((nominal-function (%fixnum-ref xcf x8664::xcf.nominal-function))
         (containing-object (%fixnum-ref xcf x8664::xcf.containing-object)))
    (when (typep nominal-function 'function)
      (if (eq containing-object (function-to-function-vector nominal-function))
        (- (%fixnum-ref xcf x8664::xcf.relative-pc)
           x8664::tag-function)
        (let* ((tra (%fixnum-ref xcf x8664::xcf.ra0)))
          (if (and (= (lisptag tra) x8664::tag-tra)
                   (eq nominal-function (%return-address-function tra)))
            (%return-address-offset tra)))))))
            
(defun cfp-lfun (p)
  (if (xcf-p p)
    (values
     (%fixnum-ref p x8664::xcf.nominal-function)
     (pc-from-xcf p))
    (%cfp-lfun p)))

;;; On PPC, some frames on the control stack are associated with catch
;;; frames rather than with function calls.  The whole concept doesn't
;;; really apply here (e.g., nothing we encounter while walking frame
;;; pointer links belongs to a catch frame.)
(defun catch-csp-p (p context)
  (declare (ignore p context)))

(defun %raw-frame-ref (frame context idx bad)
  (declare (fixnum frame idx))
  (let* ((base (parent-frame frame context))
         (raw-size (- base frame)))
    (declare (fixnum base raw-size))
    (if (and (>= idx 0)
             (< idx raw-size))
      (let* ((addr (- (the fixnum (1- base))
                      idx)))
        (multiple-value-bind (db-count first-db last-db)
            (count-db-links-in-frame frame base context)
          (let* ((is-db-link
                  (unless (zerop db-count)
                    (do* ((last last-db (previous-db-link last first-db)))
                         ((null last))
                      (when (= addr last)
                        (return t))))))
            (if is-db-link
              (oldest-binding-frame-value context addr)
              (%fixnum-ref addr)))))
      bad)))

(defun %raw-frame-set (frame context idx new)
  (declare (fixnum frame idx))
  (let* ((base (parent-frame frame context))
         (raw-size (- base frame)))
    (declare (fixnum base raw-size))
    (if (and (>= idx 0)
             (< idx raw-size))
      (let* ((addr (- (the fixnum (1- base))
                      idx)))
        (multiple-value-bind (db-count first-db last-db)
            (count-db-links-in-frame frame base context)
          (let* ((is-db-link
                  (unless (zerop db-count)
                    (do* ((last last-db (previous-db-link last first-db)))
                         ((null last))
                      (when (= addr last)
                        (return t))))))
            (if is-db-link
              (setf (oldest-binding-frame-value context addr) new)
              (setf (%fixnum-ref addr) new))))))))

(defun %stack< (index1 index2 &optional context)
  (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
         (vs-area (%fixnum-ref tcr target::tcr.vs-area)))
    (and (%ptr-in-area-p index1 vs-area)
         (%ptr-in-area-p index2 vs-area)
         (< (the fixnum index1) (the fixnum index2)))))




(defun register-number->saved-register-index (regnum)
  (ecase regnum
    (#.x8664::save3 0)
    (#.x8664::save2 1)
    (#.x8664::save1 2)
    (#.x8664::save0 3)))


(defun get-register-value (address last-catch index)
  (if address
    (%fixnum-ref address)
    (uvref last-catch (+ index target::catch-frame.save-save3-cell))))

;;; Inverse of get-register-value

(defun set-register-value (value address last-catch index)
  (if address
    (%fixnum-set address value)
    (setf (uvref last-catch (+ index target::catch-frame.save-save3-cell))
          value)))

(defun %find-register-argument-value (context cfp regval bad)
  (let* ((last-catch (last-catch-since cfp context))
         (index (register-number->saved-register-index regval)))
    (do* ((frame cfp (child-frame frame context))
          (first t))
         ((null frame))
      (if (xcf-p frame)
        (with-macptrs (xp)
          (%setf-macptr-to-object xp (%fixnum-ref frame x8664::xcf.xp))
          (return-from %find-register-argument-value
            (encoded-gpr-lisp xp regval)))
        (progn
          (unless first
            (multiple-value-bind (lfun pc)
                (cfp-lfun frame)
              (when lfun
                (multiple-value-bind (mask where)
                    (registers-used-by lfun pc)
                  (when (if mask (logbitp index mask))
                    (incf where (logcount (logandc2 mask (1- (ash 1 (1+ index))))))


                    (return-from %find-register-argument-value
                      (raw-frame-ref frame context where bad)))))))
          (setq first nil))))
    (get-register-value nil last-catch index)))

(defun %set-register-argument-value (context cfp regval new)
  (let* ((last-catch (last-catch-since cfp context))
         (index (register-number->saved-register-index regval)))
    (do* ((frame cfp (child-frame frame context))
          (first t))
         ((null frame))
      (if (xcf-p frame)
        (with-macptrs (xp)
          (%setf-macptr-to-object xp (%fixnum-ref frame x8664::xcf.xp))
          (return-from %set-register-argument-value
            (setf (encoded-gpr-lisp xp regval) new)))
        (progn
          (unless first
            (multiple-value-bind (lfun pc)
                (cfp-lfun frame)
              (when lfun
                (multiple-value-bind (mask where)
                    (registers-used-by lfun pc)
                  (when (if mask (logbitp index mask))
                    (incf where (logcount (logandc2 mask (1- (ash 1 (1+ index))))))

                    (return-from %set-register-argument-value
                      (raw-frame-set frame context where new)))))))
          (setq first nil))))
    (set-register-value new nil last-catch index)))

;;; Used for printing only.
(defun index->address (p)
  (ldb (byte #+32-bit-target 32 #+64-bit-target 64 0)  (ash p target::fixnumshift)))

(defun exception-frame-p (x)
  (and x (xcf-p x)))

;;; Function has failed a number-of-arguments check; return a list
;;; of the actual arguments.
;;; On x86-64, the kernel has finished the frame and pushed everything
;;; for us, so all that we need to do is to hide any inherited arguments.
(defun arg-check-call-arguments (fp function)
  (when (xcf-p fp)
    (with-macptrs (xp)
      (%setf-macptr-to-object xp (%fixnum-ref fp target::xcf.xp))
      (let* ((numinh (ldb $lfbits-numinh (lfun-bits function)))
             (nargs (- (xp-argument-count xp) numinh))
             (p (- (%fixnum-ref fp target::xcf.backptr)
                   (* target::node-size numinh))))
        (declare (fixnum numinh nargs p))
        (collect ((args))
          (dotimes (i nargs (args))
            (args (%fixnum-ref p (- target::node-size)))
            (decf p)))))))

(defun vsp-limits (frame context)
  (let* ((parent (parent-frame frame context)))
    (if (xcf-p frame)
      (values (+ frame (ash x8664::xcf.size (- x8664::word-shift)))
              parent)
      (let* ((tra (%fixnum-ref frame x8664::lisp-frame.return-address)))
        (values (+ frame 2 (if (eq tra (%get-kernel-global ret1valaddr)) 1 0))
                parent)))))

(defun last-catch-since (fp context)
  (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
         (catch (%catch-top tcr))
         (last-catch nil))
    (loop
      (unless catch (return last-catch))
      (let ((catch-fp (uvref catch target::catch-frame.rbp-cell)))
        (when (%stack< fp catch-fp context) (return last-catch))
        (setq last-catch catch
              catch (next-catch catch))))))

(defun last-xcf-since (target-fp start-fp context)
  (do* ((last-xcf nil)
        (fp start-fp (parent-frame fp context)))
       ((or (eql fp target-fp)
            (null fp)
            (%stack< target-fp fp)) last-xcf)
    (if (xcf-p fp) (setq last-xcf fp))))

(defun match-local-name (cellno info pc)
  (when info
    (let* ((syms (%car info))
           (ptrs (%cdr info)))
      (dotimes (i (length syms))
        (let ((j (%i+ i (%i+ i i ))))
          (and (eq (uvref ptrs j) (%ilogior (%ilsl (+ 6 target::word-shift) cellno) #o77))
               (%i>= pc (uvref ptrs (%i+ j 1)))
               (%i< pc (uvref ptrs (%i+ j 2)))
               (return (aref syms i))))))))

(defun apply-in-frame (frame function arglist &optional context)
  (setq function (coerce-to-function function))
  (let* ((parent (parent-frame frame context)))
    (when parent
      (if (xcf-p parent)
        (error "Can't unwind to exception frame ~s" frame)
        (setq frame parent))
      (if (or (null context)
              (eq (bt.tcr context) (%current-tcr)))
        (%apply-in-frame frame function arglist)
        (let* ((process (tcr->process (bt.tcr context))))
          (if process
            (process-interrupt process #'%apply-in-frame frame function arglist)
            (error "Can't find process for backtrace context ~s" context)))))))

(defun return-from-frame (frame &rest values)
  (apply-in-frame frame #'values values nil))
    

(defun last-tsp-before (target)
  (declare (fixnum target))
  (do* ((tsp (%fixnum-ref (%current-tcr) target::tcr.save-tsp)
             (%fixnum-ref tsp target::tsp-frame.backptr)))
       ((zerop tsp) nil)
    (declare (fixnum tsp))
    (when (> (the fixnum (%fixnum-ref tsp target::tsp-frame.rbp))
             target)
      (return tsp))))

    


;;; We can't determine this reliably (yet).
(defun last-foreign-sp-before (target)
  (declare (fixnum target))
  (do* ((cfp (%fixnum-ref (%current-tcr) target::tcr.foreign-sp)
             (%fixnum-ref cfp target::csp-frame.backptr)))
       ((zerop cfp))
    (declare (fixnum cfp))
    (let* ((rbp (%fixnum-ref cfp target::csp-frame.rbp)))
      (declare (fixnum rbp))
      (if (> rbp target)
        (return cfp)
        (if (zerop rbp)
          (return nil))))))


(defun %tsp-frame-containing-progv-binding (db)
  (declare (fixnum db))
  (do* ((tsp (%fixnum-ref (%current-tcr) target::tcr.save-tsp) next)
        (next (%fixnum-ref tsp target::tsp-frame.backptr)
              (%fixnum-ref tsp target::tsp-frame.backptr)))
       ()
    (declare (fixnum tsp next))
    (let* ((rbp (%fixnum-ref tsp target::tsp-frame.rbp)))
      (declare (fixnum rbp))
      (if (zerop rbp)
        (return (values nil nil))
        (if (and (> db tsp)
                 (< db next))
          (return (values tsp rbp)))))))

        




(defun last-binding-before (frame)
  (declare (fixnum frame))
  (do* ((db (%current-db-link) (%fixnum-ref db 0))
        (tcr (%current-tcr))
        (vs-area (%fixnum-ref tcr target::tcr.vs-area))
        (vs-low (%fixnum-ref vs-area target::area.low))
        (vs-high (%fixnum-ref vs-area target::area.high)))
       ((eql db 0) nil)
    (declare (fixnum db vs-low vs-high))
    (if (and (> db vs-low)
             (< db vs-high))
      (if (> db frame)
        (return db))
      ;; db link points elsewhere; PROGV uses the temp stack
      ;; to store an indefinite number of bindings.
      (multiple-value-bind (tsp rbp)
          (%tsp-frame-containing-progv-binding db)
        (if tsp
          (if (> rbp frame)
            (return db)
            ;; If the tsp frame is too young, we can skip
            ;; all of the bindings it contains.  The tsp
            ;; frame contains two words of overhead, followed
            ;; by a count of binding records in the frame,
            ;; followed by the youngest of "count" binding
            ;; records (which happens to be the value of
            ;; "db".)  Skip "count" binding records.
            (dotimes (i (the fixnum (%fixnum-ref tsp target::dnode-size)))
              (setq db (%fixnum-ref db 0))))
          ;; If the binding record wasn't on the temp stack and wasn't
          ;; on the value stack, that probably means that things are
          ;; seriously screwed up.  This error will be almost
          ;; meaningless to the user.
          (error "binding record (#x~16,'0x/#x~16,'0x) not on temp or value stack" (index->address db) db))))))
          


(defun find-x8664-saved-nvrs (frame start-fp context)
  (let* ((locations (make-array 16 :initial-element nil))
         (need (logior (ash 1 x8664::save0)
                       (ash 1 x8664::save1)
                       (ash 1 x8664::save2)
                       (ash 1 x8664::save3))))
    (declare (fixnum need)
             (dynamic-extent locations))
    (do* ((parent frame child)
          (child (child-frame parent context) (child-frame child context)))
         ((or (= need 0) (eq child start-fp))
          (values (%svref locations x8664::save0)
                  (%svref locations x8664::save1)
                  (%svref locations x8664::save2)
                  (%svref locations x8664::save3)))
      (multiple-value-bind (lfun pc) (cfp-lfun child)
        (when (and lfun pc)
          (multiple-value-bind (used where) (registers-used-by lfun pc)
            (when (and used where (logtest used need))
              (locally (declare (fixnum used))
                (do* ((i x8664::save3 (1+ i)))
                     ((or (= i 16) (= used 0)))
                  (declare (type (mod 16) i))
                  (when (logbitp i used)
                    (when (logbitp i need)
                      (setq need (logandc2 need (ash 1 i)))
                      (setf (%svref locations i)
                            (- (the fixnum (1- parent))
                               (+ where (logcount (logandc2 used (1+ (ash 1 (1+ i)))))))))
                    (setq used (logandc2 used (ash 1 i)))))))))))))
                                         
              
         
(defun %apply-in-frame (frame function arglist)
  (let* ((target-catch (last-catch-since frame nil))
         (start-fp (if target-catch
                     (uvref target-catch target::catch-frame.rbp-cell)
                     (%get-frame-ptr)))
         (target-xcf (last-xcf-since frame start-fp nil))
         (target-db-link (last-binding-before frame))
         (target-tsp (last-tsp-before frame))
         (target-foreign-sp (last-foreign-sp-before frame)))
    (multiple-value-bind (save0-loc save1-loc save2-loc save3-loc)
        (find-x8664-saved-nvrs frame start-fp nil)
      (let* ((thunk (%clone-x86-function #'%%apply-in-frame-proto
                                         frame
                                         target-catch
                                         target-db-link
                                         target-xcf
                                         target-tsp
                                         target-foreign-sp
                                         (if save0-loc
                                           (- save0-loc frame)
                                           0)
                                         (if save1-loc
                                           (- save1-loc frame)
                                           0)
                                         (if save2-loc
                                           (- save2-loc frame)
                                           0)
                                         (if save3-loc
                                           (- save3-loc frame)
                                           0)
                                         (coerce-to-function function)
                                         arglist
                                         0)))
        (funcall thunk)))))

            
    
