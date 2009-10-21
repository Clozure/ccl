;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html


(in-package "CCL")

(def-accessors (fake-stack-frame) %svref
  nil                           ; 'fake-stack-frame
  %fake-stack-frame.sp          ; fixnum. The stack pointer where this frame "should" be
  %fake-stack-frame.next-sp     ; Either sp or another fake-stack-frame
  %fake-stack-frame.fn          ; The current function
  %fake-stack-frame.lr          ; fixnum offset from fn (nil if fn is not functionp)
  %fake-stack-frame.vsp         ; The value stack pointer
  %fake-stack-frame.xp          ; Exception frame.
  %fake-stack-frame.link        ; next in *fake-stack-frames* list
  )

;;; Linked list of fake stack frames.
;;; %frame-backlink looks here
(def-standard-initial-binding *fake-stack-frames* nil)
  

(defun fake-stack-frame-p (x)
  (istruct-typep x 'fake-stack-frame))

(defun cfp-lfun (p)
  (if (fake-stack-frame-p p)
    (let* ((fn (%fake-stack-frame.fn p))
           (lr (%fake-stack-frame.lr p)))
      (if (and (typep fn 'function)
               (typep lr 'fixnum))
        (values fn lr)
        (values nil nil)))
    (%cfp-lfun p)))


(defun %stack< (index1 index2 &optional context)
  (cond ((fake-stack-frame-p index1)
         (let ((sp1 (%fake-stack-frame.sp index1)))
           (declare (fixnum sp1))
           (if (fake-stack-frame-p index2)
             (or (%stack< sp1 (%fake-stack-frame.sp index2) context)
                 (eq index2 (%fake-stack-frame.next-sp index1)))
             (%stack< sp1 (%i+ index2 1) context))))
        ((fake-stack-frame-p index2)
         (%stack< index1 (%fake-stack-frame.sp index2) context))
        (t (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
                  (cs-area (%fixnum-ref tcr target::tcr.cs-area)))
             (and (%ptr-in-area-p index1 cs-area)
                  (%ptr-in-area-p index2 cs-area)
                  (< (the fixnum index1) (the fixnum index2)))))))

;;; Returns two values:
;;;  [nil, nil] if it can be reliably determined that function uses no registers at PC
;;;  [mask, savevsp]  if it can be reliably determined that the registers specified by "mask"
;;;      were saved at "savevsp" in the function's stack frame
;;;  [mask, nil] if registers in "mask" MAY have been saved, but we don't know how to restore them
;;;      (perhaps because the "at-pc" argument wasn't specified.


;;; If the last instruction in a code vector is an
;;; LWZ instruction (of the form "(LWZ rx s16 ry)"),
;;; then 
;;;   this function uses registers RX-R31.  Note that this leaves
;;;    us 2 extra bits, since we're only encoding 3 bits worth of
;;;    register info.
;;;   RX is saved nearest the top of the vstack
;;;   s16 is the offset from the saved-vsp to the address at which
;;;    RX was saved; this is a negative value whose low two bits
;;;    are ignored
;;;   (logior (ash (logand s16 3) 5) rY) is the pc at which
;;;   the registers were saved (a fullword code-vector index).
;;; This scheme lets us encode any "simple" register usage, where
;;; the registers were saved once, saved somewhere within the first 
;;; 128 instructions in the code vector, and nothing interesting (to
;;; backtrace) happens after the registers have been restored.
;;; If the compiler ever gets cleverer about this, we'll have to use
;;; some other scheme (perhaps a STW instruction, preceded by branches).
;;;
;;; Note that the "last instruction" really means "last instruction
;;; before any traceback table"; we should be able to truncate the code
;;; vector (probably by copying it) to strip off the traceback table
;;; without losing this information.
;;; Note also that the disassembler would probably ordinarily want to
;;; hide this last instruction ...
;;;   

#+ppc32-target
(defun registers-used-by (lfun &optional at-pc)
  (let* ((regs-used nil)
         (where-saved nil))
    (multiple-value-bind (op-high op-low) (%code-vector-last-instruction (uvref lfun 0))
      (declare (fixnum op-high op-low))
      (if (eql (ldb (byte 6 (- 26 16)) op-high) 32)       ; LWZ
        (let* ((nregs (- 32 (ldb (byte 5 (- 21 16)) op-high)))
               (pc (dpb (ldb (byte 2 0) op-low) (byte 2 5) (ldb (byte 5 (- 16 16)) op-high)))
               (offset (%word-to-int (logand op-low (lognot 3)))))
          (declare (fixnum nregs pc offset))
          (setq regs-used (1- (ash 1 nregs)))
          (if at-pc
            (if (>= at-pc pc)
              (setq where-saved (- (ash (- offset) -2) nregs))
              (setq regs-used nil))))))
    (values (and regs-used (bit-reverse-8 regs-used)) where-saved)))

#+ppc64-target
(defun registers-used-by (lfun &optional at-pc)
  (let* ((regs-used nil)
         (where-saved nil)
         (instr (%code-vector-last-instruction (uvref lfun 0))))
      (if (eql (ldb (byte 6 26) instr) 32)       ; LWZ
        (let* ((nregs (- 32 (ldb (byte 5 21) instr)))
               (pc (dpb (ldb (byte 2 0) instr) (byte 2 5) (ldb (byte 5 16) instr)))
               (offset (%word-to-int (logand instr (lognot 7)))))
          (declare (fixnum nregs pc offset))
          (setq regs-used (1- (ash 1 nregs)))
          (if at-pc
            (if (>= at-pc pc)
              (setq where-saved (- (ash (- offset) -3) nregs))
              (setq regs-used nil)))))        
      (values (and regs-used (bit-reverse-8 regs-used)) where-saved)))    
  

(defparameter *bit-reverse-8-table*
  #.(let ((table (make-array 256 :element-type '(unsigned-byte 8))))
      (dotimes (i 256)
        (let ((j 0)
              (out-mask (ash 1 7)))
          (declare (fixnum j out-mask))
          (dotimes (bit 8)
            (when (logbitp bit i)
              (setq j (logior j out-mask)))
            (setq out-mask (ash out-mask -1)))
          (setf (aref table i) j)))
      table))

(defun bit-reverse-8 (x)
  (aref *bit-reverse-8-table* x))

(defun %frame-savefn (p)
  (if (fake-stack-frame-p p)
    (%fake-stack-frame.fn p)
    (%%frame-savefn p)))

(defun %frame-savevsp (p)
  (if (fake-stack-frame-p p)
    (%fake-stack-frame.vsp p)
    (%%frame-savevsp p)))

(defun frame-vsp (frame)
  (%frame-savevsp frame))

;;; Return two values: the vsp of p and the vsp of p's "parent" frame.
;;; The "parent" frame vsp might actually be the end of p's segment,
;;; if the real "parent" frame vsp is in another segment.
(defun vsp-limits (p context)
  (let* ((vsp (%frame-savevsp p))
         parent)
    (when (eql vsp 0)
      ; This frame is where the code continues after an unwind-protect cleanup form
      (setq vsp (%frame-savevsp (child-frame p context))))
    (flet ((grand-parent (frame)
             (let ((parent (parent-frame frame context)))
               (when (and parent (eq parent (%frame-backlink frame context)))
                 (let ((grand-parent (parent-frame parent context)))
                   (when (and grand-parent (eq grand-parent (%frame-backlink parent context)))
                     grand-parent))))))
      (declare (dynamic-extent #'grand-parent))
      (let* ((frame p)
             grand-parent)
        (loop
          (setq grand-parent (grand-parent frame))
          (when (or (null grand-parent) (not (eql 0 (%frame-savevsp grand-parent))))
            (return))
          (setq frame grand-parent))
        (setq parent (parent-frame frame context)))
      (let* ((parent-vsp (if parent (%frame-savevsp parent) vsp))
             (tcr (if context (bt.tcr context) (%current-tcr)))
             (vsp-area (%fixnum-ref tcr target::tcr.vs-area)))
        (if (eql 0 parent-vsp)
          (values vsp vsp)              ; p is the kernel frame pushed by an unwind-protect cleanup form
          (progn
            (unless vsp-area
              (error "~s is not a stack frame pointer for context ~s" p tcr))
            (unless (%ptr-in-area-p parent-vsp vsp-area)
              (setq parent-vsp (%fixnum-ref vsp-area target::area.high)))
            (values vsp parent-vsp)))))))


(defun catch-csp-p (p context)
  (let ((catch (if context
                 (bt.top-catch context)
                 (%catch-top (%current-tcr)))))
    (loop
      (when (null catch) (return nil))
      (let ((sp (catch-frame-sp catch)))
        (when (eql sp p)
          (return t)))
      (setq catch (next-catch catch)))))

(defun last-catch-since (sp context)
  (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
         (catch (%catch-top tcr))
         (last-catch nil))
    (loop
      (unless catch (return last-catch))
      (let ((csp (uvref catch target::catch-frame.csp-cell)))
        (when (%stack< sp csp context) (return last-catch))
        (setq last-catch catch
              catch (next-catch catch))))))

(defun register-number->saved-register-index (regno)
  (- regno ppc::save7))

(defun %find-register-argument-value (context cfp regval bad)
  (let* ((last-catch (last-catch-since cfp context))
         (index (register-number->saved-register-index regval)))
    (do* ((frame cfp
                 (child-frame frame context))
          (first t))
         ((null frame))
      (if (fake-stack-frame-p frame)
        (return-from %find-register-argument-value
          (xp-gpr-lisp (%fake-stack-frame.xp frame) regval))
        (if first
          (setq first nil)
          (multiple-value-bind (lfun pc)
              (cfp-lfun frame)
            (when lfun
              (multiple-value-bind (mask where)
                  (registers-used-by lfun pc)
                (when (if mask (logbitp index mask))
                  (incf where (logcount (logandc2 mask (1- (ash 1 (1+ index))))))
                  (return-from
                   %find-register-argument-value
                    (raw-frame-ref frame context where bad)))))))))
    (get-register-value nil last-catch index)))

(defun %set-register-argument-value (context cfp regval new)
  (let* ((last-catch (last-catch-since cfp context))
         (index (register-number->saved-register-index regval)))
    (do* ((frame cfp
                 (child-frame frame context))
          (first t))
         ((null frame))
      (if (fake-stack-frame-p frame)
        (return-from %set-register-argument-value
          (setf (xp-gpr-lisp (%fake-stack-frame.xp frame) regval) new))
        (if first
          (setq first nil)
          (multiple-value-bind (lfun pc)
              (cfp-lfun frame)
            (when lfun
              (multiple-value-bind (mask where)
                  (registers-used-by lfun pc)
                (when (if mask (logbitp index mask))
                  (incf where (logcount (logandc2 mask (1- (ash 1 (1+ index))))))
                  (return-from
                   %set-register-argument-value
                    (raw-frame-set frame context where new)))))))))
    (set-register-value new nil last-catch index)))

(defun %raw-frame-ref (cfp context idx bad)
  (declare (fixnum idx))
  (multiple-value-bind (frame base)
      (vsp-limits cfp context)
    (let* ((raw-size (- base frame)))
      (declare (fixnum frame base raw-size))
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
        bad))))

(defun %raw-frame-set (cfp context idx new)
  (declare (fixnum idx))
  (multiple-value-bind (frame base)
      (vsp-limits cfp context)
    (let* ((raw-size (- base frame)))
      (declare (fixnum frame base raw-size))
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
                (setf (%fixnum-ref addr) new))))
          t)))))

;;; Used for printing only.
(defun index->address (p)
  (when (fake-stack-frame-p p)
    (setq p (%fake-stack-frame.sp p)))
  (ldb (byte #+32-bit-target 32 #+64-bit-target 64 0)  (ash p target::fixnumshift)))


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

(defun get-register-value (address last-catch index)
  (if address
    (%fixnum-ref address)
    (uvref last-catch (+ index target::catch-frame.save-save7-cell))))

;;; Inverse of get-register-value

(defun set-register-value (value address last-catch index)
  (if address
    (%fixnum-set address value)
    (setf (uvref last-catch (+ index target::catch-frame.save-save7-cell))
          value)))

;;; I'm skeptical about a lot of this stuff on the PPC, but if anything it's
;;; pretty PPC-specific

;;; Act as if VSTACK-INDEX points somewhere where DATA could go & put it there.
(defun set-lisp-data (vstack-index data)
  (let* ((old (%access-lisp-data vstack-index)))
    (if (closed-over-value-p old)
      (set-closed-over-value old data)
      (%store-lisp-data vstack-index data))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;extensions to let user access and modify values





;;; nth-frame-info, set-nth-frame-info, & frame-lfun are in "inspector;new-backtrace"






(defparameter *saved-register-count+1*
  (1+ *saved-register-count*))



(defparameter *saved-register-numbers*
  #+x8664-target #(wrong)
  #+ppc-target #(31 30 29 28 27 26 25 24))

;;; Don't do unbound checks in compiled code
(declaim (type t *saved-register-count* *saved-register-count+1*
               *saved-register-names* *saved-register-numbers*))

(defmacro %cons-saved-register-vector ()
  `(make-array (the fixnum *saved-register-count+1*) :initial-element nil))

(defun copy-srv (from-srv &optional to-srv)
  (if to-srv
    (if (eq from-srv to-srv)
      to-srv
      (dotimes (i (uvsize from-srv) to-srv)
        (setf (uvref to-srv i) (uvref from-srv i))))
    (copy-uvector from-srv)))

(defmacro srv.unresolved (saved-register-vector)
  `(svref ,saved-register-vector 0))

(defmacro srv.register-n (saved-register-vector n)
  `(svref ,saved-register-vector (1+ ,n)))

;;; This isn't quite right - has to look at all functions on stack,
;;; not just those that saved VSPs.


(defun frame-restartable-p (target &optional context)
  (multiple-value-bind (frame last-catch srv) (last-catch-since-saved-vars target context)
    (when frame
      (loop
        (when (null frame)
          (return-from frame-restartable-p nil))
        (when (eq frame target) (return))
        (multiple-value-setq (frame last-catch srv)
          (ccl::parent-frame-saved-vars context frame last-catch srv srv)))
      (when (and srv (eql 0 (srv.unresolved srv)))
        (setf (srv.unresolved srv) last-catch)
        srv))))


;;; get the saved register addresses for this frame
;;; still need to worry about this unresolved business
;;; could share some code with parent-frame-saved-vars
(defun my-saved-vars (frame &optional (srv-out (%cons-saved-register-vector)))
  (let ((unresolved 0))
    (multiple-value-bind (lfun pc) (cfp-lfun frame)
        (if lfun
          (multiple-value-bind (mask where) (registers-used-by lfun pc)
            (when mask
              (if (not where) 
                (setq unresolved (%ilogior unresolved mask))
                (let ((vsp (- (frame-vsp frame) where (1- (logcount mask))))
                      (j *saved-register-count*))
                  (declare (fixnum j))
                  (dotimes (i j)
                    (declare (fixnum i))
                    (when (%ilogbitp (decf j) mask)
                      (setf (srv.register-n srv-out i) vsp
                            vsp (1+ vsp)
                            unresolved (%ilogand unresolved (%ilognot (%ilsl j 1))))))))))
          (setq unresolved (1- (ash 1 *saved-register-count*)))))
    (setf (srv.unresolved srv-out) unresolved)
    srv-out))

(defun parent-frame-saved-vars 
       (context frame last-catch srv &optional (srv-out (%cons-saved-register-vector)))
  (copy-srv srv srv-out)
  (let* ((parent (and frame (parent-frame frame context)))
         (grand-parent (and parent (parent-frame parent context))))
    (when grand-parent
      (loop (let ((next-catch (and last-catch (next-catch last-catch))))
              ;(declare (ignore next-catch))
              (if (and next-catch (%stack< (catch-frame-sp next-catch) grand-parent context))
                (progn
                  (setf last-catch next-catch
                        (srv.unresolved srv-out) 0)
                  (dotimes (i *saved-register-count*)
                    (setf (srv.register-n srv i) nil)))
                (return))))
      (lookup-registers parent context grand-parent srv-out)
      (values parent last-catch srv-out))))

(defun lookup-registers (parent context grand-parent srv-out)
  (unless (or (eql (frame-vsp grand-parent) 0)
              (let ((gg-parent (parent-frame grand-parent context)))
                (eql (frame-vsp gg-parent) 0)))
    (multiple-value-bind (lfun pc) (cfp-lfun parent)
      (when lfun
        (multiple-value-bind (mask where) (registers-used-by lfun pc)
          (when mask
            (locally (declare (fixnum mask))
              (if (not where) 
                (setf (srv.unresolved srv-out) (%ilogior (srv.unresolved srv-out) mask))
                (let* ((grand-parent-vsp (frame-vsp grand-parent)))

                  (let ((vsp (- grand-parent-vsp where 1))
                        (j *saved-register-count*))
                    (declare (fixnum j))
                    (dotimes (i j)
                      (declare (fixnum i))
                      (when (%ilogbitp (decf j) mask)
                        (setf (srv.register-n srv-out i) vsp
                              vsp (1- vsp)
                              (srv.unresolved srv-out)
                              (%ilogand (srv.unresolved srv-out) (%ilognot (%ilsl j 1))))))))))))))))

;;; initialization for looping on parent-frame-saved-vars
(defun last-catch-since-saved-vars (frame context)
  (let* ((parent (parent-frame frame context))
         (last-catch (and parent (last-catch-since parent context))))
    (when last-catch
      (let ((frame (catch-frame-sp last-catch))
            (srv (%cons-saved-register-vector)))
        (setf (srv.unresolved srv) 0)
        (let* ((parent (parent-frame frame context))
               (child (and parent (child-frame parent context))))
          (when child
            (lookup-registers child context parent srv))
          (values child last-catch srv))))))

;;; Returns 2 values:
;;; mask srv
;;; The mask says which registers are used at PC in LFUN.  srv is a
;;; saved-register-vector whose register contents are the register
;;; values registers whose bits are not set in MASK or set in
;;; UNRESOLVED will be returned as NIL.

(defun saved-register-values 
       (lfun pc child last-catch srv &optional (srv-out (%cons-saved-register-vector)))
  (declare (ignore child))
  (cond ((null srv-out) (setq srv-out (copy-uvector srv)))
        ((eq srv-out srv))
        (t (dotimes (i (the fixnum (uvsize srv)))
             (setf (uvref srv-out i) (uvref srv i)))))
  (let ((mask (or (registers-used-by lfun pc) 0))
        (unresolved (srv.unresolved srv))
        (j *saved-register-count*))
    (declare (fixnum j))
    (dotimes (i j)
      (declare (fixnum i))
      (setf (srv.register-n srv-out i)
            (and (%ilogbitp (setq j (%i- j 1)) mask)
                 (not (%ilogbitp j unresolved))
                 (safe-cell-value (get-register-value (srv.register-n srv i) last-catch j)))))
    (setf (srv.unresolved srv-out) mask)
    (values mask srv-out)))

; Set the nth saved register to value.
(defun set-saved-register (value n lfun pc child last-catch srv)
  (declare (ignore lfun pc child) (dynamic-extent))
  (let ((j (- target::node-size n))
        (unresolved (srv.unresolved srv))
        (addr (srv.register-n srv n)))
    (when (logbitp j unresolved)
      (error "Can't set register ~S to ~S" n value))
    (set-register-value value addr last-catch j))
  value)





(defun return-from-nth-frame (n &rest values)
  (apply-in-nth-frame n #'values values))

(defun apply-in-nth-frame (n fn arglist)
  (let* ((bt-info (car *backtrace-contexts*)))
    (and bt-info
         (let* ((frame (nth-frame nil (bt.youngest bt-info) n bt-info)))
           (and frame (apply-in-frame frame fn arglist)))))
  (format t "Can't return to frame ~d ." n))

;;; This method is shadowed by one for the backtrace window.
(defmethod nth-frame (w target n context)
  (declare (ignore w))
  (and target (dotimes (i n target)
                (declare (fixnum i))
                (unless (setq target (parent-frame target context)) (return nil)))))

; If this returns at all, it's because the frame wasn't restartable.
(defun apply-in-frame (frame fn arglist &optional context)
  (let* ((srv (frame-restartable-p frame context))
         (target-sp (and srv (srv.unresolved srv))))
    (if target-sp
      (apply-in-frame-internal context frame fn arglist srv))))

(defun apply-in-frame-internal (context frame fn arglist srv)
  (let* ((tcr (if context (bt.tcr context) (%current-tcr))))
    (if (eq tcr (%current-tcr))
      (%apply-in-frame frame fn arglist srv)
      (let ((process (tcr->process tcr)))
        (if process
          (process-interrupt
           process
           #'%apply-in-frame
           frame fn arglist srv)
          (error "Can't find active process for ~s" tcr))))))


(defun return-from-frame (frame &rest values)
  (apply-in-frame frame #'values values nil))


;;; (srv.unresolved srv) is the last catch frame, left there by
;;; frame-restartable-p The registers in srv are locations of
;;; variables saved between frame and that catch frame.
(defun %apply-in-frame (frame fn arglist srv)
  (declare (fixnum frame))
  (let* ((catch (srv.unresolved srv))
         (tsp-count 0)
         (tcr (%current-tcr))
         (parent (parent-frame frame tcr))
         (vsp (frame-vsp parent))
         (catch-top (%catch-top tcr))
         (db-link (%svref catch target::catch-frame.db-link-cell))
         (catch-count 0))
    (declare (fixnum parent vsp db-link catch-count))
    ;; Figure out how many catch frames to throw through
    (loop
      (unless catch-top
        (error "Didn't find catch frame"))
      (incf catch-count)
      (when (eq catch-top catch)
        (return))
      (setq catch-top (next-catch catch-top)))
    ;; Figure out where the db-link should be
    (loop
      (when (or (eql db-link 0) (>= db-link vsp))
        (return))
      (setq db-link (%fixnum-ref db-link)))
    ;; Figure out how many TSP frames to pop after throwing.
    (let ((sp (catch-frame-sp catch)))
      (loop
        (multiple-value-bind (f pc) (cfp-lfun sp)
          (when f (incf tsp-count (active-tsp-count f pc))))
        (setq sp (parent-frame sp tcr))
        (when (eql sp parent) (return))
        (unless sp (error "Didn't find frame: ~s" frame))))
    #+debug
    (cerror "Continue" "(apply-in-frame ~s ~s ~s ~s ~s ~s ~s)"
            catch-count srv tsp-count db-link parent fn arglist)
    (%%apply-in-frame catch-count srv tsp-count db-link parent fn arglist)))




;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code to determine how many tsp frames to pop.
;;; This is done by parsing the code.
;;; active-tsp-count is the entry point below.
;;;

(defstruct (branch-tree (:print-function print-branch-tree))
  first-instruction
  last-instruction
  branch-target     ; a branch-tree or nil
  fall-through)     ; a branch-tree or nil

(defun print-branch-tree (tree stream print-level)
  (declare (ignore print-level))
  (print-unreadable-object (tree stream :type t :identity t)
    (format stream "~s-~s"
            (branch-tree-first-pc tree)
            (branch-tree-last-pc tree))))

(defun branch-tree-first-pc (branch-tree)
  (let ((first (branch-tree-first-instruction branch-tree)))
    (and first (instruction-element-address first))))

(defun branch-tree-last-pc (branch-tree)
  (let ((last (branch-tree-last-instruction branch-tree)))
    (if last
      (instruction-element-address last)
      (branch-tree-first-pc branch-tree))))

(defun branch-tree-contains-pc-p (branch-tree pc)
  (<= (branch-tree-first-pc branch-tree)
      pc
      (branch-tree-last-pc branch-tree)))

(defvar *branch-tree-hash*
  (make-hash-table :test 'eq :weak :value))

(defun get-branch-tree (function)
  (or (gethash function *branch-tree-hash*)
      (let* ((dll (function-to-dll-header function))
             (tree (dll-to-branch-tree dll)))
        (setf (gethash function *branch-tree-hash*) tree))))         

; Return the number of TSP frames that will be active after throwing out
; of all the active catch frames in function at pc.
; PC is a byte address, a multiple of 4.
(defun active-tsp-count (function pc)
  (setq function
        (require-type
         (if (symbolp function)
           (symbol-function function)
           function)
         'compiled-function))
  (let* ((tree (get-branch-tree function))
         (visited nil))
    (labels ((find-pc (branch path)
               (unless (memq branch visited)
                 (push branch path)
                 (if (branch-tree-contains-pc-p branch pc)
                   path
                   (let ((target (branch-tree-branch-target branch))
                         (fall-through (branch-tree-fall-through branch)))
                     (push branch visited)
                     (if fall-through
                       (or (and target (find-pc target path))
                           (find-pc fall-through path))
                       (and target (find-pc target path))))))))
      (let* ((path (nreverse (find-pc tree nil)))
             (last-tree (car (last path)))
             (catch-count 0)
             (tsp-count 0))
        (unless path
          (error "Can't find path to pc: ~s in ~s" pc function))
        (dolist (tree path)
          (let ((next (branch-tree-first-instruction tree))
                (last (branch-tree-last-instruction tree)))
            (loop
              (when (and (eq tree last-tree)
                         (eql pc (instruction-element-address next)))
                ; If the instruction before the current one is an ff-call,
                ; then callback pushed a TSP frame.
                #| ; Not any more
                (when (ff-call-instruction-p (dll-node-pred next))
                  (incf tsp-count))
                |#
                (return))
              (multiple-value-bind (type target fall-through count) (categorize-instruction next)
                (declare (ignore target fall-through))
                (case type
                  (:tsp-push
                   (when (eql catch-count 0)
                     (incf tsp-count count)))
                  (:tsp-pop
                   (when (eql catch-count 0)
                     (decf tsp-count count)))
                  ((:catch :unwind-protect)
                   (incf catch-count))
                  (:throw
                   (decf catch-count count))))
              (when (eq next last)
                (return))
              (setq next (dll-node-succ next)))))
        tsp-count))))
        

(defun dll-to-branch-tree (dll)
  (let* ((hash (make-hash-table :test 'eql))    ; start-pc -> branch-tree
         (res (collect-branch-tree (dll-header-first dll) dll hash))
         (did-something nil))
    (loop
      (setq did-something nil)
      (let ((mapper #'(lambda (key value)
                        (declare (ignore key))
                        (flet ((maybe-collect (pc)
                                 (when (integerp pc)
                                   (let ((target-tree (gethash pc hash)))
                                     (if target-tree
                                       target-tree
                                       (progn
                                         (collect-branch-tree (dll-pc->instr dll pc) dll hash)
                                         (setq did-something t)
                                         nil))))))
                          (declare (dynamic-extent #'maybe-collect))
                          (let ((target-tree (maybe-collect (branch-tree-branch-target value))))
                            (when target-tree (setf (branch-tree-branch-target value) target-tree)))
                          (let ((target-tree (maybe-collect (branch-tree-fall-through value))))
                            (when target-tree (setf (branch-tree-fall-through value) target-tree)))))))
        (declare (dynamic-extent mapper))
        (maphash mapper hash))
      (unless did-something (return)))
    ; To be totally correct, we should fix up the trees containing
    ; the BLR instruction for unwind-protect cleanups, but none
    ; of the users of this code yet care that it appears that the code
    ; stops there.
    res))

(defun collect-branch-tree (instr dll hash)
  (unless (eq instr dll)
    (let ((tree (make-branch-tree :first-instruction instr))
          (pred nil)
          (next instr))
      (setf (gethash (instruction-element-address instr) hash)
            tree)
      (loop
        (when (eq next dll)
          (setf (branch-tree-last-instruction tree) pred)
          (return))
        (multiple-value-bind (type target fall-through) (categorize-instruction next)
          (case type
            (:label
             (when pred
               (setf (branch-tree-last-instruction tree) pred
                     (branch-tree-fall-through tree) (instruction-element-address next))
               (return)))
            ((:branch :catch :unwind-protect)
             (setf (branch-tree-last-instruction tree) next
                   (branch-tree-branch-target tree) target
                   (branch-tree-fall-through tree) fall-through)
             (return))))
        (setq pred next
              next (dll-node-succ next)))
      tree)))

;;; Returns 4 values:
;;; 1) type: one of :regular, :label, :branch, :catch, :unwind-protect, :throw, :tsp-push, :tsp-pop
;;; 2) branch target (or catch or unwind-protect cleanup)
;;; 3) branch-fallthrough (or catch or unwind-protect body)
;;; 4) Count for throw, tsp-push, tsp-pop
(defun categorize-instruction (instr)
  (etypecase instr
    (lap-label :label)
    (lap-instruction
     (let* ((opcode (lap-instruction-opcode instr))
            (opcode-p (typep opcode 'opcode))
            (name (if opcode-p (opcode-name opcode) opcode))
            (pc (lap-instruction-address instr))
            (operands (lap-instruction-parsed-operands instr)))
       (cond ((equalp name "bla")
              (let ((subprim (car operands)))
                (case subprim
                  (.SPmkunwind
                   (values :unwind-protect (+ pc 4) (+ pc 8)))
                  ((.SPmkcatch1v .SPmkcatchmv)
                   (values :catch (+ pc 4) (+ pc 8)))
                  (.SPthrow
                   (values :branch nil nil))
                  ((.SPnthrowvalues .SPnthrow1value)
                   (let* ((prev-instr (require-type (lap-instruction-pred instr)
                                                    'lap-instruction))
                          (prev-name (opcode-name (lap-instruction-opcode prev-instr)))
                          (prev-operands (lap-instruction-parsed-operands prev-instr)))
                     ; Maybe we should recognize the other possible outputs of ppc2-lwi, but I
                     ; can't imagine we'll ever see them
                     (unless (and (equalp prev-name "li")
                                  (equalp (car prev-operands) "imm0"))
                       (error "Can't determine throw count for ~s" instr))
                     (values :throw nil (+ pc 4) (ash (cadr prev-operands) (- target::fixnum-shift)))))
                  ((.SPprogvsave
                    .SPstack-rest-arg .SPreq-stack-rest-arg .SPstack-cons-rest-arg
                    .SPmakestackblock .SPmakestackblock0 .SPmakestacklist .SPstkgvector
                    .SPstkconslist .SPstkconslist-star
                    .SPmkstackv .SPstack-misc-alloc .SPstack-misc-alloc-init
                    .SPstkvcell0 .SPstkvcellvsp
                    .SPsave-values)
                   (values :tsp-push nil nil 1))
                  (.SPrecover-values
                   (values :tsp-pop nil nil 1))
                  (t :regular))))
             ((or (equalp name "lwz") (equalp name "addi"))
              (if (equalp (car operands) "tsp")
                (values :tsp-pop nil nil 1)
                :regular))
             ((equalp name "stwu")
              (if (equalp (car operands) "tsp")
                (values :tsp-push nil nil 1)
                :regular))
             ((member name '("ba" "blr" "bctr") :test 'equalp)
              (values :branch nil nil))
             ; It would probably be faster to determine the branch address by adding the PC and the offset.
             ((equalp name "b")
              (values :branch (branch-label-address instr (car (last operands))) nil))
             ((and opcode-p (eql (opcode-majorop opcode) 16))
              (values :branch (branch-label-address instr (car (last operands))) (+ pc 4)))
             (t :regular))))))

(defun branch-label-address (instr label-name &aux (next instr))
  (loop
    (setq next (dll-node-succ next))
    (when (eq next instr)
      (error "Couldn't find label ~s" label-name))
    (when (and (typep next 'lap-label)
               (eq (lap-label-name next) label-name))
      (return (instruction-element-address next)))))

(defun dll-pc->instr (dll pc)
  (let ((next (dll-node-succ dll)))
    (loop
      (when (eq next dll)
        (error "Couldn't find pc: ~s in ~s" pc dll))
      (when (eql (instruction-element-address next) pc)
        (return next))
      (setq next (dll-node-succ next)))))

(defun exception-frame-p (frame)
  (fake-stack-frame-p frame))

(defun arg-check-call-arguments (frame function)
  (declare (ignore function))
  (xp-argument-list (%fake-stack-frame.xp frame)))
