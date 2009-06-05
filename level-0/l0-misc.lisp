;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
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


;;; Bootstrapping for futexes
#+(and linux-target x86-target)
(eval-when (:compile-toplevel :execute)
  (pushnew :futex *features*))

#+futex
(eval-when (:compile-toplevel :execute)
  ;; We only need a few constants from <linux/futex.h>, which may
  ;; not have been included in the :libc .cdb files.
  (defconstant FUTEX-WAIT 0)
  (defconstant FUTEX-WAKE 1)
  (defconstant futex-avail 0)
  (defconstant futex-locked 1)
  (defconstant futex-contended 2)
  (declaim (inline %lock-futex %unlock-futex)))

;;; Miscellany.

(defun memq (item list)
  (do* ((tail list (%cdr tail)))
       ((null tail))
    (if (eq item (car tail))
      (return tail))))

(defun %copy-u8-to-string (u8-vector source-idx string dest-idx n)
  (declare (optimize (speed 3) (safety 0))
           (fixnum source-idx dest-idx n)
           (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (simple-base-string string))
  (do* ((i 0 (1+ i)))
       ((= i n) string)
    (declare (fixnum i))
    (setf (%scharcode string dest-idx) (aref u8-vector source-idx))
    (incf source-idx)
    (incf dest-idx)))

(defun %copy-string-to-u8 (string source-idx u8-vector dest-idx n)
  (declare (optimize (speed 3) (safety 0))
           (fixnum source-idx dest-idx n)
           (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (simple-base-string string))
  (do* ((i 0 (1+ i)))
       ((= i n) u8-vector)
    (declare (fixnum i))
    (let* ((code (%scharcode string source-idx)))
      (declare (type (mod #x11000) code))
      (if (> code #xff)
        (setq code (char-code #\Sub)))
      (setf (aref u8-vector dest-idx) code)
      (incf source-idx)
      (incf dest-idx))))
    
        


(defun append-2 (y z)
  (if (null y)
    z
    (let* ((new (cons (car y) nil))
           (tail new))
      (declare (list new tail))
      (dolist (head (cdr y))
        (setq tail (cdr (rplacd tail (cons head nil)))))
      (rplacd tail z)
      new)))









(defun dbg (&optional arg)
  (dbg arg))


; This takes a simple-base-string and passes a C string into
; the kernel "Bug" routine.  Not too fancy, but neither is #_DebugStr,
; and there's a better chance that users would see this message.
(defun bug (arg)
  (if (typep arg 'simple-base-string)
    #+x86-target
    (debug-trap-with-string arg)
    #-x86-target
    (let* ((len (length arg)))
      (%stack-block ((buf (1+ len)))
        (%cstr-pointer arg buf)
        (ff-call 
         (%kernel-import target::kernel-import-lisp-bug)
         :address buf
         :void)))
    (bug "Bug called with non-simple-base-string.")))

(defun total-bytes-allocated ()
  (%heap-bytes-allocated)
  #+not-any-more
  (+ (unsignedwide->integer *total-bytes-freed*)
     (%heap-bytes-allocated)))

(defun %freebytes ()
  (with-macptrs (p)
    (%setf-macptr-to-object p
                            (%fixnum-ref (%get-kernel-global 'all-areas)
                                         target::area.succ))
    (- (%get-natural p target::area.high)
       (%get-natural p target::area.active))))

(defun %reservedbytes ()
  (with-macptrs (p)
    (%setf-macptr-to-object p (%get-kernel-global 'all-areas))
    (- #+32-bit-target
       (%get-unsigned-long p target::area.high)
       #+64-bit-target
       (%%get-unsigned-longlong p target::area.high)
       #+32-bit-target
       (%get-unsigned-long p target::area.low)
       #+64-bit-target
       (%%get-unsigned-longlong p target::area.low))))

(defun object-in-application-heap-p (address)
  (declare (ignore address))
  t)

(defun frozen-space-dnodes ()
  "Returns the current size of the frozen area."
  (%fixnum-ref-natural (%get-kernel-global 'tenured-area)
                       target::area.static-dnodes))
(defun %usedbytes ()
  (with-lock-grabbed (*kernel-exception-lock*)
    (with-lock-grabbed (*kernel-tcr-area-lock*)
      (%normalize-areas)
      (let ((static 0)
            (dynamic 0)
            (library 0))
        (do-consing-areas (area)
          (let* ((active (%fixnum-ref area target::area.active))
                 (bytes (ash (- active
                                (%fixnum-ref area target::area.low))
                             target::fixnumshift))
                 (code (%fixnum-ref area target::area.code)))
            (when (object-in-application-heap-p active)
              (if (eql code area-dynamic)
                (incf dynamic bytes)
                (if (eql code area-managed-static)
                  (incf library bytes)
                  (incf static bytes))))))
        (let* ((frozen-size (ash (frozen-space-dnodes) target::dnode-shift)))
          (decf dynamic frozen-size)
          (values dynamic static library frozen-size))))))



(defun %stack-space ()
  (%normalize-areas)
  (let ((free 0)
        (used 0))
    (with-macptrs (p)
      (do-gc-areas (area)
	(when (member (%fixnum-ref area target::area.code)
		      '(#.area-vstack
			#.area-cstack
                      #.area-tstack))
	  (%setf-macptr-to-object p area)
	  (let ((active
                 #+32-bit-target
                  (%get-unsigned-long p target::area.active)
                  #+64-bit-target
                  (%%get-unsigned-longlong p target::area.active))
		(high
                 #+32-bit-target
                  (%get-unsigned-long p target::area.high)
                  #+64-bit-target
                  (%%get-unsigned-longlong p target::area.high))
		(low
                 #+32-bit-target
                 (%get-unsigned-long p target::area.low)
                 #+64-bit-target
                 (%%get-unsigned-longlong p target::area.low)))
	    (incf used (- high active))
	    (incf free (- active low))))))
    (values (+ free used) used free)))



; Returns an alist of the form:
; ((thread cstack-free cstack-used vstack-free vstack-used tstack-free tstack-used)
;  ...)
(defun %stack-space-by-lisp-thread ()
  (let* ((res nil))
    (without-interrupts
     (dolist (p (all-processes))
       (let* ((thread (process-thread p)))
         (when thread
           (push (cons thread (multiple-value-list (%thread-stack-space thread))) res)))))
    res))



;;; Returns six values.
;;;   sp free
;;;   sp used
;;;   vsp free
;;;   vsp used
;;;   tsp free
;;;   tsp used
(defun %thread-stack-space (&optional (thread *current-lisp-thread*))
  (when (eq thread *current-lisp-thread*)
    (%normalize-areas))
  (labels ((free-and-used (area)
	     (with-macptrs (p)
	       (%setf-macptr-to-object p area)
	       (let* ((low
                       #+32-bit-target
                       (%get-unsigned-long p target::area.low)
                       #+64-bit-target
                       (%%get-unsigned-longlong p target::area.low))
		      (high
                       #+32-bit-target
                        (%get-unsigned-long p target::area.high)
                        #+64-bit-target
                        (%%get-unsigned-longlong p target::area.high))
		      (active
                       #+32-bit-target
                       (%get-unsigned-long p target::area.active)
                       #+64-bit-target
                       (%%get-unsigned-longlong p target::area.active))
		      (free (- active low))
		      (used (- high active)))
		 (loop
		     (setq area (%fixnum-ref area target::area.older))
		     (when (eql area 0) (return))
		   (%setf-macptr-to-object p area)
		   (let ((low
                          #+32-bit-target
                           (%get-unsigned-long p target::area.low)
                           #+64-bit-target
                           (%%get-unsigned-longlong p target::area.low))
			 (high
                          #+32-bit-target
                           (%get-unsigned-long p target::area.high)
                           #+64-bit-target
                           (%%get-unsigned-longlong p target::area.high)))
		     (declare (fixnum low high))
		     (incf used (- high low))))
		 (values free used)))))
    (let* ((tcr (lisp-thread.tcr thread)))
      (if (or (null tcr)
	      (zerop (%fixnum-ref (%fixnum-ref tcr target::tcr.cs-area))))
	(values 0 0 0 0 0 0)
	(multiple-value-bind (cf cu) (free-and-used (%fixnum-ref tcr target::tcr.cs-area))
	  (multiple-value-bind (vf vu) (free-and-used (%fixnum-ref tcr target::tcr.vs-area))
	    (multiple-value-bind (tf tu) (free-and-used (%fixnum-ref tcr target::tcr.ts-area ))
	      (values cf cu vf vu tf tu))))))))


(defun room (&optional (verbose :default))
  "Print to *STANDARD-OUTPUT* information about the state of internal
  storage and its management. The optional argument controls the
  verbosity of output. If it is T, ROOM prints out a maximal amount of
  information. If it is NIL, ROOM prints out a minimal amount of
  information. If it is :DEFAULT or it is not supplied, ROOM prints out
  an intermediate amount of information."
  (let* ((freebytes nil)
         (usedbytes nil)
         (static-used nil)
         (staticlib-used nil)
         (frozen-space-size nil)
         (lispheap nil)
         (reserved nil)
         (static nil)
         (stack-total)
         (stack-used)
         (stack-free)
         (stack-used-by-thread nil))
    (progn
      (progn
        (setq freebytes (%freebytes))
        (when verbose
          (multiple-value-setq (usedbytes static-used staticlib-used frozen-space-size)
            (%usedbytes))
          (setq lispheap (+ freebytes usedbytes)
                reserved (%reservedbytes)
                static (+ static-used staticlib-used frozen-space-size))
          (multiple-value-setq (stack-total stack-used stack-free)
            (%stack-space))
          (unless (eq verbose :default)
            (setq stack-used-by-thread (%stack-space-by-lisp-thread))))))
    (format t "~&Approximately ~:D bytes of memory can be allocated ~%before the next full GC is triggered. ~%" freebytes)
    (when verbose
      (flet ((k (n) (round n 1024)))
        (princ "
                   Total Size             Free                 Used")
        (format t "~&Lisp Heap:~15t~10D (~DK)~35t~10D (~DK)~55t~10D (~DK)"
                lispheap (k lispheap)
                freebytes (k freebytes)
                usedbytes (k usedbytes))
        (format t "~&Stacks:~15t~10D (~DK)~35t~10D (~DK)~55t~10D (~DK)"
                stack-total (k stack-total)
                stack-free (k stack-free)
                stack-used (k stack-used))
        (format t "~&Static:~15t~10D (~DK)~35t~10D (~DK)~55t~10D (~DK)"
                static (k static)
                0 0
                static (k static))
        (when (and frozen-space-size (not (zerop frozen-space-size)))
          (format t "~&~,3f MB of static memory is \"frozen\" dynamic memory"
                  (/ frozen-space-size (float (ash 1 20)))))
        (format t "~&~,3f MB reserved for heap expansion."
                (/ reserved (float (ash 1 20))))
        (unless (eq verbose :default)
          (terpri)
          (let* ((processes (all-processes)))
            (dolist (thread-info stack-used-by-thread)
              (destructuring-bind (thread sp-free sp-used vsp-free vsp-used tsp-free tsp-used)
                  thread-info
                (let* ((process (dolist (p processes)
                                  (when (eq (process-thread p) thread)
                                    (return p)))))
                  (when process
                    (let ((sp-total (+ sp-used sp-free))
                          (vsp-total (+ vsp-used vsp-free))
                          (tsp-total (+ tsp-used tsp-free)))
                      (format t "~%~a(~d)~%  cstack:~12T~10D (~DK)  ~33T~10D (~DK)  ~54T~10D (~DK)~
                               ~%  vstack:~12T~10D (~DK)  ~33T~10D (~DK)  ~54T~10D (~DK)~
                               ~%  tstack:~12T~10D (~DK)  ~33T~10D (~DK)  ~54T~10D (~DK)"
                              (process-name process)
                              (process-serial-number process)
                              sp-total (k sp-total) sp-free (k sp-free) sp-used (k sp-used)
                              vsp-total (k vsp-total) vsp-free (k vsp-free) vsp-used (k vsp-used)
                              tsp-total (k tsp-total) tsp-free (k tsp-free) tsp-used (k tsp-used)))))))))))))


(defun list-length (l)
  "Return the length of the given LIST, or NIL if the LIST is circular."
  (do* ((n 0 (+ n 2))
        (fast l (cddr fast))
        (slow l (cdr slow)))
       ((null fast) n)
    (declare (fixnum n))
    (if (null (cdr fast))
      (return (the fixnum (1+ n)))
      (if (and (eq fast slow)
               (> n 0))
        (return nil)))))

(defun proper-list-p (l)
  (and (typep l 'list)
       (do* ((n 0 (+ n 2))
             (fast l (if (and (listp fast) (listp (cdr fast)))
                       (cddr fast)
                       (return-from proper-list-p nil)))
             (slow l (cdr slow)))
            ((null fast) n)
         (declare (fixnum n))
         (if (atom fast)
           (return nil)
           (if (null (cdr fast))
             (return t)
             (if (and (eq fast slow)
                      (> n 0))
               (return nil)))))))

(defun proper-sequence-p (x)
  (cond ((typep x 'vector))
	((typep x 'list) (not (null (list-length x))))))


(defun length (seq)
  "Return an integer that is the length of SEQUENCE."
  (seq-dispatch
   seq
   (or (list-length seq)
       (%err-disp $XIMPROPERLIST seq))
   (if (= (the fixnum (typecode seq)) target::subtag-vectorH)
     (%svref seq target::vectorH.logsize-cell)
     (uvsize seq))))

(defun %str-from-ptr (pointer len &optional (dest (make-string len)))
  (declare (fixnum len)
           (optimize (speed 3) (safety 0)))
  (dotimes (i len dest)
    (setf (%scharcode dest i) (%get-unsigned-byte pointer i))))

(defun %get-cstring (pointer)
  (do* ((end 0 (1+ end)))
       ((zerop (the (unsigned-byte 8) (%get-unsigned-byte pointer end)))
        (%str-from-ptr pointer end))
    (declare (fixnum end))))

(defun %get-utf-8-cstring (pointer)
  (do* ((end 0 (1+ end)))
       ((zerop (the (unsigned-byte 8) (%get-unsigned-byte pointer end)))
        (let* ((len (utf-8-length-of-memory-encoding pointer end 0))
               (string (make-string len)))
          (utf-8-memory-decode pointer end 0 string)
          string))
    (declare (fixnum end))))

;;; Assumes that pointer is terminated by a 0-valued 16-bit word
;;; and that it points to a valid utf-16 string with native endianness.
(defun %get-native-utf-16-cstring (pointer)
  (do* ((nchars 0 (1+ nchars))
        (i 0 (+ i 2))
        (code (%get-unsigned-word pointer i) (%get-unsigned-word pointer i)))
       ((zerop code)
        (do* ((string (make-string nchars))
              (out 0 (1+ out))
              (i 0 (+ i 2)))
             ((= out nchars) string)
          (declare (fixnum i out))
          (let* ((code (%get-unsigned-word pointer i)))
            (declare (type (unsigned-byte 16) code))
            (when (and (>= code #xd800)
                       (< code #xdc00))
              (incf i 2)
              (let* ((code2 (%get-unsigned-word pointer i)))
                (declare (type (unsigned-byte 16) code2))
                (setq code (utf-16-combine-surrogate-pairs code code2))))
            (setf (schar string out) (code-char code)))))
    (when (and (>= code #xd800) (< code #xdc00))
      (incf i 2))))


;;; This is mostly here so we can bootstrap shared libs without
;;; having to bootstrap #_strcmp.
;;; Return true if the cstrings are equal, false otherwise.
(defun %cstrcmp (x y)
  (do* ((i 0 (1+ i))
	(bx (%get-byte x i) (%get-byte x i))
	(by (%get-byte y i) (%get-byte y i)))
       ((not (= bx by)))
    (declare (fixnum i bx by))
    (when (zerop bx)
      (return t))))

(defun %cnstrcmp (x y n)
  (declare (fixnum n))
  (do* ((i 0 (1+ i))
	(bx (%get-byte x i) (%get-byte x i))
	(by (%get-byte y i) (%get-byte y i)))
       ((= i n) t)
    (declare (fixnum i bx by))
    (unless (= bx by)
      (return))))

(defvar %documentation nil)

(defvar %documentation-lock% nil)

(setq %documentation
  (make-hash-table :weak t :size 100 :test 'eq :rehash-threshold .95)
  %documentation-lock% (make-lock))

(defun %put-documentation (thing doc-id doc)
  (with-lock-grabbed (%documentation-lock%)
    (let* ((info (gethash thing %documentation))
	   (pair (assoc doc-id info)))
      (if doc
        (progn
          (unless (typep doc 'string)
            (report-bad-arg doc 'string))
          (if pair
            (setf (cdr pair) doc)
            (setf (gethash thing %documentation) (cons (cons doc-id doc) info))))
	(when pair
	  (if (setq info (nremove pair info))
	    (setf (gethash thing %documentation) info)
	    (remhash thing %documentation))))))
  doc)

(defun %get-documentation (object doc-id)
  (cdr (assoc doc-id (gethash object %documentation))))

;;; This pretends to be (SETF DOCUMENTATION), until that generic function
;;; is defined.  It handles a few common cases.
(defun %set-documentation (thing doc-id doc-string)
  (case doc-id
    (function 
     (if (typep thing 'function)
       (%put-documentation thing t doc-string)
       (if (typep thing 'symbol)
         (let* ((def (fboundp thing)))
           (if def
             (%put-documentation def t doc-string)))
         (if (setf-function-name-p thing)
           (%set-documentation
            (setf-function-name thing) doc-id doc-string)))))
    (variable
     (if (typep thing 'symbol)
       (%put-documentation thing doc-id doc-string)))
    (t (%put-documentation thing doc-id doc-string)))
  doc-string)


(%fhave 'set-documentation #'%set-documentation)



;;; This is intended for use by debugging tools.  It's a horrible thing
;;; to do otherwise.  The caller really needs to hold the heap-segment
;;; lock; this grabs the tcr queue lock as well.


(defparameter *spin-lock-tries* 1)
(defparameter *spin-lock-timeouts* 0)

#+(and (not futex) (not x86-target))
(defun %get-spin-lock (p)
  (let* ((self (%current-tcr))
         (n *spin-lock-tries*))
    (declare (fixnum n))
    (loop
      (dotimes (i n)
        (when (eql 0 (%ptr-store-fixnum-conditional p 0 self))
          (return-from %get-spin-lock t)))
      (%atomic-incf-node 1 '*spin-lock-timeouts* target::symbol.vcell)
      (yield))))

(eval-when (:compile-toplevel :execute)
  (declaim (inline note-lock-wait note-lock-held note-lock-released)))





#-futex
(defun %lock-recursive-lock-object (lock &optional flag)
  (let* ((ptr (recursive-lock-ptr lock)))
    (with-macptrs ((p)
                   (owner (%get-ptr ptr target::lockptr.owner))
                   (signal (%get-ptr ptr target::lockptr.signal))
                   (spin (%inc-ptr ptr target::lockptr.spinlock)))
      (%setf-macptr-to-object p (%current-tcr))
      (if (istruct-typep flag 'lock-acquisition)
        (setf (lock-acquisition.status flag) nil)
        (if flag (report-bad-arg flag 'lock-acquisition)))
      (loop
        (without-interrupts
         (when (eql p owner)
           (incf (%get-natural ptr target::lockptr.count))
           (when flag
             (setf (lock-acquisition.status flag) t))
           (return t))
         (%get-spin-lock spin)
         (when (eql 1 (incf (%get-natural ptr target::lockptr.avail)))
           (setf (%get-ptr ptr target::lockptr.owner) p
                 (%get-natural ptr target::lockptr.count) 1)
           (setf (%get-natural spin 0) 0)
           (if flag
             (setf (lock-acquisition.status flag) t))
           (return t))
         (setf (%get-natural spin 0) 0))
        (%process-wait-on-semaphore-ptr signal 1 0 (recursive-lock-whostate lock))))))



#+futex
(progn
  #-monitor-futex-wait
  (defun futex-wait (p val whostate)
    (with-process-whostate (whostate)
      (int-errno-ffcall
       (%kernel-import target::kernel-import-lisp-futex)
       :address p :int FUTEX-WAIT :int val :address (%null-ptr) :address (%null-ptr) :int 0 :int)))
  #+monitor-futex-wait
  (progn
    (defparameter *total-futex-wait-calls* 0)
    (defparameter *total-futex-wait-times* 0)
    (defun futex-wait (p val whostate)
      (with-process-whostate (whostate)
        (let* ((start (get-internal-real-time)))
          (incf *total-futex-wait-calls*)
          (int-errno-ffcall
           (%kernel-import target::kernel-import-lisp-futex)
           :address p :int FUTEX-WAIT :int val :address (%null-ptr) :address (%null-ptr) :int 0 :int)
          (incf *total-futex-wait-times* (- (get-internal-real-time) start)))))))
    



#+futex
(defun futex-wake (p n)
  (int-errno-ffcall (%kernel-import target::kernel-import-lisp-futex)
                    :address p :int FUTEX-WAKE :int n :address (%null-ptr) :address (%null-ptr) :int 0 :int))

#+futex
(defun %lock-futex (p wait-level lock fwhostate)
  (let* ((val (%ptr-store-conditional p futex-avail futex-locked)))
    (declare (fixnum val))
    (or (eql val futex-avail)
        (loop
          (if (eql val futex-contended)
            (let* ((*interrupt-level* wait-level))
              (futex-wait p val (if fwhostate (funcall fwhostate lock) "futex wait")))
            (setq val futex-contended))
          (when (eql futex-avail (xchgl val p))
            (return t))))))

#+futex
(defun %unlock-futex (p)
  (unless (eql futex-avail (%atomic-decf-ptr p))
    (setf (%get-natural p target::lockptr.avail) futex-avail)
    (futex-wake p #$INT_MAX)))




#+futex
(defun %lock-recursive-lock-object (lock &optional flag)
  (if (istruct-typep flag 'lock-acquisition)
    (setf (lock-acquisition.status flag) nil)
    (if flag (report-bad-arg flag 'lock-acquisition)))
  (let* ((self (%current-tcr))
         (level *interrupt-level*)
         (ptr (recursive-lock-ptr lock)))
    (declare (fixnum self))
    (without-interrupts
     (cond ((eql self (%get-object ptr target::lockptr.owner))
            (incf (%get-natural ptr target::lockptr.count)))
           (t (%lock-futex ptr level lock #'recursive-lock-whostate)
              (%set-object ptr target::lockptr.owner self)
              (setf (%get-natural ptr target::lockptr.count) 1)))
     (when flag
       (setf (lock-acquisition.status flag) t))
     t)))

          




#-futex
(defun %try-recursive-lock-object (lock &optional flag)
  (let* ((ptr (recursive-lock-ptr lock)))
    (with-macptrs ((p)
                   (owner (%get-ptr ptr target::lockptr.owner))
                   (spin (%inc-ptr ptr target::lockptr.spinlock)))
      (%setf-macptr-to-object p (%current-tcr))
      (if flag
        (if (istruct-typep flag 'lock-acquisition)
          (setf (lock-acquisition.status flag) nil)
          (report-bad-arg flag 'lock-acquisition)))
      (without-interrupts
       (cond ((eql p owner)
              (incf (%get-natural ptr target::lockptr.count))
              (if flag (setf (lock-acquisition.status flag) t))
              t)
             (t
              (let* ((win nil))
                (%get-spin-lock spin)
                (when (setq win (eql 1 (incf (%get-natural ptr target::lockptr.avail))))
                  (setf (%get-ptr ptr target::lockptr.owner) p
                        (%get-natural ptr target::lockptr.count) 1)
                  (if flag (setf (lock-acquisition.status flag) t)))
                (setf (%get-ptr spin) (%null-ptr))
                win)))))))



#+futex
(defun %try-recursive-lock-object (lock &optional flag)
  (let* ((self (%current-tcr))
         (ptr (recursive-lock-ptr lock)))
    (declare (fixnum self))
    (if flag
      (if (istruct-typep flag 'lock-acquisition)
        (setf (lock-acquisition.status flag) nil)
        (report-bad-arg flag 'lock-acquisition)))
    (without-interrupts
     (cond ((eql (%get-object ptr target::lockptr.owner) self)
            (incf (%get-natural ptr target::lockptr.count))
            (if flag (setf (lock-acquisition.status flag) t))
            t)
           (t
            (when (eql 0 (%ptr-store-conditional ptr futex-avail futex-locked))
              (%set-object ptr target::lockptr.owner self)
              (setf (%get-natural ptr target::lockptr.count) 1)
              (if flag (setf (lock-acquisition.status flag) t))
              t))))))





#-futex
(defun %unlock-recursive-lock-object (lock)
  (let* ((ptr (%svref lock target::lock._value-cell)))
    (with-macptrs ((signal (%get-ptr ptr target::lockptr.signal))
                   (spin (%inc-ptr ptr target::lockptr.spinlock)))
      (unless (eql (%get-object ptr target::lockptr.owner) (%current-tcr))
        (error 'not-lock-owner :lock lock))
      (without-interrupts
       (when (eql 0 (decf (the fixnum
                            (%get-natural ptr target::lockptr.count))))
         (%get-spin-lock spin)
         (setf (%get-ptr ptr target::lockptr.owner) (%null-ptr))
         (let* ((pending (+ (the fixnum
                              (1- (the fixnum (%get-fixnum ptr target::lockptr.avail))))
                            (the fixnum (%get-fixnum ptr target::lockptr.waiting)))))
           (declare (fixnum pending))
           (setf (%get-natural ptr target::lockptr.avail) 0
                 (%get-natural ptr target::lockptr.waiting) 0)
           (decf pending)
           (if (> pending 0)
             (setf (%get-natural ptr target::lockptr.waiting) pending))
           (setf (%get-ptr spin) (%null-ptr))
           (if (>= pending 0)
             (%signal-semaphore-ptr signal)))))))
  nil)



#+futex
(defun %unlock-recursive-lock-object (lock)
  (let* ((ptr (%svref lock target::lock._value-cell)))
    (unless (eql (%get-object ptr target::lockptr.owner) (%current-tcr))
      (error 'not-lock-owner :lock lock))
    (without-interrupts
     (when (eql 0 (decf (the fixnum
                          (%get-natural ptr target::lockptr.count))))
    (setf (%get-natural ptr target::lockptr.owner) 0)
    (%unlock-futex ptr))))
  nil)




(defun %%lock-owner (lock)
  "Intended for debugging only; ownership may change while this code
   is running."
  (let* ((tcr (%get-object (recursive-lock-ptr lock) target::lockptr.owner)))
    (unless (zerop tcr)
      (tcr->process tcr))))

 
  




(defun %rplaca-conditional (cons-cell old new)
  (%store-node-conditional target::cons.car cons-cell old new))

(defun %rplacd-conditional (cons-cell old new)
  (%store-node-conditional target::cons.cdr cons-cell old new))

;;; Atomically push NEW onto the list in the I'th cell of uvector V.

(defun atomic-push-uvector-cell (v i new)
  (let* ((cell (cons new nil))
         (offset (+ target::misc-data-offset (ash i target::word-shift))))
    (loop
      (let* ((old (%svref v i)))
        (rplacd cell old)
        (when (%store-node-conditional offset v old cell)
          (return cell))))))

(defun atomic-pop-uvector-cell (v i)
  (let* ((offset (+ target::misc-data-offset (ash i target::word-shift))))
    (loop
      (let* ((old (%svref v i)))
        (if (null old)
          (return (values nil nil))
          (let* ((tail (cdr old)))
            (when (%store-node-conditional offset v old tail)
              (return (values (car old) t)))))))))


(defun store-gvector-conditional (index gvector old new)
  (%store-node-conditional (+ target::misc-data-offset
			      (ash index target::word-shift))
			   gvector
			   old
			   new))

(defun %atomic-incf-car (cell &optional (by 1))
  (%atomic-incf-node (require-type by 'fixnum)
		     (require-type cell 'cons)
		     target::cons.car))

(defun %atomic-incf-cdr (cell &optional (by 1))
  (%atomic-incf-node (require-type by 'fixnum)
		     (require-type cell 'cons)
		     target::cons.cdr))

(defun %atomic-incf-gvector (v i &optional (by 1))
  (setq v (require-type v 'gvector))
  (setq i (require-type i 'fixnum))
  (%atomic-incf-node by v (+ target::misc-data-offset (ash i target::word-shift))))

(defun %atomic-incf-symbol-value (s &optional (by 1))
  (setq s (require-type s 'symbol))
  (multiple-value-bind (base offset) (%symbol-binding-address s)
    (%atomic-incf-node by base offset)))

;;; What happens if there are some pending readers and another writer,
;;; and we abort out of the semaphore wait ?  If the writer semaphore is
;;; signaled before we abandon interest in it
#-futex
(defun %write-lock-rwlock-ptr (ptr lock &optional flag)
  (with-macptrs ((write-signal (%get-ptr ptr target::rwlock.writer-signal)) )
    (if (istruct-typep flag 'lock-acquisition)
      (setf (lock-acquisition.status flag) nil)
      (if flag (report-bad-arg flag 'lock-acquisition)))
    (let* ((level *interrupt-level*)
           (tcr (%current-tcr)))
      (declare (fixnum tcr))
      (without-interrupts
       (%get-spin-lock ptr)               ;(%get-spin-lock (%inc-ptr ptr target::rwlock.spin))
       (if (eq (%get-object ptr target::rwlock.writer) tcr)
         (progn
           (incf (%get-signed-natural ptr target::rwlock.state))
           (setf (%get-natural ptr target::rwlock.spin) 0)
           (if flag
             (setf (lock-acquisition.status flag) t))
           t)
         (do* ()
              ((eql 0 (%get-signed-natural ptr target::rwlock.state))
               ;; That wasn't so bad, was it ?  We have the spinlock now.
               (setf (%get-signed-natural ptr target::rwlock.state) 1
                     (%get-natural ptr target::rwlock.spin) 0)
               (%set-object ptr target::rwlock.writer tcr)
               (if flag
                 (setf (lock-acquisition.status flag) t))
               t)
           (incf (%get-natural ptr target::rwlock.blocked-writers))
           (setf (%get-natural ptr target::rwlock.spin) 0)
           (let* ((*interrupt-level* level))
                  (%process-wait-on-semaphore-ptr write-signal 1 0 (rwlock-write-whostate lock)))
           (%get-spin-lock ptr)))))))
#+futex
(defun %write-lock-rwlock-ptr (ptr lock &optional flag)
  (with-macptrs ((write-signal (%INC-ptr ptr target::rwlock.writer-signal)) )
    (if (istruct-typep flag 'lock-acquisition)
      (setf (lock-acquisition.status flag) nil)
      (if flag (report-bad-arg flag 'lock-acquisition)))
    (let* ((level *interrupt-level*)
           (tcr (%current-tcr)))
      (declare (fixnum tcr))
      (without-interrupts
       (%lock-futex ptr level lock nil)
       (if (eq (%get-object ptr target::rwlock.writer) tcr)
         (progn
           (incf (%get-signed-natural ptr target::rwlock.state))
           (%unlock-futex ptr)
           (if flag
             (setf (lock-acquisition.status flag) t))
           t)
         (do* ()
              ((eql 0 (%get-signed-natural ptr target::rwlock.state))
               ;; That wasn't so bad, was it ?  We have the spinlock now.
               (setf (%get-signed-natural ptr target::rwlock.state) 1)
               (%unlock-futex ptr)
               (%set-object ptr target::rwlock.writer tcr)
               (if flag
                 (setf (lock-acquisition.status flag) t))
               t)
           (incf (%get-natural ptr target::rwlock.blocked-writers))
           (let* ((waitval (%get-natural write-signal 0)))
             (%unlock-futex ptr)
             (with-process-whostate ((rwlock-write-whostate lock))
               (let* ((*interrupt-level* level))
                 (futex-wait write-signal waitval (rwlock-write-whostate lock)))))
           (%lock-futex ptr level lock nil)
           (decf (%get-natural ptr target::rwlock.blocked-writers))))))))



(defun write-lock-rwlock (lock &optional flag)
  (%write-lock-rwlock-ptr (read-write-lock-ptr lock) lock flag))

#-futex
(defun %read-lock-rwlock-ptr (ptr lock &optional flag)
  (with-macptrs ((read-signal (%get-ptr ptr target::rwlock.reader-signal)))
    (if (istruct-typep flag 'lock-acquisition)
      (setf (lock-acquisition.status flag) nil)
      (if flag (report-bad-arg flag 'lock-acquisition)))
    (let* ((level *interrupt-level*)
           (tcr (%current-tcr)))
      (declare (fixnum tcr))
      (without-interrupts
       (%get-spin-lock ptr)             ;(%get-spin-lock (%inc-ptr ptr target::rwlock.spin))
       (if (eq (%get-object ptr target::rwlock.writer) tcr)
         (progn
           (setf (%get-natural ptr target::rwlock.spin) 0)
           (error 'deadlock :lock lock))
         (do* ((state
                (%get-signed-natural ptr target::rwlock.state)
                (%get-signed-natural ptr target::rwlock.state)))
              ((<= state 0)
               ;; That wasn't so bad, was it ?  We have the spinlock now.
               (setf (%get-signed-natural ptr target::rwlock.state)
                     (the fixnum (1- state))
                     (%get-natural ptr target::rwlock.spin) 0)
               (if flag
                 (setf (lock-acquisition.status flag) t))
               t)
           (declare (fixnum state))
           (incf (%get-natural ptr target::rwlock.blocked-readers))
           (setf (%get-natural ptr target::rwlock.spin) 0)
           (let* ((*interrupt-level* level))
             (%process-wait-on-semaphore-ptr read-signal 1 0 (rwlock-read-whostate lock)))
           (%get-spin-lock ptr)))))))

#+futex
(defun %read-lock-rwlock-ptr (ptr lock &optional flag) 
  (with-macptrs ((reader-signal (%INC-ptr ptr target::rwlock.reader-signal)))
    (if (istruct-typep flag 'lock-acquisition)
      (setf (lock-acquisition.status flag) nil)
      (if flag (report-bad-arg flag 'lock-acquisition)))
    (let* ((level *interrupt-level*)
           (tcr (%current-tcr)))
      (declare (fixnum tcr))
      (without-interrupts
       (%lock-futex ptr level lock nil)
       (if (eq (%get-object ptr target::rwlock.writer) tcr)
         (progn
           (%unlock-futex ptr)
           (error 'deadlock :lock lock))
         (do* ((state
                (%get-signed-natural ptr target::rwlock.state)
                (%get-signed-natural ptr target::rwlock.state)))
              ((<= state 0)
               ;; That wasn't so bad, was it ?  We have the spinlock now.
               (setf (%get-signed-natural ptr target::rwlock.state)
                     (the fixnum (1- state)))
               (%unlock-futex ptr)
               (if flag
                 (setf (lock-acquisition.status flag) t))
               t)
           (declare (fixnum state))
           (incf (%get-natural ptr target::rwlock.blocked-readers))
           (let* ((waitval (%get-natural reader-signal 0)))
             (%unlock-futex ptr)
             (let* ((*interrupt-level* level))
               (futex-wait reader-signal waitval (rwlock-read-whostate lock))))
           (%lock-futex ptr level lock nil)
           (decf (%get-natural ptr target::rwlock.blocked-readers))))))))



(defun read-lock-rwlock (lock &optional flag)
  (%read-lock-rwlock-ptr (read-write-lock-ptr lock) lock flag))



#-futex
(defun %unlock-rwlock-ptr (ptr lock)
  (with-macptrs ((reader-signal (%get-ptr ptr target::rwlock.reader-signal))
                 (writer-signal (%get-ptr ptr target::rwlock.writer-signal)))
    (without-interrupts
     (%get-spin-lock ptr)
     (let* ((state (%get-signed-natural ptr target::rwlock.state))
            (tcr (%current-tcr)))
       (declare (fixnum state tcr))
       (cond ((> state 0)
              (unless (eql tcr (%get-object ptr target::rwlock.writer))
                (setf (%get-natural ptr target::rwlock.spin) 0)
                (error 'not-lock-owner :lock lock))
              (decf state))
             ((< state 0) (incf state))
             (t (setf (%get-natural ptr target::rwlock.spin) 0)
                (error 'not-locked :lock lock)))
       (setf (%get-signed-natural ptr target::rwlock.state) state)
       (when (zerop state)
         ;; We want any thread waiting for a lock semaphore to
         ;; be able to wait interruptibly.  When a thread waits,
         ;; it increments either the "blocked-readers" or "blocked-writers"
         ;; field, but since it may get interrupted before obtaining
         ;; the semaphore that's more of "an expression of interest"
         ;; in taking the lock than it is "a firm commitment to take it."
         ;; It's generally (much) better to signal the semaphore(s)
         ;; too often than it would be to not signal them often
         ;; enough; spurious wakeups are better than deadlock.
         ;; So: if there are blocked writers, the writer-signal
         ;; is raised once for each apparent blocked writer.  (At most
         ;; one writer will actually succeed in taking the lock.)
         ;; If there are blocked readers, the reader-signal is raised
         ;; once for each of them.  (It's possible for both the
         ;; reader and writer semaphores to be raised on the same
         ;; unlock; the writer semaphore is raised first, so in that
         ;; sense, writers still have priority but it's not guaranteed.)
         ;; Both the "blocked-writers" and "blocked-readers" fields
         ;; are cleared here (they can't be changed from another thread
         ;; until this thread releases the spinlock.)
         (setf (%get-signed-natural ptr target::rwlock.writer) 0)
         (let* ((nwriters (%get-natural ptr target::rwlock.blocked-writers))
                (nreaders (%get-natural ptr target::rwlock.blocked-readers)))
           (declare (fixnum nreaders nwriters))
           (when (> nwriters 0)
             (setf (%get-natural ptr target::rwlock.blocked-writers) 0)
             (dotimes (i nwriters)
               (%signal-semaphore-ptr writer-signal)))
           (when (> nreaders 0)
             (setf (%get-natural ptr target::rwlock.blocked-readers) 0)
             (dotimes (i nreaders)
               (%signal-semaphore-ptr reader-signal)))))
       (setf (%get-natural ptr target::rwlock.spin) 0)
       t))))

#+futex
(defun %unlock-rwlock-ptr (ptr lock)
  (with-macptrs ((reader-signal (%INC-ptr ptr target::rwlock.reader-signal))
                 (writer-signal (%INC-ptr ptr target::rwlock.writer-signal)))
    (let* ((signal nil)
           (wakeup 0))
    (without-interrupts
     (%lock-futex ptr -1 lock nil)
     (let* ((state (%get-signed-natural ptr target::rwlock.state))
            (tcr (%current-tcr)))
       (declare (fixnum state tcr))
       (cond ((> state 0)
              (unless (eql tcr (%get-object ptr target::rwlock.writer))
                (%unlock-futex ptr)
                (error 'not-lock-owner :lock lock))
              (decf state))
             ((< state 0) (incf state))
             (t (%unlock-futex ptr)
                (error 'not-locked :lock lock)))
       (setf (%get-signed-natural ptr target::rwlock.state) state)
       (when (zerop state)
         (setf (%get-signed-natural ptr target::rwlock.writer) 0)
         (let* ((nwriters (%get-natural ptr target::rwlock.blocked-writers))
                (nreaders (%get-natural ptr target::rwlock.blocked-readers)))
           (declare (fixnum nreaders nwriters))
           (if (> nwriters 0)
             (setq signal writer-signal wakeup 1)
             (if (> nreaders 0)
               (setq signal reader-signal wakeup #$INT_MAX)))))
       (when signal (incf (%get-signed-natural signal 0)))
       (%unlock-futex ptr)
       (when signal (futex-wake signal wakeup))
       t)))))


(defun unlock-rwlock (lock)
  (%unlock-rwlock-ptr (read-write-lock-ptr lock) lock))

;;; There are all kinds of ways to lose here.
;;; The caller must have read access to the lock exactly once,
;;; or have write access.
;;; there's currently no way to detect whether the caller has
;;; read access at all.
;;; If we have to block and get interrupted, cleanup code may
;;; try to unlock a lock that we don't hold. (It might be possible
;;; to circumvent that if we use the same notifcation object here
;;; that controls that cleanup process.)

(defun %promote-rwlock (lock &optional flag)
  (let* ((ptr (read-write-lock-ptr lock)))
    (if (istruct-typep flag 'lock-acquisition)
      (setf (lock-acquisition.status flag) nil)
      (if flag (report-bad-arg flag 'lock-acquisition)))
    (let* ((level *interrupt-level*)
           (tcr (%current-tcr)))
      (without-interrupts
       #+futex
       (%lock-futex ptr level lock nil)
       #-futex
       (%get-spin-lock ptr)
       (let* ((state (%get-signed-natural ptr target::rwlock.state)))
         (declare (fixnum state))
         (cond ((> state 0)
                (unless (eql (%get-object ptr target::rwlock.writer) tcr)
                  #+futex
                  (%unlock-futex ptr)
                  #-futex
                  (setf (%get-natural ptr target::rwlock.spin) 0)
                  (error :not-lock-owner :lock lock)))
               ((= state 0)
                #+futex (%unlock-futex ptr)
                #-futex (setf (%get-natural ptr target::rwlock.spin) 0)
                (error :not-locked :lock lock))
               (t
                (if (= state -1)
                  (progn
                    (setf (%get-signed-natural ptr target::rwlock.state) 1)
                    (%set-object ptr target::rwlock.writer tcr)
                    #+futex
                    (%unlock-futex ptr)
                    #-futex
                    (setf (%get-natural ptr target::rwlock.spin) 0)
                    (if flag
                      (setf (lock-acquisition.status flag) t))
                    t)
                  (progn                    
                    #+futex
                    (%unlock-futex ptr)
                    #-futex
                    (setf (%get-natural ptr target::rwlock.spin) 0)
                    (%unlock-rwlock-ptr ptr lock)
                    (let* ((*interrupt-level* level))
                      (%write-lock-rwlock-ptr ptr lock flag)))))))))))
                      


(defun safe-get-ptr (p &optional dest)
  (if (null dest)
    (setq dest (%null-ptr))
    (unless (typep dest 'macptr)
      (check-type dest macptr)))
  (without-interrupts                   ;reentrancy
   (%safe-get-ptr p dest)))


;;; Useless for anything but using RLET in early level-1 code without
;;; having to bootstrap canonical type ordinals.
(%fhave 'parse-foreign-type (lambda (spec) (declare (ignore spec))))
(%fhave 'foreign-type-ordinal (lambda (thing) (declare (ignore thing)) 0))
(%fhave '%foreign-type-or-record (lambda (x) (declare (ignore x))))
