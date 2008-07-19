;;;-*-Mode: LISP; Package: CCL -*-
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

;; L1-aprims.lisp


(in-package "CCL")


(let* ((standard-initial-bindings ())
       (standard-initial-bindings-lock (make-read-write-lock)))

  (defun standard-initial-bindings ()
    (with-read-lock (standard-initial-bindings-lock)
      (copy-list standard-initial-bindings)))

  (defun define-standard-initial-binding (symbol initform)
    (setq symbol (require-type symbol 'symbol))
    (%proclaim-special symbol)
    (unless (boundp symbol)
      (set symbol (funcall initform)))
    (with-write-lock (standard-initial-bindings-lock)
      (let* ((pair (assoc symbol standard-initial-bindings)))
	(if pair
	  (setf (cdr pair) initform)
	  (push (cons symbol initform) standard-initial-bindings))))
    (record-source-file symbol 'variable)
    symbol))

(def-standard-initial-binding *package*)
(def-standard-initial-binding *gensym-counter* 0)
(def-standard-initial-binding *random-state* (initialize-random-state #xFBF1 9))
#+lock-accounting
(progn
(def-standard-initial-binding *locks-held* ())
(def-standard-initial-binding *locks-pending* ())
(def-standard-initial-binding *lock-conses* (make-list 20)))
(def-standard-initial-binding *whostate* "Reset")
(setq *whostate* "Active")
(def-standard-initial-binding *error-print-length* 20)
(def-standard-initial-binding *error-print-level* 8)

(defun %badarg (arg type)
  (%err-disp $XWRONGTYPE arg type))

(defun atom (arg)
  "Return true if OBJECT is an ATOM, and NIL otherwise."
  (not (consp arg)))

(defun list (&rest args)
  "Return constructs and returns a list of its arguments."
  args)

(%fhave '%temp-list #'list)

(defun list* (arg &rest others)
  "Return a list of the arguments with last cons a dotted pair"
  (cond ((null others) arg)
	((null (cdr others)) (cons arg (car others)))
	(t (do ((x others (cdr x)))
	       ((null (cddr x)) (rplacd x (cadr x))))
	   (cons arg others))))



(defun funcall (fn &rest args)
  "Call FUNCTION with the given ARGUMENTS."
  (declare (dynamic-extent args))
  (apply fn args))


(defun apply (function arg &rest args)
  "Apply FUNCTION to a list of arguments produced by evaluating ARGUMENTS in
   the manner of LIST*. That is, a list is made of the values of all but the
   last argument, appended to the value of the last argument, which must be a
   list."
  (declare (dynamic-extent args))
  (cond ((null args)
	 (apply function arg))
	((null (cdr args))
	 (apply function arg (car args)))
	(t (do* ((a1 args a2)
		 (a2 (cdr args) (cdr a2)))
		((atom (cdr a2))
		 (rplacd a1 (car a2))
		 (apply function arg args))))))


;;; This is not fast, but it gets the functionality that
;;; Wood and possibly other code depend on.
(defun applyv (function arg &rest other-args)
  (declare (dynamic-extent other-args))
  (let* ((other-args (cons arg other-args))
	 (last-arg (car (last other-args)))
	 (last-arg-length (length last-arg))
	 (butlast-args (nbutlast other-args))
	 (rest-args (make-list last-arg-length))
	 (rest-args-tail rest-args))
    (declare (dynamic-extent other-args rest-args))
    (dotimes (i last-arg-length)
      (setf (car rest-args-tail) (aref last-arg i))
      (pop rest-args-tail))
    (apply function (nconc butlast-args rest-args))))

;;; This is slow, and since %apply-lexpr isn't documented either,
;;; nothing in the world should depend on it.  This is just being
;;; anal retentive.  VERY anal retentive.

(defun %apply-lexpr (function arg &rest args)
  (cond ((null args) (%apply-lexpr function arg))
        (t (apply function arg (nconc (nbutlast args)
                                      (collect-lexpr-args (car (last args)) 0))))))


(defun values-list (arg)
  "Return all of the elements of LIST, in order, as values."
  (apply #'values arg))






; copy-list

(defun copy-list (list)
  "Return a new list which is EQUAL to LIST."
  (if list
    (let ((result (cons (car list) '()) ))
      (do ((x (cdr list) (cdr x))
           (splice result
                   (%cdr (%rplacd splice (cons (%car x) '() ))) ))
          ((atom x) (unless (null x)
                      (%rplacd splice x)) result)))))

(defun alt-list-length (l)
  "Detect (and complain about) cirucular lists; allow any atom to
terminate the list"
  (do* ((n 0 (1+ n))
        (fast l)
        (slow l))
       ((atom fast) n)
    (declare (fixnum n))
    (setq fast (cdr fast))
    (if (logbitp 0 n)
      (if (eq (setq slow (cdr slow)) fast)
	(%err-disp $XIMPROPERLIST l)))))


(defun last (list &optional (n 1))
  "Return the last N conses (not the last element!) of a list."
  (if (and (typep n 'fixnum)
	   (>= (the fixnum n) 0))
    (locally (declare (fixnum n))
      (do* ((checked-list list (cdr checked-list))
	    (returned-list list)
	    (index 0 (1+ index)))
	   ((atom checked-list) returned-list)
	(declare (type index index))
	(if (>= index n)
	  (pop returned-list))))
    (if (and (typep n 'bignum)
	     (> n 0))
      (require-type list 'list)
      (report-bad-arg  n 'unsigned-byte))))





(defun nthcdr (index list)
  "Performs the cdr function n times on a list."
  (setq list (require-type list 'list))
  (if (and (typep index 'fixnum)
	   (>= (the fixnum index) 0))
      (locally (declare (fixnum index))
	(dotimes (i index list)
	  (when (null (setq list (cdr list))) (return))))
      (progn
	(unless (typep index 'unsigned-byte)
	  (report-bad-arg index 'unsigned-byte))
	(do* ((n index (- n target::target-most-positive-fixnum)))
	     ((typep n 'fixnum) (nthcdr n list))
	  (unless (setq list (nthcdr target::target-most-positive-fixnum list))
	    (return))))))


(defun nth (index list)
  "Return the nth object in a list where the car is the zero-th element."
  (car (nthcdr index list)))


(defun nconc (&rest lists)
  (declare (dynamic-extent lists))
  "Concatenates the lists given as arguments (by changing them)"
  (do* ((top lists (cdr top)))
       ((null top) nil)
    (let* ((top-of-top (car top)))
      (cond
       ((consp top-of-top)
        (let* ((result top-of-top)
               (splice result))
          (do* ((elements (cdr top) (cdr elements)))
	         ((endp elements))
            (let ((ele (car elements)))
              (typecase ele
                (cons (rplacd (last splice) ele)
                      (setf splice ele))
                (null (rplacd (last splice) nil))
                (atom (if (cdr elements)
                        (report-bad-arg ele 'list)
                        (rplacd (last splice) ele)))
                (t (report-bad-arg ele 'list)))))
          (return result)))
       ((null top-of-top) nil)
       (t
        (if (cdr top)
          (report-bad-arg top-of-top 'list)
          (return top-of-top)))))))


(defvar %setf-function-names% (make-hash-table :weak t :test 'eq))
(defvar %setf-function-name-inverses% (make-hash-table :weak t :test 'eq))

(defun setf-function-name (sym)
   (or (gethash sym %setf-function-names%)
       (progn
         (let* ((setf-package-sym (construct-setf-function-name sym)))
           (setf (gethash setf-package-sym %setf-function-name-inverses%) sym
                 (gethash sym %setf-function-names%) setf-package-sym)))))

(defun existing-setf-function-name (sym)
  (gethash sym %setf-function-names%))

(defun maybe-setf-name (sym)
  (let* ((other (gethash sym %setf-function-name-inverses%)))
    (if other
      `(setf ,other)
      sym)))

                     

(defconstant *setf-package* (or (find-package "SETF") (make-package "SETF" :use nil :external-size 1)))

(defun construct-setf-function-name (sym)
  (let ((pkg (symbol-package sym)))
    (setq sym (symbol-name sym))
    (if (null pkg)
      (gentemp sym *setf-package*)
      (values
       (intern
        ;;I wonder, if we didn't check, would anybody report it as a bug?
        (if (not (%str-member #\: (setq pkg (package-name pkg))))
          (%str-cat pkg "::" sym)
          (%str-cat (prin1-to-string pkg) "::" (princ-to-string sym)))
        *setf-package*)))))

(defun setf-function-name-p (name)
  (and (consp name)
             (consp (%cdr name))
             (null (%cddr name))
             (symbolp (%cadr name))
             (eq (car name) 'setf)))

(defun valid-function-name-p (name)
  (if (symbolp name)                    ; Nil is a valid function name.  I guess.
    (values t name)
    (if (setf-function-name-p name)
      (values t (setf-function-name (%cadr name)))
      ; What other kinds of function names do we care to support ?
      (values nil nil))))

;;; Why isn't this somewhere else ?
(defun ensure-valid-function-name (name)
  (multiple-value-bind (valid-p nm) (valid-function-name-p name)
    (if valid-p nm (error "Invalid function name ~s." name))))


;;; Returns index if char appears in string, else nil.

(defun %str-member (char string &optional start end)
  (let* ((base-string-p (typep string 'simple-base-string)))
    (unless base-string-p
      (setq string (require-type string 'simple-string)))
    (unless (characterp char)
      (setq char (require-type char 'character)))
    (do* ((i (or start 0) (1+ i))
            (n (or end (uvsize string))))
           ((= i n))
        (declare (fixnum i n) (optimize (speed 3) (safety 0)))
        (if (eq (schar (the simple-base-string string) i) char)
          (return i)))))



;;; Returns index of elt in vector, or nil if it's not there.
(defun %vector-member (elt vector)
  (unless (typep vector 'simple-vector)
    (report-bad-arg vector 'simple-vector))
  (dotimes (i (the fixnum (length vector)))
    (when (eq elt (%svref vector i)) (return i))))

(defun logical-pathname-p (thing) (istruct-typep thing 'logical-pathname))

(progn
;;; It's back ...
(defun list-nreverse (list)
  (nreconc list nil))

;;; We probably want to make this smarter so that less boxing
;;; (and bignum/double-float consing!) takes place.

(defun vector-nreverse (v)
  (let* ((len (length v))
         (middle (ash (the fixnum len) -1)))
    (declare (fixnum middle len))
    (do* ((left 0 (1+ left))
          (right (1- len) (1- right)))
         ((= left middle) v)
      (declare (fixnum left right))
      (rotatef (aref v left) (aref v right)))))
    
(defun nreverse (seq)
  "Return a sequence of the same elements in reverse order; the argument
   is destroyed."
  (when seq
    (seq-dispatch seq
                  (list-nreverse seq)
                  (vector-nreverse seq)))))

(defun nreconc (x y)
  "Return (NCONC (NREVERSE X) Y)."
  (do ((1st (cdr x) (if (endp 1st) 1st (cdr 1st)))
       (2nd x 1st)		;2nd follows first down the list.
       (3rd y 2nd))		;3rd follows 2nd down the list.
      ((atom 2nd) 3rd)
    (rplacd 2nd 3rd)))

;;; The two-arg case is maybe a bit faster.  We -don't- want to
;;; do the two-arg case repeatedly to implement the N-arg case.
(defun append (&rest lists)
  (declare (dynamic-extent lists))
  "Construct a new list by concatenating the list arguments"
  (if lists
    (let* ((head (cons nil nil))
           (tail head))
      (declare (dynamic-extent head)
               (cons head tail))
      (do* ()
           ((null lists) (cdr head))
        (let* ((list (pop lists)))
          (if (null lists)
            (rplacd tail list)
            (dolist (element list)
                (setq tail (cdr (rplacd tail (cons element nil)))))))))))



                     







(progn
(defun list-reverse (l)
  (do* ((new ()))
       ((null l) new)
    (push (pop l) new)))

; Again, it's worth putting more work into this when the dust settles.
(defun vector-reverse (v)
  (let* ((len (length v))
         (new (make-array (the fixnum len) :element-type (array-element-type v))))   ; a LOT more work ...
    (declare (fixnum len))
    (do* ((left 0 (1+ left))
          (right (1- len) (1- right)))
         ((= left len) new)
      (declare (fixnum left right))
      (setf (uvref new left)
            (aref v right)))))

(defun reverse (seq)
  "Return a new sequence containing the same elements but in reverse order."
  (seq-dispatch seq (list-reverse seq) (vector-reverse seq)))
)

(defun check-sequence-bounds (seq start end)
  (let* ((length (length seq)))
    (declare (fixnum length))
    (if (not end)
      (setq end length)
      (unless (typep end 'fixnum)
	(report-bad-arg end 'fixnum)))
    (unless (typep start 'fixnum)
      (report-bad-arg start 'fixnum))
    (locally (declare (fixnum start end))
      (cond ((> end length)
	     (report-bad-arg end `(integer 0 (,length))))
	    ((< start 0)
	     (report-bad-arg start `(integer 0)))
            ((< end 0)
             (report-bad-arg end `(integer 0 (,length))))
	    ((> start end)
	     (report-bad-arg start `(integer 0 ,end)))
	    (t end)))))
  

(defun byte-length (string &optional  (start 0) end)
  (setq end (check-sequence-bounds string start end))
  (- end start))



(defun make-cstring (string)
  (let* ((len (length string)))
    (declare (fixnum len))
    (let* ((s (malloc (the fixnum (1+ len)))))
      (setf (%get-byte s len) 0)
      (multiple-value-bind (data offset) (array-data-and-offset string)
        (dotimes (i len s)
          (setf (%get-unsigned-byte s i) (%scharcode data (+ offset i))))
	s))))


(defun extended-string-p (thing)
  (declare (ignore thing)))

(defun simple-extended-string-p (thing)
  (declare (ignore thing)))



(defun move-string-bytes (source dest off1 off2 n)
  (declare (fixnum off1 off2 n)
           (simple-base-string source dest)
           (optimize (speed 3) (safety 0)))
  (dotimes (i n dest)
    (setf (schar dest off2) (schar source off1))
    (incf off1)
    (incf off2)))


(defun %str-cat (s1 s2 &rest more)
  (declare (dynamic-extent more))
  (require-type s1 'simple-string)
  (require-type s2 'simple-string)
  (let* ((len1 (length s1))
         (len2 (length s2))
         (len (%i+ len2 len1)))
    (declare (optimize (speed 3)(safety 0)))
    (dolist (s more)
      (require-type s 'simple-string)
      (setq len (+ len (length s))))
    (let ((new-string (make-string len :element-type 'base-char)))
      (move-string-bytes s1 new-string 0 0 len1)
      (move-string-bytes s2 new-string 0 len1 len2)
      (dolist (s more)
        (setq len2 (%i+ len1 len2))
        (move-string-bytes s new-string 0 len2 (setq len1 (length s))))
      new-string)))


(defun %substr (str start end)
  (require-type start 'fixnum)
  (require-type end 'fixnum)
  (require-type str 'string)
  (let ((len (length str)))
    (multiple-value-bind (str strb)(array-data-and-offset str)
      (let ((newlen (%i- end start)))
        (when (%i> end len)(error "End ~S exceeds length ~S." end len))
        (when (%i< start 0)(error "Negative start"))
        (let ((new (make-string newlen)))
          (do* ((i 0 (1+ i))
                (pos (%i+ start strb) (1+ pos)))
               ((= i newlen) new)
            (declare (fixnum i pos))
            (setf (schar new i) (schar str pos))))))))



;;; 3 callers
(defun %list-to-uvector (subtype list)   ; subtype may be nil (meaning simple-vector
  (let* ((n (length list))
         (new (%alloc-misc n (or subtype target::subtag-simple-vector))))  ; yech
    (dotimes (i n)
      (declare (fixnum i))
      (uvset new i (%car list))
      (setq list (%cdr list)))
    new))


; appears to be unused
(defun upgraded-array-element-type (type &optional env)
  "Return the element type that will actually be used to implement an array
   with the specifier :ELEMENT-TYPE Spec."
  (declare (ignore env))
  (element-subtype-type (element-type-subtype type)))

(defun upgraded-complex-part-type (type &optional env)
  (declare (ignore env))
  (declare (ignore type))               ; Ok, ok.  So (upgraded-complex-part-type 'bogus) is 'REAL. So ?
  'real)


#+ppc32-target
(progn
  (defparameter array-element-subtypes
    #(single-float 
      (unsigned-byte 32)
      (signed-byte 32)
      fixnum
      base-char                         ;ucs4
      (unsigned-byte 8)
      (signed-byte 8)
      base-char
      (unsigned-byte 16)
      (signed-byte 16)
      double-float
      bit))
  
  ;; given uvector subtype - what is the corresponding element-type
  (defun element-subtype-type (subtype)
    (declare (fixnum subtype))
    (if  (= subtype ppc32::subtag-simple-vector) t
        (svref array-element-subtypes 
               (ash (- subtype ppc32::min-cl-ivector-subtag) (- ppc32::ntagbits)))))
  )

#+x8632-target
(progn
  (defparameter array-element-subtypes
    #(single-float 
      (unsigned-byte 32)
      (signed-byte 32)
      fixnum
      base-char                         ;ucs4
      (unsigned-byte 8)
      (signed-byte 8)
      base-char
      (unsigned-byte 16)
      (signed-byte 16)
      double-float
      bit))
  
  ;; given uvector subtype - what is the corresponding element-type
  (defun element-subtype-type (subtype)
    (declare (fixnum subtype))
    (if  (= subtype x8632::subtag-simple-vector) t
        (svref array-element-subtypes 
               (ash (- subtype x8632::min-cl-ivector-subtag) (- x8632::ntagbits)))))
  )

#+ppc64-target
(progn

(defparameter array-element-subtypes
  #(bogus
    bogus
    bogus
    bogus
    (signed-byte 8)
    (signed-byte 16)
    (signed-byte 32)
    (signed-byte 64)
    (unsigned-byte 8)
    (unsigned-byte 16)
    (unsigned-byte 32)
    (unsigned-byte 64)
    bogus
    bogus
    single-float
    fixnum
    bogus
    bogus
    bogus
    double-float
    bogus
    bogus
    base-char
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bit
    bogus
    bogus))  

  
  ;;; given uvector subtype - what is the corresponding element-type
  (defun element-subtype-type (subtype)
    (declare (fixnum subtype))
    (if  (= subtype ppc64::subtag-simple-vector)
      t
      (svref array-element-subtypes 
             (ash (- subtype 128) -2))))
  )

#+x8664-target
(progn

  ;;; 1, 8, 16-bit element types
  (defparameter *immheader-0-array-element-types*
    #(bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      (signed-byte 16)
      (unsigned-byte 16)
      base-char
      (signed-byte 8)
      (unsigned-byte 8)
      bit))

  ;;; 32-bit element types
  (defparameter *immheader-1-array-element-types*
    #(bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      base-char
      (signed-byte 32)
      (unsigned-byte 32)
      single-float))

  ;;; 64-bit element types
  (defparameter *immheader-2-array-element-types*
    #(bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      fixnum
      (signed-byte 64)
      (unsigned-byte 64)
      double-float))  
      
  
  (defun element-subtype-type (subtype)
    (declare (type (unsigned-byte 8) subtype))
    (if (= subtype x8664::subtag-simple-vector)
      t
      (let* ((class (ash subtype (- x8664::ntagbits)))
             (tag (logand subtype x8664::fulltagmask)))
        (declare (type (unsigned-byte 4) class tag))
        (cond ((= tag x8664::fulltag-immheader-0)
               (%svref *immheader-0-array-element-types* class))
              ((= tag x8664::fulltag-immheader-1)
               (%svref *immheader-1-array-element-types* class))
              ((= tag x8664::fulltag-immheader-2)
               (%svref *immheader-2-array-element-types* class))
              (t 'bogus)))))
  )


;;; %make-displaced-array assumes the following

(eval-when (:compile-toplevel)
  (assert (eql target::arrayH.flags-cell target::vectorH.flags-cell))
  (assert (eql target::arrayH.displacement-cell target::vectorH.displacement-cell))
  (assert (eql target::arrayH.data-vector-cell target::vectorH.data-vector-cell)))


(defun %make-displaced-array (dimensions displaced-to
                                         &optional fill adjustable
					 offset explicitp)
  (if offset 
    (unless (and (fixnump offset) (>= (the fixnum offset) 0))
      (setq offset (require-type offset '(and fixnum (integer 0 *)))))
    (setq offset 0))
  (locally (declare (fixnum offset))
    (let* ((disp-size (array-total-size displaced-to))
           (rank (if (listp dimensions)(length dimensions) 1))
           (new-size (if (fixnump dimensions)
                       dimensions
                       (if (listp dimensions)
                         (if (eql rank 1)
                           (car dimensions)
                           (if (eql rank 0) 1 ; why not 0?
                           (apply #'* dimensions))))))
           (vect-subtype (typecode displaced-to))
           (target displaced-to)
           (real-offset offset)
           (flags 0))
      (declare (fixnum disp-size rank flags vect-subtype real-offset))
      (when explicitp
	(setq flags (bitset $arh_exp_disp_bit flags)))
      (if (not (fixnump new-size))(error "Bad array dimensions ~s." dimensions)) 
      (locally (declare (fixnum new-size))
        ; (when (> (+ offset new-size) disp-size) ...), but don't cons bignums
        (when (or (> new-size disp-size)
                  (let ((max-offset (- disp-size new-size)))
                    (declare (fixnum max-offset))
                    (> offset max-offset)))
          (%err-disp $err-disp-size displaced-to))
        (if adjustable  (setq flags (bitset $arh_adjp_bit flags)))
        (when fill
          (if (eq fill t)
            (setq fill new-size)
            (unless (and (eql rank 1)
                         (fixnump fill)
                         (locally (declare (fixnum fill))
                           (and (>= fill 0) (<= fill new-size))))
              (error "Bad fill pointer ~s" fill)))
          (setq flags (bitset $arh_fill_bit flags))))
      ; If displaced-to is an array or vector header and is either
      ; adjustable or its target is a header, then we need to set the
      ; $arh_disp_bit. If displaced-to is not adjustable, then our
      ; target can be its target instead of itself.
      (when (or (eql vect-subtype target::subtag-arrayH)
                (eql vect-subtype target::subtag-vectorH))
        (let ((dflags (%svref displaced-to target::arrayH.flags-cell)))
          (declare (fixnum dflags))
          (when (or (logbitp $arh_adjp_bit dflags)
		    t
                    (progn
		      #+nope
                      (setq target (%svref displaced-to target::arrayH.data-vector-cell)
                            real-offset (+ offset (%svref displaced-to target::arrayH.displacement-cell)))
                      (logbitp $arh_disp_bit dflags)
		      #-nope t))
            (setq flags (bitset $arh_disp_bit flags))))
        (setq vect-subtype (%array-header-subtype displaced-to)))
      ; assumes flags is low byte
      (setq flags (dpb vect-subtype target::arrayH.flags-cell-subtag-byte flags))
      (if (eq rank 1)
        (%gvector target::subtag-vectorH 
                      (if (fixnump fill) fill new-size)
                      new-size
                      target
                      real-offset
                      flags)
        (let ((val (%alloc-misc (+ target::arrayh.dim0-cell rank) target::subtag-arrayH)))
          (setf (%svref val target::arrayH.rank-cell) rank)
          (setf (%svref val target::arrayH.physsize-cell) new-size)
          (setf (%svref val target::arrayH.data-vector-cell) target)
          (setf (%svref val target::arrayH.displacement-cell) real-offset)
          (setf (%svref val target::arrayH.flags-cell) flags)
          (do* ((dims dimensions (cdr dims))
                (i 0 (1+ i)))              
               ((null dims))
            (declare (fixnum i)(list dims))
            (setf (%svref val (%i+ target::arrayH.dim0-cell i)) (car dims)))
          val)))))

(defun make-array (dims &key (element-type t element-type-p)
                        displaced-to
                        displaced-index-offset
                        adjustable
                        fill-pointer
                        (initial-element nil initial-element-p)
                        (initial-contents nil initial-contents-p))
  (when (and initial-element-p initial-contents-p)
        (error "Cannot specify both ~S and ~S" :initial-element-p :initial-contents-p))
  (make-array-1 dims element-type element-type-p
                displaced-to
                displaced-index-offset
                adjustable
                fill-pointer
                initial-element initial-element-p
                initial-contents initial-contents-p
                nil))





(defun vector-pop (vector)
  "Decrease the fill pointer by 1 and return the element pointed to by the
  new fill pointer."
  (let* ((fill (fill-pointer vector)))
    (declare (fixnum fill))
    (if (zerop fill)
      (error "Fill pointer of ~S is 0 ." vector)
      (progn
        (decf fill)
        (%set-fill-pointer vector fill)
        (aref vector fill)))))




(defun elt (sequence idx)
  "Return the element of SEQUENCE specified by INDEX."
  (seq-dispatch
   sequence
   (let* ((cell (nthcdr idx sequence)))
     (if (consp cell)
       (car (the cons cell))
       (if cell
         (report-bad-arg sequence '(satisfies proper-list-p))
         (%err-disp $XACCESSNTH idx sequence))))
       
   (progn
     (unless (and (typep idx 'fixnum) (>= (the fixnum idx) 0))
       (report-bad-arg idx 'unsigned-byte))
     (locally 
       (if (>= idx (length sequence))
         (%err-disp $XACCESSNTH idx sequence)
         (aref sequence idx))))))




(defun set-elt (sequence idx value)
  (seq-dispatch
   sequence
   (let* ((cell (nthcdr idx sequence)))
     (if (consp cell)
       (setf (car (the cons cell)) value)
       (if cell
         (report-bad-arg sequence '(satisfies proper-list-p))
         (%err-disp $XACCESSNTH idx sequence))))
   (progn
     (unless (and (typep idx 'fixnum) (>= (the fixnum idx) 0))
       (report-bad-arg idx 'unsigned-byte))
     (locally 
       (declare (fixnum idx))
       (if (>= idx (length sequence))
         (%err-disp $XACCESSNTH idx sequence)
         (setf (aref sequence idx) value))))))




(%fhave 'equalp #'equal)                ; bootstrapping

(defun copy-tree (tree)
  "Recursively copy trees of conses."
  (if (atom tree)
    tree
    (locally (declare (type cons tree))
      (do* ((tail (cdr tree) (cdr tail))
            (result (cons (copy-tree (car tree)) nil))
            (ptr result (cdr ptr)))
           ((atom tail)
            (setf (cdr ptr) tail)
            result)
        (declare (type cons ptr result))
        (locally 
          (declare (type cons tail))
          (setf (cdr ptr) (cons (copy-tree (car tail)) nil)))))))




(defvar *periodic-task-interval* 0.3)
(defvar *periodic-task-seconds* 0)
(defvar *periodic-task-nanoseconds* 300000000)

(defun set-periodic-task-interval (n)
  (multiple-value-setq (*periodic-task-seconds* *periodic-task-nanoseconds*)
    (nanoseconds n))
  (setq *periodic-task-interval* n))

(defun periodic-task-interval ()
  *periodic-task-interval*)



(defun char-downcase (c)
  "Return CHAR converted to lower-case if that is possible."
  (let* ((code (char-code c)))
    (declare (type (mod #x110000) code))
    (if (and (>= code (char-code #\A))(<= code (char-code #\Z)))
      (%code-char (%i+ code #.(- (char-code #\a)(char-code #\A))))
      (or (and (>= code #x80)
               (%non-standard-lower-case-equivalent c))
          c))))



(defun digit-char-p (char &optional radix)
  "If char is a digit in the specified radix, returns the fixnum for
  which that digit stands, else returns NIL."
  (let* ((code (char-code char))
         (r (if radix (if (and (typep radix 'fixnum)
                               (%i>= radix 2)
                               (%i<= radix 36))
                        radix
                        (%validate-radix radix)) 10))
         (weight (if (and (<= code (char-code #\9))
                          (>= code (char-code #\0)))
                   (the fixnum (- code (char-code #\0)))
                   (if (and (<= code (char-code #\Z))
                            (>= code (char-code #\A)))
                     (the fixnum (+ 10 (the fixnum (- code (char-code #\A)))))
                   (if (and (<= code (char-code #\z))
                            (>= code (char-code #\a)))
                     (the fixnum (+ 10 (the fixnum (- code (char-code #\a))))))))))
    (declare (fixnum code r))
    (and weight (< (the fixnum weight) r) weight)))





(defun char-upcase (c)
  "Return CHAR converted to upper-case if that is possible.  Don't convert
   lowercase eszet (U+DF)."
  (let* ((code (char-code c)))
    (declare (type (mod #x110000) code))
    (if (and (>= code (char-code #\a))(<= code (char-code #\z)))
      (%code-char (%i- code #.(- (char-code #\a)(char-code #\A))))
      (or (and (>= code #x80) (%non-standard-upper-case-equivalent c))
          c))))

(defun %non-standard-char-code-upcase (code)
  (declare (type (mod #x110000) code))
  (if (>= code #x80)
    (let* ((upper (%non-standard-upper-case-equivalent (code-char code))))
      (if upper
        (char-code upper)
        code))
    code))


(defun string-start-end (string start end)
  (setq string (string string))
  (let ((len (length (the string string))))
    (flet ((are (a i)(error "Array index ~S out of bounds for ~S." i a)))    
      (if (and end (> end len))(are string end))
      (if (and start (or (< start 0)(> start len)))(are string start))
      (setq start (or start 0) end (or end len))
      (if (%i> start end)
        (error "Start ~S exceeds end ~S." start end))
      (if (typep string 'simple-string)
        (values string start end)
        (multiple-value-bind (str off)(array-data-and-offset string)
          (values str (%i+ off start)(%i+ off end)))))))

(defun get-properties (place indicator-list)
  "Like GETF, except that INDICATOR-LIST is a list of indicators which will
  be looked for in the property list stored in PLACE. Three values are
  returned, see manual for details."
  (do ((plist place (cddr plist)))
      ((null plist) (values nil nil nil))
    (cond ((atom (cdr plist))
	   (report-bad-arg place '(satisfies proper-list-p)))
	  ((memq (car plist) indicator-list) ;memq defined in kernel
	   (return (values (car plist) (cadr plist) plist))))))

(defun string= (string1 string2 &key start1 end1 start2 end2)
  "Given two strings (string1 and string2), and optional integers start1,
  start2, end1 and end2, compares characters in string1 to characters in
  string2 (using char=)."
    (locally (declare (optimize (speed 3)(safety 0)))
      (if (and (simple-string-p string1)(null start1)(null end1))
        (setq start1 0 end1 (length string1))
        (multiple-value-setq (string1 start1 end1)(string-start-end string1 start1 end1)))
      (if (and (simple-string-p string2)(null start2)(null end2))
        (setq start2 0 end2 (length string2))
        (multiple-value-setq (string2 start2 end2)(string-start-end string2 start2 end2)))    
      (%simple-string= string1 string2 start1 start2 end1 end2)))


(defun lfun-keyvect (lfun)
  (let ((bits (lfun-bits lfun)))
    (declare (fixnum bits))
    (and (logbitp $lfbits-keys-bit bits)
         (or (logbitp $lfbits-method-bit bits)
             (and (not (logbitp $lfbits-gfn-bit bits))
                  (not (logbitp $lfbits-cm-bit bits))))
         (nth-immediate lfun 1))))



(defun function-lambda-expression (fn)
  "Return (VALUES DEFINING-LAMBDA-EXPRESSION CLOSURE-P NAME), where
  DEFINING-LAMBDA-EXPRESSION is NIL if unknown, or a suitable argument
  to COMPILE otherwise, CLOSURE-P is non-NIL if the function's definition
  might have been enclosed in some non-null lexical environment, and
  NAME is some name (for debugging only) or NIL if there is no name."
  ;(declare (values def env-p name))
  (let* ((bits (lfun-bits (setq fn (require-type fn 'function)))))
    (declare (fixnum bits))
    (if (logbitp $lfbits-trampoline-bit bits)
      (function-lambda-expression (nth-immediate fn 1))
      (values (uncompile-function fn)
              (logbitp $lfbits-nonnullenv-bit bits)
              (function-name fn)))))

; env must be a lexical-environment or NIL.
; If env contains function or variable bindings or SPECIAL declarations, return t.
; Else return nil
(defun %non-empty-environment-p (env)
  (loop
    (when (or (null env) (istruct-typep env 'definition-environment))
      (return nil))
    (when (or (consp (lexenv.variables env))
              (consp (lexenv.functions env))
              (dolist (vdecl (lexenv.vdecls env))
                (when (eq (cadr vdecl) 'special)
                  (return t))))
      (return t))
    (setq env (lexenv.parent-env env))))

;(coerce object 'compiled-function)
(defun coerce-to-compiled-function (object)
  (setq object (coerce-to-function object))
  (unless (typep object 'compiled-function)
    (multiple-value-bind (def envp) (function-lambda-expression object)
      (when (or envp (null def))
        (%err-disp $xcoerce object 'compiled-function))
      (setq object (compile-user-function def nil))))
  object)



(defun %set-toplevel (&optional (fun nil fun-p))
  ;(setq fun (require-type fun '(or symbol function)))
  (let* ((tcr (%current-tcr)))
    (prog1 (%tcr-toplevel-function tcr)
      (when fun-p
	(%set-tcr-toplevel-function tcr fun)))))


(defun gccounts ()
  (let* ((total (%get-gc-count))
         (full (full-gccount))
         (g2-count 0)
         (g1-count 0)
         (g0-count 0))
    (when (egc-enabled-p)
      (let* ((a (%active-dynamic-area)))
        (setq g0-count (%fixnum-ref a target::area.gc-count) a (%fixnum-ref a target::area.older))
        (setq g1-count (%fixnum-ref a target::area.gc-count) a (%fixnum-ref a target::area.older))
        (setq g2-count (%fixnum-ref a target::area.gc-count))))
    (values total full g2-count g1-count g0-count)))

      



(defstatic %pascal-functions%
    #(NIL NIL NIL NIL NIL NIL NIL NIL
      NIL NIL NIL NIL NIL NIL NIL NIL
      NIL NIL NIL NIL NIL NIL NIL NIL
      NIL NIL NIL NIL NIL NIL NIL NIL))


(defun gc-retain-pages (arg)
  "Try to influence the GC to retain/recycle the pages allocated between
GCs if arg is true, and to release them otherwise. This is generally a
gtradeoff between paging and other VM considerations."
  (setq *gc-event-status-bits*
        (if arg
          (bitset $gc-retain-pages-bit *gc-event-status-bits*)
          (bitclr $gc-retain-pages-bit *gc-event-status-bits*)))
  (not (null arg)))

(defun gc-retaining-pages ()
  "Return T if the GC tries to retain pages between full GCs and NIL if
it's trying to release them to improve VM paging performance."
  (logbitp $gc-retain-pages-bit *gc-event-status-bits*))  


(defun gc-verbose (on-full-gc &optional (egc-too on-full-gc))
  "If the first (required) argument is non-NIL, configures the GC to print
informational messages on entry and exit to each full GC; if the first argument
is NIL, suppresses those messages.  The second (optional) argument controls printing of messages on entry and exit to an ephemeral GC.  Returns values as per GC-VERBOSE-P."
  (let* ((bits *gc-event-status-bits*))
    (if on-full-gc
      (bitsetf $gc-verbose-bit bits)
      (bitclrf $gc-verbose-bit bits))
    (if egc-too
      (bitsetf $egc-verbose-bit bits)
      (bitclrf $egc-verbose-bit bits))
    (setq *gc-event-status-bits* bits)
    (values on-full-gc egc-too)))


(defun gc-verbose-p ()
  "Returns two values: the first is true if the GC is configured to
print messages on each full GC; the second is true if the GC is configured
to print messages on each ephemeral GC."
  (let* ((bits *gc-event-status-bits*))
    (values (logbitp $gc-verbose-bit bits)
            (logbitp $egc-verbose-bit bits))))

(defun egc-active-p ()
  "Return T if the EGC was active at the time of the call, NIL otherwise.
Since this is generally a volatile piece of information, it's not clear
whether this function serves a useful purpose when native threads are
involved."
  (and (egc-enabled-p)
       (not (eql 0 (%get-kernel-global 'oldest-ephemeral)))))

; this IS effectively a passive way of inquiring about enabled status.
(defun egc-enabled-p ()
  "Return T if the EGC was enabled at the time of the call, NIL otherwise."
  (not (eql 0 (%fixnum-ref (%active-dynamic-area) target::area.older))))

(defun egc-configuration ()
  "Return as multiple values the sizes in kilobytes of the thresholds
associated with the youngest ephemeral generation, the middle ephemeral
generation, and the oldest ephemeral generation."
  (let* ((ta (%get-kernel-global 'tenured-area))
         (g2 (%fixnum-ref ta target::area.younger))
         (g1 (%fixnum-ref g2 target::area.younger))
         (g0 (%fixnum-ref g1 target::area.younger)))
    (values (ash (the fixnum (%fixnum-ref g0 target::area.threshold)) (- (- 10 target::fixnum-shift)))
            (ash (the fixnum (%fixnum-ref g1 target::area.threshold)) (- (- 10 target::fixnum-shift)))
            (ash (the fixnum (%fixnum-ref g2 target::area.threshold)) (- (- 10 target::fixnum-shift))))))


(defun configure-egc (e0size e1size e2size)
  "If the EGC is currently disabled, put the indicated threshold sizes in
effect and returns T, otherwise, returns NIL. (The provided threshold sizes
are rounded up to a multiple of 64Kbytes in OpenMCL 0.14 and to a multiple
of 32KBytes in earlier versions.)"
  (let* ((was-enabled (egc-active-p)))
    (unwind-protect
         (progn
           (egc nil)
           (setq e2size (logand (lognot #xffff) (+ #xffff (ash (require-type e2size '(unsigned-byte 18)) 10)))
                 e1size (logand (lognot #xffff) (+ #xffff (ash (require-type e1size '(unsigned-byte 18)) 10)))
                 e0size (logand (lognot #xffff) (+ #xffff (ash (require-type e0size '(integer 1 #.(ash 1 18))) 10))))
           (%configure-egc e0size e1size e2size))
      (egc was-enabled))))



(defun macptr-flags (macptr)
  (if (eql (uvsize (setq macptr (require-type macptr 'macptr))) 1)
    0
    (uvref macptr TARGET::XMACPTR.FLAGS-CELL)))


; This doesn't really make the macptr be gcable (now has to be
; on linked list), but we might have other reasons for setting
; other flag bits.
(defun set-macptr-flags (macptr value) 
  (unless (eql (uvsize (setq macptr (require-type macptr 'macptr))) 1)
    (setf (%svref macptr TARGET::XMACPTR.FLAGS-CELL) value)
    value))

(defun %new-gcable-ptr (size &optional clear-p)
  (let ((p (make-gcable-macptr $flags_DisposPtr)))
    (%setf-macptr p (malloc size))
    (if clear-p
      (#_bzero p size))
    p))

(defun %gcable-ptr-p (p)
  (and (typep p 'macptr)
       (= (uvsize p) target::xmacptr.element-count)))

(defstatic *non-standard-lower-to-upper* (make-hash-table :test #'eq)
  "Maps non-STANDARD-CHAR lowercase chars to uppercase equivalents")

(defstatic *non-standard-upper-to-lower* (make-hash-table :test #'eq)
  "Maps non-STANDARD-CHAR uppercase chars to lowercase equivalents")

;;; This alist is automatically (and not to cleverly ...) generated.
;;; The (upper . lower) pairs have the property that UPPER is the
;;; value "simple uppercase equivalent" entry for LOWER in the
;;; UnicodeData.txt file and LOWER is the corresponding entry for
;;; UPPER.
(dolist (pair '((#\Latin_Capital_Letter_A_With_Grave . #\Latin_Small_Letter_A_With_Grave)
                (#\Latin_Capital_Letter_A_With_Acute . #\Latin_Small_Letter_A_With_Acute)
                (#\Latin_Capital_Letter_A_With_Circumflex
                 . #\Latin_Small_Letter_A_With_Circumflex)
                (#\Latin_Capital_Letter_A_With_Tilde . #\Latin_Small_Letter_A_With_Tilde)
                (#\Latin_Capital_Letter_A_With_Diaeresis
                 . #\Latin_Small_Letter_A_With_Diaeresis)
                (#\Latin_Capital_Letter_A_With_Ring_Above
                 . #\Latin_Small_Letter_A_With_Ring_Above)
                (#\Latin_Capital_Letter_Ae . #\Latin_Small_Letter_Ae)
                (#\Latin_Capital_Letter_C_With_Cedilla . #\Latin_Small_Letter_C_With_Cedilla)
                (#\Latin_Capital_Letter_E_With_Grave . #\Latin_Small_Letter_E_With_Grave)
                (#\Latin_Capital_Letter_E_With_Acute . #\Latin_Small_Letter_E_With_Acute)
                (#\Latin_Capital_Letter_E_With_Circumflex
                 . #\Latin_Small_Letter_E_With_Circumflex)
                (#\Latin_Capital_Letter_E_With_Diaeresis
                 . #\Latin_Small_Letter_E_With_Diaeresis)
                (#\Latin_Capital_Letter_I_With_Grave . #\Latin_Small_Letter_I_With_Grave)
                (#\Latin_Capital_Letter_I_With_Acute . #\Latin_Small_Letter_I_With_Acute)
                (#\Latin_Capital_Letter_I_With_Circumflex
                 . #\Latin_Small_Letter_I_With_Circumflex)
                (#\Latin_Capital_Letter_I_With_Diaeresis
                 . #\Latin_Small_Letter_I_With_Diaeresis)
                (#\Latin_Capital_Letter_Eth . #\Latin_Small_Letter_Eth)
                (#\Latin_Capital_Letter_N_With_Tilde . #\Latin_Small_Letter_N_With_Tilde)
                (#\Latin_Capital_Letter_O_With_Grave . #\Latin_Small_Letter_O_With_Grave)
                (#\Latin_Capital_Letter_O_With_Acute . #\Latin_Small_Letter_O_With_Acute)
                (#\Latin_Capital_Letter_O_With_Circumflex
                 . #\Latin_Small_Letter_O_With_Circumflex)
                (#\Latin_Capital_Letter_O_With_Tilde . #\Latin_Small_Letter_O_With_Tilde)
                (#\Latin_Capital_Letter_O_With_Diaeresis
                 . #\Latin_Small_Letter_O_With_Diaeresis)
                (#\Latin_Capital_Letter_O_With_Stroke . #\Latin_Small_Letter_O_With_Stroke)
                (#\Latin_Capital_Letter_U_With_Grave . #\Latin_Small_Letter_U_With_Grave)
                (#\Latin_Capital_Letter_U_With_Acute . #\Latin_Small_Letter_U_With_Acute)
                (#\Latin_Capital_Letter_U_With_Circumflex
                 . #\Latin_Small_Letter_U_With_Circumflex)
                (#\Latin_Capital_Letter_U_With_Diaeresis
                 . #\Latin_Small_Letter_U_With_Diaeresis)
                (#\Latin_Capital_Letter_Y_With_Acute . #\Latin_Small_Letter_Y_With_Acute)
                (#\Latin_Capital_Letter_Thorn . #\Latin_Small_Letter_Thorn)
                (#\Latin_Capital_Letter_A_With_Macron . #\Latin_Small_Letter_A_With_Macron)
                (#\Latin_Capital_Letter_A_With_Breve . #\Latin_Small_Letter_A_With_Breve)
                (#\Latin_Capital_Letter_A_With_Ogonek . #\Latin_Small_Letter_A_With_Ogonek)
                (#\Latin_Capital_Letter_C_With_Acute . #\Latin_Small_Letter_C_With_Acute)
                (#\Latin_Capital_Letter_C_With_Circumflex
                 . #\Latin_Small_Letter_C_With_Circumflex)
                (#\Latin_Capital_Letter_C_With_Dot_Above
                 . #\Latin_Small_Letter_C_With_Dot_Above)
                (#\Latin_Capital_Letter_C_With_Caron . #\Latin_Small_Letter_C_With_Caron)
                (#\Latin_Capital_Letter_D_With_Caron . #\Latin_Small_Letter_D_With_Caron)
                (#\Latin_Capital_Letter_D_With_Stroke . #\Latin_Small_Letter_D_With_Stroke)
                (#\Latin_Capital_Letter_E_With_Macron . #\Latin_Small_Letter_E_With_Macron)
                (#\Latin_Capital_Letter_E_With_Breve . #\Latin_Small_Letter_E_With_Breve)
                (#\Latin_Capital_Letter_E_With_Dot_Above
                 . #\Latin_Small_Letter_E_With_Dot_Above)
                (#\Latin_Capital_Letter_E_With_Ogonek . #\Latin_Small_Letter_E_With_Ogonek)
                (#\Latin_Capital_Letter_E_With_Caron . #\Latin_Small_Letter_E_With_Caron)
                (#\Latin_Capital_Letter_G_With_Circumflex
                 . #\Latin_Small_Letter_G_With_Circumflex)
                (#\Latin_Capital_Letter_G_With_Breve . #\Latin_Small_Letter_G_With_Breve)
                (#\Latin_Capital_Letter_G_With_Dot_Above
                 . #\Latin_Small_Letter_G_With_Dot_Above)
                (#\Latin_Capital_Letter_G_With_Cedilla . #\Latin_Small_Letter_G_With_Cedilla)
                (#\Latin_Capital_Letter_H_With_Circumflex
                 . #\Latin_Small_Letter_H_With_Circumflex)
                (#\Latin_Capital_Letter_H_With_Stroke . #\Latin_Small_Letter_H_With_Stroke)
                (#\Latin_Capital_Letter_I_With_Tilde . #\Latin_Small_Letter_I_With_Tilde)
                (#\Latin_Capital_Letter_I_With_Macron . #\Latin_Small_Letter_I_With_Macron)
                (#\Latin_Capital_Letter_I_With_Breve . #\Latin_Small_Letter_I_With_Breve)
                (#\Latin_Capital_Letter_I_With_Ogonek . #\Latin_Small_Letter_I_With_Ogonek)
                (#\Latin_Capital_Ligature_Ij . #\Latin_Small_Ligature_Ij)
                (#\Latin_Capital_Letter_J_With_Circumflex
                 . #\Latin_Small_Letter_J_With_Circumflex)
                (#\Latin_Capital_Letter_K_With_Cedilla . #\Latin_Small_Letter_K_With_Cedilla)
                (#\Latin_Capital_Letter_L_With_Acute . #\Latin_Small_Letter_L_With_Acute)
                (#\Latin_Capital_Letter_L_With_Cedilla . #\Latin_Small_Letter_L_With_Cedilla)
                (#\Latin_Capital_Letter_L_With_Caron . #\Latin_Small_Letter_L_With_Caron)
                (#\Latin_Capital_Letter_L_With_Middle_Dot
                 . #\Latin_Small_Letter_L_With_Middle_Dot)
                (#\Latin_Capital_Letter_L_With_Stroke . #\Latin_Small_Letter_L_With_Stroke)
                (#\Latin_Capital_Letter_N_With_Acute . #\Latin_Small_Letter_N_With_Acute)
                (#\Latin_Capital_Letter_N_With_Cedilla . #\Latin_Small_Letter_N_With_Cedilla)
                (#\Latin_Capital_Letter_N_With_Caron . #\Latin_Small_Letter_N_With_Caron)
                (#\Latin_Capital_Letter_Eng . #\Latin_Small_Letter_Eng)
                (#\Latin_Capital_Letter_O_With_Macron . #\Latin_Small_Letter_O_With_Macron)
                (#\Latin_Capital_Letter_O_With_Breve . #\Latin_Small_Letter_O_With_Breve)
                (#\Latin_Capital_Letter_O_With_Double_Acute
                 . #\Latin_Small_Letter_O_With_Double_Acute)
                (#\Latin_Capital_Ligature_Oe . #\Latin_Small_Ligature_Oe)
                (#\Latin_Capital_Letter_R_With_Acute . #\Latin_Small_Letter_R_With_Acute)
                (#\Latin_Capital_Letter_R_With_Cedilla . #\Latin_Small_Letter_R_With_Cedilla)
                (#\Latin_Capital_Letter_R_With_Caron . #\Latin_Small_Letter_R_With_Caron)
                (#\Latin_Capital_Letter_S_With_Acute . #\Latin_Small_Letter_S_With_Acute)
                (#\Latin_Capital_Letter_S_With_Circumflex
                 . #\Latin_Small_Letter_S_With_Circumflex)
                (#\Latin_Capital_Letter_S_With_Cedilla . #\Latin_Small_Letter_S_With_Cedilla)
                (#\Latin_Capital_Letter_S_With_Caron . #\Latin_Small_Letter_S_With_Caron)
                (#\Latin_Capital_Letter_T_With_Cedilla . #\Latin_Small_Letter_T_With_Cedilla)
                (#\Latin_Capital_Letter_T_With_Caron . #\Latin_Small_Letter_T_With_Caron)
                (#\Latin_Capital_Letter_T_With_Stroke . #\Latin_Small_Letter_T_With_Stroke)
                (#\Latin_Capital_Letter_U_With_Tilde . #\Latin_Small_Letter_U_With_Tilde)
                (#\Latin_Capital_Letter_U_With_Macron . #\Latin_Small_Letter_U_With_Macron)
                (#\Latin_Capital_Letter_U_With_Breve . #\Latin_Small_Letter_U_With_Breve)
                (#\Latin_Capital_Letter_U_With_Ring_Above
                 . #\Latin_Small_Letter_U_With_Ring_Above)
                (#\Latin_Capital_Letter_U_With_Double_Acute
                 . #\Latin_Small_Letter_U_With_Double_Acute)
                (#\Latin_Capital_Letter_U_With_Ogonek . #\Latin_Small_Letter_U_With_Ogonek)
                (#\Latin_Capital_Letter_W_With_Circumflex
                 . #\Latin_Small_Letter_W_With_Circumflex)
                (#\Latin_Capital_Letter_Y_With_Circumflex
                 . #\Latin_Small_Letter_Y_With_Circumflex)
                (#\Latin_Capital_Letter_Y_With_Diaeresis
                 . #\Latin_Small_Letter_Y_With_Diaeresis)
                (#\Latin_Capital_Letter_Z_With_Acute . #\Latin_Small_Letter_Z_With_Acute)
                (#\Latin_Capital_Letter_Z_With_Dot_Above
                 . #\Latin_Small_Letter_Z_With_Dot_Above)
                (#\Latin_Capital_Letter_Z_With_Caron . #\Latin_Small_Letter_Z_With_Caron)
                (#\Latin_Capital_Letter_B_With_Hook . #\Latin_Small_Letter_B_With_Hook)
                (#\Latin_Capital_Letter_B_With_Topbar . #\Latin_Small_Letter_B_With_Topbar)
                (#\Latin_Capital_Letter_Tone_Six . #\Latin_Small_Letter_Tone_Six)
                (#\Latin_Capital_Letter_Open_O . #\Latin_Small_Letter_Open_O)
                (#\Latin_Capital_Letter_C_With_Hook . #\Latin_Small_Letter_C_With_Hook)
                (#\Latin_Capital_Letter_African_D . #\Latin_Small_Letter_D_With_Tail)
                (#\Latin_Capital_Letter_D_With_Hook . #\Latin_Small_Letter_D_With_Hook)
                (#\Latin_Capital_Letter_D_With_Topbar . #\Latin_Small_Letter_D_With_Topbar)
                (#\Latin_Capital_Letter_Reversed_E . #\Latin_Small_Letter_Turned_E)
                (#\Latin_Capital_Letter_Schwa . #\Latin_Small_Letter_Schwa)
                (#\Latin_Capital_Letter_Open_E . #\Latin_Small_Letter_Open_E)
                (#\Latin_Capital_Letter_F_With_Hook . #\Latin_Small_Letter_F_With_Hook)
                (#\Latin_Capital_Letter_G_With_Hook . #\Latin_Small_Letter_G_With_Hook)
                (#\Latin_Capital_Letter_Gamma . #\Latin_Small_Letter_Gamma)
                (#\Latin_Capital_Letter_Iota . #\Latin_Small_Letter_Iota)
                (#\Latin_Capital_Letter_I_With_Stroke . #\Latin_Small_Letter_I_With_Stroke)
                (#\Latin_Capital_Letter_K_With_Hook . #\Latin_Small_Letter_K_With_Hook)
                (#\Latin_Capital_Letter_Turned_M . #\Latin_Small_Letter_Turned_M)
                (#\Latin_Capital_Letter_N_With_Left_Hook
                 . #\Latin_Small_Letter_N_With_Left_Hook)
                (#\Latin_Capital_Letter_O_With_Middle_Tilde . #\Latin_Small_Letter_Barred_O)
                (#\Latin_Capital_Letter_O_With_Horn . #\Latin_Small_Letter_O_With_Horn)
                (#\Latin_Capital_Letter_Oi . #\Latin_Small_Letter_Oi)
                (#\Latin_Capital_Letter_P_With_Hook . #\Latin_Small_Letter_P_With_Hook)
                (#\Latin_Letter_Yr . #\Latin_Letter_Small_Capital_R)
                (#\Latin_Capital_Letter_Tone_Two . #\Latin_Small_Letter_Tone_Two)
                (#\Latin_Capital_Letter_Esh . #\Latin_Small_Letter_Esh)
                (#\Latin_Capital_Letter_T_With_Hook . #\Latin_Small_Letter_T_With_Hook)
                (#\Latin_Capital_Letter_T_With_Retroflex_Hook
                 . #\Latin_Small_Letter_T_With_Retroflex_Hook)
                (#\Latin_Capital_Letter_U_With_Horn . #\Latin_Small_Letter_U_With_Horn)
                (#\Latin_Capital_Letter_Upsilon . #\Latin_Small_Letter_Upsilon)
                (#\Latin_Capital_Letter_V_With_Hook . #\Latin_Small_Letter_V_With_Hook)
                (#\Latin_Capital_Letter_Y_With_Hook . #\Latin_Small_Letter_Y_With_Hook)
                (#\Latin_Capital_Letter_Z_With_Stroke . #\Latin_Small_Letter_Z_With_Stroke)
                (#\Latin_Capital_Letter_Ezh . #\Latin_Small_Letter_Ezh)
                (#\Latin_Capital_Letter_Ezh_Reversed . #\Latin_Small_Letter_Ezh_Reversed)
                (#\Latin_Capital_Letter_Tone_Five . #\Latin_Small_Letter_Tone_Five)
                (#\Latin_Capital_Letter_Dz_With_Caron . #\Latin_Small_Letter_Dz_With_Caron)
                (#\Latin_Capital_Letter_Lj . #\Latin_Small_Letter_Lj)
                (#\Latin_Capital_Letter_Nj . #\Latin_Small_Letter_Nj)
                (#\Latin_Capital_Letter_A_With_Caron . #\Latin_Small_Letter_A_With_Caron)
                (#\Latin_Capital_Letter_I_With_Caron . #\Latin_Small_Letter_I_With_Caron)
                (#\Latin_Capital_Letter_O_With_Caron . #\Latin_Small_Letter_O_With_Caron)
                (#\Latin_Capital_Letter_U_With_Caron . #\Latin_Small_Letter_U_With_Caron)
                (#\Latin_Capital_Letter_U_With_Diaeresis_And_Macron
                 . #\Latin_Small_Letter_U_With_Diaeresis_And_Macron)
                (#\Latin_Capital_Letter_U_With_Diaeresis_And_Acute
                 . #\Latin_Small_Letter_U_With_Diaeresis_And_Acute)
                (#\Latin_Capital_Letter_U_With_Diaeresis_And_Caron
                 . #\Latin_Small_Letter_U_With_Diaeresis_And_Caron)
                (#\Latin_Capital_Letter_U_With_Diaeresis_And_Grave
                 . #\Latin_Small_Letter_U_With_Diaeresis_And_Grave)
                (#\Latin_Capital_Letter_A_With_Diaeresis_And_Macron
                 . #\Latin_Small_Letter_A_With_Diaeresis_And_Macron)
                (#\Latin_Capital_Letter_A_With_Dot_Above_And_Macron
                 . #\Latin_Small_Letter_A_With_Dot_Above_And_Macron)
                (#\Latin_Capital_Letter_Ae_With_Macron . #\Latin_Small_Letter_Ae_With_Macron)
                (#\Latin_Capital_Letter_G_With_Stroke . #\Latin_Small_Letter_G_With_Stroke)
                (#\Latin_Capital_Letter_G_With_Caron . #\Latin_Small_Letter_G_With_Caron)
                (#\Latin_Capital_Letter_K_With_Caron . #\Latin_Small_Letter_K_With_Caron)
                (#\Latin_Capital_Letter_O_With_Ogonek . #\Latin_Small_Letter_O_With_Ogonek)
                (#\Latin_Capital_Letter_O_With_Ogonek_And_Macron
                 . #\Latin_Small_Letter_O_With_Ogonek_And_Macron)
                (#\Latin_Capital_Letter_Ezh_With_Caron . #\Latin_Small_Letter_Ezh_With_Caron)
                (#\Latin_Capital_Letter_Dz . #\Latin_Small_Letter_Dz)
                (#\Latin_Capital_Letter_G_With_Acute . #\Latin_Small_Letter_G_With_Acute)
                (#\Latin_Capital_Letter_Hwair . #\Latin_Small_Letter_Hv)
                (#\Latin_Capital_Letter_Wynn . #\Latin_Letter_Wynn)
                (#\Latin_Capital_Letter_N_With_Grave . #\Latin_Small_Letter_N_With_Grave)
                (#\Latin_Capital_Letter_A_With_Ring_Above_And_Acute
                 . #\Latin_Small_Letter_A_With_Ring_Above_And_Acute)
                (#\Latin_Capital_Letter_Ae_With_Acute . #\Latin_Small_Letter_Ae_With_Acute)
                (#\Latin_Capital_Letter_O_With_Stroke_And_Acute
                 . #\Latin_Small_Letter_O_With_Stroke_And_Acute)
                (#\Latin_Capital_Letter_A_With_Double_Grave
                 . #\Latin_Small_Letter_A_With_Double_Grave)
                (#\Latin_Capital_Letter_A_With_Inverted_Breve
                 . #\Latin_Small_Letter_A_With_Inverted_Breve)
                (#\Latin_Capital_Letter_E_With_Double_Grave
                 . #\Latin_Small_Letter_E_With_Double_Grave)
                (#\Latin_Capital_Letter_E_With_Inverted_Breve
                 . #\Latin_Small_Letter_E_With_Inverted_Breve)
                (#\Latin_Capital_Letter_I_With_Double_Grave
                 . #\Latin_Small_Letter_I_With_Double_Grave)
                (#\Latin_Capital_Letter_I_With_Inverted_Breve
                 . #\Latin_Small_Letter_I_With_Inverted_Breve)
                (#\Latin_Capital_Letter_O_With_Double_Grave
                 . #\Latin_Small_Letter_O_With_Double_Grave)
                (#\Latin_Capital_Letter_O_With_Inverted_Breve
                 . #\Latin_Small_Letter_O_With_Inverted_Breve)
                (#\Latin_Capital_Letter_R_With_Double_Grave
                 . #\Latin_Small_Letter_R_With_Double_Grave)
                (#\Latin_Capital_Letter_R_With_Inverted_Breve
                 . #\Latin_Small_Letter_R_With_Inverted_Breve)
                (#\Latin_Capital_Letter_U_With_Double_Grave
                 . #\Latin_Small_Letter_U_With_Double_Grave)
                (#\Latin_Capital_Letter_U_With_Inverted_Breve
                 . #\Latin_Small_Letter_U_With_Inverted_Breve)
                (#\Latin_Capital_Letter_S_With_Comma_Below
                 . #\Latin_Small_Letter_S_With_Comma_Below)
                (#\Latin_Capital_Letter_T_With_Comma_Below
                 . #\Latin_Small_Letter_T_With_Comma_Below)
                (#\Latin_Capital_Letter_Yogh . #\Latin_Small_Letter_Yogh)
                (#\Latin_Capital_Letter_H_With_Caron . #\Latin_Small_Letter_H_With_Caron)
                (#\Latin_Capital_Letter_N_With_Long_Right_Leg
                 . #\Latin_Small_Letter_N_With_Long_Right_Leg)
                (#\Latin_Capital_Letter_Ou . #\Latin_Small_Letter_Ou)
                (#\Latin_Capital_Letter_Z_With_Hook . #\Latin_Small_Letter_Z_With_Hook)
                (#\Latin_Capital_Letter_A_With_Dot_Above
                 . #\Latin_Small_Letter_A_With_Dot_Above)
                (#\Latin_Capital_Letter_E_With_Cedilla . #\Latin_Small_Letter_E_With_Cedilla)
                (#\Latin_Capital_Letter_O_With_Diaeresis_And_Macron
                 . #\Latin_Small_Letter_O_With_Diaeresis_And_Macron)
                (#\Latin_Capital_Letter_O_With_Tilde_And_Macron
                 . #\Latin_Small_Letter_O_With_Tilde_And_Macron)
                (#\Latin_Capital_Letter_O_With_Dot_Above
                 . #\Latin_Small_Letter_O_With_Dot_Above)
                (#\Latin_Capital_Letter_O_With_Dot_Above_And_Macron
                 . #\Latin_Small_Letter_O_With_Dot_Above_And_Macron)
                (#\Latin_Capital_Letter_Y_With_Macron . #\Latin_Small_Letter_Y_With_Macron)
                (#\Latin_Capital_Letter_A_With_Stroke . #\U+2C65)
                (#\Latin_Capital_Letter_C_With_Stroke . #\Latin_Small_Letter_C_With_Stroke)
                (#\Latin_Capital_Letter_L_With_Bar . #\Latin_Small_Letter_L_With_Bar)
                (#\Latin_Capital_Letter_T_With_Diagonal_Stroke . #\U+2C66)
                (#\Latin_Capital_Letter_Glottal_Stop . #\Latin_Small_Letter_Glottal_Stop)
                (#\Latin_Capital_Letter_B_With_Stroke . #\Latin_Small_Letter_B_With_Stroke)
                (#\Latin_Capital_Letter_U_Bar . #\Latin_Small_Letter_U_Bar)
                (#\Latin_Capital_Letter_Turned_V . #\Latin_Small_Letter_Turned_V)
                (#\Latin_Capital_Letter_E_With_Stroke . #\Latin_Small_Letter_E_With_Stroke)
                (#\Latin_Capital_Letter_J_With_Stroke . #\Latin_Small_Letter_J_With_Stroke)
                (#\Latin_Capital_Letter_Small_Q_With_Hook_Tail
                 . #\Latin_Small_Letter_Q_With_Hook_Tail)
                (#\Latin_Capital_Letter_R_With_Stroke . #\Latin_Small_Letter_R_With_Stroke)
                (#\Latin_Capital_Letter_Y_With_Stroke . #\Latin_Small_Letter_Y_With_Stroke)
                (#\Greek_Capital_Letter_Alpha_With_Tonos
                 . #\Greek_Small_Letter_Alpha_With_Tonos)
                (#\Greek_Capital_Letter_Epsilon_With_Tonos
                 . #\Greek_Small_Letter_Epsilon_With_Tonos)
                (#\Greek_Capital_Letter_Eta_With_Tonos . #\Greek_Small_Letter_Eta_With_Tonos)
                (#\Greek_Capital_Letter_Iota_With_Tonos
                 . #\Greek_Small_Letter_Iota_With_Tonos)
                (#\Greek_Capital_Letter_Omicron_With_Tonos
                 . #\Greek_Small_Letter_Omicron_With_Tonos)
                (#\Greek_Capital_Letter_Upsilon_With_Tonos
                 . #\Greek_Small_Letter_Upsilon_With_Tonos)
                (#\Greek_Capital_Letter_Omega_With_Tonos
                 . #\Greek_Small_Letter_Omega_With_Tonos)
                (#\Greek_Capital_Letter_Alpha . #\Greek_Small_Letter_Alpha)
                (#\Greek_Capital_Letter_Beta . #\Greek_Small_Letter_Beta)
                (#\Greek_Capital_Letter_Gamma . #\Greek_Small_Letter_Gamma)
                (#\Greek_Capital_Letter_Delta . #\Greek_Small_Letter_Delta)
                (#\Greek_Capital_Letter_Epsilon . #\Greek_Small_Letter_Epsilon)
                (#\Greek_Capital_Letter_Zeta . #\Greek_Small_Letter_Zeta)
                (#\Greek_Capital_Letter_Eta . #\Greek_Small_Letter_Eta)
                (#\Greek_Capital_Letter_Theta . #\Greek_Small_Letter_Theta)
                (#\Greek_Capital_Letter_Iota . #\Greek_Small_Letter_Iota)
                (#\Greek_Capital_Letter_Kappa . #\Greek_Small_Letter_Kappa)
                (#\Greek_Capital_Letter_Lamda . #\Greek_Small_Letter_Lamda)
                (#\Greek_Capital_Letter_Mu . #\Greek_Small_Letter_Mu)
                (#\Greek_Capital_Letter_Nu . #\Greek_Small_Letter_Nu)
                (#\Greek_Capital_Letter_Xi . #\Greek_Small_Letter_Xi)
                (#\Greek_Capital_Letter_Omicron . #\Greek_Small_Letter_Omicron)
                (#\Greek_Capital_Letter_Pi . #\Greek_Small_Letter_Pi)
                (#\Greek_Capital_Letter_Rho . #\Greek_Small_Letter_Rho)
                (#\Greek_Capital_Letter_Sigma . #\Greek_Small_Letter_Sigma)
                (#\Greek_Capital_Letter_Tau . #\Greek_Small_Letter_Tau)
                (#\Greek_Capital_Letter_Upsilon . #\Greek_Small_Letter_Upsilon)
                (#\Greek_Capital_Letter_Phi . #\Greek_Small_Letter_Phi)
                (#\Greek_Capital_Letter_Chi . #\Greek_Small_Letter_Chi)
                (#\Greek_Capital_Letter_Psi . #\Greek_Small_Letter_Psi)
                (#\Greek_Capital_Letter_Omega . #\Greek_Small_Letter_Omega)
                (#\Greek_Capital_Letter_Iota_With_Dialytika
                 . #\Greek_Small_Letter_Iota_With_Dialytika)
                (#\Greek_Capital_Letter_Upsilon_With_Dialytika
                 . #\Greek_Small_Letter_Upsilon_With_Dialytika)
                (#\Greek_Letter_Archaic_Koppa . #\Greek_Small_Letter_Archaic_Koppa)
                (#\Greek_Letter_Stigma . #\Greek_Small_Letter_Stigma)
                (#\Greek_Letter_Digamma . #\Greek_Small_Letter_Digamma)
                (#\Greek_Letter_Koppa . #\Greek_Small_Letter_Koppa)
                (#\Greek_Letter_Sampi . #\Greek_Small_Letter_Sampi)
                (#\Coptic_Capital_Letter_Shei . #\Coptic_Small_Letter_Shei)
                (#\Coptic_Capital_Letter_Fei . #\Coptic_Small_Letter_Fei)
                (#\Coptic_Capital_Letter_Khei . #\Coptic_Small_Letter_Khei)
                (#\Coptic_Capital_Letter_Hori . #\Coptic_Small_Letter_Hori)
                (#\Coptic_Capital_Letter_Gangia . #\Coptic_Small_Letter_Gangia)
                (#\Coptic_Capital_Letter_Shima . #\Coptic_Small_Letter_Shima)
                (#\Coptic_Capital_Letter_Dei . #\Coptic_Small_Letter_Dei)
                (#\Greek_Capital_Letter_Sho . #\Greek_Small_Letter_Sho)
                (#\Greek_Capital_Lunate_Sigma_Symbol . #\Greek_Lunate_Sigma_Symbol)
                (#\Greek_Capital_Letter_San . #\Greek_Small_Letter_San)
                (#\Greek_Capital_Reversed_Lunate_Sigma_Symbol
                 . #\Greek_Small_Reversed_Lunate_Sigma_Symbol)
                (#\Greek_Capital_Dotted_Lunate_Sigma_Symbol
                 . #\Greek_Small_Dotted_Lunate_Sigma_Symbol)
                (#\Greek_Capital_Reversed_Dotted_Lunate_Sigma_Symbol
                 . #\Greek_Small_Reversed_Dotted_Lunate_Sigma_Symbol)
                (#\Cyrillic_Capital_Letter_Ie_With_Grave
                 . #\Cyrillic_Small_Letter_Ie_With_Grave)
                (#\Cyrillic_Capital_Letter_Io . #\Cyrillic_Small_Letter_Io)
                (#\Cyrillic_Capital_Letter_Dje . #\Cyrillic_Small_Letter_Dje)
                (#\Cyrillic_Capital_Letter_Gje . #\Cyrillic_Small_Letter_Gje)
                (#\Cyrillic_Capital_Letter_Ukrainian_Ie
                 . #\Cyrillic_Small_Letter_Ukrainian_Ie)
                (#\Cyrillic_Capital_Letter_Dze . #\Cyrillic_Small_Letter_Dze)
                (#\Cyrillic_Capital_Letter_Byelorussian-Ukrainian_I
                 . #\Cyrillic_Small_Letter_Byelorussian-Ukrainian_I)
                (#\Cyrillic_Capital_Letter_Yi . #\Cyrillic_Small_Letter_Yi)
                (#\Cyrillic_Capital_Letter_Je . #\Cyrillic_Small_Letter_Je)
                (#\Cyrillic_Capital_Letter_Lje . #\Cyrillic_Small_Letter_Lje)
                (#\Cyrillic_Capital_Letter_Nje . #\Cyrillic_Small_Letter_Nje)
                (#\Cyrillic_Capital_Letter_Tshe . #\Cyrillic_Small_Letter_Tshe)
                (#\Cyrillic_Capital_Letter_Kje . #\Cyrillic_Small_Letter_Kje)
                (#\Cyrillic_Capital_Letter_I_With_Grave
                 . #\Cyrillic_Small_Letter_I_With_Grave)
                (#\Cyrillic_Capital_Letter_Short_U . #\Cyrillic_Small_Letter_Short_U)
                (#\Cyrillic_Capital_Letter_Dzhe . #\Cyrillic_Small_Letter_Dzhe)
                (#\Cyrillic_Capital_Letter_A . #\Cyrillic_Small_Letter_A)
                (#\Cyrillic_Capital_Letter_Be . #\Cyrillic_Small_Letter_Be)
                (#\Cyrillic_Capital_Letter_Ve . #\Cyrillic_Small_Letter_Ve)
                (#\Cyrillic_Capital_Letter_Ghe . #\Cyrillic_Small_Letter_Ghe)
                (#\Cyrillic_Capital_Letter_De . #\Cyrillic_Small_Letter_De)
                (#\Cyrillic_Capital_Letter_Ie . #\Cyrillic_Small_Letter_Ie)
                (#\Cyrillic_Capital_Letter_Zhe . #\Cyrillic_Small_Letter_Zhe)
                (#\Cyrillic_Capital_Letter_Ze . #\Cyrillic_Small_Letter_Ze)
                (#\Cyrillic_Capital_Letter_I . #\Cyrillic_Small_Letter_I)
                (#\Cyrillic_Capital_Letter_Short_I . #\Cyrillic_Small_Letter_Short_I)
                (#\Cyrillic_Capital_Letter_Ka . #\Cyrillic_Small_Letter_Ka)
                (#\Cyrillic_Capital_Letter_El . #\Cyrillic_Small_Letter_El)
                (#\Cyrillic_Capital_Letter_Em . #\Cyrillic_Small_Letter_Em)
                (#\Cyrillic_Capital_Letter_En . #\Cyrillic_Small_Letter_En)
                (#\Cyrillic_Capital_Letter_O . #\Cyrillic_Small_Letter_O)
                (#\Cyrillic_Capital_Letter_Pe . #\Cyrillic_Small_Letter_Pe)
                (#\Cyrillic_Capital_Letter_Er . #\Cyrillic_Small_Letter_Er)
                (#\Cyrillic_Capital_Letter_Es . #\Cyrillic_Small_Letter_Es)
                (#\Cyrillic_Capital_Letter_Te . #\Cyrillic_Small_Letter_Te)
                (#\Cyrillic_Capital_Letter_U . #\Cyrillic_Small_Letter_U)
                (#\Cyrillic_Capital_Letter_Ef . #\Cyrillic_Small_Letter_Ef)
                (#\Cyrillic_Capital_Letter_Ha . #\Cyrillic_Small_Letter_Ha)
                (#\Cyrillic_Capital_Letter_Tse . #\Cyrillic_Small_Letter_Tse)
                (#\Cyrillic_Capital_Letter_Che . #\Cyrillic_Small_Letter_Che)
                (#\Cyrillic_Capital_Letter_Sha . #\Cyrillic_Small_Letter_Sha)
                (#\Cyrillic_Capital_Letter_Shcha . #\Cyrillic_Small_Letter_Shcha)
                (#\Cyrillic_Capital_Letter_Hard_Sign . #\Cyrillic_Small_Letter_Hard_Sign)
                (#\Cyrillic_Capital_Letter_Yeru . #\Cyrillic_Small_Letter_Yeru)
                (#\Cyrillic_Capital_Letter_Soft_Sign . #\Cyrillic_Small_Letter_Soft_Sign)
                (#\Cyrillic_Capital_Letter_E . #\Cyrillic_Small_Letter_E)
                (#\Cyrillic_Capital_Letter_Yu . #\Cyrillic_Small_Letter_Yu)
                (#\Cyrillic_Capital_Letter_Ya . #\Cyrillic_Small_Letter_Ya)
                (#\Cyrillic_Capital_Letter_Omega . #\Cyrillic_Small_Letter_Omega)
                (#\Cyrillic_Capital_Letter_Yat . #\Cyrillic_Small_Letter_Yat)
                (#\Cyrillic_Capital_Letter_Iotified_E . #\Cyrillic_Small_Letter_Iotified_E)
                (#\Cyrillic_Capital_Letter_Little_Yus . #\Cyrillic_Small_Letter_Little_Yus)
                (#\Cyrillic_Capital_Letter_Iotified_Little_Yus
                 . #\Cyrillic_Small_Letter_Iotified_Little_Yus)
                (#\Cyrillic_Capital_Letter_Big_Yus . #\Cyrillic_Small_Letter_Big_Yus)
                (#\Cyrillic_Capital_Letter_Iotified_Big_Yus
                 . #\Cyrillic_Small_Letter_Iotified_Big_Yus)
                (#\Cyrillic_Capital_Letter_Ksi . #\Cyrillic_Small_Letter_Ksi)
                (#\Cyrillic_Capital_Letter_Psi . #\Cyrillic_Small_Letter_Psi)
                (#\Cyrillic_Capital_Letter_Fita . #\Cyrillic_Small_Letter_Fita)
                (#\Cyrillic_Capital_Letter_Izhitsa . #\Cyrillic_Small_Letter_Izhitsa)
                (#\Cyrillic_Capital_Letter_Izhitsa_With_Double_Grave_Accent
                 . #\Cyrillic_Small_Letter_Izhitsa_With_Double_Grave_Accent)
                (#\Cyrillic_Capital_Letter_Uk . #\Cyrillic_Small_Letter_Uk)
                (#\Cyrillic_Capital_Letter_Round_Omega . #\Cyrillic_Small_Letter_Round_Omega)
                (#\Cyrillic_Capital_Letter_Omega_With_Titlo
                 . #\Cyrillic_Small_Letter_Omega_With_Titlo)
                (#\Cyrillic_Capital_Letter_Ot . #\Cyrillic_Small_Letter_Ot)
                (#\Cyrillic_Capital_Letter_Koppa . #\Cyrillic_Small_Letter_Koppa)
                (#\Cyrillic_Capital_Letter_Short_I_With_Tail
                 . #\Cyrillic_Small_Letter_Short_I_With_Tail)
                (#\Cyrillic_Capital_Letter_Semisoft_Sign
                 . #\Cyrillic_Small_Letter_Semisoft_Sign)
                (#\Cyrillic_Capital_Letter_Er_With_Tick
                 . #\Cyrillic_Small_Letter_Er_With_Tick)
                (#\Cyrillic_Capital_Letter_Ghe_With_Upturn
                 . #\Cyrillic_Small_Letter_Ghe_With_Upturn)
                (#\Cyrillic_Capital_Letter_Ghe_With_Stroke
                 . #\Cyrillic_Small_Letter_Ghe_With_Stroke)
                (#\Cyrillic_Capital_Letter_Ghe_With_Middle_Hook
                 . #\Cyrillic_Small_Letter_Ghe_With_Middle_Hook)
                (#\Cyrillic_Capital_Letter_Zhe_With_Descender
                 . #\Cyrillic_Small_Letter_Zhe_With_Descender)
                (#\Cyrillic_Capital_Letter_Ze_With_Descender
                 . #\Cyrillic_Small_Letter_Ze_With_Descender)
                (#\Cyrillic_Capital_Letter_Ka_With_Descender
                 . #\Cyrillic_Small_Letter_Ka_With_Descender)
                (#\Cyrillic_Capital_Letter_Ka_With_Vertical_Stroke
                 . #\Cyrillic_Small_Letter_Ka_With_Vertical_Stroke)
                (#\Cyrillic_Capital_Letter_Ka_With_Stroke
                 . #\Cyrillic_Small_Letter_Ka_With_Stroke)
                (#\Cyrillic_Capital_Letter_Bashkir_Ka . #\Cyrillic_Small_Letter_Bashkir_Ka)
                (#\Cyrillic_Capital_Letter_En_With_Descender
                 . #\Cyrillic_Small_Letter_En_With_Descender)
                (#\Cyrillic_Capital_Ligature_En_Ghe . #\Cyrillic_Small_Ligature_En_Ghe)
                (#\Cyrillic_Capital_Letter_Pe_With_Middle_Hook
                 . #\Cyrillic_Small_Letter_Pe_With_Middle_Hook)
                (#\Cyrillic_Capital_Letter_Abkhasian_Ha
                 . #\Cyrillic_Small_Letter_Abkhasian_Ha)
                (#\Cyrillic_Capital_Letter_Es_With_Descender
                 . #\Cyrillic_Small_Letter_Es_With_Descender)
                (#\Cyrillic_Capital_Letter_Te_With_Descender
                 . #\Cyrillic_Small_Letter_Te_With_Descender)
                (#\Cyrillic_Capital_Letter_Straight_U . #\Cyrillic_Small_Letter_Straight_U)
                (#\Cyrillic_Capital_Letter_Straight_U_With_Stroke
                 . #\Cyrillic_Small_Letter_Straight_U_With_Stroke)
                (#\Cyrillic_Capital_Letter_Ha_With_Descender
                 . #\Cyrillic_Small_Letter_Ha_With_Descender)
                (#\Cyrillic_Capital_Ligature_Te_Tse . #\Cyrillic_Small_Ligature_Te_Tse)
                (#\Cyrillic_Capital_Letter_Che_With_Descender
                 . #\Cyrillic_Small_Letter_Che_With_Descender)
                (#\Cyrillic_Capital_Letter_Che_With_Vertical_Stroke
                 . #\Cyrillic_Small_Letter_Che_With_Vertical_Stroke)
                (#\Cyrillic_Capital_Letter_Shha . #\Cyrillic_Small_Letter_Shha)
                (#\Cyrillic_Capital_Letter_Abkhasian_Che
                 . #\Cyrillic_Small_Letter_Abkhasian_Che)
                (#\Cyrillic_Capital_Letter_Abkhasian_Che_With_Descender
                 . #\Cyrillic_Small_Letter_Abkhasian_Che_With_Descender)
                (#\Cyrillic_Letter_Palochka . #\Cyrillic_Small_Letter_Palochka)
                (#\Cyrillic_Capital_Letter_Zhe_With_Breve
                 . #\Cyrillic_Small_Letter_Zhe_With_Breve)
                (#\Cyrillic_Capital_Letter_Ka_With_Hook
                 . #\Cyrillic_Small_Letter_Ka_With_Hook)
                (#\Cyrillic_Capital_Letter_El_With_Tail
                 . #\Cyrillic_Small_Letter_El_With_Tail)
                (#\Cyrillic_Capital_Letter_En_With_Hook
                 . #\Cyrillic_Small_Letter_En_With_Hook)
                (#\Cyrillic_Capital_Letter_En_With_Tail
                 . #\Cyrillic_Small_Letter_En_With_Tail)
                (#\Cyrillic_Capital_Letter_Khakassian_Che
                 . #\Cyrillic_Small_Letter_Khakassian_Che)
                (#\Cyrillic_Capital_Letter_Em_With_Tail
                 . #\Cyrillic_Small_Letter_Em_With_Tail)
                (#\Cyrillic_Capital_Letter_A_With_Breve
                 . #\Cyrillic_Small_Letter_A_With_Breve)
                (#\Cyrillic_Capital_Letter_A_With_Diaeresis
                 . #\Cyrillic_Small_Letter_A_With_Diaeresis)
                (#\Cyrillic_Capital_Ligature_A_Ie . #\Cyrillic_Small_Ligature_A_Ie)
                (#\Cyrillic_Capital_Letter_Ie_With_Breve
                 . #\Cyrillic_Small_Letter_Ie_With_Breve)
                (#\Cyrillic_Capital_Letter_Schwa . #\Cyrillic_Small_Letter_Schwa)
                (#\Cyrillic_Capital_Letter_Schwa_With_Diaeresis
                 . #\Cyrillic_Small_Letter_Schwa_With_Diaeresis)
                (#\Cyrillic_Capital_Letter_Zhe_With_Diaeresis
                 . #\Cyrillic_Small_Letter_Zhe_With_Diaeresis)
                (#\Cyrillic_Capital_Letter_Ze_With_Diaeresis
                 . #\Cyrillic_Small_Letter_Ze_With_Diaeresis)
                (#\Cyrillic_Capital_Letter_Abkhasian_Dze
                 . #\Cyrillic_Small_Letter_Abkhasian_Dze)
                (#\Cyrillic_Capital_Letter_I_With_Macron
                 . #\Cyrillic_Small_Letter_I_With_Macron)
                (#\Cyrillic_Capital_Letter_I_With_Diaeresis
                 . #\Cyrillic_Small_Letter_I_With_Diaeresis)
                (#\Cyrillic_Capital_Letter_O_With_Diaeresis
                 . #\Cyrillic_Small_Letter_O_With_Diaeresis)
                (#\Cyrillic_Capital_Letter_Barred_O . #\Cyrillic_Small_Letter_Barred_O)
                (#\Cyrillic_Capital_Letter_Barred_O_With_Diaeresis
                 . #\Cyrillic_Small_Letter_Barred_O_With_Diaeresis)
                (#\Cyrillic_Capital_Letter_E_With_Diaeresis
                 . #\Cyrillic_Small_Letter_E_With_Diaeresis)
                (#\Cyrillic_Capital_Letter_U_With_Macron
                 . #\Cyrillic_Small_Letter_U_With_Macron)
                (#\Cyrillic_Capital_Letter_U_With_Diaeresis
                 . #\Cyrillic_Small_Letter_U_With_Diaeresis)
                (#\Cyrillic_Capital_Letter_U_With_Double_Acute
                 . #\Cyrillic_Small_Letter_U_With_Double_Acute)
                (#\Cyrillic_Capital_Letter_Che_With_Diaeresis
                 . #\Cyrillic_Small_Letter_Che_With_Diaeresis)
                (#\Cyrillic_Capital_Letter_Ghe_With_Descender
                 . #\Cyrillic_Small_Letter_Ghe_With_Descender)
                (#\Cyrillic_Capital_Letter_Yeru_With_Diaeresis
                 . #\Cyrillic_Small_Letter_Yeru_With_Diaeresis)
                (#\Cyrillic_Capital_Letter_Ghe_With_Stroke_And_Hook
                 . #\Cyrillic_Small_Letter_Ghe_With_Stroke_And_Hook)
                (#\Cyrillic_Capital_Letter_Ha_With_Hook
                 . #\Cyrillic_Small_Letter_Ha_With_Hook)
                (#\Cyrillic_Capital_Letter_Ha_With_Stroke
                 . #\Cyrillic_Small_Letter_Ha_With_Stroke)
                (#\Cyrillic_Capital_Letter_Komi_De . #\Cyrillic_Small_Letter_Komi_De)
                (#\Cyrillic_Capital_Letter_Komi_Dje . #\Cyrillic_Small_Letter_Komi_Dje)
                (#\Cyrillic_Capital_Letter_Komi_Zje . #\Cyrillic_Small_Letter_Komi_Zje)
                (#\Cyrillic_Capital_Letter_Komi_Dzje . #\Cyrillic_Small_Letter_Komi_Dzje)
                (#\Cyrillic_Capital_Letter_Komi_Lje . #\Cyrillic_Small_Letter_Komi_Lje)
                (#\Cyrillic_Capital_Letter_Komi_Nje . #\Cyrillic_Small_Letter_Komi_Nje)
                (#\Cyrillic_Capital_Letter_Komi_Sje . #\Cyrillic_Small_Letter_Komi_Sje)
                (#\Cyrillic_Capital_Letter_Komi_Tje . #\Cyrillic_Small_Letter_Komi_Tje)
                (#\Cyrillic_Capital_Letter_Reversed_Ze . #\Cyrillic_Small_Letter_Reversed_Ze)
                (#\Cyrillic_Capital_Letter_El_With_Hook
                 . #\Cyrillic_Small_Letter_El_With_Hook)
                (#\Armenian_Capital_Letter_Ayb . #\Armenian_Small_Letter_Ayb)
                (#\Armenian_Capital_Letter_Ben . #\Armenian_Small_Letter_Ben)
                (#\Armenian_Capital_Letter_Gim . #\Armenian_Small_Letter_Gim)
                (#\Armenian_Capital_Letter_Da . #\Armenian_Small_Letter_Da)
                (#\Armenian_Capital_Letter_Ech . #\Armenian_Small_Letter_Ech)
                (#\Armenian_Capital_Letter_Za . #\Armenian_Small_Letter_Za)
                (#\Armenian_Capital_Letter_Eh . #\Armenian_Small_Letter_Eh)
                (#\Armenian_Capital_Letter_Et . #\Armenian_Small_Letter_Et)
                (#\Armenian_Capital_Letter_To . #\Armenian_Small_Letter_To)
                (#\Armenian_Capital_Letter_Zhe . #\Armenian_Small_Letter_Zhe)
                (#\Armenian_Capital_Letter_Ini . #\Armenian_Small_Letter_Ini)
                (#\Armenian_Capital_Letter_Liwn . #\Armenian_Small_Letter_Liwn)
                (#\Armenian_Capital_Letter_Xeh . #\Armenian_Small_Letter_Xeh)
                (#\Armenian_Capital_Letter_Ca . #\Armenian_Small_Letter_Ca)
                (#\Armenian_Capital_Letter_Ken . #\Armenian_Small_Letter_Ken)
                (#\Armenian_Capital_Letter_Ho . #\Armenian_Small_Letter_Ho)
                (#\Armenian_Capital_Letter_Ja . #\Armenian_Small_Letter_Ja)
                (#\Armenian_Capital_Letter_Ghad . #\Armenian_Small_Letter_Ghad)
                (#\Armenian_Capital_Letter_Cheh . #\Armenian_Small_Letter_Cheh)
                (#\Armenian_Capital_Letter_Men . #\Armenian_Small_Letter_Men)
                (#\Armenian_Capital_Letter_Yi . #\Armenian_Small_Letter_Yi)
                (#\Armenian_Capital_Letter_Now . #\Armenian_Small_Letter_Now)
                (#\Armenian_Capital_Letter_Sha . #\Armenian_Small_Letter_Sha)
                (#\Armenian_Capital_Letter_Vo . #\Armenian_Small_Letter_Vo)
                (#\Armenian_Capital_Letter_Cha . #\Armenian_Small_Letter_Cha)
                (#\Armenian_Capital_Letter_Peh . #\Armenian_Small_Letter_Peh)
                (#\Armenian_Capital_Letter_Jheh . #\Armenian_Small_Letter_Jheh)
                (#\Armenian_Capital_Letter_Ra . #\Armenian_Small_Letter_Ra)
                (#\Armenian_Capital_Letter_Seh . #\Armenian_Small_Letter_Seh)
                (#\Armenian_Capital_Letter_Vew . #\Armenian_Small_Letter_Vew)
                (#\Armenian_Capital_Letter_Tiwn . #\Armenian_Small_Letter_Tiwn)
                (#\Armenian_Capital_Letter_Reh . #\Armenian_Small_Letter_Reh)
                (#\Armenian_Capital_Letter_Co . #\Armenian_Small_Letter_Co)
                (#\Armenian_Capital_Letter_Yiwn . #\Armenian_Small_Letter_Yiwn)
                (#\Armenian_Capital_Letter_Piwr . #\Armenian_Small_Letter_Piwr)
                (#\Armenian_Capital_Letter_Keh . #\Armenian_Small_Letter_Keh)
                (#\Armenian_Capital_Letter_Oh . #\Armenian_Small_Letter_Oh)
                (#\Armenian_Capital_Letter_Feh . #\Armenian_Small_Letter_Feh)
                (#\U+10A0 . #\U+2D00) (#\U+10A1 . #\U+2D01) (#\U+10A2 . #\U+2D02)
                (#\U+10A3 . #\U+2D03) (#\U+10A4 . #\U+2D04) (#\U+10A5 . #\U+2D05)
                (#\U+10A6 . #\U+2D06) (#\U+10A7 . #\U+2D07) (#\U+10A8 . #\U+2D08)
                (#\U+10A9 . #\U+2D09) (#\U+10AA . #\U+2D0A) (#\U+10AB . #\U+2D0B)
                (#\U+10AC . #\U+2D0C) (#\U+10AD . #\U+2D0D) (#\U+10AE . #\U+2D0E)
                (#\U+10AF . #\U+2D0F) (#\U+10B0 . #\U+2D10) (#\U+10B1 . #\U+2D11)
                (#\U+10B2 . #\U+2D12) (#\U+10B3 . #\U+2D13) (#\U+10B4 . #\U+2D14)
                (#\U+10B5 . #\U+2D15) (#\U+10B6 . #\U+2D16) (#\U+10B7 . #\U+2D17)
                (#\U+10B8 . #\U+2D18) (#\U+10B9 . #\U+2D19) (#\U+10BA . #\U+2D1A)
                (#\U+10BB . #\U+2D1B) (#\U+10BC . #\U+2D1C) (#\U+10BD . #\U+2D1D)
                (#\U+10BE . #\U+2D1E) (#\U+10BF . #\U+2D1F) (#\U+10C0 . #\U+2D20)
                (#\U+10C1 . #\U+2D21) (#\U+10C2 . #\U+2D22) (#\U+10C3 . #\U+2D23)
                (#\U+10C4 . #\U+2D24) (#\U+10C5 . #\U+2D25) (#\U+1E00 . #\U+1E01)
                (#\U+1E02 . #\U+1E03) (#\U+1E04 . #\U+1E05) (#\U+1E06 . #\U+1E07)
                (#\U+1E08 . #\U+1E09) (#\U+1E0A . #\U+1E0B) (#\U+1E0C . #\U+1E0D)
                (#\U+1E0E . #\U+1E0F) (#\U+1E10 . #\U+1E11) (#\U+1E12 . #\U+1E13)
                (#\U+1E14 . #\U+1E15) (#\U+1E16 . #\U+1E17) (#\U+1E18 . #\U+1E19)
                (#\U+1E1A . #\U+1E1B) (#\U+1E1C . #\U+1E1D) (#\U+1E1E . #\U+1E1F)
                (#\U+1E20 . #\U+1E21) (#\U+1E22 . #\U+1E23) (#\U+1E24 . #\U+1E25)
                (#\U+1E26 . #\U+1E27) (#\U+1E28 . #\U+1E29) (#\U+1E2A . #\U+1E2B)
                (#\U+1E2C . #\U+1E2D) (#\U+1E2E . #\U+1E2F) (#\U+1E30 . #\U+1E31)
                (#\U+1E32 . #\U+1E33) (#\U+1E34 . #\U+1E35) (#\U+1E36 . #\U+1E37)
                (#\U+1E38 . #\U+1E39) (#\U+1E3A . #\U+1E3B) (#\U+1E3C . #\U+1E3D)
                (#\U+1E3E . #\U+1E3F) (#\U+1E40 . #\U+1E41) (#\U+1E42 . #\U+1E43)
                (#\U+1E44 . #\U+1E45) (#\U+1E46 . #\U+1E47) (#\U+1E48 . #\U+1E49)
                (#\U+1E4A . #\U+1E4B) (#\U+1E4C . #\U+1E4D) (#\U+1E4E . #\U+1E4F)
                (#\U+1E50 . #\U+1E51) (#\U+1E52 . #\U+1E53) (#\U+1E54 . #\U+1E55)
                (#\U+1E56 . #\U+1E57) (#\U+1E58 . #\U+1E59) (#\U+1E5A . #\U+1E5B)
                (#\U+1E5C . #\U+1E5D) (#\U+1E5E . #\U+1E5F) (#\U+1E60 . #\U+1E61)
                (#\U+1E62 . #\U+1E63) (#\U+1E64 . #\U+1E65) (#\U+1E66 . #\U+1E67)
                (#\U+1E68 . #\U+1E69) (#\U+1E6A . #\U+1E6B) (#\U+1E6C . #\U+1E6D)
                (#\U+1E6E . #\U+1E6F) (#\U+1E70 . #\U+1E71) (#\U+1E72 . #\U+1E73)
                (#\U+1E74 . #\U+1E75) (#\U+1E76 . #\U+1E77) (#\U+1E78 . #\U+1E79)
                (#\U+1E7A . #\U+1E7B) (#\U+1E7C . #\U+1E7D) (#\U+1E7E . #\U+1E7F)
                (#\U+1E80 . #\U+1E81) (#\U+1E82 . #\U+1E83) (#\U+1E84 . #\U+1E85)
                (#\U+1E86 . #\U+1E87) (#\U+1E88 . #\U+1E89) (#\U+1E8A . #\U+1E8B)
                (#\U+1E8C . #\U+1E8D) (#\U+1E8E . #\U+1E8F) (#\U+1E90 . #\U+1E91)
                (#\U+1E92 . #\U+1E93) (#\U+1E94 . #\U+1E95) (#\U+1EA0 . #\U+1EA1)
                (#\U+1EA2 . #\U+1EA3) (#\U+1EA4 . #\U+1EA5) (#\U+1EA6 . #\U+1EA7)
                (#\U+1EA8 . #\U+1EA9) (#\U+1EAA . #\U+1EAB) (#\U+1EAC . #\U+1EAD)
                (#\U+1EAE . #\U+1EAF) (#\U+1EB0 . #\U+1EB1) (#\U+1EB2 . #\U+1EB3)
                (#\U+1EB4 . #\U+1EB5) (#\U+1EB6 . #\U+1EB7) (#\U+1EB8 . #\U+1EB9)
                (#\U+1EBA . #\U+1EBB) (#\U+1EBC . #\U+1EBD) (#\U+1EBE . #\U+1EBF)
                (#\U+1EC0 . #\U+1EC1) (#\U+1EC2 . #\U+1EC3) (#\U+1EC4 . #\U+1EC5)
                (#\U+1EC6 . #\U+1EC7) (#\U+1EC8 . #\U+1EC9) (#\U+1ECA . #\U+1ECB)
                (#\U+1ECC . #\U+1ECD) (#\U+1ECE . #\U+1ECF) (#\U+1ED0 . #\U+1ED1)
                (#\U+1ED2 . #\U+1ED3) (#\U+1ED4 . #\U+1ED5) (#\U+1ED6 . #\U+1ED7)
                (#\U+1ED8 . #\U+1ED9) (#\U+1EDA . #\U+1EDB) (#\U+1EDC . #\U+1EDD)
                (#\U+1EDE . #\U+1EDF) (#\U+1EE0 . #\U+1EE1) (#\U+1EE2 . #\U+1EE3)
                (#\U+1EE4 . #\U+1EE5) (#\U+1EE6 . #\U+1EE7) (#\U+1EE8 . #\U+1EE9)
                (#\U+1EEA . #\U+1EEB) (#\U+1EEC . #\U+1EED) (#\U+1EEE . #\U+1EEF)
                (#\U+1EF0 . #\U+1EF1) (#\U+1EF2 . #\U+1EF3) (#\U+1EF4 . #\U+1EF5)
                (#\U+1EF6 . #\U+1EF7) (#\U+1EF8 . #\U+1EF9) (#\U+1F08 . #\U+1F00)
                (#\U+1F09 . #\U+1F01) (#\U+1F0A . #\U+1F02) (#\U+1F0B . #\U+1F03)
                (#\U+1F0C . #\U+1F04) (#\U+1F0D . #\U+1F05) (#\U+1F0E . #\U+1F06)
                (#\U+1F0F . #\U+1F07) (#\U+1F18 . #\U+1F10) (#\U+1F19 . #\U+1F11)
                (#\U+1F1A . #\U+1F12) (#\U+1F1B . #\U+1F13) (#\U+1F1C . #\U+1F14)
                (#\U+1F1D . #\U+1F15) (#\U+1F28 . #\U+1F20) (#\U+1F29 . #\U+1F21)
                (#\U+1F2A . #\U+1F22) (#\U+1F2B . #\U+1F23) (#\U+1F2C . #\U+1F24)
                (#\U+1F2D . #\U+1F25) (#\U+1F2E . #\U+1F26) (#\U+1F2F . #\U+1F27)
                (#\U+1F38 . #\U+1F30) (#\U+1F39 . #\U+1F31) (#\U+1F3A . #\U+1F32)
                (#\U+1F3B . #\U+1F33) (#\U+1F3C . #\U+1F34) (#\U+1F3D . #\U+1F35)
                (#\U+1F3E . #\U+1F36) (#\U+1F3F . #\U+1F37) (#\U+1F48 . #\U+1F40)
                (#\U+1F49 . #\U+1F41) (#\U+1F4A . #\U+1F42) (#\U+1F4B . #\U+1F43)
                (#\U+1F4C . #\U+1F44) (#\U+1F4D . #\U+1F45) (#\U+1F59 . #\U+1F51)
                (#\U+1F5B . #\U+1F53) (#\U+1F5D . #\U+1F55) (#\U+1F5F . #\U+1F57)
                (#\U+1F68 . #\U+1F60) (#\U+1F69 . #\U+1F61) (#\U+1F6A . #\U+1F62)
                (#\U+1F6B . #\U+1F63) (#\U+1F6C . #\U+1F64) (#\U+1F6D . #\U+1F65)
                (#\U+1F6E . #\U+1F66) (#\U+1F6F . #\U+1F67) (#\U+1F88 . #\U+1F80)
                (#\U+1F89 . #\U+1F81) (#\U+1F8A . #\U+1F82) (#\U+1F8B . #\U+1F83)
                (#\U+1F8C . #\U+1F84) (#\U+1F8D . #\U+1F85) (#\U+1F8E . #\U+1F86)
                (#\U+1F8F . #\U+1F87) (#\U+1F98 . #\U+1F90) (#\U+1F99 . #\U+1F91)
                (#\U+1F9A . #\U+1F92) (#\U+1F9B . #\U+1F93) (#\U+1F9C . #\U+1F94)
                (#\U+1F9D . #\U+1F95) (#\U+1F9E . #\U+1F96) (#\U+1F9F . #\U+1F97)
                (#\U+1FA8 . #\U+1FA0) (#\U+1FA9 . #\U+1FA1) (#\U+1FAA . #\U+1FA2)
                (#\U+1FAB . #\U+1FA3) (#\U+1FAC . #\U+1FA4) (#\U+1FAD . #\U+1FA5)
                (#\U+1FAE . #\U+1FA6) (#\U+1FAF . #\U+1FA7) (#\U+1FB8 . #\U+1FB0)
                (#\U+1FB9 . #\U+1FB1) (#\U+1FBA . #\U+1F70) (#\U+1FBB . #\U+1F71)
                (#\U+1FBC . #\U+1FB3) (#\U+1FC8 . #\U+1F72) (#\U+1FC9 . #\U+1F73)
                (#\U+1FCA . #\U+1F74) (#\U+1FCB . #\U+1F75) (#\U+1FCC . #\U+1FC3)
                (#\U+1FD8 . #\U+1FD0) (#\U+1FD9 . #\U+1FD1) (#\U+1FDA . #\U+1F76)
                (#\U+1FDB . #\U+1F77) (#\U+1FE8 . #\U+1FE0) (#\U+1FE9 . #\U+1FE1)
                (#\U+1FEA . #\U+1F7A) (#\U+1FEB . #\U+1F7B) (#\U+1FEC . #\U+1FE5)
                (#\U+1FF8 . #\U+1F78) (#\U+1FF9 . #\U+1F79) (#\U+1FFA . #\U+1F7C)
                (#\U+1FFB . #\U+1F7D) (#\U+1FFC . #\U+1FF3) (#\U+2132 . #\U+214E)
                (#\U+2160 . #\U+2170) (#\U+2161 . #\U+2171) (#\U+2162 . #\U+2172)
                (#\U+2163 . #\U+2173) (#\U+2164 . #\U+2174) (#\U+2165 . #\U+2175)
                (#\U+2166 . #\U+2176) (#\U+2167 . #\U+2177) (#\U+2168 . #\U+2178)
                (#\U+2169 . #\U+2179) (#\U+216A . #\U+217A) (#\U+216B . #\U+217B)
                (#\U+216C . #\U+217C) (#\U+216D . #\U+217D) (#\U+216E . #\U+217E)
                (#\U+216F . #\U+217F) (#\U+2183 . #\U+2184) (#\U+24B6 . #\U+24D0)
                (#\U+24B7 . #\U+24D1) (#\U+24B8 . #\U+24D2) (#\U+24B9 . #\U+24D3)
                (#\U+24BA . #\U+24D4) (#\U+24BB . #\U+24D5) (#\U+24BC . #\U+24D6)
                (#\U+24BD . #\U+24D7) (#\U+24BE . #\U+24D8) (#\U+24BF . #\U+24D9)
                (#\U+24C0 . #\U+24DA) (#\U+24C1 . #\U+24DB) (#\U+24C2 . #\U+24DC)
                (#\U+24C3 . #\U+24DD) (#\U+24C4 . #\U+24DE) (#\U+24C5 . #\U+24DF)
                (#\U+24C6 . #\U+24E0) (#\U+24C7 . #\U+24E1) (#\U+24C8 . #\U+24E2)
                (#\U+24C9 . #\U+24E3) (#\U+24CA . #\U+24E4) (#\U+24CB . #\U+24E5)
                (#\U+24CC . #\U+24E6) (#\U+24CD . #\U+24E7) (#\U+24CE . #\U+24E8)
                (#\U+24CF . #\U+24E9) (#\U+2C00 . #\U+2C30) (#\U+2C01 . #\U+2C31)
                (#\U+2C02 . #\U+2C32) (#\U+2C03 . #\U+2C33) (#\U+2C04 . #\U+2C34)
                (#\U+2C05 . #\U+2C35) (#\U+2C06 . #\U+2C36) (#\U+2C07 . #\U+2C37)
                (#\U+2C08 . #\U+2C38) (#\U+2C09 . #\U+2C39) (#\U+2C0A . #\U+2C3A)
                (#\U+2C0B . #\U+2C3B) (#\U+2C0C . #\U+2C3C) (#\U+2C0D . #\U+2C3D)
                (#\U+2C0E . #\U+2C3E) (#\U+2C0F . #\U+2C3F) (#\U+2C10 . #\U+2C40)
                (#\U+2C11 . #\U+2C41) (#\U+2C12 . #\U+2C42) (#\U+2C13 . #\U+2C43)
                (#\U+2C14 . #\U+2C44) (#\U+2C15 . #\U+2C45) (#\U+2C16 . #\U+2C46)
                (#\U+2C17 . #\U+2C47) (#\U+2C18 . #\U+2C48) (#\U+2C19 . #\U+2C49)
                (#\U+2C1A . #\U+2C4A) (#\U+2C1B . #\U+2C4B) (#\U+2C1C . #\U+2C4C)
                (#\U+2C1D . #\U+2C4D) (#\U+2C1E . #\U+2C4E) (#\U+2C1F . #\U+2C4F)
                (#\U+2C20 . #\U+2C50) (#\U+2C21 . #\U+2C51) (#\U+2C22 . #\U+2C52)
                (#\U+2C23 . #\U+2C53) (#\U+2C24 . #\U+2C54) (#\U+2C25 . #\U+2C55)
                (#\U+2C26 . #\U+2C56) (#\U+2C27 . #\U+2C57) (#\U+2C28 . #\U+2C58)
                (#\U+2C29 . #\U+2C59) (#\U+2C2A . #\U+2C5A) (#\U+2C2B . #\U+2C5B)
                (#\U+2C2C . #\U+2C5C) (#\U+2C2D . #\U+2C5D) (#\U+2C2E . #\U+2C5E)
                (#\U+2C60 . #\U+2C61) (#\U+2C62 . #\Latin_Small_Letter_L_With_Middle_Tilde)
                (#\U+2C63 . #\U+1D7D) (#\U+2C64 . #\Latin_Small_Letter_R_With_Tail)
                (#\U+2C67 . #\U+2C68) (#\U+2C69 . #\U+2C6A) (#\U+2C6B . #\U+2C6C)
                (#\U+2C75 . #\U+2C76) (#\U+2C80 . #\U+2C81) (#\U+2C82 . #\U+2C83)
                (#\U+2C84 . #\U+2C85) (#\U+2C86 . #\U+2C87) (#\U+2C88 . #\U+2C89)
                (#\U+2C8A . #\U+2C8B) (#\U+2C8C . #\U+2C8D) (#\U+2C8E . #\U+2C8F)
                (#\U+2C90 . #\U+2C91) (#\U+2C92 . #\U+2C93) (#\U+2C94 . #\U+2C95)
                (#\U+2C96 . #\U+2C97) (#\U+2C98 . #\U+2C99) (#\U+2C9A . #\U+2C9B)
                (#\U+2C9C . #\U+2C9D) (#\U+2C9E . #\U+2C9F) (#\U+2CA0 . #\U+2CA1)
                (#\U+2CA2 . #\U+2CA3) (#\U+2CA4 . #\U+2CA5) (#\U+2CA6 . #\U+2CA7)
                (#\U+2CA8 . #\U+2CA9) (#\U+2CAA . #\U+2CAB) (#\U+2CAC . #\U+2CAD)
                (#\U+2CAE . #\U+2CAF) (#\U+2CB0 . #\U+2CB1) (#\U+2CB2 . #\U+2CB3)
                (#\U+2CB4 . #\U+2CB5) (#\U+2CB6 . #\U+2CB7) (#\U+2CB8 . #\U+2CB9)
                (#\U+2CBA . #\U+2CBB) (#\U+2CBC . #\U+2CBD) (#\U+2CBE . #\U+2CBF)
                (#\U+2CC0 . #\U+2CC1) (#\U+2CC2 . #\U+2CC3) (#\U+2CC4 . #\U+2CC5)
                (#\U+2CC6 . #\U+2CC7) (#\U+2CC8 . #\U+2CC9) (#\U+2CCA . #\U+2CCB)
                (#\U+2CCC . #\U+2CCD) (#\U+2CCE . #\U+2CCF) (#\U+2CD0 . #\U+2CD1)
                (#\U+2CD2 . #\U+2CD3) (#\U+2CD4 . #\U+2CD5) (#\U+2CD6 . #\U+2CD7)
                (#\U+2CD8 . #\U+2CD9) (#\U+2CDA . #\U+2CDB) (#\U+2CDC . #\U+2CDD)
                (#\U+2CDE . #\U+2CDF) (#\U+2CE0 . #\U+2CE1) (#\U+2CE2 . #\U+2CE3)
                (#\U+FF21 . #\U+FF41) (#\U+FF22 . #\U+FF42) (#\U+FF23 . #\U+FF43)
                (#\U+FF24 . #\U+FF44) (#\U+FF25 . #\U+FF45) (#\U+FF26 . #\U+FF46)
                (#\U+FF27 . #\U+FF47) (#\U+FF28 . #\U+FF48) (#\U+FF29 . #\U+FF49)
                (#\U+FF2A . #\U+FF4A) (#\U+FF2B . #\U+FF4B) (#\U+FF2C . #\U+FF4C)
                (#\U+FF2D . #\U+FF4D) (#\U+FF2E . #\U+FF4E) (#\U+FF2F . #\U+FF4F)
                (#\U+FF30 . #\U+FF50) (#\U+FF31 . #\U+FF51) (#\U+FF32 . #\U+FF52)
                (#\U+FF33 . #\U+FF53) (#\U+FF34 . #\U+FF54) (#\U+FF35 . #\U+FF55)
                (#\U+FF36 . #\U+FF56) (#\U+FF37 . #\U+FF57) (#\U+FF38 . #\U+FF58)
                (#\U+FF39 . #\U+FF59) (#\U+FF3A . #\U+FF5A) (#\U+10400 . #\U+10428)
                (#\U+10401 . #\U+10429) (#\U+10402 . #\U+1042A) (#\U+10403 . #\U+1042B)
                (#\U+10404 . #\U+1042C) (#\U+10405 . #\U+1042D) (#\U+10406 . #\U+1042E)
                (#\U+10407 . #\U+1042F) (#\U+10408 . #\U+10430) (#\U+10409 . #\U+10431)
                (#\U+1040A . #\U+10432) (#\U+1040B . #\U+10433) (#\U+1040C . #\U+10434)
                (#\U+1040D . #\U+10435) (#\U+1040E . #\U+10436) (#\U+1040F . #\U+10437)
                (#\U+10410 . #\U+10438) (#\U+10411 . #\U+10439) (#\U+10412 . #\U+1043A)
                (#\U+10413 . #\U+1043B) (#\U+10414 . #\U+1043C) (#\U+10415 . #\U+1043D)
                (#\U+10416 . #\U+1043E) (#\U+10417 . #\U+1043F) (#\U+10418 . #\U+10440)
                (#\U+10419 . #\U+10441) (#\U+1041A . #\U+10442) (#\U+1041B . #\U+10443)
                (#\U+1041C . #\U+10444) (#\U+1041D . #\U+10445) (#\U+1041E . #\U+10446)
                (#\U+1041F . #\U+10447) (#\U+10420 . #\U+10448) (#\U+10421 . #\U+10449)
                (#\U+10422 . #\U+1044A) (#\U+10423 . #\U+1044B) (#\U+10424 . #\U+1044C)
                (#\U+10425 . #\U+1044D) (#\U+10426 . #\U+1044E) (#\U+10427 . #\U+1044F)))
  (destructuring-bind (upper . lower) pair
    (setf (gethash upper *non-standard-upper-to-lower*) lower
          (gethash lower *non-standard-lower-to-upper*) upper)))

(assert-hash-table-readonly *non-standard-upper-to-lower*)
(assert-hash-table-readonly *non-standard-lower-to-upper*)

(defun %non-standard-upper-case-equivalent (char)
  (gethash char *non-standard-lower-to-upper*))

;;;True for a-z, and maybe other things.
(defun lower-case-p (c)
  "The argument must be a character object; LOWER-CASE-P returns T if the
   argument is a lower-case character, NIL otherwise."
  (let ((code (char-code c)))
    (if (< code #x80)
      (and (>= code (char-code #\a))
           (<= code (char-code #\z)))
     (not (null (%non-standard-upper-case-equivalent c))))))


;;;True for a-z A-Z, others.


(defun alpha-char-p (c)
  "The argument must be a character object. ALPHA-CHAR-P returns T if the
   argument is an alphabetic character; otherwise NIL."
  (let* ((code (char-code c)))
    (declare (fixnum code))
    (or (and (>= code (char-code #\A)) (<= code (char-code #\Z)))
        (and (>= code (char-code #\a)) (<= code (char-code #\z)))
        (and (>= code #x80)
             (or (not (null (%non-standard-upper-case-equivalent c)))
                 (not (null (%non-standard-lower-case-equivalent c))))))))




;;; def-accessors type-tracking stuff.  Used by inspector
(defvar *def-accessor-types* nil)

(defun add-accessor-types (types names)
  (dolist (type types)
    (let ((cell (or (assq type *def-accessor-types*)
                    (car (push (cons type nil) *def-accessor-types*)))))
      (setf (cdr cell) (if (vectorp names) names (%list-to-uvector nil names))))))


;;; Some simple explicit storage management for cons cells

(def-standard-initial-binding *cons-pool* (%cons-pool nil))

(defun cheap-cons (car cdr)
  (let* ((pool *cons-pool*)
         (cons (pool.data pool)))
    (if cons
      (locally (declare (type cons cons))
        (setf (pool.data pool) (cdr cons)
              (car cons) car
              (cdr cons) cdr)
        cons)
      (cons car cdr))))

(defun free-cons (cons)
  (when (consp cons)
    (locally (declare (type cons cons))
      (setf (car cons) nil
            (cdr cons) nil)
      (let* ((pool *cons-pool*)
             (freelist (pool.data pool)))
        (setf (pool.data pool) cons
              (cdr cons) freelist)))))

(defun cheap-copy-list (list)
  (let ((l list)
        res)
    (loop
      (when (atom l)
        (return (nreconc res l)))
      (setq res (cheap-cons (pop l) res)))))

(defun cheap-list (&rest args)
  (declare (dynamic-extent args))
  (cheap-copy-list args))

;;; Works for dotted lists
(defun cheap-free-list (list)
  (let ((l list)
        next-l)
    (loop
      (setq next-l (cdr l))
      (free-cons l)
      (when (atom (setq l next-l))
        (return)))))

(defmacro pop-and-free (place)
  (setq place (require-type place 'symbol))     ; all I need for now.
  (let ((list (gensym))
        (cdr (gensym)))
    `(let* ((,list ,place)
            (,cdr (cdr ,list)))
       (prog1
         (car ,list)
         (setf ,place ,cdr)
         (free-cons ,list)))))

;;; Support for defresource & using-resource macros
(defun make-resource (constructor &key destructor initializer)
  (%cons-resource constructor destructor initializer))

(defun allocate-resource (resource)
  (setq resource (require-type resource 'resource))
  (with-lock-grabbed ((resource.lock resource))
    (let ((pool (resource.pool resource))
          res)
      (let ((data (pool.data pool)))
        (when data
          (setf res (car data)
                (pool.data pool) (cdr (the cons data)))
          (free-cons data)))
      (if res
        (let ((initializer (resource.initializer resource)))
          (when initializer
            (funcall initializer res)))
        (setq res (funcall (resource.constructor resource))))
      res)))

(defun free-resource (resource instance)
  (setq resource (require-type resource 'resource))
  (with-lock-grabbed ((resource.lock resource))
    (let ((pool (resource.pool resource))
          (destructor (resource.destructor resource)))
      (when destructor
        (funcall destructor instance))
      (setf (pool.data pool)
            (cheap-cons instance (pool.data pool)))))
  resource)




(defpackage #.(ftd-interface-package-name
               (backend-target-foreign-type-data *target-backend*))
  (:nicknames "OS")
  (:use "COMMON-LISP"))



