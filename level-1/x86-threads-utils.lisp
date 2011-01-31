;;; x86-trap-support
;;;
;;;   Copyright (C) 2005-2009 Clozure Associates and contributors
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


(defun %frame-backlink (p &optional context)
  (declare (ignore context))
  (cond ((fixnump p) (%%frame-backlink p))
        (t (error "~s is not a valid stack frame" p))))

(defun bottom-of-stack-p (p context)
  (and (fixnump p)
       (locally (declare (fixnum p))
	 (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
                (vs-area (%fixnum-ref tcr (- target::tcr.vs-area
					     target::tcr-bias))))
	   (not (%ptr-in-area-p p vs-area))))))


(defun lisp-frame-p (p context)
  (declare (fixnum p))
  (let ((next-frame (%frame-backlink p context)))
    (declare (fixnum next-frame))
    (if (bottom-of-stack-p next-frame context)
        (values nil t)
        (values t nil))))


(defun catch-frame-sp (catch)
  (uvref catch
	 #+x8632-target x8632::catch-frame.ebp-cell
	 #+x8664-target x8664::catch-frame.rbp-cell))

;;; Sure would be nice to have &optional in defppclapfunction arglists
;;; Sure would be nice not to do this at runtime.

(let ((bits (lfun-bits #'(lambda (x &optional y) (declare (ignore x y))))))
  (lfun-bits #'%fixnum-ref
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-ref)))))

(let ((bits (lfun-bits #'(lambda (x &optional y) (declare (ignore x y))))))
  (lfun-bits #'%fixnum-ref-natural
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-ref-natural)))))


;;; Sure would be nice to have &optional in defppclapfunction arglists
;;; Sure would be nice not to do this at runtime.

(let ((bits (lfun-bits #'(lambda (x &optional y) (declare (ignore x y))))))
  (lfun-bits #'%fixnum-ref
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-ref)))))

(let ((bits (lfun-bits #'(lambda (x &optional y) (declare (ignore x y))))))
  (lfun-bits #'%fixnum-ref-natural
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-ref-natural)))))

(let ((bits (lfun-bits #'(lambda (x y &optional z) (declare (ignore x y z))))))
  (lfun-bits #'%fixnum-set
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-set)))))

(let ((bits (lfun-bits #'(lambda (x y &optional z) (declare (ignore x y z))))))
  (lfun-bits #'%fixnum-set-natural
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-set-natural)))))


#+x8632-target
(defun valid-subtag-p (subtag)
  (declare (fixnum subtag))
  (let* ((tagval (ldb (byte (- x8632::num-subtag-bits x8632::ntagbits) x8632::ntagbits) subtag)))
    (declare (fixnum tagval))
    (case (logand subtag x8632::fulltagmask)
      (#. x8632::fulltag-immheader (not (eq (%svref *immheader-types* tagval) 'bogus)))
      (#. x8632::fulltag-nodeheader (not (eq (%svref *nodeheader-types* tagval) 'bogus)))
      (t nil))))

#+x8664-target
(defun valid-subtag-p (subtag)
  (declare (fixnum subtag))
  (let* ((tagval (logand x8664::fulltagmask subtag))
         (high4 (ash subtag (- x8664::ntagbits))))
    (declare (fixnum tagval high4))
    (not (eq 'bogus
             (case tagval
               (#.x8664::fulltag-immheader-0
                (%svref *immheader-0-types* high4))
               (#.x8664::fulltag-immheader-1
                (%svref *immheader-1-types* high4))
               (#.x8664::fulltag-immheader-2
                (%svref *immheader-2-types* high4))
               (#.x8664::fulltag-nodeheader-0
                (%svref *nodeheader-0-types* high4))
               (#.x8664::fulltag-nodeheader-1
                (%svref *nodeheader-1-types* high4))
               (t 'bogus))))))

#+x8632-target
(defun valid-header-p (thing)
  (let* ((fulltag (fulltag thing)))
    (declare (fixnum fulltag))
    (case fulltag
      (#.x8632::fulltag-misc (valid-subtag-p (typecode thing)))
      ((#.x8632::fulltag-immheader #.x8632::fulltag-nodeheader) nil)
      (t t))))

#+x8664-target
(defun valid-header-p (thing)
  (let* ((fulltag (fulltag thing)))
    (declare (fixnum fulltag))
    (case fulltag
      ((#.x8664::fulltag-even-fixnum
        #.x8664::fulltag-odd-fixnum
        #.x8664::fulltag-imm-0
        #.x8664::fulltag-imm-1)
       t)
      (#.x8664::fulltag-function
       (= x8664::subtag-function (typecode (%function-to-function-vector thing))))
      (#.x8664::fulltag-symbol
       (= x8664::subtag-symbol (typecode (%symptr->symvector thing))))
      (#.x8664::fulltag-misc
       (valid-subtag-p (typecode thing)))
      ((#.x8664::fulltag-tra-0
        #.x8664::fulltag-tra-1)
       (let* ((disp (%return-address-offset thing)))
         (and disp
              (let* ((f (%return-address-function thing)))
                (and (typep f 'function) (valid-header-p f))))))
      (#.x8664::fulltag-cons t)
      (#.x8664::fulltag-nil (null thing))
      (t nil))))
             
#+x8632-target
(defun bogus-thing-p (x)
  (when x
    (or (not (valid-header-p x))
        (let ((tag (lisptag x))
	      (fulltag (fulltag x)))
          (unless (or (eql tag x8632::tag-fixnum)
                      (eql tag x8632::tag-imm)
                      (in-any-consing-area-p x)
		      (temporary-cons-p x)
		      (and (or (typep x 'function)
			       (typep x 'gvector))
			   (on-any-tsp-stack x))
		      (and (eql fulltag x8632::fulltag-tra)
			   (%return-address-offset x))
		      (and (typep x 'ivector)
			   (on-any-csp-stack x))
		      (%heap-ivector-p x))
	    t)))))

#+x8664-target
(defun bogus-thing-p (x)
  (when x
    (or (not (valid-header-p x))
        (let* ((tag (lisptag x)))
          (unless (or (eql tag x8664::tag-fixnum)
                      (eql tag x8664::tag-imm-0)
                      (eql tag x8664::tag-imm-1)
                      (in-any-consing-area-p x)
                      (temporary-cons-p x)
                      (and (or (typep x 'function)
                               (typep x 'gvector))
                           (on-any-tsp-stack x))
                      (and (eql tag x8664::tag-tra)
                           (eql 0 (%return-address-offset x)))
                      (and (typep x 'ivector)
                           (on-any-csp-stack x))
                      (%heap-ivector-p x))
            t)))))

