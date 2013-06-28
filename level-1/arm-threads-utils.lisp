;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates
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
  (cond ((fake-stack-frame-p p)
         (%fixnum-ref p arm::fake-stack-frame.next-sp))
        ((fixnump p) (%%frame-backlink p))
        (t (error "~s is not a valid stack frame" p))))



(defun catch-frame-sp (catch)
  (%stack-block ((ptr arm::node-size))
    (%set-object ptr 0 catch)           ;catch frame is stack-consed
    (setf (%get-long ptr) (logandc2 (%get-long ptr) arm::fulltagmask))
    (+ (%get-object ptr 0)
       (1+ arm::catch-frame.element-count))))

(defun fake-stack-frame-p (x)
  (and (typep x 'fixnum)
       (evenp x)
       (eql (%fixnum-ref-natural x)
            (logior (ash (ash (- arm::fake-stack-frame.size arm::node-size)
                              (- arm::word-shift))
                         arm::num-subtag-bits)
                    arm::subtag-istruct))
       (let* ((type (%fixnum-ref x arm::node-size)))
         (and (consp type)
              (eq (car type) 'arm::fake-stack-frame)))))

(defun current-fake-stack-frame ()
  (do* ((p (%get-frame-ptr) (%%frame-backlink p)))
       ((or (zerop p) (bottom-of-stack-p p nil)))
    (when (fake-stack-frame-p p) (return p))))



(defun bottom-of-stack-p (p context)
  (and (fixnump p)
       (locally (declare (fixnum p))
	 (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
                (cs-area (%fixnum-ref tcr target::tcr.cs-area)))
	   (not (%ptr-in-area-p p cs-area))))))

(defun lisp-frame-p (p context)
  (if (bottom-of-stack-p p context)
    (values nil t)
    (values (or (fake-stack-frame-p p)
                (eql (%fixnum-ref-natural p) arm::lisp-frame-marker)) nil)))





(defun valid-subtag-p (subtag)
  (declare (fixnum subtag))
  (let* ((tagval (ldb (byte (- arm::num-subtag-bits arm::ntagbits) arm::ntagbits) subtag)))
    (declare (fixnum tagval))
    (case (logand subtag arm::fulltagmask)
      (#. arm::fulltag-immheader (not (eq (%svref *immheader-types* tagval) 'bogus)))
      (#. arm::fulltag-nodeheader (not (eq (%svref *nodeheader-types* tagval) 'bogus)))
      (t nil))))



(defun valid-header-p (thing)
  (let* ((fulltag (fulltag thing)))
    (declare (fixnum fulltag))
    (case fulltag
      (#.arm::fulltag-misc (valid-subtag-p (typecode thing)))
      ((#.arm::fulltag-immheader #.arm::fulltag-nodeheader) nil)
      (t t))))






(defun bogus-thing-p (x)
  (when x
    #+cross-compiling (return-from bogus-thing-p nil)
    (or (not (valid-header-p x))
        (let ((tag (lisptag x)))
          (unless (or (eql tag arm::tag-fixnum)
                      (eql tag arm::tag-imm)
                      (in-any-consing-area-p x))
            ;; This is terribly complicated, should probably write some LAP
            (let ((typecode (typecode x)))
                  (not (or (case typecode
                             (#.arm::tag-list
                              (temporary-cons-p x))
                             ((#.arm::subtag-symbol #.arm::subtag-code-vector)
                              t)              ; no stack-consed symbols or code vectors
                             (#.arm::subtag-value-cell
                              (on-any-vstack x))
                             (t
                              (on-any-csp-stack x)))
                           (%heap-ivector-p x)))))))))





