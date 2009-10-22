;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
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

; low-level support for PPC threads and stack-backtrace printing

(in-package "CCL")


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


  
				  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;



    
    
(defun %frame-backlink (p &optional context)
  (cond ((fake-stack-frame-p p)
         (%fake-stack-frame.next-sp p))
        ((fixnump p)
         (let ((backlink (%%frame-backlink p))
               (fake-frame
                (if context (bt.fake-frames context) *fake-stack-frames*)))
           (loop
             (when (null fake-frame) (return backlink))
             (when (eq backlink (%fake-stack-frame.sp fake-frame))
               (return fake-frame))
             (setq fake-frame (%fake-stack-frame.link fake-frame)))))
        (t (error "~s is not a valid stack frame" p))))




(defun catch-frame-sp (catch)
  (uvref catch target::catch-frame.csp-cell))

(defun bottom-of-stack-p (p context)
  (and (fixnump p)
       (locally (declare (fixnum p))
	 (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
                (cs-area (%fixnum-ref tcr target::tcr.cs-area)))
	   (not (%ptr-in-area-p p cs-area))))))

(defun lisp-frame-p (p context)
  (or (fake-stack-frame-p p)
      (locally (declare (fixnum p))
        (let ((next-frame (%frame-backlink p context)))
          (when (fake-stack-frame-p next-frame)
            (setq next-frame (%fake-stack-frame.sp next-frame)))
          (locally (declare (fixnum next-frame))
            (if (bottom-of-stack-p next-frame context)
              (values nil t)
              (and
               (eql (ash target::lisp-frame.size (- target::fixnum-shift))
                    (the fixnum (- next-frame p)))
               ;; EABI C functions keep their saved LRs where we save FN or 0
               ;; The saved LR of such a function would be fixnum-tagged and never 0.
               (let* ((fn (%fixnum-ref p target::lisp-frame.savefn)))
                 (or (eql fn 0) (typep fn 'function))))))))))





#+ppc32-target
(defun valid-subtag-p (subtag)
  (declare (fixnum subtag))
  (let* ((tagval (ldb (byte (- ppc32::num-subtag-bits ppc32::ntagbits) ppc32::ntagbits) subtag)))
    (declare (fixnum tagval))
    (case (logand subtag ppc32::fulltagmask)
      (#. ppc32::fulltag-immheader (not (eq (%svref *immheader-types* tagval) 'bogus)))
      (#. ppc32::fulltag-nodeheader (not (eq (%svref *nodeheader-types* tagval) 'bogus)))
      (t nil))))

#+ppc64-target
(defun valid-subtag-p (subtag)
  (declare (fixnum subtag))
  (let* ((tagval (ash subtag (- ppc64::nlowtagbits))))
    (declare (fixnum tagval))
    (case (logand subtag ppc64::lowtagmask)
      (#. ppc64::lowtag-immheader (not (eq (%svref *immheader-types* tagval) 'bogus)))
      (#. ppc64::lowtag-nodeheader (not (eq (%svref *nodeheader-types* tagval) 'bogus)))
      (t nil))))

#+ppc32-target
(defun valid-header-p (thing)
  (let* ((fulltag (fulltag thing)))
    (declare (fixnum fulltag))
    (case fulltag
      (#.ppc32::fulltag-misc (valid-subtag-p (typecode thing)))
      ((#.ppc32::fulltag-immheader #.ppc32::fulltag-nodeheader) nil)
      (t t))))



#+ppc64-target
(defun valid-header-p (thing)
  (let* ((fulltag (fulltag thing)))
    (declare (fixnum fulltag))
    (case fulltag
      (#.ppc64::fulltag-misc (valid-subtag-p (typecode thing)))
      ((#.ppc64::fulltag-immheader-0
        #.ppc64::fulltag-immheader-1
        #.ppc64::fulltag-immheader-2
        #.ppc64::fulltag-immheader-3
        #.ppc64::fulltag-nodeheader-0
        #.ppc64::fulltag-nodeheader-1
        #.ppc64::fulltag-nodeheader-2
        #.ppc64::fulltag-nodeheader-3) nil)
      (t t))))




#+ppc32-target
(defun bogus-thing-p (x)
  (when x
    #+cross-compiling (return-from bogus-thing-p nil)
    (or (not (valid-header-p x))
        (let ((tag (lisptag x)))
          (unless (or (eql tag ppc32::tag-fixnum)
                      (eql tag ppc32::tag-imm)
                      (in-any-consing-area-p x))
            ;; This is terribly complicated, should probably write some LAP
            (let ((typecode (typecode x)))
                  (not (or (case typecode
                             (#.ppc32::tag-list
                              (temporary-cons-p x))
                             ((#.ppc32::subtag-symbol #.ppc32::subtag-code-vector)
                              t)              ; no stack-consed symbols or code vectors
                             (#.ppc32::subtag-value-cell
                              (on-any-vstack x))
                             (t
                              (on-any-tsp-stack x)))
                           (%heap-ivector-p x)))))))))



#+ppc64-target
(defun bogus-thing-p (x)
  (when x
    (or (not (valid-header-p x))
        (let ((tag (lisptag x)))
          (unless (or (eql tag ppc64::tag-fixnum)
                      (eql tag ppc64::tag-imm-0)
                      (eql tag ppc64::tag-imm-2)
                      (in-any-consing-area-p x))
            ;; This is terribly complicated, should probably write some LAP
            (let ((typecode (typecode x)))
                  (not (or (case typecode
                             (#.ppc64::fulltag-cons
                              (temporary-cons-p x))
                             ((#.ppc64::subtag-symbol #.ppc64::subtag-code-vector)
                              t)              ; no stack-consed symbols or code vectors
                             (#.ppc64::subtag-value-cell
                              (on-any-vstack x))
                             (t
                              (on-any-tsp-stack x)))
                           (%heap-ivector-p x)))))))))
