;;; Copyright 2009 Clozure Associates
;;; This file is part of Clozure CL.
;;;
;;; Clozure CL is licensed under the terms of the Lisp Lesser GNU
;;; Public License , known as the LLGPL and distributed with Clozure
;;; CL as the file "LICENSE".  The LLGPL consists of a preamble and
;;; the LGPL, which is distributed with Clozure CL as the file "LGPL".
;;; Where these conflict, the preamble takes precedence.
;;;
;;; Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;; The LLGPL is also available online at
;;; http://opensource.franz.com/preamble.html

(in-package "CCL")

(defloadvar *readonly-remapped-p* nil)

(defun %remap-readonly-area ()
  (unless *readonly-remapped-p*
    (impurify)
    (let* ((a (do-consing-areas (a)
                (when (eql (%fixnum-ref a target::area.code)
                           ccl::area-readonly)
                  (return a))))
           (low (%int-to-ptr (ash (%fixnum-ref a target::area.low) target::fixnumshift)))
           (active (ash (%fixnum-ref a target::area.active) target::fixnumshift))
           (high (ash (%fixnum-ref a target::area.active) target::fixnumshift))
           (tsize (- high (%ptr-to-int low)))
           (lsize (- active (%ptr-to-int low)))
           (p (#_malloc lsize)))
      (#_memcpy p low lsize)
      (#_munmap low tsize)
      (#_mmap low
              tsize
              (logior #$PROT_READ #$PROT_WRITE #$PROT_EXEC)
              (logior #$MAP_FIXED #$MAP_ANONYMOUS #$MAP_PRIVATE)
              -1
              0)
      (#_memcpy low p lsize)
      (#_mprotect low tsize (logior #$PROT_READ  #$PROT_EXEC))
      (#_free p)
      (setq *readonly-remapped-p* t))))

(defun perf-lisp-function-name (f)
  (let* ((name (function-name f)))
    (if (and (symbolp name)
	     (eq f (fboundp name)))
      (with-standard-io-syntax
	(format nil "~s" name))
      (let ((str (format nil "~s" f)))
	(subseq (nsubstitute #\0 #\# (nsubstitute #\. #\Space str)) 1)))))

#+x86-target
(defun collect-pure-functions ()
  (purify)
  (collect ((functions))
    (%map-areas (lambda (o)
                  (when (typep o
                               #+x8664-target 'function-vector
                               #-x8664-target 'function)
                    (functions (function-vector-to-function o))))
                :readonly)
    (functions)))

(defun write-perf-map (stream)
  (dolist (f (collect-pure-functions))
    (format stream "~16,'0x ~x ~a~%"
            (logandc2 (%address-of f) target::fulltagmask)
            (1+ (ash (1- (%function-code-words f)) target::word-shift))
            (perf-lisp-function-name f))))

(defun create-perf-map (&key path)
  (let* ((pid (getpid))
         (path (or path (format nil "/tmp/perf-~d.map" pid))))
    (%remap-readonly-area)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-perf-map out))))
