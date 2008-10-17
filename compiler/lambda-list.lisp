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


(in-package "CCL")


;;; Compiler functions needed elsewhere

;;; used-by: backtrace, arglist
(defun function-symbol-map (fn)
  (getf (%lfun-info fn) 'function-symbol-map))

(defun %lfun-info-index (fn)
  (and (compiled-function-p fn)
       (let ((bits (lfun-bits fn)))
         (declare (fixnum bits))
         (and (logbitp $lfbits-info-bit bits)
               (%i- (uvsize (function-to-function-vector fn))
                              (if (logbitp $lfbits-noname-bit bits) 2 3))))))
(defun %lfun-info (fn)
  (let* ((index (%lfun-info-index fn)))
    (if index (%svref (function-to-function-vector fn) index))))

(defun uncompile-function (fn)
  (getf (%lfun-info fn) 'function-lambda-expression ))


;;; Lambda-list utilities



;;; Lambda-list verification:

;;; these things MUST be compiled.
(eval-when (:load-toplevel)

(defvar *structured-lambda-list* nil)




(defun parse-body (body env &optional (doc-string-allowed t) &aux
   decls
   doc
   (tail body)
   form)
  (declare (ignore env))
  (loop
   (if (endp tail) (return))  ; otherwise, it has a %car and a %cdr
   (if (and (stringp (setq form (%car tail))) (%cdr tail))
    (if doc-string-allowed
     (setq doc form)
     (return))
    (if (not (and (consp form) (symbolp (%car form)))) 
     (return)
     (if (eq (%car form) 'declare)
      (push form decls)
      (return))))
   (setq tail (%cdr tail)))
  (return-from parse-body (values tail (nreverse decls) doc)))

) ; end of eval-when (load)

;;; End of verify-lambda-list.lisp
