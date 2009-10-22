;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2002-2009 Clozure Associates
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *objc-readtable* (copy-readtable nil))
  (set-syntax-from-char #\] #\) *objc-readtable*))



;;; We use the convention that [:super ....] denotes a send to the
;;; defining object's superclass's method, and that a return value
;;; specification of the form (:-> ... x) indicates a message send
;;; that returns a structure (by reference) via the pointer x.

(set-macro-character
 #\[
 (nfunction
  |objc-[-reader|
  (lambda (stream ignore)
    (declare (ignore ignore))
    (let* ((tail (read-delimited-list #\] stream))
	   (structptr nil))
      (unless *read-suppress*
        (let* ((return (car (last tail))))
          (when (and (consp return) (eq (car return) :->))
            (rplaca (last tail) :void)
            (setq structptr (car (last return)))))
        (if (eq (car tail) :super)
          (if structptr
            `(objc-message-send-super-stret ,structptr (super) ,@(cdr tail))
            `(objc-message-send-super (super) ,@(cdr tail)))
          (if structptr
            `(objc-message-send-stret ,structptr ,@tail)
            `(objc-message-send ,@tail)))))))
 nil
 *objc-readtable*)

(set-dispatch-macro-character
 #\#
 #\@
 (nfunction
  |objc-#@-reader|
  (lambda (stream subchar numarg)
    (declare (ignore subchar numarg))
    (let* ((string (read stream)))
      (unless *read-suppress*
        (check-type string string)
        `(@ ,string)))))
 *objc-readtable*)

