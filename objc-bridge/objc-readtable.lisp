;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 2002-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

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

