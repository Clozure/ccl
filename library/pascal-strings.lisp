;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 2003-2009 Clozure Associates
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


;;; Utilities for dealing with Pascal strings
;;;
;;; In 68K Mac Pascal, strings were represented by a pointer to a
;;; "length byte", which indicated the number of data bytes immediately
;;; following.

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; What else should be exported ?  What else should be added
  ;; to this file ?
  (export '(with-pstrs with-returned-pstrs %get-string)))

(defun %pstr-pointer (string pointer)
  (multiple-value-bind (s o n) (dereference-base-string string)
    (declare (fixnum o n))
    (%copy-ivector-to-ptr s o pointer 1 n)
    (setf (%get-byte pointer 0) n))
  nil)

(defun %pstr-segment-pointer (string pointer start end)
  (declare (fixnum start end))
  (let* ((n (- end start)))
    (multiple-value-bind (s o) (dereference-base-string string)
      (declare (fixnum o))
      (%copy-ivector-to-ptr s (the fixnum (+ o start)) pointer 1 n)
    (setf (%get-byte pointer 0) n)
    nil)))

(defun %get-string (pointer)
  (let* ((len (%get-unsigned-byte pointer)))
    (%copy-ptr-to-ivector
     pointer
     1
     (make-string len :element-type 'base-char)
     0
     len)))

(defun (setf %get-string) (lisp-string pointer)
  (let* ((len (length lisp-string)))
    (multiple-value-bind (string offset)
        (dereference-base-string lisp-string)
      (setf (%get-unsigned-byte pointer) len)
      (%copy-ivector-to-ptr string offset pointer 1 len))
    lisp-string))

(defmacro with-pstr ((sym str &optional start end) &rest body &environment env)
  (multiple-value-bind (body decls) (parse-body body env nil)
    (if (and (base-string-p str) (null start) (null end))
      (let ((strlen (%i+ (length str) 1)))
        `(%stack-block ((,sym ,strlen))
           ,@decls
           (%pstr-pointer ,str ,sym)
           ,@body))
      (let ((strname (gensym))
            (start-name (gensym))
            (end-name (gensym)))
        `(let ((,strname ,str)
               ,@(if (or start end)
                   `((,start-name ,(or start 0))
                     (,end-name ,(or end `(length ,strname))))))
           (%vstack-block (,sym
                           (the fixnum
                             (1+
                              (the fixnum
                                ,(if (or start end)
                                     `(byte-length
                                       ,strname ,start-name ,end-name)
                                     `(length ,strname))))))
             ,@decls
             ,(if (or start end)
                `(%pstr-segment-pointer ,strname ,sym ,start-name ,end-name)
                `(%pstr-pointer ,strname ,sym))
             ,@body))))))


(defmacro with-returned-pstr ((sym str &optional start end) &body body)
   `(%stack-block ((,sym 256))
      ,(if (or start end)
         `(%pstr-segment-pointer ,str ,sym ,start ,end)
         `(%pstr-pointer ,str ,sym))
      ,@body))

(defmacro with-pstrs (speclist &body body)
   (with-specs-aux 'with-pstr speclist body))

(defmacro with-returned-pstrs (speclist &body body)
   (with-specs-aux 'with-returned-pstr speclist body))


