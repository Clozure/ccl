;;;-*-Mode: LISP; Package: CCL -*-
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

(in-package "CCL")

;;; mcl-compat.lisp - (some) backwards-compatibility with traditional MCL
;;;  (CLtL2/ANSI, etc.)

;;;  Gratuitous name changes, for the most part:

(deftype base-character () 'base-char)
(deftype extended-character () 'extended-char)

(defmacro define-setf-method (access-fn lambda-list &body body)
  `(define-setf-expander ,access-fn ,lambda-list ,@body))

(defun get-setf-method (form &optional environment)
  (get-setf-expansion-aux form environment nil))

(defun get-setf-method-multiple-value (form &optional environment)
  "Like Get-Setf-Method, but may return multiple new-value variables."
  (get-setf-expansion-aux form environment t))

;;; Traditional MCL I/O primitives:

(defun tyi (stream)
  (let* ((ch (stream-read-char stream)))
    (unless (eq ch :eof) ch)))

(defun untyi (ch &optional stream)
  (stream-unread-char (designated-input-stream stream) ch))

(defun tyo (ch &optional stream)
  (stream-write-char (real-print-stream stream) ch))
