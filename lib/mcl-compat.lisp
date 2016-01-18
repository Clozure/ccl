;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
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
