; -*- Mode: Lisp; Package: CCL; -*-
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

;
; setf-runtime.lisp - runtime support for setf expressions

(in-package "CCL")

(defun set-cadr (list new-value)
  (set-car (cdr list) new-value))

(defun set-cdar (list new-value)
  (set-cdr (car list) new-value))

(defun set-caar (list new-value)
  (set-car (car list) new-value))

(defun set-cddr (list new-value)
  (set-cdr (cdr list) new-value))

(defun %set-nthcdr (index list new-value)
  "If INDEX is 0, just return NEW-VALUE."
  (if (not (zerop index))
    (rplacd (nthcdr (1- index) list)
            new-value))
  new-value)

(defun set-fifth (list new-value)
  (set-car (cddddr list) new-value))

(defun set-sixth (list new-value)
  (set-car (cdr (cddddr list)) new-value))

(defun set-seventh (list new-value)
  (set-car (cddr (cddddr list)) new-value))

(defun set-eighth (list new-value)
  (set-car (cdddr (cddddr list)) new-value))

(defun set-ninth (list new-value)
  (set-car (cddddr (cddddr list)) new-value))

(defun set-tenth (list new-value)
  (set-car (cdr (cddddr (cddddr list))) new-value))

(defun set-caaar (list new-value)
  (set-car (caar list) new-value))

(defun set-caadr (list new-value)
  (set-car (cadr list) new-value))

(defun set-cadar (list new-value)
  (set-car (cdar list) new-value))

(defun set-caddr (list new-value)
  (set-car (cddr list) new-value))

(defun set-cdaar (list new-value)
  (set-cdr (caar list) new-value))

(defun set-cdadr (list new-value)
  (set-cdr (cadr list) new-value))

(defun set-cddar (list new-value)
  (set-cdr (cdar list) new-value))

(defun set-cdddr (list new-value)
  (set-cdr (cddr list) new-value))

(defun set-caaaar (list new-value)
  (set-car (caaar list) new-value))

(defun set-caaadr (list new-value)
  (set-car (caadr list) new-value))

(defun set-caadar (list new-value)
  (set-car (cadar list) new-value))

(defun set-caaddr (list new-value)
  (set-car (caddr list) new-value))

(defun set-cadaar (list new-value)
  (set-car (cdaar list) new-value))

(defun set-cadadr (list new-value)
  (set-car (cdadr list) new-value))

(defun set-caddar (list new-value)
  (set-car (cddar list) new-value))

(defun set-cadddr (list new-value)
  (set-car (cdddr list) new-value))

(defun set-cdaaar (list new-value)
  (set-cdr (caaar list) new-value))

(defun set-cdaadr (list new-value)
  (set-cdr (caadr list) new-value))

(defun set-cdadar (list new-value)
  (set-cdr (cadar list) new-value))

(defun set-cdaddr (list new-value)
  (set-cdr (caddr list) new-value))

(defun set-cddaar (list new-value)
  (set-cdr (cdaar list) new-value))

(defun set-cddadr (list new-value)
  (set-cdr (cdadr list) new-value))

(defun set-cdddar (list new-value)
  (set-cdr (cddar list) new-value))

(defun set-cddddr (list new-value)
  (set-cdr (cdddr list) new-value))

(defun (setf package-%local-nicknames) (newval package)
  (setf-package-%local-nicknames newval package))
(defun (setf package-%locally-nicknamed-by) (newval package)
  (setf-package-%locally-nicknamed-by newval package))
