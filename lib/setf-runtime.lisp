; -*- Mode: Lisp; Package: CCL; -*-
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



; End of setf-runtime.lisp
