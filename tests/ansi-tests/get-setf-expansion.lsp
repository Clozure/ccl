;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 13 17:05:17 2003
;;;; Contains: Tests for GET-SETF-EXPANSION

(in-package :cl-test)

(deftest get-setf-expansion.error.1
  (signals-error (get-setf-expansion) program-error)
  t)

(deftest get-setf-expansion.error.2
  (signals-error (get-setf-expansion 'x nil nil) program-error)
  t)

;;; FIXME
;;; Tests for proper behavior will go here
;;; There are tests in DEFINE-SETF-EXPANDER too

;;; For a function on which the setf expansion is otherwise
;;; undefined, produce a call to #'(setf <fn>).  Note: this
;;; form has to be present, since portable code walkers may
;;; grovel over the setf expansion (sorry, clisp).

(deftest get-setf-expansion.1
  (let* ((fn (gensym))
	 (vals (multiple-value-list (get-setf-expansion (list fn)))))
    (values
     (length vals)
     (first  vals)
     (second vals)
     (length (third vals))
     (block done
       (subst-if nil
		 #'(lambda (term)
		     (when (equal term `(function (setf ,fn)))
		       (return-from done :good)))
		 (fourth vals)))
     (if (equal (fifth vals) (list fn))
	 :good
       (fifth vals))))
  5 nil nil 1 :good :good)

(deftest get-setf-expansion.2
  (let* ((fn (gensym))
	 (vals (multiple-value-list (get-setf-expansion (list fn) nil))))
    (length vals))
  5)

(deftest get-setf-expansion.3
  (let* ((var (gensym))
	 (vals (multiple-value-list (get-setf-expansion var))))
    (length vals))
  5)
