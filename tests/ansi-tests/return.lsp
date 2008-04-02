;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 16:00:32 2003
;;;; Contains: Tests of RETURN

(in-package :cl-test)

;;; RETURN is tested extensively in other files

(deftest return.error.1
  (signals-error (funcall (macro-function 'return)) program-error)
  t)
  
(deftest return.error.2
  (signals-error (funcall (macro-function 'return) '(return nil))
		 program-error)
  t)

(deftest return.error.3
  (signals-error (funcall (macro-function 'return)
			  '(return nil) nil nil)
		 program-error)
  t)

;;;

(deftest return.1
  (block nil (return) :bad)
  nil)

(deftest return.2
  (block nil (return :good) :bad)
  :good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest return.3
  (macrolet
   ((%m (z) z))
   (block nil (return (expand-in-current-env (%m :good))) :bad))
  :good)
