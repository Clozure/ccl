;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Dec 12 16:17:47 2004
;;;; Contains: Tests for APROPOS

(in-package :cl-test)

(deftest apropos.1
  (loop for n from 10
	for x = (coerce (loop repeat n collect (random-from-seq +standard-chars+)) 'string)
	unless (apropos-list x)
	return (with-output-to-string (*standard-output*)
				      (assert (null (multiple-value-list (apropos x))))))
  "")

(deftest apropos.2
  (let ((s (with-output-to-string
	     (*standard-output*)
	     (assert (null (multiple-value-list (apropos "CAR")))))))
    (notnot (search "CAR" s :test #'string-equal)))
  t)

(deftest apropos.3
  (let ((s (with-output-to-string
	     (*standard-output*)
	     (assert (null (multiple-value-list (apropos "CAR" (find-package "CL"))))))))
    (notnot (search "CAR" s :test #'string-equal)))
  t)

(deftest apropos.4
  (let ((result nil))
    (do-special-strings
     (s "CAR" t)
     (setq result (with-output-to-string
		    (*standard-output*)
		    (assert (null (multiple-value-list (apropos s))))))
     (assert (search "CAR" result :test #'string-equal))))
  t)

(deftest apropos.5
  (let ((result nil)
	(pkg (find-package "COMMON-LISP")))
    (do-special-strings
     (s "APROPOS" t)
     (setq result (with-output-to-string
		    (*standard-output*)
		    (assert (null (multiple-value-list (apropos s pkg))))))
     (assert (search "APROPOS" result :test #'string-equal))))
  t)

(deftest apropos.6
  (let ((s (with-output-to-string
	     (*standard-output*)
	     (assert (null (multiple-value-list (apropos "CAR" "CL")))))))
    (notnot (search "CAR" s :test #'string-equal)))
  t)

(deftest apropos.7
  (let ((s (with-output-to-string
	     (*standard-output*)
	     (assert (null (multiple-value-list (apropos "CAR" :|CL|)))))))
    (notnot (search "CAR" s :test #'string-equal)))
  t)

(deftest apropos.8
  (let ((s (with-output-to-string
	     (*standard-output*)
	     (assert (null (multiple-value-list (apropos "CAR" nil)))))))
    (notnot (search "CAR" s :test #'string-equal)))
  t)

(deftest apropos.9
  (macrolet
   ((%m (z) z))
   (let ((s (with-output-to-string
	      (*standard-output*)
	      (assert (null (multiple-value-list
			     (apropos (expand-in-current-env (%m "CAR")))))))))
     (notnot (search "CAR" s :test #'string-equal))))
  t)

(deftest apropos.10
  (macrolet
   ((%m (z) z))
   (let ((s (with-output-to-string
	      (*standard-output*)
	      (assert (null (multiple-value-list
			     (apropos "CAR"
				      (expand-in-current-env (%m nil)))))))))
     (notnot (search "CAR" s :test #'string-equal))))
  t)

;;; Error tests

(deftest apropos.error.1
  (signals-error (apropos) program-error)
  t)

(deftest apropos.error.2
  (signals-error (apropos "SJLJALKSJDKLJASKLDJKLAJDLKJA" (find-package "CL") nil) program-error)
  t)
