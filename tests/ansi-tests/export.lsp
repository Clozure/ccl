;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 07:59:45 1998
;;;; Contains: Tests of EXPORT

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; export

(deftest export.1
  (let ((return-value nil))
    (safely-delete-package "TEST1")
    (let ((p (make-package "TEST1")))
      (let ((sym (intern "FOO" p))
	    (i 0) x y)
	(setf return-value (export (progn (setf x (incf i)) sym)
				   (progn (setf y (incf i)) p)))
	(multiple-value-bind* (sym2 status)
	    (find-symbol "FOO" p)
	  (prog1
	      (and sym2
		   (eql i 2)
		   (eql x 1)
		   (eql y 2)
		   (eqt (symbol-package sym2) p)
		   (string= (symbol-name sym2) "FOO")
		   (eqt sym sym2)
		   (eqt status :external))
	    (delete-package p)))))
    return-value)
  t)

(deftest export.2
  (progn
    (safely-delete-package "TEST1")
    (let ((p (make-package "TEST1")))
      (let ((sym (intern "FOO" p)))
	(export (list sym) p)
	(multiple-value-bind* (sym2 status)
	    (find-symbol "FOO" p)
	  (prog1
	      (and sym2
		   (eqt (symbol-package sym2) p)
		   (string= (symbol-name sym2) "FOO")
		   (eqt sym sym2)
		   (eqt status :external))
	    (delete-package p))))))
  t)

(deftest export.3
  (handler-case
   (progn
     (safely-delete-package "F")
     (make-package "F")
     (let ((sym (intern "FOO" "F")))
       (export sym #\F)
       (delete-package "F")
       t))
   (error (c) (safely-delete-package "F") c))
  t)

;;
;; When a symbol not in a package is exported, export
;; should signal a correctable package-error asking the
;; user whether the symbol should be imported.
;;
(deftest export.4
  (progn
    (set-up-packages)
    (handler-case
     (export 'b::bar "A")
     (package-error () 'package-error)
     (error (c) c)))
  package-error)

;;
;; Test that it catches an attempt to export a symbol
;; from a package that is used by another package that
;; is exporting a symbol with the same name.
;;
(deftest export.5
  (progn
    (safely-delete-package "TEST1")
    (safely-delete-package "TEST2")
    (make-package "TEST1")
    (make-package "TEST2" :use '("TEST1"))
    (export (intern "X" "TEST2") "TEST2")
    (prog1
	(handler-case
	 (let ((sym (intern "X" "TEST1")))
	   (handler-case
	    (export sym "TEST1")
	    (error (c)
		   (format t "Caught error in EXPORT.5: ~A~%" c)
		   'caught)))
	 (error (c) c))
      (delete-package "TEST2")
      (delete-package "TEST1")))
  caught)

(deftest export.error.1
  (signals-error (export) program-error)
  t)

(deftest export.error.2
  (signals-error (export 'X "CL-TEST" NIL) program-error)
  t)
