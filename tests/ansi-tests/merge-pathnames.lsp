;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Dec 31 11:25:55 2003
;;;; Contains: Tests of MERGE-PATHNAMES

(in-package :cl-test)

#|
(defun merge-pathnames-test (&rest args)
  (assert (<= 1 (length args) 3))
  (let* ((p1 (car args))
	 (p2 (if (cdr args) (cadr args) *default-pathname-defaults*))
	 (default-version (if (cddr args) (caddr args) :newest))
	 (results (multiple-value-list (apply #'merge-pathnames args))))
    (assert (= (length results) 1))
    (let ((p3 (first results)))
      
|#

(deftest merge-pathnames.1
  (let* ((p1 (make-pathname :name "foo"))
	 (p2 (merge-pathnames p1 p1 nil)))
    (values
     (equalpt (pathname-name p1) "foo")
     (if (equalpt p1 p2) t
       (list p1 p2))))
  t t)

(deftest merge-pathnames.2
  (let* ((p1 (make-pathname :name "foo"))
	 (p2 (merge-pathnames p1 p1)))
    (values
     (equalpt (pathname-host p1) (pathname-host p2))
     (equalpt (pathname-device p1) (pathname-device p2))
     (equalpt (pathname-directory p1) (pathname-directory p2))
     (pathname-name p1)
     (pathname-name p2)
     (equalpt (pathname-type p1) (pathname-type p2))
     (if (pathname-version p1)
	 (equalpt (pathname-version p1) (pathname-version p2))
       (equalpt (pathname-version p2) :newest))))
  t t t "foo" "foo" t t)

(deftest merge-pathnames.3
  (let* ((p1 (make-pathname :name "foo"))
	 (p2 (make-pathname :name "bar"))
	 (p3 (merge-pathnames p1 p2)))
    (values
     (equalpt (pathname-host p1) (pathname-host p3))
     (equalpt (pathname-device p1) (pathname-device p3))
     (equalpt (pathname-directory p1) (pathname-directory p3))
     (pathname-name p1)
     (pathname-name p3)
     (equalpt (pathname-type p1) (pathname-type p3))
     (if (pathname-version p1)
	 (equalpt (pathname-version p1) (pathname-version p3))
       (equalpt (pathname-version p3) :newest))))
  t t t "foo" "foo" t t)

(deftest merge-pathnames.4
  (let* ((p1 (make-pathname :name "foo"))
	 (p2 (make-pathname :type "lsp"))
	 (p3 (merge-pathnames p1 p2)))
    (values
     (equalpt (pathname-host p1) (pathname-host p3))
     (equalpt (pathname-device p1) (pathname-device p3))
     (equalpt (pathname-directory p1) (pathname-directory p3))
     (pathname-name p1)
     (pathname-type p2)
     (pathname-type p3)
     (equalpt (pathname-type p2) (pathname-type p3))
     (if (pathname-version p1)
	 (equalpt (pathname-version p1) (pathname-version p3))
       (equalpt (pathname-version p3) :newest))))
  t t t "foo" "lsp" "lsp" t t)

(deftest merge-pathnames.5
  (let* ((p1 (make-pathname :name "foo"))
	 (p2 (make-pathname :type "lsp" :version :newest))
	 (p3 (merge-pathnames p1 p2 nil)))
    (values
     (equalpt (pathname-host p1) (pathname-host p3))
     (equalpt (pathname-device p1) (pathname-device p3))
     (equalpt (pathname-directory p1) (pathname-directory p3))
     (pathname-name p1)
     (pathname-name p3)
     (pathname-type p2)
     (pathname-type p3)
     (equalpt (pathname-version p1) (pathname-version p3))))
  t t t "foo" "foo" "lsp" "lsp" t)

(deftest merge-pathnames.6
  (let* ((p1 (make-pathname))
	 (p2 (make-pathname :name "foo" :version :newest))
	 (p3 (merge-pathnames p1 p2 nil)))
    (values
     (equalpt (pathname-host p1) (pathname-host p3))
     (equalpt (pathname-device p1) (pathname-device p3))
     (equalpt (pathname-directory p1) (pathname-directory p3))
     (pathname-name p2)
     (pathname-name p3)
     (equalpt (pathname-type p2) (pathname-type p3))
     (pathname-version p2)
     (pathname-version p3)))
  t t t "foo" "foo" t :newest :newest)

(deftest merge-pathnames.7
  (let* ((p1 (make-pathname))
	 (p2 *default-pathname-defaults*)
	 (p3 (merge-pathnames p1)))
    (values
     (equalpt (pathname-host p1) (pathname-host p3))
     (equalpt (pathname-host p2) (pathname-host p3))
     (equalpt (pathname-device p2) (pathname-device p3))
     (equalpt (pathname-directory p2) (pathname-directory p3))
     (equalpt (pathname-name p2) (pathname-name p3))
     (equalpt (pathname-type p2) (pathname-type p3))
     (cond
      ((pathname-version p1) (equalpt (pathname-version p1)
				      (pathname-version p3)))
      ((pathname-version p2) (equalpt (pathname-version p2)
				      (pathname-version p3)))
      (t (equalpt (pathname-version p3) :newest)))))
  t t t t t t t)
