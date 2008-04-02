;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Dec 31 16:54:55 2003
;;;; Contains: Tests of WILD-PATHNAME-P

(in-package :cl-test)

(compile-and-load "pathnames-aux.lsp")

(deftest wild-pathname-p.1
  (wild-pathname-p (make-pathname))
  nil)

(deftest wild-pathname-p.2
  (loop for key in '(:host :device :directory :name :type :version nil)
	when (wild-pathname-p (make-pathname) key)
	collect key)
  nil)

(deftest wild-pathname-p.3
  (let ((p (make-pathname :directory :wild)))
    (notnot-mv (wild-pathname-p p)))
  t)

(deftest wild-pathname-p.4
  (let ((p (make-pathname :directory :wild)))
    (notnot-mv (wild-pathname-p p nil)))
  t)

(deftest wild-pathname-p.5
  (let ((p (make-pathname :directory :wild)))
    (notnot-mv (wild-pathname-p p :directory)))
  t)

(deftest wild-pathname-p.6
  (let ((p (make-pathname :directory :wild)))
    (loop for key in '(:host :device :name :type :version)
	when (wild-pathname-p p key)
	collect key))
  nil)


(deftest wild-pathname-p.7
  (let ((p (make-pathname :directory '(:absolute :wild))))
    (notnot-mv (wild-pathname-p p)))
  t)

(deftest wild-pathname-p.8
  (let ((p (make-pathname :directory '(:absolute :wild))))
    (notnot-mv (wild-pathname-p p nil)))
  t)

(deftest wild-pathname-p.9
  (let ((p (make-pathname :directory '(:absolute :wild))))
    (notnot-mv (wild-pathname-p p :directory)))
  t)

(deftest wild-pathname-p.10
  (let ((p (make-pathname :directory '(:absolute :wild))))
    (loop for key in '(:host :device :name :type :version)
	when (wild-pathname-p p key)
	collect key))
  nil)


(deftest wild-pathname-p.11
  (let ((p (make-pathname :directory '(:relative :wild))))
    (notnot-mv (wild-pathname-p p)))
  t)

(deftest wild-pathname-p.12
  (let ((p (make-pathname :directory '(:relative :wild))))
    (notnot-mv (wild-pathname-p p nil)))
  t)

(deftest wild-pathname-p.13
  (let ((p (make-pathname :directory '(:relative :wild))))
    (notnot-mv (wild-pathname-p p :directory)))
  t)

(deftest wild-pathname-p.14
  (let ((p (make-pathname :directory '(:relative :wild))))
    (loop for key in '(:host :device :name :type :version)
	when (wild-pathname-p p key)
	collect key))
  nil)

;;;

(deftest wild-pathname-p.15
  (let ((p (make-pathname :name :wild)))
    (notnot-mv (wild-pathname-p p)))
  t)

(deftest wild-pathname-p.16
  (let ((p (make-pathname :name :wild)))
    (notnot-mv (wild-pathname-p p nil)))
  t)

(deftest wild-pathname-p.17
  (let ((p (make-pathname :name :wild)))
    (notnot-mv (wild-pathname-p p :name)))
  t)

(deftest wild-pathname-p.18
  (let ((p (make-pathname :name :wild)))
    (loop for key in '(:host :device :directory :type :version)
	when (wild-pathname-p p key)
	collect key))
  nil)

;;;    
  
(deftest wild-pathname-p.19
  (let ((p (make-pathname :type :wild)))
    (notnot-mv (wild-pathname-p p)))
  t)

(deftest wild-pathname-p.20
  (let ((p (make-pathname :type :wild)))
    (notnot-mv (wild-pathname-p p nil)))
  t)

(deftest wild-pathname-p.21
  (let ((p (make-pathname :type :wild)))
    (notnot-mv (wild-pathname-p p :type)))
  t)

(deftest wild-pathname-p.22
  (let ((p (make-pathname :type :wild)))
    (loop for key in '(:host :device :directory :name :version)
	when (wild-pathname-p p key)
	collect key))
  nil)

;;;

 (deftest wild-pathname-p.23
  (let ((p (make-pathname :version :wild)))
    (notnot-mv (wild-pathname-p p)))
  t)

(deftest wild-pathname-p.24
  (let ((p (make-pathname :version :wild)))
    (notnot-mv (wild-pathname-p p nil)))
  t)

(deftest wild-pathname-p.25
  (let ((p (make-pathname :version :wild)))
    (notnot-mv (wild-pathname-p p :version)))
  t)

(deftest wild-pathname-p.26
  (let ((p (make-pathname :version :wild)))
    (loop for key in '(:host :device :directory :name :type)
	when (wild-pathname-p p key)
	collect key))
  nil)

;;;

(deftest wild-pathname-p.27
  (loop for p in (append *pathnames* *logical-pathnames*)
	unless (if (wild-pathname-p p) (wild-pathname-p p nil)
		 (not (wild-pathname-p p nil)))
	collect p)
  nil)

(deftest wild-pathname-p.28
  (loop for p in (append *pathnames* *logical-pathnames*)
	when (and (loop for key in '(:host :device :directory
					   :name :type :version)
			thereis (wild-pathname-p p key))
		  (not (wild-pathname-p p)))
	collect p)
  nil)

;;; On streams associated with files

(deftest wild-pathname-p.29
  (with-open-file (s "foo.lsp"
		     :direction :output
		     :if-exists :append
		     :if-does-not-exist :create)
		  (wild-pathname-p s))
  nil)

(deftest wild-pathname-p.30
  (let ((s (open "foo.lsp"
		 :direction :output
		 :if-exists :append
		 :if-does-not-exist :create)))
    (close s)
    (wild-pathname-p s))
  nil)

;;; logical pathname designators

(deftest wild-pathname-p.31
  (wild-pathname-p "CLTEST:FOO.LISP")
  nil)

;;; Odd strings

(deftest wild-pathname-p.32
  (do-special-strings
   (s "CLTEST:FOO.LISP" nil)
   (let ((vals (multiple-value-list (wild-pathname-p s))))
     (assert (equal vals '(nil)))))
  nil)

;;;

(deftest wild-pathname-p.error.1
  (signals-error (wild-pathname-p) program-error)
  t)

(deftest wild-pathname-p.error.2
  (signals-error (wild-pathname-p *default-pathname-defaults* nil nil)
		 program-error)
  t)

(deftest wild-pathname-p.error.3
  (check-type-error #'wild-pathname-p
		    (typef '(or pathname string file-stream
				synonym-stream)))
  nil)

(deftest wild-pathname-p.error.4
  (check-type-error #'(lambda (x) (declare (optimize (safety 0)))
			(wild-pathname-p x))
		    (typef '(or pathname string file-stream
				synonym-stream)))
  nil)
