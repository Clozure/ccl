;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 29 05:54:30 2003
;;;; Contains: Tests of MAKE-PATHNAME

(in-package :cl-test)

(defvar *null-pathname*
    (make-pathname))

(defun make-pathname-test
  (&rest args &key (defaults nil)
	 (host (if defaults (pathname-host defaults)
		 (pathname-host *default-pathname-defaults*)))
	 (device (if defaults (pathname-device defaults)
		   (pathname-device *null-pathname*)))
	 (directory (if defaults (pathname-directory defaults)
		      (pathname-directory *null-pathname*)))
	 (name (if defaults (pathname-name defaults)
		 (pathname-name  *null-pathname*)))
	 (type (if defaults (pathname-type defaults)
		 (pathname-type *null-pathname*)))
	 (version (if defaults (pathname-version defaults)
		    (pathname-version *null-pathname*)))
	 case)
  (declare (ignorable case))
  (let* ((vals (multiple-value-list (apply #'make-pathname args)))
	 (pn (first vals)))
    (and (= (length vals) 1)
	 (typep pn 'pathname)
	 (equalp (pathname-host pn) host)
	 (equalp (pathname-device pn) device)
	 ;; (equalp (pathname-directory pn) directory)
	 (let ((pnd (pathname-directory pn)))
	   (if (eq directory :wild)
	       (member pnd '((:absolute :wild-inferiors)
			     (:absolute :wild))
		       :test #'equal)
	     (equalp pnd directory)))	     
	 (equalp (pathname-name pn) name)
	 (equalp (pathname-type pn) type)
	 (equalp (pathname-version pn) version)
	 t)))
  
  

(deftest make-pathname.1
  (make-pathname-test)
  t)

(deftest make-pathname.2
  (make-pathname-test :name "foo")
  t)

(deftest make-pathname.2a
  (do-special-strings
   (s "foo")
   (assert (make-pathname-test :name s)))
  nil)

(deftest make-pathname.3
  (make-pathname-test :name "foo" :type "txt")
  t)

(deftest make-pathname.3a
  (do-special-strings
   (s "txt")
   (assert (make-pathname-test :name "foo" :type s)))
  nil)

(deftest make-pathname.4
  (make-pathname-test :type "lsp")
  t)

(deftest make-pathname.5
  (make-pathname-test :directory :wild)
  t)

(deftest make-pathname.6
  (make-pathname-test :name :wild)
  t)

(deftest make-pathname.7
  (make-pathname-test :type :wild)
  t)

(deftest make-pathname.8
  (make-pathname-test :version :wild)
  t)

(deftest make-pathname.9
  (make-pathname-test :defaults *default-pathname-defaults*)
  t)

(deftest make-pathname.10
  (make-pathname-test :defaults (make-pathname :name "foo" :type "bar"))
  t)

(deftest make-pathname.11
  (make-pathname-test :version :newest)
  t)

(deftest make-pathname.12
  (make-pathname-test :case :local)
  t)

(deftest make-pathname.13
  (make-pathname-test :case :common)
  t)

(deftest make-pathname.14
  (let ((*default-pathname-defaults*
	 (make-pathname :name "foo" :type "lsp" :version :newest)))
    (make-pathname-test))
  t)

;;; Works on the components of actual pathnames
(deftest make-pathname.rebuild
  (loop for p in *pathnames*
	for host = (pathname-host p)
	for device = (pathname-device p)
	for directory = (pathname-directory p)
	for name = (pathname-name p)
	for type = (pathname-type p)
	for version = (pathname-version p)
	for p2 = (make-pathname
		  :host host
		  :device device
		  :directory directory
		  :name name
		  :type type
		  :version version)
	unless (equal p p2)
	collect (list p p2))
  nil)

;;; Various constraints on :directory

(deftest make-pathname-error-absolute-up
  (signals-error (directory (make-pathname :directory '(:absolute :up)))
		 file-error)
  t)

(deftest make-pathname-error-absolute-back
  (signals-error (directory (make-pathname :directory '(:absolute :back)))
		 file-error)
  t)

;; The next test is correct, but was causing very large amounts of time to be spent
;; in buggy implementations
#|
(deftest make-pathname-error-absolute-wild-inferiors-up
  (signals-error (directory (make-pathname :directory '(:absolute :wild-inferiors :up)))
		 file-error)
  t)
|#

(deftest make-pathname-error-relative-wild-inferiors-up
  (signals-error (length (directory (make-pathname :directory '(:relative :wild-inferiors :up))))
		 file-error)
  t)

(deftest make-pathname-error-absolute-wild-inferiors-back
  (signals-error (directory (make-pathname :directory '(:absolute :wild-inferiors :back)))
		 file-error)
  t)

(deftest make-pathname-error-relative-wild-inferiors-back
  (signals-error (directory (make-pathname :directory '(:relative :wild-inferiors :back)))
		 file-error)
  t)
