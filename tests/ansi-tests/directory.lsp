;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jan  1 12:00:18 2004
;;;; Contains: Tests of DIRECTORY

(in-package :cl-test)

(deftest directory.1
  (directory "nonexistent")
  nil)

(deftest directory.2
  (directory #p"nonexistent")
  nil)

(deftest directory.3
  (directory "nonexistent" :allow-other-keys nil)
  nil)

(deftest directory.4
  (directory "nonexistent" :allow-other-keys t :foo 'bar)
  nil)

(deftest directory.5
  (directory "nonexistent" :foo 0 :allow-other-keys t)
  nil)

(deftest directory.6
  (let* ((pattern-pathname (make-pathname :name :wild :type :wild
					  :defaults *default-pathname-defaults*))
	 (pathnames (directory pattern-pathname)))
    (values
     (remove-if #'pathnamep pathnames)
     (loop for pn in pathnames
	   unless (equal pn (truename pn))
	   collect pn)
;;     (loop for pn in pathnames
;;	   unless (pathname-match-p pn pattern-pathname)
;;	   collect pn))
     ))
  nil nil ;; nil
  )

(deftest directory.7
  (let* ((pattern-pathname (make-pathname :name :wild :type :wild
					  :defaults *default-pathname-defaults*))
	 (pathnames (directory pattern-pathname)))
    (loop for pn in pathnames
	  unless (equal pn (probe-file pn))
	  collect pn))
  nil)

(deftest directory.8
  (let* ((pathname-pattern "CLTEST:*.*")
	 (len (length (directory pathname-pattern))))
    (if (< len 500) len nil))
  nil)

;;; Specialized string tests

(deftest directory.9
  (do-special-strings
   (s "nonexistent" nil)
   (assert (null (directory s))))
  nil)

;;; Error tests

(deftest directory.error.1
  (signals-error (directory) program-error)
  t)
