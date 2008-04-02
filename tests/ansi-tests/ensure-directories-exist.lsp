;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan  5 20:53:03 2004
;;;; Contains: Tests of ENSURE-DIRECTORIES-EXIST

(in-package :cl-test)

(deftest ensure-directories-exist.1
  (let* ((pn (make-pathname :name "ensure-directories-exist.lsp"
			    :defaults *default-pathname-defaults*))
	 (results nil)
	 (verbosity
	  (with-output-to-string
	    (*standard-output*)
	    (setq results (multiple-value-list (ensure-directories-exist pn))))))
    (values
     (length results)
     (equalt (truename pn) (truename (first results)))
     (second results)
     verbosity))
  2 t nil "")

(deftest ensure-directories-exist.2
  (with-open-file
   (s "ensure-directories-exist.lsp" :direction :input)
   (let* ((results (multiple-value-list (ensure-directories-exist s))))
     (values
      (length results)
      (equalt (truename (first results)) (truename s))
      (second results))))
   2 t nil)

(deftest ensure-directories-exist.3
  (let ((s (open "ensure-directories-exist.lsp" :direction :input)))
    (close s)
    (let* ((results (multiple-value-list (ensure-directories-exist s))))
      (values
       (length results)
       (equalt (truename (first results)) (truename s))
       (second results))))
   2 t nil)

(deftest ensure-directories-exist.4
  (let* ((pn (make-pathname :name "ensure-directories-exist.lsp"
			    :defaults *default-pathname-defaults*))
	 (results nil)
	 (verbosity
	  (with-output-to-string
	    (*standard-output*)
	    (setq results (multiple-value-list
			   (ensure-directories-exist pn :verbose nil))))))
    (values
     (length results)
     (equalt (truename pn) (truename (first results)))
     (second results)
     verbosity))
  2 t nil "")

(deftest ensure-directories-exist.5
  (let* ((pn (make-pathname :name "ensure-directories-exist.lsp"
			    :defaults *default-pathname-defaults*))
	 (results nil)
	 (verbosity
	  (with-output-to-string
	    (*standard-output*)
	    (setq results (multiple-value-list
			   (ensure-directories-exist pn :verbose t))))))
    (values
     (length results)
     (equalt (truename pn) (truename (first results)))
     (second results)
     verbosity))
  2 t nil "")

(deftest ensure-directories-exist.6
  (let* ((pn (make-pathname :name "ensure-directories-exist.lsp"
			    :defaults *default-pathname-defaults*))
	 (results nil)
	 (verbosity
	  (with-output-to-string
	    (*standard-output*)
	    (setq results (multiple-value-list
			   (ensure-directories-exist
			    pn :allow-other-keys nil))))))
    (values
     (length results)
     (equalt (truename pn) (truename (first results)))
     (second results)
     verbosity))
  2 t nil "")

(deftest ensure-directories-exist.7
  (let* ((pn (make-pathname :name "ensure-directories-exist.lsp"
			    :defaults *default-pathname-defaults*))
	 (results nil)
	 (verbosity
	  (with-output-to-string
	    (*standard-output*)
	    (setq results (multiple-value-list
			   (ensure-directories-exist
			    pn :allow-other-keys t :nonsense t))))))
    (values
     (length results)
     (equalt (truename pn) (truename (first results)))
     (second results)
     verbosity))
  2 t nil "")

;;; Case where directory shouldn't exist

;; The directort ansi-tests/scratch must not exist before this
;; test is run
(deftest ensure-directories-exist.8
  (let* ((subdir (make-pathname :directory '(:relative "scratch")
				:defaults *default-pathname-defaults*))
	 (pn (make-pathname :name "foo" :type "txt"
			    :defaults subdir)))
    (assert (not (probe-file pn)) ()
	    "Delete subdirectory scratch and its contents!")
    (let* ((results nil)
	   (verbosity
	    (with-output-to-string
	      (*standard-output*)
	      (setq results (multiple-value-list (ensure-directories-exist pn)))))
	   (result-pn (first results))
	   (created (second results)))
      ;; Create the file and write to it
      (with-open-file (*standard-output*
		       pn :direction :output :if-exists :error
		       :if-does-not-exist :create)
		      (print nil))		      
      (values
       (length results)
       (notnot created)
       (equalt pn result-pn)
       (notnot (probe-file pn))
       verbosity
       )))
  2 t t t "")

;;; Specialized string tests

(deftest ensure-directories-exist.9
  (do-special-strings
   (str "ensure-directories-exist.lsp" nil)
   (let* ((results (multiple-value-list (ensure-directories-exist str))))
     (assert (eql (length results) 2))
     (assert (equalt (truename (first results)) (truename str)))
     (assert (null (second results)))))
  nil)

;; FIXME
;; Need to add a LPN test

(deftest ensure-directories-exist.error.1
  (signals-error-always
   (ensure-directories-exist
    (make-pathname :directory '(:relative :wild)
		   :defaults *default-pathname-defaults*))
   file-error)
  t t)

(deftest ensure-directories-exist.error.2
  (signals-error (ensure-directories-exist) program-error)
  t)
