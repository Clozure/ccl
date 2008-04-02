;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 13 19:10:02 2004
;;;; Contains: Tests of the FILE-ERROR condition, and associated accessor function

(in-package :cl-test)

(deftest file-error.1
  (let ((pn (make-pathname :name :wild
			   :type "txt"
			   :version :newest
			   :defaults *default-pathname-defaults*)))
    (handler-case
     (probe-file pn)
     (error (c)
	    (values
	     (notnot (typep c 'file-error))
	     (if (equalp (file-error-pathname c) pn)
		 t
	       (list (file-error-pathname c) pn))))))
  t t)

(deftest file-error-pathname.1
  (let ((c (make-condition 'file-error :pathname "foo.txt")))
    (values
     (notnot (typep c 'file-error))
     (eqlt (class-of c) (find-class 'file-error))
     (file-error-pathname c)))
  t t "foo.txt")

(deftest file-error-pathname.2
  (let ((c (make-condition 'file-error :pathname #p"foo.txt")))
    (values
     (notnot (typep c 'file-error))
     (eqlt (class-of c) (find-class 'file-error))
     (equalt #p"foo.txt" (file-error-pathname c))))
  t t t)

(deftest file-error-pathname.3
  (let ((c (make-condition 'file-error :pathname "CLTEST:foo.txt")))
    (values
     (notnot (typep c 'file-error))
     (eqlt (class-of c) (find-class 'file-error))
     (equalpt "CLTEST:foo.txt"
	      (file-error-pathname c))))
  t t t)

(deftest file-error-pathname.4
  (let ((c (make-condition 'file-error :pathname (logical-pathname "CLTEST:foo.txt"))))
    (values
     (notnot (typep c 'file-error))
     (eqlt (class-of c) (find-class 'file-error))
     (equalpt (logical-pathname "CLTEST:foo.txt")
	      (file-error-pathname c))))
  t t t)

(deftest file-error-pathname.5
  (with-open-file (s "file-error.lsp" :direction :input)
		  (let ((c (make-condition 'file-error :pathname s)))
		    (values
		     (notnot (typep c 'file-error))
		     (eqlt (class-of c) (find-class 'file-error))
		     (equalpt s (file-error-pathname c)))))
  t t t)

(deftest file-error-pathname.6
  (let ((s (open "file-error.lsp" :direction :input)))
    (close s)
    (let ((c (make-condition 'file-error :pathname s)))
      (values
       (notnot (typep c 'file-error))
       (eqlt (class-of c) (find-class 'file-error))
       (equalpt s (file-error-pathname c)))))
  t t t)

(deftest file-error-pathname.error.1
  (signals-error (file-error-pathname) program-error)
  t)

(deftest file-error-pathname.error.2
  (signals-error
   (file-error-pathname (make-condition 'file-error :pathname "foo.txt") nil)
   program-error)
  t)





