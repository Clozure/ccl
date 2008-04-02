;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Dec 30 19:05:01 2003
;;;; Contains: Tests of LOGICAL-PATHNAME

(in-package :cl-test)

(deftest logical-pathname.1
  (loop for x in *logical-pathnames*
	always (eql x (logical-pathname x)))
  t)

(deftest logical-pathname.2
  (notnot-mv (typep (logical-pathname "CLTEST:FOO") 'logical-pathname))
  t)

(deftest logical-pathname.3
  (let ((name "CLTEST:TEMP.DAT.NEWEST"))
    (with-open-file
     (s (logical-pathname name)
	:direction :output
	:if-exists :supersede
	:if-does-not-exist :create)
     (or (equalt (logical-pathname s) (logical-pathname name))
	 (list (logical-pathname s) (logical-pathname name)))))
  t)


;;; Error tests

(deftest logical-pathname.error.1
  (check-type-error #'logical-pathname
		    (typef '(or string stream logical-pathname)))
  nil)

(deftest logical-pathname.error.2
  ;; Doesn't specify a host
  (signals-error (logical-pathname "FOO.TXT") type-error)
  t)

(deftest logical-pathname.error.3
  (signals-error
   (with-open-file (s #p"logical-pathname.lsp" :direction :input)
		   (logical-pathname s))
   type-error)
  t)

(deftest logical-pathname.error.4
  (signals-error
   (with-open-stream
    (is (make-concatenated-stream))
    (with-open-stream
     (os (make-broadcast-stream))
     (with-open-stream
      (s (make-two-way-stream is os))
      (logical-pathname s))))
   type-error)
  t)

(deftest logical-pathname.error.5
  (signals-error
   (with-open-stream
    (is (make-concatenated-stream))
    (with-open-stream
     (os (make-broadcast-stream))
     (with-open-stream
      (s (make-echo-stream is os))
      (logical-pathname s))))
   type-error)
  t)

(deftest logical-pathname.error.6
  (signals-error (with-open-stream (s (make-broadcast-stream)) (logical-pathname s)) type-error)
  t)

(deftest logical-pathname.error.7
  (signals-error (with-open-stream (s (make-concatenated-stream)) (logical-pathname s)) type-error)
  t)

(deftest logical-pathname.error.8
  (signals-error (with-open-stream (s (make-string-input-stream "foo"))
				   (logical-pathname s)) type-error)
  t)

(deftest logical-pathname.error.9
  (signals-error (with-output-to-string (s) (logical-pathname s)) type-error)
  t)

(deftest logical-pathname.error.10
  (handler-case
   (progn (eval '(locally (declare (optimize safety)) (logical-pathname "CLROOT:%"))) t)
   (type-error () t))
  t)
