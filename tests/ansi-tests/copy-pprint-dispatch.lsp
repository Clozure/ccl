;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 23 04:41:29 2004
;;;; Contains: Tests of COPY-PPRINT-DISPATCH

(in-package :cl-test)

(deftest copy-pprint-dispatch.1
  (with-standard-io-syntax
   (let ((obj '(foo bar))
	 (*package* (find-package :cl-test))
	 (*print-readably* nil)
	 (*print-pretty* t))
     (values
      (prin1-to-string obj)
      (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
	(set-pprint-dispatch
	 `(eql ,obj)
	 #'(lambda (s obj2) (let ((*print-pretty* nil))
			      (format s "#.'~S" obj2))))
	(prin1-to-string obj))
     (prin1-to-string obj))))
  "(FOO BAR)"
  "#.'(FOO BAR)"
  "(FOO BAR)")

(deftest copy-pprint-dispatch.2
  (with-standard-io-syntax
   (let ((obj '(foo bar))
	 (*package* (find-package :cl-test))
	 (*print-readably* nil)
	 (*print-pretty* t))
     (values
      (prin1-to-string obj)
      (let ((*print-pprint-dispatch* (copy-pprint-dispatch
				      *print-pprint-dispatch*)))
	(set-pprint-dispatch
	 `(eql ,obj)
	 #'(lambda (s obj2) (let ((*print-pretty* nil))
			      (format s "#.'~S" obj2))))
	(prin1-to-string obj))
      (prin1-to-string obj))))
  "(FOO BAR)"
  "#.'(FOO BAR)"
  "(FOO BAR)")

(deftest copy-pprint-dispatch.3
  (with-standard-io-syntax
   (let ((obj '(foo bar))
	 (*package* (find-package :cl-test))
	 (*print-readably* nil)
	 (*print-pretty* t))
     (values
      (prin1-to-string obj)
      (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
	(set-pprint-dispatch
	 `(eql ,obj)
	 #'(lambda (s obj2) (let ((*print-pretty* nil))
			      (format s "#.'~S" obj2))))
	(prin1-to-string obj))
      (prin1-to-string obj))))
  "(FOO BAR)"
  "#.'(FOO BAR)"
  "(FOO BAR)")

(deftest copy-pprint-dispatch.4
  (with-standard-io-syntax
   (let ((obj '(foo bar))
	 (*package* (find-package :cl-test))
	 (*print-readably* nil)
	 (*print-pretty* t))
     (values
      (prin1-to-string obj)
      (let ((table (copy-pprint-dispatch)))
	(set-pprint-dispatch
	 `(eql ,obj)
	 #'(lambda (s obj2) (let ((*print-pretty* nil))
			      (format s "#.'~S" obj2)))
	 0
	 table)
	(let ((*print-pprint-dispatch* (copy-pprint-dispatch table)))
	  (prin1-to-string obj)))
      (prin1-to-string obj))))
  "(FOO BAR)"
  "#.'(FOO BAR)"
  "(FOO BAR)")

(deftest copy-pprint-dispatch.5
  (let ((new-table (copy-pprint-dispatch)))
    (values
     (eql new-table *print-pprint-dispatch*)
     (member new-table *universe*)))
  nil nil)

(deftest copy-pprint-dispatch.6
  (let ((new-table (copy-pprint-dispatch *print-pprint-dispatch*)))
    (values
     (eql new-table *print-pprint-dispatch*)
     (member new-table *universe*)))
  nil nil)

(deftest copy-pprint-dispatch.7
  (let ((new-table (copy-pprint-dispatch nil)))
    (values
     (eql new-table *print-pprint-dispatch*)
     (member new-table *universe*)))
  nil nil)


(deftest copy-pprint-dispatch.8
  (let* ((table1 (copy-pprint-dispatch))
	 (table2 (copy-pprint-dispatch table1)))
    (eql table1 table2))
  nil)

;;; Error tests

(deftest copy-pprint-dispatch.error.1
  (signals-error (copy-pprint-dispatch nil nil) program-error)
  t)

(deftest copy-pprint-dispatch.error.2
  (check-type-error #'copy-pprint-dispatch #'null)
  nil)
