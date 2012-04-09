;; -*- Mode: Lisp; tab-width: 2; indent-tabs-mode: nil -*-

;; Methods for compiling and running CL-PPCRE unit tests with code coverage analysis

(in-package :code-cover-test)

(require :cl-ppcre-test)

;; Compiling CL-PPCRE unit tests with code coverage analysis (maybe) enabled

(defmethod asdf:perform :around ((op asdf:compile-op) (system (eql (asdf:find-system :cl-ppcre))))
  (with-code-coverage-compile ()
    (call-next-method)))

(defmethod asdf:perform :around ((op asdf:compile-op) (system (eql (asdf:find-system :cl-ppcre-test))))
  (with-code-coverage-compile ()
    (call-next-method)))

;; Running unit tests with code coverage analysis (maybe) enabled

(defclass cl-ppcre-tests (code-cover-test)
  ((verbose-p :initform nil :initarg :verbose-p))
  (:default-initargs :systems '("cl-ppcre-test" "cl-ppcre"))
  )

(defmethod do-tests ((test cl-ppcre-tests) &rest args)
  (declare (ignore args))
  ;; see cl-ppcre-test/test/tests.lisp
  (with-slots (verbose-p) test
    (do-test "perl-test"
      (cl-ppcre-test::perl-test :verbose verbose-p))
    (do-test "test-optimized-test-functions"
      (cl-ppcre-test::test-optimized-test-functions :verbose verbose-p))
    (dotimes (n 10)
      (do-test (format nil "simple-tests-~d" n)
        (cl-ppcre-test::simple-tests :verbose verbose-p)))))
