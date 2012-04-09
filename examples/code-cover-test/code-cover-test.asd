;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(defpackage code-cover-test.system
  (:use #:cl #:asdf))

(in-package code-cover-test.system)

(defpackage code-cover-test
  (:use #:cl)
  (:export #:init-test-code-coverage
           #:run-all-tests-with-code-coverage
           #:report-code-coverage-test))

(defsystem code-cover-test
  :components
  ((:file "compile-with-code-coverage")
   (:file "code-cover-test" :depends-on ("compile-with-code-coverage"))))

(defsystem code-cover-tests
  :depends-on (code-cover-test cl-ppcre cl-ppcre-test)
  :components
  ((:file "cl-ppcre-tests")))
