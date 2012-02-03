;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(defpackage code-cover-test.system
  (:use #:cl #:asdf))

(in-package code-cover-test.system)

(defsystem code-cover-test-loader
  :depends-on ( cl-ppcre cl-ppcre-test hunchentoot )
  :serial t
  :components
  ((:file "package")))

(defsystem code-cover-test
  :depends-on ( code-cover-test-loader )
  :serial t
  :components
  ((:file "code-cover-test")
   (:file "code-cover-server")))
