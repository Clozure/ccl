;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(defpackage code-cover-test-server.system
  (:use #:cl #:asdf))

(in-package code-cover-test-server.system)

(defpackage code-cover-test-server
  (:use #:cl)
  (:import-from #:code-cover-test #:index-file-path #:output-path)
  (:export #:init-server
           #:start-server
           #:stop-server))

(defsystem code-cover-test-server
  :depends-on ( code-cover-test hunchentoot )
  :components
  ((:file "code-cover-test-server")))

