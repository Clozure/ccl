;;; -*- mode: lisp -*-
(defpackage :spell-system (:use :cl :asdf))
(in-package :spell-system)

(defsystem spell
  :version "0.4"
  :components ((:file "package")
               (:file "constants" :depends-on ("package"))
               (:file "hashing" :depends-on ("package"))
               (:file "flags")
               (:file "classes" :depends-on ("package"))
               (:file "build" :depends-on ("constants" "hashing"
                                           "flags" "classes"))
               ;; kind of a fake dependency
               (:file "io" :depends-on ("build"))
               (:file "correlate" :depends-on ("build"))))