;;; -*-  Lisp -*-

(defpackage #:asdf-install-system 
  (:use #:cl #:asdf))

(in-package #:asdf-install-system)
#+:sbcl
(require 'sb-executable)

;;; this is appalling misuse of asdf.  please don't treat it as any
;;; kind of example.  this shouldn't be a compile-op, or if it is, should
;;; define output-files properly instead of leaving it be the fasl
#+:sbcl
(defclass exe-file (cl-source-file) ())
#+:sbcl
(defmethod perform :after ((o compile-op) (c exe-file))
  (sb-executable:make-executable
   (make-pathname :name "asdf-install"
		  :type nil
		  :defaults (component-pathname c))
   (output-files o c)
   :initial-function "RUN"))

#+:sbcl
(defmethod perform ((o load-op) (c exe-file)) nil)

(defsystem asdf-install
  #+:sbcl :depends-on
  #+:sbcl (sb-posix sb-bsd-sockets)
  :version "0.3"
  :components ((:file "defpackage")
               #+:sbcl
	       (:exe-file "loader" :depends-on ("installer"))
               (:file "split-sequence")
               (:file "port" :depends-on ("defpackage"))
               #+:digitool
               (:file "digitool" :depends-on ("port"))
	       (:file "installer" :depends-on ("port" "split-sequence" #+:digitool "digitool"))))
	       
(defmethod perform :after ((o load-op) (c (eql (find-system :asdf-install))))
  (provide 'asdf-install))

(defmethod perform ((o test-op) (c (eql (find-system :asdf-install))))
  t)
