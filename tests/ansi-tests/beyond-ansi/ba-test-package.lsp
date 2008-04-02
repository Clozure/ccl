;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 28 06:38:29 2005
;;;; Contains: Definition of BA-TEST package.

(in-package :cl-user)

(let* ((name :ba-test)
       (pkg (find-package name)))
  (unless pkg (setq pkg (make-package name :use '(:cl :regression-test
						      :cl-test))))
  (let ((*package* pkg))
    (shadow '(#:handler-case #:handler-bind))
    (import '(common-lisp-user::compile-and-load) pkg)
    (import '(cl-test::*universe* cl-test::*mini-universe*) pkg)
    )
  (let ((s (find-symbol "QUIT" "CL-USER")))
    (when s (import s :ba-test))))
