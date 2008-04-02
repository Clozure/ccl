;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 14 10:13:21 1998
;;;; Contains: CL test case package definition

(let* ((name :cl-test)
       (pkg (find-package name)))
  (unless pkg (setq pkg (make-package name :use '(:cl :regression-test))))
  (let ((*package* pkg))
    (shadow '(#:handler-case #:handler-bind))
    (import '(common-lisp-user::compile-and-load)
	    pkg)
    (export (mapcar #'intern
		    (mapcar #'symbol-name
			    '(#:random-from-seq #:random-case #:coin
			      #:random-permute #:*universe* #:*mini-universe*
			      #:*cl-symbols*
			      #:signals-error #:typef)))))
  (let ((s (find-symbol "QUIT" "CL-USER")))
    (when s (import s :cl-test))))


