;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Dec 17 21:10:53 2002
;;;; Contains: Package definition for RT


;; (eval-when (:execute :compile-toplevel :load-toplevel)
#| (defpackage :regression-test
    (:use :cl)
    (:nicknames :rtest #-lispworks :rt)
    (:export
     #:*do-tests-when-defined*
     #:*compile-tests*
     #:*test*
     #:continue-testing
     #:deftest
     #:do-test
     #:do-tests
     #:get-test
     #:pending-tests
     #:rem-all-tests
     #:rem-test
     #:defnote
     #:my-aref
     #:*catch-errors*
     #:disable-note
     ))
 |#
 (let* ((name (symbol-name :regression-test))
	(pkg (find-package name)))
   (unless pkg (setq pkg (make-package name
				       :nicknames (mapcar #'symbol-name '(:rtest #-lispworks :rt))
				       :use '(#-wcl :cl #+wcl :lisp)
				       )))
   (let ((*package* pkg))
     (export (mapcar #'intern
		     (mapcar #'symbol-name
			     '(#:*compile-tests*
			       #:*do-tests-when-defined*
			       #:*test*
			       #:continue-testing
			       #:deftest
			       #:do-test
			       #:do-tests
			       #:do-extended-tests
			       #:get-test
			       #:pending-tests
			       #:rem-all-tests
			       #:rem-test
			       #:defnote
			       #:my-aref
			       #:*catch-errors*
			       #:*passed-tests*
			       #:*failed-tests*
			       #:disable-note))))))
;;  )

;; (in-package :regression-test)
