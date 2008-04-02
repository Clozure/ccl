;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Feb 28 21:59:57 2003
;;;; Contains: Tests of INVOKE-DEBUGGER

(in-package :cl-test)

;;; We can't test actual entry into the debugger, but we can test
;;; that the function in *debugger-hook* is properly called.

(deftest invoke-debugger.1
  (block done
   (let (fn (cnd (make-condition 'simple-error)))
       (setq fn #'(lambda (c hook)
		    (return-from done
		      (and (null *debugger-hook*)
			   (eqt hook fn)
			   (eqt cnd c)
			   'good))))
       (let ((*debugger-hook* fn))
	 (invoke-debugger cnd)))
   'bad)
  good)

(deftest invoke-debugger.error.1
  (signals-error
   (block done
     (let ((*debugger-hook* #'(lambda (&rest args)
				(declare (ignore args))
				(return-from done 'bad))))
       (invoke-debugger)))
   program-error)
  t)

(deftest invoke-debugger.error.2
  (signals-error
   (block done
     (let ((*debugger-hook* #'(lambda (&rest args)
				(declare (ignore args))
				(return-from done 'bad))))
       (invoke-debugger (make-condition 'simple-error) nil)))
   program-error)
  t)

;;; If the debugger hook function expects the wrong number
;;; of arguments, a program-error should be thrown in safe code
;;; This error is thrown 'prior to entry to the standard debugger'.

(deftest invoke-debugger.error.3
  (signals-error
   (let ((*debugger-hook* #'(lambda () nil)))
     (invoke-debugger (make-condition 'simple-error)))
   program-error)
  t)

(deftest invoke-debugger.error.4
  (signals-error
   (let ((*debugger-hook* #'(lambda (c) c)))
     (invoke-debugger (make-condition 'simple-error)))
   program-error)
  t)

(deftest invoke-debugger.error.5
  (signals-error
   (let ((*debugger-hook* #'(lambda (c hook x) (list c hook x))))
     (invoke-debugger (make-condition 'simple-error)))
   program-error)
  t)
