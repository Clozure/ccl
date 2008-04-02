;-*-syntax:COMMON-LISP-*-

#|----------------------------------------------------------------------------|
 | Copyright 1990 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.    M.I.T.   makes   no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#

;This is the December 19, 1990 version of a set of tests that use the
;RT regression tester to test itself.  See the documentation of RT for
;a discusion of how to use this file.

(cl:defpackage :rt-tests
  (:use :cl :regression-test))

(in-package :rt-tests)
;; (require "RT")
;;(use-package :regression-test)

(defmacro setup (&rest body)
  `(do-setup '(progn ., body)))

(defmacro with-blank-tests (&body body)
  `(let ((regression-test::*entries* (list nil))
	 (regression-test::*entries-table* (make-hash-table :test #'equal))
	 (*test* nil)
	 (regression-test::*in-test* nil))
    (let ((regression-test::*entries-tail* regression-test::*entries*))
      ,@body)))

(defun do-setup (form)
  (with-blank-tests
      (let ((*do-tests-when-defined* nil)
	    (regression-test::*debug* t)
	    result)
	(deftest t1 4 4)
	(deftest (t 2) 4 3)
	(values-list
	 (cons (normalize
	      (with-output-to-string (*standard-output*)
		(setq result
		      (multiple-value-list
			(catch 'regression-test::*debug* (eval form))))))
	    result)))))

(defun normalize (string)
  (with-input-from-string (s string)
    (normalize-stream s)))

(defvar *file-name* nil)

(defun get-file-name ()
  (loop (if *file-name* (return *file-name*))
	(format *error-output*
		"~%Type a string representing naming of a scratch disk file: ")
	(setq *file-name* (read))
	(if (not (stringp *file-name*)) (setq *file-name* nil))))

(get-file-name)

(defmacro with-temporary-file (f &body forms)
  `(let ((,f *file-name*))
     ,@ forms
     (get-file-output ,f)))

(defun get-file-output (f)
  (prog1 (with-open-file (in f)
	   (normalize-stream in))
	 (delete-file f)))

(defun normalize-stream (s)
  (let ((l nil))
    (loop (push (read-line s nil s) l)
	  (when (eq (car l) s)
	    (setq l (nreverse (cdr l)))
	    (return nil)))
    (delete "" l :test #'equal)))

(rem-all-tests)

(deftest deftest-1
  (setup (deftest t1 3 3) (values (get-test 't1) *test* (pending-tests)))
  ("Redefining test RT-TESTS::T1") (t1 3 3) t1 (t1 (t 2)))
(deftest deftest-2
  (setup (deftest (t 2) 3 3) (get-test '(t 2)))
  ("Redefining test (T 2)") ((t 2) 3 3))
(deftest deftest-3
  (setup (deftest 2 3 3) (values (get-test 2) *test* (pending-tests)))
  () (2 3 3) 2 (t1 (t 2) 2))
(deftest deftest-4
  (setup (let ((*do-tests-when-defined* t)) (deftest (temp) 4 3)))
  ("Test (RT-TESTS::TEMP) failed"
   "Form: 4"
   "Expected value: 3"
   "Actual value: 4.")
  (temp))

(deftest do-test-1
  (setup (values (do-test 't1) *test* (pending-tests)))
  () t1 t1 ((t 2)))
(deftest do-test-2
  (setup (values (do-test '(t 2)) (pending-tests)))
  ("Test (T 2) failed"
   "Form: 4"
   "Expected value: 3"
   "Actual value: 4.") nil (t1 (t 2)))
(deftest do-test-3
  (setup (let ((*test* 't1)) (do-test)))
  () t1)

(deftest get-test-1
  (setup (values (get-test 't1) *test*))
  () (t1 4 4) (t 2))
(deftest get-test-2
  (setup (get-test '(t 2)))
  () ((t 2) 4 3))
(deftest get-test-3
  (setup (let ((*test* 't1)) (get-test)))
  () (t1 4 4))
(deftest get-test-4
  (setup (deftest t3 1 1) (get-test))
  () (t3 1 1))
(deftest get-test-5 
  (setup (get-test 't0))
  ("No test with name RT-TESTS::T0.") nil)

(deftest rem-test-1
  (setup (values (rem-test 't1) (pending-tests)))
  () t1 ((t 2)))
(deftest rem-test-2
  (setup (values (rem-test '(t 2)) (pending-tests)))
  () (t 2) (t1))
(deftest rem-test-3
  (setup (let ((*test* '(t 2))) (rem-test)) (pending-tests)) 
  () (t1))
(deftest rem-test-4
  (setup (values (rem-test 't0) (pending-tests)))
  () nil (t1 (t 2)))
(deftest rem-test-5
  (setup (rem-all-tests) (rem-test 't0) (pending-tests))
  () ())

(deftest rem-all-tests-1
  (setup (values (rem-all-tests) (pending-tests)))
  () nil nil)
(deftest rem-all-tests-2
  (setup (rem-all-tests) (rem-all-tests) (pending-tests))
  () nil) 

(deftest do-tests-1
  (setup (let ((*print-case* :downcase))
	   (values (do-tests) (continue-testing) (do-tests))))
  ("Doing 2 pending tests of 2 tests total."
   " RT-TESTS::T1"
   "Test (T 2) failed"
   "Form: 4"
   "Expected value: 3"
   "Actual value: 4."
   "1 out of 2 total tests failed: (T 2)."
   "Doing 1 pending test of 2 tests total."
   "Test (T 2) failed"
   "Form: 4"
   "Expected value: 3"
   "Actual value: 4."
   "1 out of 2 total tests failed: (T 2)."
   "Doing 2 pending tests of 2 tests total."
   " RT-TESTS::T1"
   "Test (T 2) failed"
   "Form: 4"
   "Expected value: 3"
   "Actual value: 4."
   "1 out of 2 total tests failed: (T 2).")
  nil
  nil
  nil)

(deftest do-tests-2
  (setup (rem-test '(t 2))
	 (deftest (t 2) 3 3)
	 (values (do-tests) (continue-testing) (do-tests)))
  ("Doing 2 pending tests of 2 tests total."
   " RT-TESTS::T1 (T 2)"
   "No tests failed."
   "Doing 0 pending tests of 2 tests total."
   "No tests failed."
   "Doing 2 pending tests of 2 tests total."
   " RT-TESTS::T1 (T 2)"
   "No tests failed.")
  t
  t
  t)
(deftest do-tests-3
  (setup (rem-all-tests) (values (do-tests) (continue-testing)))
  ("Doing 0 pending tests of 0 tests total."
   "No tests failed."
   "Doing 0 pending tests of 0 tests total."
   "No tests failed.")
  t
  t)
(deftest do-tests-4
  (setup (normalize (with-output-to-string (s) (do-tests :out s))))
  ()
  ("Doing 2 pending tests of 2 tests total."
   " RT-TESTS::T1"
   "Test (T 2) failed"
   "Form: 4"
   "Expected value: 3"
   "Actual value: 4."
   "1 out of 2 total tests failed: (T 2)."))
(deftest do-tests-5
  (setup (with-temporary-file s (do-tests :out s)))
  ()
  ("Doing 2 pending tests of 2 tests total."
   " RT-TESTS::T1"
   "Test (T 2) failed"
   "Form: 4"
   "Expected value: 3"
   "Actual value: 4."
   "1 out of 2 total tests failed: (T 2)."))

(deftest continue-testing-1
  (setup (deftest temp (continue-testing) 5) (do-test 'temp) (pending-tests))
  () (t1 (t 2) temp))
