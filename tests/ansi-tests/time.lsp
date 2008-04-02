;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Dec 12 09:43:47 2004
;;;; Contains: Tests of TIME

(in-package :cl-test)

(deftest time.1
  (let ((s (with-output-to-string
	     (*trace-output*)
	     (assert (null (time nil))))))
    (= (length s) 0))
  nil)

(deftest time.2
  (let ((s (with-output-to-string
	     (*trace-output*)
	     (let ((x (cons 'a 'b)))
	       (assert (eq (time x) x))))))
    (= (length s) 0))
  nil)

(deftest time.3
  (let ((s (with-output-to-string
	     (*trace-output*)
	     (let ((x (cons 'a 'b)))
	       (flet ((%f () x))
		 (assert (eq (time (%f)) x)))))))
    (= (length s) 0))
  nil)

(deftest time.4
  (let ((s (with-output-to-string
	     (*trace-output*)
	     (assert (null (multiple-value-list (time (values))))))))
    (= (length s) 0))
  nil)

(deftest time.5
  (let ((s (with-output-to-string
	     (*trace-output*)
	     (assert (equal '(a b c d)
			    (multiple-value-list (time (values 'a 'b 'c 'd))))))))
    (= (length s) 0))
  nil)

(deftest time.6
  (let ((fn (compile nil '(lambda () (time nil)))))
    (let ((s (with-output-to-string
	       (*trace-output*)
	       (assert (null (funcall fn))))))
      (= (length s) 0)))
  nil)

(deftest time.7
  (flet ((%f () (time nil)))
    (let ((s (with-output-to-string
	       (*trace-output*)
	       (assert (null (%f))))))
      (= (length s) 0)))
  nil)

(deftest time.8
  (let ((s (with-output-to-string
	     (*trace-output*)
	     (macrolet ((%m () 1))
	       (assert (eql (time (%m)) 1))))))
    (= (length s) 0))
  nil)

;;; The TIME definition is weasely, so strenuous complaints from
;;; implementors about specific tests lead me to remove them.
;;; Someone didn't like this one at all.
#|
(deftest time.9
  (let ((s (with-output-to-string
	     (*trace-output*)
	     (block done
	       (time (return-from done nil))))))
    (= (length s) 0))
  nil)
|#

  