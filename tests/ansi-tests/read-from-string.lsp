;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan  1 14:50:09 2005
;;;; Contains: Tests of READ-FROM-STRING

(in-package :cl-test)

(deftest read-from-string.1
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "123")
     (let ((vals (multiple-value-list (read-from-string s))))
       (assert (= (length vals) 2))
       (assert (eql (first vals) 123))
       (assert (member (second vals) '(3 4))))))
  nil)

(deftest read-from-string.2
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "XYZ   ")
     (let ((vals (multiple-value-list (read-from-string s))))
       (assert (equal vals '(|XYZ| 4))))))
  nil)

(deftest read-from-string.3
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "(1 2 3)X")
     (let ((vals (multiple-value-list (read-from-string s))))
       (assert (equal vals '((1 2 3) 7))))))
  nil)

(deftest read-from-string.4
  (do-special-strings
     (s "")
     (let ((vals (multiple-value-list (read-from-string s nil :good))))
       (assert (= (length vals) 2))
       (assert (equal (first vals) :good))
       (assert (member (second vals) '(0 1)))))
  nil)

(deftest read-from-string.5
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "71235")
     (let ((vals (multiple-value-list
		  (read-from-string s t nil :start 1 :end 4))))
       (assert (equal vals '(123 4))))))
  nil)

(deftest read-from-string.6
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "7123  ")
     (let ((vals (multiple-value-list
		  (read-from-string s t nil :start 1))))
       (assert (equal vals '(123 5))))))
  nil)


(deftest read-from-string.7
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "7123  ")
     (let ((vals (multiple-value-list
		  (read-from-string s t nil :end 4))))
       (assert (equal vals '(7123 4))))))
  nil)

(deftest read-from-string.8
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "7123")
     (let ((vals (multiple-value-list
		  (read-from-string s nil 'foo :start 2 :end 2))))
       (assert (equal vals '(foo 2))))))
  nil)

(deftest read-from-string.9
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "123  ")
     (let ((vals (multiple-value-list
		  (read-from-string s t nil :preserve-whitespace t))))
       (assert (equal vals '(123 3))))))
  nil)

(deftest read-from-string.10
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s (concatenate 'string "( )" (string #\Newline)))
     (let ((vals (multiple-value-list
		  (read-from-string s t nil :preserve-whitespace t))))
       (assert (equal vals '(nil 3))))))
  nil)

;;; Multiple keywords

(deftest read-from-string.11
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "7123  ")
     (let ((vals (multiple-value-list
		  (read-from-string s t nil :start 1 :start 2))))
       (assert (equal vals '(123 5))))))
  nil)

(deftest read-from-string.12
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "7123  ")
     (let ((vals (multiple-value-list
		  (read-from-string s t nil :end 4 :end 2))))
       (assert (equal vals '(7123 4))))))
  nil)

(deftest read-from-string.13
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s (concatenate 'string "( )" (string #\Newline)))
     (let ((vals (multiple-value-list
		  (read-from-string s t nil :preserve-whitespace t
				    :preserve-whitespace nil))))
       (assert (equal vals '(nil 3))))))
  nil)

;;; Allow other keys

(deftest read-from-string.14
  (with-standard-io-syntax
   (let ((*package* (find-package :cl-test)))
     (do-special-strings
      (s "abc   ")
      (let ((vals (multiple-value-list
		   (read-from-string s t nil :allow-other-keys nil))))
	(assert (equal vals '(|ABC| 4)) (vals) "VALS is ~A" vals)))))
  nil)

(deftest read-from-string.15
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "123   ")
     (let ((vals (multiple-value-list
		  (read-from-string s t nil :foo 'bar :allow-other-keys t))))
       (assert (equal vals '(123 4)) (vals) "VALS is ~A" vals))))
  nil)

(deftest read-from-string.16
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "123   ")
     (let ((vals (multiple-value-list
		  (read-from-string s t nil :allow-other-keys t
				    :allow-other-keys nil :foo 'bar))))
       (assert (equal vals '(123 4)) (vals) "VALS is ~A" vals))))
  nil)

;;; default for :end

(deftest read-from-string.17
  (let ((*package* (find-package :cl-test)))
    (do-special-strings
     (s "XYZ   ")
     (let ((vals (multiple-value-list (read-from-string s t nil :end nil))))
       (assert (equal vals '(|XYZ| 4))))))
  nil)

;;; TODO  Add tests for reading from strings containing non-base characters

;;; Error tests

(deftest read-from-string.error.1
  (signals-error (read-from-string "") error)
  t)

(deftest read-from-string.error.2
  (signals-error (read-from-string "(A B ") error)
  t)

(deftest read-from-string.error.3
  (signals-error (read-from-string "" t) error)
  t)

(deftest read-from-string.error.4
  (signals-error (read-from-string "" t nil) error)
  t)

(deftest read-from-string.error.5
  (signals-error (read-from-string "(A B " nil) error)
  t)

(deftest read-from-string.error.6
  (signals-error (read-from-string "(A B " t) error)
  t)

(deftest read-from-string.error.7
  (signals-error (read-from-string "123" t nil :start 0 :end 0) error)
  t)

(deftest read-from-string.error.8
  (signals-error (read-from-string) program-error)
  t)

(deftest read-from-string.error.9
  (signals-error (read-from-string "A" nil t :bad-keyword t) program-error)
  t)


(deftest read-from-string.error.10
  (signals-error (read-from-string "A" nil t
				   :bad-keyword t
				   :allow-other-keys nil)
		 program-error)
  t)

(deftest read-from-string.error.11
  (signals-error (read-from-string "A" nil t
				   :bad-keyword t
				   :allow-other-keys nil
				   :allow-other-keys t)
		 program-error)
  t)

(deftest read-from-string.error.12
  (signals-error (read-from-string "A" nil t
				   :allow-other-keys nil
				   :allow-other-keys t
				   :bad-keyword t)
		 program-error)
  t)

(deftest read-from-string.error.13
  (signals-error (read-from-string "A" nil t :start)
		 program-error)
  t)


