;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct  5 12:32:20 2002
;;;; Contains: Tests for MAKE-STRING

(in-package :cl-test)

(deftest make-string.1
  (let ((s (make-string 10)))
    (and (stringp s)
	 #+:ansi-tests-strict-initial-element
	 (string-all-the-same s)
	 (eqlt (length s) 10)
	 ))
  t)

(deftest make-string.2
  (let ((s (make-string 10 :initial-element #\a)))
    (and (stringp s)
	 (eql (length s) 10)
	 s))
  "aaaaaaaaaa")

(deftest make-string.3
  (let ((s (make-string 10 :initial-element #\a
			:element-type 'character)))
    (and (stringp s)
	 (eql (length s) 10)
	 s))
  "aaaaaaaaaa")

(deftest make-string.4
  (let ((s (make-string 10 :initial-element #\a
			:element-type 'standard-char)))
    (and (stringp s)
	 (eql (length s) 10)
	 s))
  "aaaaaaaaaa")

(deftest make-string.5
  (let ((s (make-string 10 :initial-element #\a
			:element-type 'base-char)))
    (and (stringp s)
	 (eql (length s) 10)
	 s))
  "aaaaaaaaaa")

(deftest make-string.6
  (make-string 0)
  "")

(deftest make-string.7
  (let ((s (make-string 10 :element-type 'character)))
    (and (stringp s)
	 (eqlt (length s) 10)
	 #+:ansi-tests-strict-initial-element
	 (string-all-the-same s)
	 ))
  t)

(deftest make-string.8
  (let ((s (make-string 10 :element-type 'standard-char)))
    (and (stringp s)
	 (eqlt (length s) 10)
	 #+:ansi-tests-strict-initial-element
	 (string-all-the-same s)
	 ))
  t)

(deftest make-string.9
  (let ((s (make-string 10 :element-type 'base-char)))
    (and (stringp s)
	 (eqlt (length s) 10)
	 #+:ansi-tests-strict-initial-element
	 (string-all-the-same s)
	 ))
  t)

(deftest make-string.10
  :notes (:nil-vectors-are-strings)
  (let ((s (make-string 0 :element-type nil)))
    (values
     (notnot (stringp s))
     (eqlt (length s) 0)
     (equalt s "")))
  t t t)

(def-fold-test make-string.fold.1 (make-string 5 :initial-element #\a))

;;; Keyword tests
;
(deftest make-string.allow-other-keys.1
  (make-string 5 :allow-other-keys t :initial-element #\a)
  "aaaaa")

(deftest make-string.allow-other-keys.2
  (make-string 5 :initial-element #\a :allow-other-keys t)
  "aaaaa")

(deftest make-string.allow-other-keys.3
  (make-string 5 :initial-element #\a :allow-other-keys t
	       :bad t)
  "aaaaa")

(deftest make-string.allow-other-keys.4
  (make-string 5 :bad t :allow-other-keys t :allow-other-keys nil
	       :initial-element #\a)
  "aaaaa")

(deftest make-string.allow-other-keys.5
  (make-string 5 :allow-other-keys t :bad t :allow-other-keys nil
	       :initial-element #\a)
  "aaaaa")

(deftest make-string.allow-other-keys.6
  (make-string 5 :allow-other-keys t :allow-other-keys nil :bad nil
	       :initial-element #\a)
  "aaaaa")

(deftest make-string.keywords.7
  (make-string 5 :initial-element #\a :initial-element #\b)
  "aaaaa")

;; Error cases

(deftest make-string.error.1
  (signals-error (make-string) program-error)
  t)

(deftest make-string.error.2
  (signals-error (make-string 10 :bad t) program-error)
  t)

(deftest make-string.error.3
  (signals-error (make-string 10 :bad t :allow-other-keys nil)
		 program-error)
  t)

(deftest make-string.error.4
  (signals-error (make-string 10 :initial-element) program-error)
  t)

(deftest make-string.error.5
  (signals-error (make-string 10 1 1) program-error)
  t)

(deftest make-string.error.6
  (signals-error (make-string 10 :element-type) program-error)
  t)

;;; Order of evaluation

(deftest make-string.order.1
  (let ((i 0) a b)
    (values
     (make-string (progn (setf a (incf i)) 4)
		  :initial-element (progn (setf b (incf i)) #\a))
     i a b))
  "aaaa" 2 1 2)

(deftest make-string.order.2
  (let ((i 0) a b c)
    (values
     (make-string (progn (setf a (incf i)) 4)
		  :initial-element (progn (setf b (incf i)) #\a)
		  :element-type (progn (setf c (incf i)) 'base-char))
     i a b c))
  "aaaa" 3 1 2 3)

(deftest make-string.order.3
  (let ((i 0) a b c)
    (values
     (make-string (progn (setf a (incf i)) 4)
		  :element-type (progn (setf b (incf i)) 'base-char)
		  :initial-element (progn (setf c (incf i)) #\a))
     i a b c))
  "aaaa" 3 1 2 3)


