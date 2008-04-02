;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct  4 06:32:41 2002
;;;; Contains: Tests of string comparison functions

(in-package :cl-test)

(compile-and-load "string-aux.lsp")

(deftest string=.1
  (not (string= "abc" (copy-seq "abc")))
  nil)

(deftest string=.2
  (string= "A" "a")
  nil)

(deftest string=.3
  (not (string= #\a "a"))
  nil)

(deftest string=.4
  (not (string= '|abc| (copy-seq "abc")))
  nil)

(deftest string=.5
  (not (string= (copy-seq "abc") '#:|abc|))
  nil)

;;; Test that it doesn't stop at null characters
(deftest string=.6
  (let ((s1 (copy-seq "abc"))
	(s2 (copy-seq "abd"))
	(c (or (code-char 0) #\a)))
    (setf (char s1 1) c)
    (setf (char s2 1) c)
    (values (length s1) (length s2) (string= s1 s2)))
  3 3 nil)

(deftest string=.7
  (loop for i from 0 to 3
	collect (not (string= "abc" "abd" :start1 0 :end1 i :end2 i)))
  (nil nil nil t))

(deftest string=.8
  (loop for i from 0 to 3
	collect (not (string= "abc" "ab" :end1 i)))
  (t t nil t))

(deftest string=.9
  (loop for i from 0 to 3
	collect (not (string= "abc" "abd" :start2 0 :end2 i :end1 i)))
  (nil nil nil t))

(deftest string=.10
  (loop for i from 0 to 3
	collect (not (string= "ab" "abc" :end2 i)))
  (t t nil t))

(deftest string=.11
  (loop for i from 0 to 3
	collect (not (string= "xyab" "ab" :start1 i)))
  (t t nil t))

(deftest string=.12
  (loop for i from 0 to 3
	collect (not (string= "ab" "xyab" :start2 i)))
  (t t nil t))

(deftest string=.13
  (loop for i from 0 to 3
	collect (not (string= "xyab" "ab" :start1 i :end1 nil)))
  (t t nil t))

(deftest string=.14
  (loop for i from 0 to 3
	collect (not (string= "ab" "xyab" :start2 i :end2 nil)))
  (t t nil t))

;;; Keyword argument processing

(deftest string-comparison.allow-other-keys.1
  (loop for fn in '(string= string<= string>= string/= string< string>
		    string-equal string-not-greaterp string-not-lessp
		    string-not-equal string-lessp string-greaterp)
	for expected in '(nil 0 nil 0 0 nil nil 0 nil 0 0 nil)
	for result = (funcall fn "a" "b" :allow-other-keys t :foo nil)
	unless (eql result expected)
	collect (list fn expected result))
  nil)

(deftest string-comparison.allow-other-keys.2
  (loop for fn in '(string= string<= string>= string/= string< string>
		    string-equal string-not-greaterp string-not-lessp
		    string-not-equal string-lessp string-greaterp)
	for expected in '(nil nil 0 0 nil 0 nil nil 0 0 nil 0)
	for result = (funcall fn "c" "b" :allow-other-keys t
			      :allow-other-keys nil :foo 1)
	unless (eql result expected)
	collect (list fn expected result))
  nil)

(deftest string-comparison.allow-other-keys.3
  (loop for fn in '(string= string<= string>= string/= string< string>
		    string-equal string-not-greaterp string-not-lessp
		    string-not-equal string-lessp string-greaterp)
	for expected in '(nil 0 nil 0 0 nil nil 0 nil 0 0 nil)
	for result = (funcall fn "a" "b" :allow-other-keys nil)
	unless (eql result expected)
	collect (list fn expected result))
  nil)


;;; Order of evaluation

(deftest string=.order.1
  (let ((i 0) x y)
    (values
     (string= (progn (setf x (incf i)) "abc")
	      (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string=.order.2
  (let ((i 0) a b c d e f)
    (values
     (string= (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :start1 (progn (setf c (incf i)) 0)
	      :start2 (progn (setf d (incf i)) 0)
	      :end1 (progn (setf e (incf i)) nil)
	      :end2 (progn (setf f (incf i)) nil)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string=.order.3
  (let ((i 0) a b c d e f)
    (values
     (string= (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :end2 (progn (setf c (incf i)) nil)
	      :end1 (progn (setf d (incf i)) nil)
	      :start2 (progn (setf e (incf i)) 0)
	      :start1 (progn (setf f (incf i)) 0)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string<=.order.1
  (let ((i 0) x y)
    (values
     (string<= (progn (setf x (incf i)) "abf")
	       (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string<=.order.2
  (let ((i 0) a b c d e f)
    (values
     (string<= (progn (setf a (incf i)) "abf")
	       (progn (setf b (incf i)) "abd")
	       :start1 (progn (setf c (incf i)) 0)
	       :start2 (progn (setf d (incf i)) 0)
	       :end1 (progn (setf e (incf i)) nil)
	       :end2 (progn (setf f (incf i)) nil)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string<=.order.3
  (let ((i 0) a b c d e f)
    (values
     (string<= (progn (setf a (incf i)) "abf")
	       (progn (setf b (incf i)) "abd")
	       :end2 (progn (setf c (incf i)) nil)
	       :end1 (progn (setf d (incf i)) nil)
	       :start2 (progn (setf e (incf i)) 0)
	       :start1 (progn (setf f (incf i)) 0)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string<.order.1
  (let ((i 0) x y)
    (values
     (string< (progn (setf x (incf i)) "abf")
	      (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string<.order.2
  (let ((i 0) a b c d e f)
    (values
     (string< (progn (setf a (incf i)) "abf")
	      (progn (setf b (incf i)) "abd")
	      :start1 (progn (setf c (incf i)) 0)
	      :start2 (progn (setf d (incf i)) 0)
	      :end1 (progn (setf e (incf i)) nil)
	      :end2 (progn (setf f (incf i)) nil)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string<.order.3
  (let ((i 0) a b c d e f)
    (values
     (string< (progn (setf a (incf i)) "abf")
	      (progn (setf b (incf i)) "abd")
	      :end2 (progn (setf c (incf i)) nil)
	      :end1 (progn (setf d (incf i)) nil)
	      :start2 (progn (setf e (incf i)) 0)
	      :start1 (progn (setf f (incf i)) 0)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)


(deftest string/=.order.1
  (let ((i 0) x y)
    (values
     (string/= (progn (setf x (incf i)) "abc")
	       (progn (setf y (incf i)) "abc"))
     i x y))
  nil 2 1 2)

(deftest string/=.order.2
  (let ((i 0) a b c d e f)
    (values
     (string/= (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abc")
	       :start1 (progn (setf c (incf i)) 0)
	       :start2 (progn (setf d (incf i)) 0)
	       :end1 (progn (setf e (incf i)) nil)
	       :end2 (progn (setf f (incf i)) nil)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string/=.order.3
  (let ((i 0) a b c d e f)
    (values
     (string/= (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abc")
	       :end2 (progn (setf c (incf i)) nil)
	       :end1 (progn (setf d (incf i)) nil)
	       :start2 (progn (setf e (incf i)) 0)
	       :start1 (progn (setf f (incf i)) 0)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string>=.order.1
  (let ((i 0) x y)
    (values
     (string<= (progn (setf x (incf i)) "abf")
	       (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string>=.order.2
  (let ((i 0) a b c d e f)
    (values
     (string>= (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abd")
	       :start1 (progn (setf c (incf i)) 0)
	       :start2 (progn (setf d (incf i)) 0)
	       :end1 (progn (setf e (incf i)) nil)
	       :end2 (progn (setf f (incf i)) nil)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string>=.order.3
  (let ((i 0) a b c d e f)
    (values
     (string>= (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abd")
	       :end2 (progn (setf c (incf i)) nil)
	       :end1 (progn (setf d (incf i)) nil)
	       :start2 (progn (setf e (incf i)) 0)
	       :start1 (progn (setf f (incf i)) 0)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string>.order.1
  (let ((i 0) x y)
    (values
     (string> (progn (setf x (incf i)) "abc")
	      (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string>.order.2
  (let ((i 0) a b c d e f)
    (values
     (string> (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :start1 (progn (setf c (incf i)) 0)
	      :start2 (progn (setf d (incf i)) 0)
	      :end1 (progn (setf e (incf i)) nil)
	      :end2 (progn (setf f (incf i)) nil)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string>.order.3
  (let ((i 0) a b c d e f)
    (values
     (string> (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :end2 (progn (setf c (incf i)) nil)
	      :end1 (progn (setf d (incf i)) nil)
	      :start2 (progn (setf e (incf i)) 0)
	      :start1 (progn (setf f (incf i)) 0)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)


(deftest string-equal.order.1
  (let ((i 0) x y)
    (values
     (string-equal (progn (setf x (incf i)) "abc")
	      (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string-equal.order.2
  (let ((i 0) a b c d e f)
    (values
     (string-equal (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :start1 (progn (setf c (incf i)) 0)
	      :start2 (progn (setf d (incf i)) 0)
	      :end1 (progn (setf e (incf i)) nil)
	      :end2 (progn (setf f (incf i)) nil)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-equal.order.3
  (let ((i 0) a b c d e f)
    (values
     (string-equal (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :end2 (progn (setf c (incf i)) nil)
	      :end1 (progn (setf d (incf i)) nil)
	      :start2 (progn (setf e (incf i)) 0)
	      :start1 (progn (setf f (incf i)) 0)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-not-greaterp.order.1
  (let ((i 0) x y)
    (values
     (string-not-greaterp (progn (setf x (incf i)) "abf")
		       (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string-not-greaterp.order.2
  (let ((i 0) a b c d e f)
    (values
     (string-not-greaterp (progn (setf a (incf i)) "abf")
	       (progn (setf b (incf i)) "abd")
	       :start1 (progn (setf c (incf i)) 0)
	       :start2 (progn (setf d (incf i)) 0)
	       :end1 (progn (setf e (incf i)) nil)
	       :end2 (progn (setf f (incf i)) nil)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-not-greaterp.order.3
  (let ((i 0) a b c d e f)
    (values
     (string-not-greaterp (progn (setf a (incf i)) "abf")
	       (progn (setf b (incf i)) "abd")
	       :end2 (progn (setf c (incf i)) nil)
	       :end1 (progn (setf d (incf i)) nil)
	       :start2 (progn (setf e (incf i)) 0)
	       :start1 (progn (setf f (incf i)) 0)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-lessp.order.1
  (let ((i 0) x y)
    (values
     (string-lessp (progn (setf x (incf i)) "abf")
	      (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string-lessp.order.2
  (let ((i 0) a b c d e f)
    (values
     (string-lessp (progn (setf a (incf i)) "abf")
	      (progn (setf b (incf i)) "abd")
	      :start1 (progn (setf c (incf i)) 0)
	      :start2 (progn (setf d (incf i)) 0)
	      :end1 (progn (setf e (incf i)) nil)
	      :end2 (progn (setf f (incf i)) nil)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-lessp.order.3
  (let ((i 0) a b c d e f)
    (values
     (string-lessp (progn (setf a (incf i)) "abf")
	      (progn (setf b (incf i)) "abd")
	      :end2 (progn (setf c (incf i)) nil)
	      :end1 (progn (setf d (incf i)) nil)
	      :start2 (progn (setf e (incf i)) 0)
	      :start1 (progn (setf f (incf i)) 0)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)


(deftest string-not-equal.order.1
  (let ((i 0) x y)
    (values
     (string-not-equal (progn (setf x (incf i)) "abc")
	       (progn (setf y (incf i)) "abc"))
     i x y))
  nil 2 1 2)

(deftest string-not-equal.order.2
  (let ((i 0) a b c d e f)
    (values
     (string-not-equal (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abc")
	       :start1 (progn (setf c (incf i)) 0)
	       :start2 (progn (setf d (incf i)) 0)
	       :end1 (progn (setf e (incf i)) nil)
	       :end2 (progn (setf f (incf i)) nil)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-not-equal.order.3
  (let ((i 0) a b c d e f)
    (values
     (string-not-equal (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abc")
	       :end2 (progn (setf c (incf i)) nil)
	       :end1 (progn (setf d (incf i)) nil)
	       :start2 (progn (setf e (incf i)) 0)
	       :start1 (progn (setf f (incf i)) 0)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-not-lessp.order.1
  (let ((i 0) x y)
    (values
     (string-not-lessp (progn (setf x (incf i)) "abc")
		       (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string-not-lessp.order.2
  (let ((i 0) a b c d e f)
    (values
     (string-not-lessp (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abd")
	       :start1 (progn (setf c (incf i)) 0)
	       :start2 (progn (setf d (incf i)) 0)
	       :end1 (progn (setf e (incf i)) nil)
	       :end2 (progn (setf f (incf i)) nil)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-not-lessp.order.3
  (let ((i 0) a b c d e f)
    (values
     (string-not-lessp (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abd")
	       :end2 (progn (setf c (incf i)) nil)
	       :end1 (progn (setf d (incf i)) nil)
	       :start2 (progn (setf e (incf i)) 0)
	       :start1 (progn (setf f (incf i)) 0)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-greaterp.order.1
  (let ((i 0) x y)
    (values
     (string-greaterp (progn (setf x (incf i)) "abc")
	      (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string-greaterp.order.2
  (let ((i 0) a b c d e f)
    (values
     (string-greaterp (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :start1 (progn (setf c (incf i)) 0)
	      :start2 (progn (setf d (incf i)) 0)
	      :end1 (progn (setf e (incf i)) nil)
	      :end2 (progn (setf f (incf i)) nil)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-greaterp.order.3
  (let ((i 0) a b c d e f)
    (values
     (string-greaterp (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :end2 (progn (setf c (incf i)) nil)
	      :end1 (progn (setf d (incf i)) nil)
	      :start2 (progn (setf e (incf i)) 0)
	      :start1 (progn (setf f (incf i)) 0)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)


;;; Random tests (of all the string comparson functions)

(deftest random-string-comparison-tests
  (loop for cmp in '(= /= < > <= >=)
	append
	(loop for case in '(nil t)
	      collect
	      (list cmp case
		    (random-string-compare-test 10 cmp case 1000))))
  ((= nil 0) (= t 0) (/= nil 0) (/= t 0) (< nil 0) (< t 0)
   (> nil 0) (> t 0) (<= nil 0) (<= t 0) (>= nil 0) (>= t 0)))

;;; Tests on nil arrays

(deftest string=.nil-array.1
  :notes (:nil-vectors-are-strings)
  (let ((s1 (make-array '(0) :element-type nil)))
    (values
     (notnot (string= s1 s1))
     (notnot (string= s1 (make-array '(0) :element-type nil)))
     (notnot (string= s1 (make-array '(0) :element-type 'base-char)))
     (notnot (string= s1 ""))
     (notnot (string= "" s1))
     (string= s1 "a")
     (string= "a" s1)))
  t t t t t nil nil)

(deftest string/=.nil-array.1
  :notes (:nil-vectors-are-strings)
  (let ((s1 (make-array '(0) :element-type nil)))
    (values
     (string/= s1 s1)
     (string/= s1 (make-array '(0) :element-type nil))
     (string/= s1 (make-array '(0) :element-type 'base-char))
     (string/= s1 "")
     (string/= "" s1)
     (string/= s1 "a")
     (string/= "a" s1)))
   nil nil nil nil nil 0 0)

(deftest string<.nil-array.1
  :notes (:nil-vectors-are-strings)
  (let ((s1 (make-array '(0) :element-type nil)))
    (values
     (string< s1 s1)
     (string< s1 (make-array '(0) :element-type nil))
     (string< s1 (make-array '(0) :element-type 'base-char))
     (string< s1 "")
     (string< "" s1)
     (string< s1 "a")
     (string< "a" s1)))
  nil nil nil nil nil 0 nil)

(deftest string<=.nil-array.1
  :notes (:nil-vectors-are-strings)
  (let ((s1 (make-array '(0) :element-type nil)))
    (values
     (string<= s1 s1)
     (string<= s1 (make-array '(0) :element-type nil))
     (string<= s1 (make-array '(0) :element-type 'base-char))
     (string<= s1 "")
     (string<= "" s1)
     (string<= s1 "a")
     (string<= "a" s1)))
  0 0 0 0 0 0 nil)

(deftest string>.nil-array.1
  :notes (:nil-vectors-are-strings)
  (let ((s1 (make-array '(0) :element-type nil)))
    (values
     (string> s1 s1)
     (string> s1 (make-array '(0) :element-type nil))
     (string> s1 (make-array '(0) :element-type 'base-char))
     (string> s1 "")
     (string> "" s1)
     (string> s1 "a")
     (string> "a" s1)))
  nil nil nil nil nil nil 0)

(deftest string>=.nil-array.1
  :notes (:nil-vectors-are-strings)
  (let ((s1 (make-array '(0) :element-type nil)))
    (values
     (string>= s1 s1)
     (string>= s1 (make-array '(0) :element-type nil))
     (string>= s1 (make-array '(0) :element-type 'base-char))
     (string>= s1 "")
     (string>= "" s1)
     (string>= s1 "a")
     (string>= "a" s1)))
  0 0 0 0 0 nil 0)

(deftest string-equal.nil-array.1
  :notes (:nil-vectors-are-strings)
  (let ((s1 (make-array '(0) :element-type nil)))
    (values
     (notnot (string-equal s1 s1))
     (notnot (string-equal s1 (make-array '(0) :element-type nil)))
     (notnot (string-equal s1 (make-array '(0) :element-type 'base-char)))
     (notnot (string-equal s1 ""))
     (notnot (string-equal "" s1))
     (string-equal s1 "a")
     (string-equal "a" s1)))
  t t t t t nil nil)

(deftest string-not-equal.nil-array.1
  :notes (:nil-vectors-are-strings)
  (let ((s1 (make-array '(0) :element-type nil)))
    (values
     (string-not-equal s1 s1)
     (string-not-equal s1 (make-array '(0) :element-type nil))
     (string-not-equal s1 (make-array '(0) :element-type 'base-char))
     (string-not-equal s1 "")
     (string-not-equal "" s1)
     (string-not-equal s1 "a")
     (string-not-equal "a" s1)))
   nil nil nil nil nil 0 0)

(deftest string-lessp.nil-array.1
  :notes (:nil-vectors-are-strings)
  (let ((s1 (make-array '(0) :element-type nil)))
    (values
     (string-lessp s1 s1)
     (string-lessp s1 (make-array '(0) :element-type nil))
     (string-lessp s1 (make-array '(0) :element-type 'base-char))
     (string-lessp s1 "")
     (string-lessp "" s1)
     (string-lessp s1 "a")
     (string-lessp "a" s1)))
  nil nil nil nil nil 0 nil)

(deftest string-not-greaterp.nil-array.1
  :notes (:nil-vectors-are-strings)
  (let ((s1 (make-array '(0) :element-type nil)))
    (values
     (string-not-greaterp s1 s1)
     (string-not-greaterp s1 (make-array '(0) :element-type nil))
     (string-not-greaterp s1 (make-array '(0) :element-type 'base-char))
     (string-not-greaterp s1 "")
     (string-not-greaterp "" s1)
     (string-not-greaterp s1 "a")
     (string-not-greaterp "a" s1)))
  0 0 0 0 0 0 nil)

(deftest string-greaterp.nil-array.1
  :notes (:nil-vectors-are-strings)
  (let ((s1 (make-array '(0) :element-type nil)))
    (values
     (string-greaterp s1 s1)
     (string-greaterp s1 (make-array '(0) :element-type nil))
     (string-greaterp s1 (make-array '(0) :element-type 'base-char))
     (string-greaterp s1 "")
     (string-greaterp "" s1)
     (string-greaterp s1 "a")
     (string-greaterp "a" s1)))
  nil nil nil nil nil nil 0)

(deftest string-not-lessp.nil-array.1
  :notes (:nil-vectors-are-strings)
  (let ((s1 (make-array '(0) :element-type nil)))
    (values
     (string-not-lessp s1 s1)
     (string-not-lessp s1 (make-array '(0) :element-type nil))
     (string-not-lessp s1 (make-array '(0) :element-type 'base-char))
     (string-not-lessp s1 "")
     (string-not-lessp "" s1)
     (string-not-lessp s1 "a")
     (string-not-lessp "a" s1)))
  0 0 0 0 0 nil 0)

;;; Error cases

(deftest string=.error.1
  (signals-error (string=) program-error)
  t)

(deftest string=.error.2
  (signals-error (string= "") program-error)
  t)

(deftest string=.error.3
  (signals-error (string= "a" "b" nil nil) program-error)
  t)

(deftest string=.error.4
  (signals-error (string= "a" "b" :start1) program-error)
  t)

(deftest string=.error.5
  (signals-error (string= "a" "b" 1 nil) program-error)
  t)

(deftest string=.error.6
  (signals-error (string= "a" "b" :allow-other-keys nil
			  :allow-other-keys t :foo 'bar)
		 program-error)
  t)

(deftest string/=.error.1
  (signals-error (string/=) program-error)
  t)

(deftest string/=.error.2
  (signals-error (string/= "") program-error)
  t)

(deftest string/=.error.3
  (signals-error (string/= "a" "b" nil nil) program-error)
  t)

(deftest string/=.error.4
  (signals-error (string/= "a" "b" :start1) program-error)
  t)

(deftest string/=.error.5
  (signals-error (string/= "a" "b" 1 nil) program-error)
  t)

(deftest string/=.error.6
  (signals-error (string/= "a" "b" :allow-other-keys nil
			  :allow-other-keys t :foo 'bar)
		 program-error)
  t)


(deftest string<.error.1
  (signals-error (string<) program-error)
  t)

(deftest string<.error.2
  (signals-error (string< "") program-error)
  t)

(deftest string<.error.3
  (signals-error (string< "a" "b" nil nil) program-error)
  t)

(deftest string<.error.4
  (signals-error (string< "a" "b" :start1) program-error)
  t)

(deftest string<.error.5
  (signals-error (string< "a" "b" 1 nil) program-error)
  t)

(deftest string<.error.6
  (signals-error (string< "a" "b" :allow-other-keys nil
			  :allow-other-keys t :foo 'bar)
		 program-error)
  t)


(deftest string<=.error.1
  (signals-error (string<=) program-error)
  t)

(deftest string<=.error.2
  (signals-error (string<= "") program-error)
  t)

(deftest string<=.error.3
  (signals-error (string<= "a" "b" nil nil) program-error)
  t)

(deftest string<=.error.4
  (signals-error (string<= "a" "b" :start1) program-error)
  t)

(deftest string<=.error.5
  (signals-error (string<= "a" "b" 1 nil) program-error)
  t)

(deftest string<=.error.6
  (signals-error (string<= "a" "b" :allow-other-keys nil
			  :allow-other-keys t :foo 'bar)
		 program-error)
  t)


(deftest string>.error.1
  (signals-error (string>) program-error)
  t)

(deftest string>.error.2
  (signals-error (string> "") program-error)
  t)

(deftest string>.error.3
  (signals-error (string> "a" "b" nil nil) program-error)
  t)

(deftest string>.error.4
  (signals-error (string> "a" "b" :start1) program-error)
  t)

(deftest string>.error.5
  (signals-error (string> "a" "b" 1 nil) program-error)
  t)

(deftest string>.error.6
  (signals-error (string> "a" "b" :allow-other-keys nil
			  :allow-other-keys t :foo 'bar)
		 program-error)
  t)


(deftest string>=.error.1
  (signals-error (string>=) program-error)
  t)

(deftest string>=.error.2
  (signals-error (string>= "") program-error)
  t)

(deftest string>=.error.3
  (signals-error (string>= "a" "b" nil nil) program-error)
  t)

(deftest string>=.error.4
  (signals-error (string>= "a" "b" :start1) program-error)
  t)

(deftest string>=.error.5
  (signals-error (string>= "a" "b" 1 nil) program-error)
  t)

(deftest string>=.error.6
  (signals-error (string>= "a" "b" :allow-other-keys nil
			  :allow-other-keys t :foo 'bar)
		 program-error)
  t)


(deftest string-equal.error.1
  (signals-error (string-equal) program-error)
  t)

(deftest string-equal.error.2
  (signals-error (string-equal "") program-error)
  t)

(deftest string-equal.error.3
  (signals-error (string-equal "a" "b" nil nil) program-error)
  t)

(deftest string-equal.error.4
  (signals-error (string-equal "a" "b" :start1) program-error)
  t)

(deftest string-equal.error.5
  (signals-error (string-equal "a" "b" 1 nil) program-error)
  t)

(deftest string-equal.error.6
  (signals-error (string-equal "a" "b" :allow-other-keys nil
			  :allow-other-keys t :foo 'bar)
		 program-error)
  t)

(deftest string-not-equal.error.1
  (signals-error (string-not-equal) program-error)
  t)

(deftest string-not-equal.error.2
  (signals-error (string-not-equal "") program-error)
  t)

(deftest string-not-equal.error.3
  (signals-error (string-not-equal "a" "b" nil nil) program-error)
  t)

(deftest string-not-equal.error.4
  (signals-error (string-not-equal "a" "b" :start1) program-error)
  t)

(deftest string-not-equal.error.5
  (signals-error (string-not-equal "a" "b" 1 nil) program-error)
  t)

(deftest string-not-equal.error.6
  (signals-error (string-not-equal "a" "b" :allow-other-keys nil
			  :allow-other-keys t :foo 'bar)
		 program-error)
  t)

(deftest string-lessp.error.1
  (signals-error (string-lessp) program-error)
  t)

(deftest string-lessp.error.2
  (signals-error (string-lessp "") program-error)
  t)

(deftest string-lessp.error.3
  (signals-error (string-lessp "a" "b" nil nil) program-error)
  t)

(deftest string-lessp.error.4
  (signals-error (string-lessp "a" "b" :start1) program-error)
  t)

(deftest string-lessp.error.5
  (signals-error (string-lessp "a" "b" 1 nil) program-error)
  t)

(deftest string-lessp.error.6
  (signals-error (string-lessp "a" "b" :allow-other-keys nil
			  :allow-other-keys t :foo 'bar)
		 program-error)
  t)

(deftest string-greaterp.error.1
  (signals-error (string-greaterp) program-error)
  t)

(deftest string-greaterp.error.2
  (signals-error (string-greaterp "") program-error)
  t)

(deftest string-greaterp.error.3
  (signals-error (string-greaterp "a" "b" nil nil) program-error)
  t)

(deftest string-greaterp.error.4
  (signals-error (string-greaterp "a" "b" :start1) program-error)
  t)

(deftest string-greaterp.error.5
  (signals-error (string-greaterp "a" "b" 1 nil) program-error)
  t)

(deftest string-greaterp.error.6
  (signals-error (string-greaterp "a" "b" :allow-other-keys nil
			  :allow-other-keys t :foo 'bar)
		 program-error)
  t)

(deftest string-not-lessp.error.1
  (signals-error (string-not-lessp) program-error)
  t)

(deftest string-not-lessp.error.2
  (signals-error (string-not-lessp "") program-error)
  t)

(deftest string-not-lessp.error.3
  (signals-error (string-not-lessp "a" "b" nil nil) program-error)
  t)

(deftest string-not-lessp.error.4
  (signals-error (string-not-lessp "a" "b" :start1) program-error)
  t)

(deftest string-not-lessp.error.5
  (signals-error (string-not-lessp "a" "b" 1 nil) program-error)
  t)

(deftest string-not-lessp.error.6
  (signals-error (string-not-lessp "a" "b" :allow-other-keys nil
			  :allow-other-keys t :foo 'bar)
		 program-error)
  t)

(deftest string-not-greaterp.error.1
  (signals-error (string-not-greaterp) program-error)
  t)

(deftest string-not-greaterp.error.2
  (signals-error (string-not-greaterp "") program-error)
  t)

(deftest string-not-greaterp.error.3
  (signals-error (string-not-greaterp "a" "b" nil nil) program-error)
  t)

(deftest string-not-greaterp.error.4
  (signals-error (string-not-greaterp "a" "b" :start1) program-error)
  t)

(deftest string-not-greaterp.error.5
  (signals-error (string-not-greaterp "a" "b" 1 nil) program-error)
  t)

(deftest string-not-greaterp.error.6
  (signals-error (string-not-greaterp "a" "b" :allow-other-keys nil
			  :allow-other-keys t :foo 'bar)
		 program-error)
  t)
