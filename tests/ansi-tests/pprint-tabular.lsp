;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun 27 06:29:39 2004
;;;; Contains: Tests of PPRINT-TABULAR

(in-package :cl-test)

;;; When printing a non-list, the result is the same as calling WRITE."
(deftest pprint-tabular.1
  (my-with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil))
     (loop for obj in *mini-universe*
	   nconc
	   (and (not (listp obj))
		(let ((s1 (write-to-string obj))
		      (s2 (with-output-to-string (s) (pprint-tabular s obj))))
		  (unless (equal s1 s2)
		    (list (list obj s1 s2))))))))
  nil)

(deftest pprint-tabular.2
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil))
     (loop for obj in *mini-universe*
	   nconc
	   (and (not (listp obj))
		(let ((s1 (write-to-string obj))
		      (s2 (with-output-to-string (s) (pprint-tabular s obj))))
		  (unless (equal s1 s2)
		    (list (list obj s1 s2))))))))
  nil)

(defmacro def-pprint-tabular-test (name args expected-value &key (margin 100) (circle nil) (pre nil))
  `(deftest ,name
     (my-with-standard-io-syntax
      (let ((*print-pretty* t)
	    (*print-readably* nil)
	    (*print-right-margin* ,margin)
	    (*package* (find-package :cl-test))
	    (*print-circle* ,circle))
	(with-output-to-string
	  (s)
	  ,@(when pre (list pre))
	  (pprint-tabular s ,@args))))
     ,expected-value))

;;;
;;; Note
;;; The prefix and suffix "(" and ")" are not considered part of the
;;; logical block they enclose (see the spec page for pprint-logical-block.
;;;

(def-pprint-tabular-test pprint-tabular.3 ('(|M|)) "(M)")
(def-pprint-tabular-test pprint-tabular.4 ('(|M|) t) "(M)")
(def-pprint-tabular-test pprint-tabular.5 ('(|M|) nil) "M")

(def-pprint-tabular-test pprint-tabular.6 ('(|M| |M|)) "(M               M)")
(def-pprint-tabular-test pprint-tabular.7 ('(|M| |M|) t nil 1) "(M M)")
(def-pprint-tabular-test pprint-tabular.8 ('(|M| |M|) t t 3) "(M  M)")
(def-pprint-tabular-test pprint-tabular.9 ('(|M| |M|) t nil 4) "(M   M)")
(def-pprint-tabular-test pprint-tabular.10 ('(|MM| |MM|) t nil 4) "(MM  MM)")
(def-pprint-tabular-test pprint-tabular.11 ('(|MM| |MM|) t nil 5) "(MM   MM)")
(def-pprint-tabular-test pprint-tabular.12 ('(|M| |MM|) t nil 5)  "(M    MM)")

(def-pprint-tabular-test pprint-tabular.13 ((let ((x (list '|A|))) (list x x)) t nil 1)
  "(#1=(A) #1#)" :circle t)

(def-pprint-tabular-test pprint-tabular.14 ('(|M| |M|) t t 4) "(M   M)")

(def-pprint-tabular-test pprint-tabular.15 ('(1 2 3 4) t t 1) "(1 2 3 4)")
(def-pprint-tabular-test pprint-tabular.16 ('(10 20 30 40) t t 1) "(10 20 30 40)")
(def-pprint-tabular-test pprint-tabular.17 ('(10 200 3000 40000) t t 1) "(10 200 3000 40000)")
(def-pprint-tabular-test pprint-tabular.18 ('(10 20 30 40) t t 2) "(10  20  30  40)")
(def-pprint-tabular-test pprint-tabular.19 ('(10 200 3000 40000) t t 2) "(10  200 3000  40000)")

(def-pprint-tabular-test pprint-tabular.20 ('(1 2 3) t nil 1)
  "     (1 2 3)"
  :pre (write "     " :stream s :escape nil))

(def-pprint-tabular-test pprint-tabular.21 ('(1 2 3) t nil 1)
  "     (1
      2
      3)"
  :pre (write "     " :stream s :escape nil) :margin 9)


(def-pprint-tabular-test pprint-tabular.22 ('(1 2 3) t nil 1)
  "     (1 2
      3)"
  :pre (write "     " :stream s :escape nil) :margin 10)

;;; Takes T, NIL as stream designators

(deftest pprint-tabular.23
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil)
	 (*print-right-margin* 100))
     (with-output-to-string
       (os)
       (with-input-from-string
	(is "")
	(with-open-stream (*terminal-io* (make-two-way-stream is os))
			  (pprint-tabular t '(1 2 3) t nil 1))))))
  "(1 2 3)")

(deftest pprint-tabular.24
  (my-with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100))
     (with-output-to-string (*standard-output*) (pprint-tabular nil '(1 2 3) t nil 1))))
  "(1 2 3)")

;;; FIXME: add test for colon-p argument of NIL

;;; Test that pprint-tabular returns NIL

(deftest pprint-tabular.return-values.1
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*package* (find-package :cl-test)))
     (with-open-stream (s (make-broadcast-stream))
		       (pprint-tabular s '(a b)))))
  nil)

(deftest pprint-tabular.return-values.2
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*package* (find-package :cl-test)))
     (with-open-stream (s (make-broadcast-stream))
		       (pprint-tabular s 10 nil nil 100))))
  nil)

;;; Error tests

(deftest pprint-tabular.error.1
  (signals-error (pprint-tabular) program-error)
  t)

(deftest pprint-tabular.error.2
  (signals-error (pprint-tabular *standard-output*) program-error)
  t)

(deftest pprint-tabular.error.3
  (signals-error (pprint-tabular *standard-output* nil t nil 1 nil) program-error)
  t)

(deftest pprint-tabular.error.4
  (signals-error (pprint-tabular *standard-output* '(a b c) t t 1 nil) program-error)
  t)
