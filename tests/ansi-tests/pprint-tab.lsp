;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jul 10 14:08:08 2004
;;;; Contains: Tests of PPRINT-TAB

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; No effect in a non-pprint stream

(def-pprint-test pprint-tab.non-pretty.1
  (with-output-to-string
    (*standard-output*)
    (write "A")
    (pprint-tab :line 10 3)
    (write "B"))
  "AB")

(def-pprint-test pprint-tab.non-pretty.2
  (with-output-to-string
    (*standard-output*)
    (write "A")
    (pprint-tab :section 10 3)
    (write "B"))
  "AB")

(def-pprint-test pprint-tab.non-pretty.3
  (with-output-to-string
    (*standard-output*)
    (write "A")
    (pprint-tab :line-relative 10 3)
    (write "B"))
  "AB")

(def-pprint-test pprint-tab.non-pretty.4
  (with-output-to-string
    (*standard-output*)
    (write "A")
    (pprint-tab :section-relative 10 3)
    (write "B"))
  "AB")

(def-ppblock-test pprint-tab.non-pretty.5
  (progn (write "A") (pprint-tab :line 10 3) (write "B"))
  "AB"
  :pretty nil)

(def-ppblock-test pprint-tab.non-pretty.6
  (progn (write "A") (pprint-tab :section 10 3) (write "B"))
  "AB"
  :pretty nil)

(def-ppblock-test pprint-tab.non-pretty.7
  (progn (write "A") (pprint-tab :line-relative 10 3) (write "B"))
  "AB"
  :pretty nil)

(def-ppblock-test pprint-tab.non-pretty.8
  (progn (write "A") (pprint-tab :section-relative 10 3) (write "B"))
  "AB"
  :pretty nil)


;;; nil designates *standard-output*

(def-ppblock-test pprint-tab.nil.1
  (progn (write "A")
	 (pprint-tab :line 10 1 nil)
	 (write "B"))
  "A         B")

;;; t designates *terminal-io*

(def-pprint-test pprint-tab.t.1
  (with-output-to-string
    (os)
    (with-input-from-string
     (is "")
     (with-open-stream
      (*terminal-io* (make-two-way-stream is os))
      (pprint-logical-block
       (*terminal-io* nil)
       (write "A" :stream t)
       (pprint-tab :line 10 1 t)
       (write "B" :stream t)))))
  "A         B")

;;; Now test actual tabbing behavior

;;; NOTE
;;; I am assuming that when colnum <= current column,
;;; and the current column == colnum + k * colinc for some positive integer k,
;;; then pprint-tab :line will tab at least 1 space.

(def-pprint-test pprint-tab.line.1
  (loop
   for offset = (random 100)
   for colnum = (random 100)
   for colinc = (min (random 50) (random 50))
   for s = (with-output-to-string
	     (*standard-output*)
	     (pprint-logical-block
	      (*standard-output* nil)
	      (dotimes (i offset) (write #\Space))
	      (pprint-tab :line colnum colinc)
	      (write #\A)))
   for expected-col = (cond ((< offset colnum) colnum)
			    ((= colinc 0) offset)
			    ((= offset colnum) (+ offset colinc))
			    (t (let ((k (mod (- colnum offset) colinc)))
				 (if (= k 0)
				     (+ offset colinc)
				   (+ offset k)))))
   repeat 200
   nconc
   (unless (string= s (concatenate
			 'string
			 (make-string expected-col :initial-element #\Space)
			 "A"))
       (list (list offset colnum colinc expected-col (count #\Space s) s))))
  nil
  :margin 1000)

(def-pprint-test pprint-tab.section.1
  (loop
   for prefix-length = (random 50)
   for offset = (random 50)
   for colnum = (random 50)
   for colinc = (min (random 50) (random 50))
   for s = (with-output-to-string
	     (*standard-output*)
	     (pprint-logical-block
	      (*standard-output* nil :prefix (make-string prefix-length
							  :initial-element #\Space))
	      (dotimes (i offset) (write #\Space))
	      (pprint-tab :section colnum colinc)
	      (write #\A)))
   for expected-col = (+ prefix-length
			 (cond ((< offset colnum) colnum)
			       ((= colinc 0) offset)
			       ((= offset colnum) (+ offset colinc))
			       (t (let ((k (mod (- colnum offset) colinc)))
				    (if (= k 0)
					(+ offset colinc)
				      (+ offset k))))))
   repeat 200
   nconc
   (unless (string= s (concatenate
			 'string
			 (make-string expected-col :initial-element #\Space)
			 "A"))
       (list (list offset colnum colinc expected-col (count #\Space s) s))))
  nil
  :margin 1000)

(def-pprint-test pprint-tab.line-relative.1
  (loop
   for offset = (random 100)
   for colrel = (random 100)
   for colinc = (1+ (min (random 50) (random 50)))
   for extra = (mod (- (+ offset colrel)) colinc)
   for s = (with-output-to-string
	     (*standard-output*)
	     (pprint-logical-block
	      (*standard-output* nil)
	      (dotimes (i offset) (write #\Space))
	      (pprint-tab :line-relative colrel colinc)
	      (write #\A)))
   for expected-col = (+ offset colrel extra)
   repeat 200
   nconc
   (unless (string= s (concatenate
			 'string
			 (make-string expected-col :initial-element #\Space)
			 "A"))
       (list (list offset colrel colinc expected-col (count #\Space s) s))))
  nil
  :margin 1000)

(def-pprint-test pprint-tab.section-relative.1
  (loop
   for prefix-length = (random 50)
   for offset = (random 50)
   for colrel = (random 50)
   for colinc = (1+ (min (random 50) (random 50)))
   for extra = (mod (- (+ offset colrel)) colinc)
   for s = (with-output-to-string
	     (*standard-output*)
	     (pprint-logical-block
	      (*standard-output* nil :prefix (make-string prefix-length
							  :initial-element #\Space))
	      (dotimes (i offset) (write #\Space))
	      (pprint-tab :section-relative colrel colinc)
	      (write #\A)))
   for expected-col = (+ prefix-length offset colrel extra)

   repeat 200
   nconc
   (unless (string= s (concatenate
			 'string
			 (make-string expected-col :initial-element #\Space)
			 "A"))
       (list (list prefix-length offset colrel colinc extra expected-col (count #\Space s) s))))
  nil
  :margin 1000)

;;; Error cases

(deftest pprint-tab.error.1
  (signals-error (pprint-tab) program-error)
  t)

(deftest pprint-tab.error.2
  (signals-error (pprint-tab :line) program-error)
  t)

(deftest pprint-tab.error.3
  (signals-error (pprint-tab :line 1) program-error)
  t)

(deftest pprint-tab.error.4
  (signals-error (pprint-tab :line 1 1 nil nil) program-error)
  t)

(deftest pprint-tab.error.5
  (loop for x in *mini-universe*
	unless (or (member x '(:line :section :line-relative :section-relative))
		   (eval `(signals-error (pprint-tab ',x 1 1) error)))
	collect x)
  nil)

(deftest pprint-tab.error.5-unsafe
  (loop for x in *mini-universe*
	unless (or (member x '(:line :section :line-relative :section-relative))
		   (eval `(signals-error (locally (declare (optimize (safety 0))) (pprint-tab ',x 1 1)) error)))
	collect x)
  nil)
