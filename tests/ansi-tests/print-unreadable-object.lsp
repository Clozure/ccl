;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jul 12 06:06:01 2004
;;;; Contains: Tests of PRINT-UNREADABLE-OBJECT

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(def-pprint-test print-unreadable-object.1
  (loop
   for x in *mini-universe*
   for return-vals = nil
   for s = (with-output-to-string
	     (s)
	     (setq return-vals
		   (multiple-value-list (print-unreadable-object (x s)))))
   unless (and (equal return-vals '(nil))
	       (equal s "#<>"))
   collect (list x return-vals s))
  nil)

(def-pprint-test print-unreadable-object.2
  (loop
   for x in *mini-universe*
   for return-vals1 = nil
   for return-vals2 = nil
   for s1 = (with-output-to-string
	      (s)
	      (setq return-vals1
		    (multiple-value-list (print-unreadable-object
					  (x s :type t)))))
   for s2 = (with-output-to-string
	      (s)
	      (setq return-vals2
		    (multiple-value-list (print-unreadable-object
					  (x s :type t)
					  (write-char #\X s)))))
   unless (and (equal return-vals1 '(nil))
	       (equal return-vals2 '(nil))
	       (string= s1 "#<" :end1 2)
	       (string= s1 s2 :end1 (- (length s1) 1)
			:end2 (- (length s2) 2))
	       (string= s2 " X>" :start1 (- (length s2) 3)))
   collect (list x return-vals1 return-vals2 s1 s2))
  nil)

(def-pprint-test print-unreadable-object.3
  (loop
   for x in *mini-universe*
   for return-vals1 = nil
   for return-vals2 = nil
   for s1 = (with-output-to-string
	      (s)
	      (setq return-vals1
		    (multiple-value-list (print-unreadable-object
					  (x s :identity t)
					  (write "FOO" :stream s)
					  (values 1 2 3 4 5) ;; test if this is ignored
					  ))))
   for s2 = (with-output-to-string
	      (s)
	      (setq return-vals2
		    (multiple-value-list (print-unreadable-object
					  (x s :identity t)
					  ))))
   unless (and (equal return-vals1 '(nil))
	       (equal return-vals2 '(nil))
	       (string= s1 "#<FOO " :end1 6)
	       (string= s2 "#< " :end1 3)
	       (eql (char s1 (1- (length s1))) #\>)
	       (eql (char s2 (1- (length s2))) #\>)
	       (string= s1 s2 :start2 3 :start1 6))
   collect (list x return-vals1 return-vals2 s1 s2))
  nil)

(def-pprint-test print-unreadable-object.4
  (loop
   for x in *mini-universe*
   for return-vals = nil
   for s = (with-output-to-string
	     (s)
	     (setq return-vals
		   (multiple-value-list (print-unreadable-object
					 (x s :identity t :type t)
					 (write "FOO" :stream s)
					 (values) ;; test if this is ignored
					 ))))
   unless (and (equal return-vals '(nil))
	       (string= s "#<" :end1 2)
	       (eql (char s (1- (length s))) #\>)
	       (>= (count #\Space s) 2))
   collect (list x return-vals s))
  nil)

;;; TODO Tests that the :identity and :type arguments are evaluated
;;; TODO Tests where :type, :identity are provided, but are nil
;;; TODO Test that the type/identity parts of the output are the same
;;;       for the both-printed case as they are in the only-one printed case,
;;;       and that only a single space occurs between them if FORMS is omitted.

;;; Error cases

(deftest print-unreadable-object.error.1
  (with-standard-io-syntax
   (let ((*print-readably* t))
     (loop for x in *mini-universe*
	   for form = `(with-output-to-string
			 (*standard-output*)
			 (assert (signals-error
				  (print-unreadable-object (',x *standard-output*))
				  print-not-readable)))
	   unless (equal (eval form) "")
	   collect x)))
  nil)

;;; Stream designators

(deftest print-unreadable-object.t.1
  (with-output-to-string
    (os)
    (with-input-from-string
     (is "")
     (with-open-stream
      (*terminal-io* (make-two-way-stream is os))
      (let ((*print-readably* nil))
	(assert
	 (equal (multiple-value-list (print-unreadable-object (1 t)))
		'(nil)))))))
  "#<>")

(deftest print-unreadable-object.nil.1
  (with-output-to-string
    (*standard-output*)
    (let ((*print-readably* nil))
      (assert
       (equal (multiple-value-list (print-unreadable-object (1 nil)))
	      '(nil)))))
  "#<>")


