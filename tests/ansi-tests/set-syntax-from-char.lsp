;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan 29 06:37:18 2005
;;;; Contains: Tests of SET-SYNTAX-FROM-CHAR

(in-package :cl-test)

(compile-and-load "reader-aux.lsp")

(defmacro def-set-syntax-from-char-test (name form &body expected-values)
  `(deftest ,name
     (with-standard-io-syntax
      (let ((*readtable* (copy-readtable nil)))
	(setf (readtable-case *readtable*) :preserve)
	,form))
     ,@expected-values))

;;; Test that constituent traits are not altered when a constituent character
;;; syntax type is set

(defmacro def-set-syntax-from-char-trait-test (c test-form expected-value)
  (setq c (typecase c
	    (character c)
	    ((or string symbol) (name-char (string c)))
	    (t nil)))
  (when c
    ;; (format t "~A ~A~%" c (char-name c))
    `(def-set-syntax-from-char-test
       ,(intern (concatenate 'string "SET-SYNTAX-FROM-CHAR-TRAIT-X-" (or (char-name c)
									 (string c)))
		:cl-test)
       (let ((c ,c))
	 (values
	  (set-syntax-from-char c #\X)
	  ,test-form))
       t ,expected-value)))

(defmacro def-set-syntax-from-char-alphabetic-trait-test (c)
  `(def-set-syntax-from-char-trait-test ,c
     (let* ((*package* (find-package "CL-TEST"))
	    (sym (read-from-string (string c))))
       (list (let ((sym2 (find-symbol (string c))))
	       (or (eqt sym sym2)
		   (list sym sym2)))
	     (or (equalt (symbol-name sym) (string c))
		 (list (symbol-name sym) (string c)))))
     (t t)))

(loop for c across "\\|!\"#$%&'()*,;<=>?@[]^_`~{}+-/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      do (eval `(def-set-syntax-from-char-alphabetic-trait-test ,c)))

;;; The invalid constituent character trait of invalid and whitespace characters
;;; is exposed when they are turned into constituent characters

(defmacro def-set-syntax-from-char-invalid-trait-test (c)
  `(def-set-syntax-from-char-trait-test ,c
     (handler-case
      (let* ((*package* (find-package "CL-TEST"))
	     (sym (read-from-string (concatenate 'string (string c) "Z"))))
	sym)
      (reader-error (c) (declare (ignore c)) :good))
     :good))

(loop for name in '("Backspace" "Tab" "Newline" "Linefeed" "Page" "Return" "Space" "Rubout")
      do (eval `(def-set-syntax-from-char-invalid-trait-test ,name)))

;;; Turning characters into single escape characters

(deftest set-syntax-from-char.single-escape.1
  (loop for c across +standard-chars+
	nconc
	(with-standard-io-syntax
	 (let ((*readtable* (copy-readtable nil))
	       (*package* (find-package "CL-TEST")))
	   (let ((results
		  (list
		   (set-syntax-from-char c #\\)
		   (read-from-string (concatenate 'string (list c #\Z))))))
	     (unless (equal results '(t |Z|))
	       (list (list c results)))))))
  nil)

(deftest set-syntax-from-char.single-escape.2
  (loop for c across +standard-chars+
	unless (eql c #\")
	nconc
	(with-standard-io-syntax
	 (let ((*readtable* (copy-readtable nil))
	       (*package* (find-package "CL-TEST")))
	   (let ((results
		  (list
		   (set-syntax-from-char c #\\)
		   (read-from-string (concatenate 'string
						  (list #\" c #\" #\"))))))
	     (unless (equal results '(t "\""))
	       (list (list c results)))))))
  nil)


(deftest set-syntax-from-char.multiple-escape
  (loop for c across +standard-chars+
	nconc
	(with-standard-io-syntax
	 (let ((*readtable* (copy-readtable nil))
	       (*package* (find-package "CL-TEST")))
	   (let ((results
		  (list
		   (set-syntax-from-char c #\|)
		   (handler-case
		    (read-from-string (concatenate 'string (list c #\Z c)))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list c #\z #\|)))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list #\| #\Z c)))
		    (error (c) c)))))
	     (unless (or (eql c #\Z) (eql c #\z) (equal results '(t |Z| |z| |Z|)))
	       (list (list c results)))))))
  nil)

(deftest set-syntax-from-char.semicolon
  (loop for c across +standard-chars+
	nconc
	(with-standard-io-syntax
	 (let ((*readtable* (copy-readtable nil))
	       (*package* (find-package "CL-TEST"))
	       (expected (if (eql c #\0) '1 '0))
	       (c2 (if (eql c #\0) #\1 #\0)))
	   (let ((results
		  (list
		   (set-syntax-from-char c #\;)
		   (handler-case
		    (read-from-string (concatenate 'string (list c2 c #\2)))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list c2 c #\2 #\Newline #\3)))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list c #\2 #\Newline c2)))
		    (error (c) c)))))
	     (unless (equal results (list t expected expected expected))
	       (list (list c results)))))))
  nil)

(deftest set-syntax-from-char.left-paren
  (loop for c across +standard-chars+
	unless (find c ")")
	nconc
	(with-standard-io-syntax
	 (let ((*readtable* (copy-readtable nil))
	       (*package* (find-package "CL-TEST"))
	       (expected (if (eql c #\0) '(1) '(0)))
	       (c2 (if (eql c #\0) #\1 #\0)))
	   (let ((results
		  (list
		   (set-syntax-from-char c #\()
		   (handler-case
		    (read-from-string (concatenate 'string (list c) ")"))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list c c2) ")2" (list #\Newline #\3)))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list c c2) ")"))
		    (error (c) c)))))
	     (unless (equal results (list t nil expected expected))
	       (list (list c results)))))))
  nil)

(deftest set-syntax-from-char.right-paren
  (loop for c across +standard-chars+
	nconc
	(with-standard-io-syntax
	 (let ((*readtable* (copy-readtable nil))
	       (*package* (find-package "CL-TEST")))
	   (let ((results
		  (list
		   (set-syntax-from-char c #\))
		   (handler-case
		    (read-from-string (string c) nil nil)
		    (reader-error (c) :good)
		    (error (c) c)))))
	     (unless (equal results '(t :good))
	       (list (list c results)))))))
  nil)

(deftest set-syntax-from-char.single-quote
  (loop for c across +standard-chars+
	nconc
	(with-standard-io-syntax
	 (let ((*readtable* (copy-readtable nil))
	       (*package* (find-package "CL-TEST"))
	       (expected (if (eql c #\0) ''1 ''0))
	       (c2 (if (eql c #\0) #\1 #\0)))
	   (let ((results
		  (list
		   (set-syntax-from-char c #\')
		   (handler-case
		    (read-from-string (concatenate 'string (list c c2)))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list c c2) " 2"))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list c c2) ")"))
		    (error (c) c)))))
	     (unless (equal results (list t expected expected expected))
	       (list (list c results)))))))
  nil)

;;; I do not test that setting syntax from #\" allows the character to be
;;; used as the terminator of a double quoted string.  It is not clear that
;;; the standard implies this.

(deftest set-syntax-from-char.double-quote
  (loop for c across +standard-chars+
	nconc
	(with-standard-io-syntax
	 (let ((*readtable* (copy-readtable nil))
	       (*package* (find-package "CL-TEST"))
	       (expected (if (eql c #\0) "1" "0"))
	       (c2 (if (eql c #\0) #\1 #\0)))
	   (let ((results
		  (list
		   (set-syntax-from-char c #\")
		   (handler-case
		    (read-from-string
		     (concatenate 'string (list c c2 c)))
		    (error (c) c))
		   (handler-case
		    (read-from-string
		     (concatenate 'string (list c c2 c #\2)))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list c c2 c) ")"))
		    (error (c) c)))))
	     (unless (equal results (list t expected expected expected))
	       (list (list c results)))))))
  nil)

(deftest set-syntax-from-char.backquote
  (loop for c across +standard-chars+
	unless (find c ",x")
	nconc
	(with-standard-io-syntax
	 (let* ((*readtable* (copy-readtable nil))
		(*package* (find-package "CL-TEST"))
		(c2 (if (eql c #\Space) #\Newline #\Space))
		(results
		 (list
		  (set-syntax-from-char c #\`)
		  (handler-case
		   (eval `(let ((x 0))
			    ,(read-from-string
			      (concatenate 'string (list c #\, #\x)))))
		   (error (c) c))
		  (handler-case
		   (eval `(let ((x 0))
			    ,(read-from-string
			      (concatenate 'string (list c #\, #\x c2)))))
		   (error (c) c))
		  (handler-case
		   (eval `(let ((x 0))
			    ,(read-from-string
			      (concatenate 'string (list c c2 #\, #\x c2)))))
		   (error (c) c)))))
	   (unless (equal results '(t 0 0 0))
	     (list (list c results))))))
  nil)

(deftest set-syntax-from-char.comma
  (loop for c across +standard-chars+
	unless (find c "`x")
	nconc
	(with-standard-io-syntax
	 (let* ((*readtable* (copy-readtable nil))
		(*package* (find-package "CL-TEST"))
		(c2 (if (eql c #\Space) #\Newline #\Space))
		(results
		 (list
		  (set-syntax-from-char c #\,)
		  (handler-case
		   (read-from-string (string c))
		   (reader-error (c) :good)
		   (error (c) c))
		  (handler-case
		   (eval `(let ((x 0))
			    ,(read-from-string
			      (concatenate 'string "`" (list c) "x"))))
		   (error (c) c)))))
	   (unless (equal results '(t :good 0))
	     (list (list c results))))))
  nil)
	       
;;; Tests of set-syntax-from-char on #\#

(deftest set-syntax-from-char.sharp.1
  (loop for c across +standard-chars+
	nconc
	(with-standard-io-syntax
	 (let* ((*readtable* (copy-readtable nil))
		(*package* (find-package "CL-TEST"))
		(results
		 (list
		  (set-syntax-from-char c #\#)
		  (if (not (eql c #\Space))
		      (handler-case
		       (read-from-string
			(concatenate 'string (list c #\Space)))
		       (reader-error () :good)
		       (error (c) c))
		    :good)
		  (if (not (find c "'X"))
		      (handler-case
		       (read-from-string
			(concatenate 'string (list c) "'X"))
		       (error (c) c))
		    '#'|X|)
		  (if (not (find c "(X)"))
		      (handler-case
		       (read-from-string
			(concatenate 'string (list c) "(X)"))
		       (error (c) c))
		    #(|X|))
		  (if (not (find c ")"))
		      (handler-case
		       (read-from-string
			(concatenate 'string (list c) ")"))
		       (reader-error (c) :good)
		       (error (c) c))
		    :good)
		  (if (not (find c "*"))
		      (handler-case
		       (read-from-string
			(concatenate 'string (list c #\*)))
		       (error (c) c))
		    #*)
		  (if (not (find c ":|"))
		      (handler-case
		       (let ((sym (read-from-string
				   (concatenate 'string (list c) ":||"))))
			 (and (symbolp sym)
			      (null (symbol-package sym))
			      (symbol-name sym)))
		       (error (c) c))
		    "")
		  (handler-case
		   (read-from-string
		    (concatenate 'string (list c #\<)))
		   (reader-error (c) :good)
		   (error (c) c))
		  (handler-case
		   (read-from-string
		    (concatenate 'string (list c #\\ #\X)))
		   (error (c) c))
		  (if (not (find c "1"))
		      (handler-case
		       (read-from-string
			(concatenate 'string (list c) "|1111|#1"))
		       (error (c) c))
		    1)
		  (if (not (find c "1"))
		      (handler-case
		       (read-from-string
			(concatenate 'string (list c) "|11#|111|#11|#1"))
		       (error (c) c))
		    1)
		  )))
	   (unless (equalp results '(t :good #'|X| #(|X|) :good #* "" :good #\X 1 1))
	     (list (list c results))))))
  nil)

(deftest set-syntax-from-char.sharp.2
  (loop for c across +standard-chars+
	nconc
	(with-standard-io-syntax
	 (let* ((*readtable* (copy-readtable nil))
		(*package* (find-package "CL-TEST"))
		(results
		 (list
		  (set-syntax-from-char c #\#)
		  (if (not (find c "+XC "))
		      (handler-case
		       (let ((*features* (cons ':X *features*)))
			 (read-from-string
			  (concatenate 'string (list c) "+X C")))
		       (error (c) c))
		    'c)
		  (if (not (find c "-(OR)"))
		      (handler-case
		       (read-from-string
			(concatenate 'string (list c) "-(OR)R"))
		       (error (c) c))
		    'r)
		  (if (not (find c ".1"))
		      (handler-case
		       (read-from-string
			(concatenate 'string (list c) ".1"))
		       (error (c) c))
		    1)
		  (if (not (find c "01aA"))
		      (handler-case
		       (list
			(read-from-string
			 (concatenate 'string (list c) "0a1"))
			(read-from-string
			 (concatenate 'string (list c) "0A1")))
		       (error (c) c))
		    '(#0a1 #0a1))
		  (if (not (find c "01bB"))
		      (handler-case
		       (list
			(read-from-string
			 (concatenate 'string (list c) "b101"))
			(read-from-string
			 (concatenate 'string (list c) "B011")))
		       (error (c) c))
		    '(5 3))
		  (if (not (find c "cC()12 "))
		      (handler-case
		       (list
			(read-from-string
			 (concatenate 'string (list c) "c(1 2)"))
			(read-from-string
			 (concatenate 'string (list c) "C(2 1)")))
		       (error (c) c))
		    '(#c(1 2) #c(2 1)))
		  (if (not (find c "oO0127"))
		      (handler-case
		       (list
			(read-from-string
			 (concatenate 'string (list c) "o172"))
			(read-from-string
			 (concatenate 'string (list c) "O7721")))
		       (error (c) c))
		    '(#o172 #o7721))
		  (if (not (find c "pP\""))
		      (handler-case
		       (list
			(read-from-string
			 (concatenate 'string (list c) "p\"\""))
			(read-from-string
			 (concatenate 'string (list c) "P\"\"")))
		       (error (c) c))
		    '(#p"" #p""))
		  (if (not (find c "rR0123"))
		      (handler-case
		       (list
			(read-from-string
			 (concatenate 'string (list c) "3r210"))
			(read-from-string
			 (concatenate 'string (list c) "3R1111")))
		       (error (c) c))
		    '(#3r210 #3r1111))
		  ;;; Add #s test here
		  (if (not (find c "xX04dF"))
		      (handler-case
		       (list
			(read-from-string
			 (concatenate 'string (list c) "x40Fd"))
			(read-from-string
			 (concatenate 'string (list c) "XFd04")))
		       (error (c) c))
		    '(#x40fd #xfd04))
		  )))
	   (unless (equalp results
			   '(t c r 1 (#0a1 #0a1) (5 3) (#c(1 2) #c(2 1))
			       (#o172 #o7721) (#p"" #p"") (#3r210 #3r1111)
			       (#x40fd #xfd04)))
	     (list (list c results)))
	   )))
  nil)
