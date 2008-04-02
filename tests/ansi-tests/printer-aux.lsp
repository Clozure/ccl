;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 23 06:20:00 2004
;;;; Contains: Auxiliary functions and macros for printer tests

(in-package :cl-test)

(eval-when (:compile-toplevel :load-toplevel :execute) (compile-and-load "random-aux.lsp"))

(defmacro def-print-test (name form result &rest bindings)
  `(deftest ,name
     (if (equalpt
	  (my-with-standard-io-syntax
	   (let ((*print-readably* nil))
	     (let ,bindings
	       (with-output-to-string (*standard-output*) (prin1 ,form)))))
	  ,result)
	 t
       ,result)
     t))

(defmacro def-pprint-test
  (name form expected-value
	&key
	(margin 100)
	(miser nil)
	(circle nil)
	(len nil)
	(pretty t)
	(escape nil)
	(readably nil)
	(package (find-package "CL-TEST")))
  `(deftest ,name
     (with-standard-io-syntax
      (let ((*print-pretty* ,pretty)
	    (*print-escape* ,escape)
	    (*print-readably* ,readably)
	    (*print-right-margin* ,margin)
	    (*package* ,package)
	    (*print-length* ,len)
	    (*print-miser-width* ,miser)
	    (*print-circle* ,circle))
	,form))
     ,expected-value))

(defmacro def-ppblock-test (name form expected-value &rest key-args)
  `(def-pprint-test ,name
     (with-output-to-string
       (*standard-output*)
       (pprint-logical-block (*standard-output* nil) ,form))
     ,expected-value
     ,@key-args))

;;; Function to test readable of printed forms, under random settings
;;; of various printer control variables.
;;;
;;; Return NIL if obj printed and read properly, or a list containing
;;; the object and the printer variable bindings otherwise.  They key
;;; argument TEST is used to compared the reread object and obj.

(defvar *random-read-check-debug* nil
  "When set to true, RANDOMLY-CHECK-READABILITY will dump out parameter
   settings before trying a test.  This is intended for cases where the
   error that occurs is fatal.")

(defun randomly-check-readability (obj &key
				       (can-fail nil)
				       (test #'equal)
				       (readable t)
				       (circle nil circle-p)
				       (escape nil escape-p)
				       (gensym nil gensym-p)
				       (debug *random-read-check-debug*))
  (declare (type function test))
  ;; Generate random printer-control values
  (my-with-standard-io-syntax
   (let ((*print-array* (coin))
	 (*print-base* (+ 2 (random 34)))
	 (*print-radix* (coin))
	 (*print-case* (random-from-seq #(:upcase :downcase :capitalize)))
	 (*print-circle* (if circle-p circle (coin)))
	 (*print-escape* (if escape-p escape (coin)))
	 (*print-gensym* (if gensym-p gensym (coin)))
	 (*print-level* (random 50))
	 (*print-length* (if readable (random 50) nil))
	 (*print-lines* (if readable (random 50) nil))
	 (*print-miser-width* (and (coin) (random 100)))
	 (*print-pretty* (coin))
	 (*print-right-margin* (and (coin) (random 100)))
	 (*print-readably* readable)
	 (*read-default-float-format* (rcase (1 'short-float) (1 'single-float)
					     (1 'double-float) (1 'long-float)
					     (1 *read-default-float-format*)))
	 (*readtable* (copy-readtable))
	 (readcase (random-from-seq #(:upcase :downcase :preserve :invert)))
	 )
     (flet ((%params ()
		     (list (list '*print-readably* *print-readably*)
			   (list '*print-array* *print-array*)
			   (list '*print-base* *print-base*)
			   (list '*print-radix* *print-radix*)
			   (list '*print-case* *print-case*)
			   (list '*print-circle* *print-circle*)
			   (list '*print-escape* *print-escape*)
			   (list '*print-gensym* *print-gensym*)
			   (list '*print-level* *print-level*)
			   (list '*print-length* *print-length*)
			   (list '*print-lines* *print-lines*)
			   (list '*print-miser-width* *print-miser-width*)
			   (list '*print-pretty* *print-pretty*)
			   (list '*print-right-margin* *print-right-margin*)
			   (list '*read-default-float-format* *read-default-float-format*)
			   (list 'readtable-case readcase))))
       (when debug
	 (let ((params (%params)))
	   (with-standard-io-syntax (format *debug-io* "~%~A~%" params)))
	 (finish-output *debug-io*))
       
       (setf (readtable-case *readtable*) readcase)
       (let* ((str (handler-case
		    (with-output-to-string (s) (write obj :stream s))
		    (print-not-readable
		     ()
		     (if can-fail
			 (return-from randomly-check-readability nil)
		       ":print-not-readable-error"))))
	      (obj2 (let ((*read-base* *print-base*))
		      (handler-case
		       (let ((*readtable* (if *print-readably*
					      (copy-readtable nil)
					    *readtable*)))
			 (read-from-string str))
		       (reader-error () :reader-error)
		       (end-of-file () :end-of-file)
		       (stream-error () :stream-error)
		       (file-error () :file-error)
		       ))))
	 (unless (funcall test obj obj2)
	   (list
	    (list* obj str obj2 (%params)
		   ))))))))

(defun parse-escaped-string (string)
  "Parse a string into a list of either characters (representing
   themselves unescaped) or lists (<char> :escape) (representing
   escaped characters.)"
  (assert (stringp string) () "Not a string: ~A" string)
  (let ((result nil)
	(len (length string))
	(index 0))
    (prog
     ()
     normal ; parsing in normal mode
     (when (= index len) (return))
     (let ((c (elt string index)))
       (cond ((eql c #\\)
	      (assert (< (incf index) len)
		      ()
		      "End of string after \\")
	      (push `(,(elt string index) :escaped) result)
	      (incf index)
	      (go normal))
	     ((eql c #\|)
	      (incf index)
	      (go multiple-escaped))
	     (t (push c result)
		(incf index)
		(go normal))))

     multiple-escaped   ; parsing inside |s
     (assert (< index len) () "End of string inside |")
     (let ((c (elt string index)))
       (cond ((eq c #\|)
	      (incf index)
	      (go normal))
	     (t
	      (push `(,c :escaped) result)
	      (incf index)
	      (go multiple-escaped)))))
    (nreverse result)))

(defun escaped-equal (list1 list2)
  "Determine that everything escaped in list1 is also escaped
   in list2, and that the characters are also the same."
  (and (= (length list1) (length list2))
       (loop for e1 in list1
	     for e2 in list2
	     for is-escaped1 = (and (consp e1) (eq (cadr e1) :escaped))
	     for is-escaped2 = (and (consp e2) (eq (cadr e2) :escaped))
	     for c1 = (if is-escaped1 (car e1) e1)
	     for c2 = (if is-escaped2 (car e2) e2)
	     always
	     (and (if is-escaped1 is-escaped2 t)
		  (char= c1 c2)))))

(defun similar-uninterned-symbols (s1 s2)
  (and (symbolp s1)
       (symbolp s2)
       (null (symbol-package s1))
       (null (symbol-package s2))
       (string= (symbol-name s1)
		(symbol-name s2))))

(defun make-random-cons-tree (size)
  (if (<= size 1)
      (rcase
       (5 nil)
       (1 (random 1000))
       (1 (random 1000.0))
       (2 (random-from-seq #(a b c d e f g |1| |2| |.|))))
    (let ((s1 (1+ (random (1- size)))))
      (cons (make-random-cons-tree s1)
	    (make-random-cons-tree (- size s1))))))

(defun make-random-vector (size)
  (if (> size 1)
      (let* ((nelems (min (1- size) (1+ (random (max 2 (floor size 4))))))
	     (sizes (mapcar #'1+ (random-partition* (- size nelems 1) nelems))))
	(make-array nelems :initial-contents (mapcar #'make-random-vector sizes)))
    (rcase
     (1 (random-from-seq #(a b c d e f g)))
     (1 (- (random 2001) 1000))
     (1 (random 1000.0))
     )))

;;; Random printing test for WRITE and related functions

(defun funcall-with-print-bindings
  (fun &key
       ((:array *print-array*)                     *print-array*)
       ((:base *print-base*)                       *print-base*)
       ((:case *print-case*)                       *print-case*)
       ((:circle *print-circle*)                   *print-circle*)
       ((:escape *print-escape*)                   *print-escape*)
       ((:gensym *print-gensym*)                   *print-gensym*)
       ((:length *print-length*)                   *print-length*)
       ((:level *print-level*)                     *print-level*)
       ((:lines *print-lines*)                     *print-lines*)
       ((:miser-width *print-miser-width*)         *print-miser-width*)
       ((:pprint-dispatch *print-pprint-dispatch*) *print-pprint-dispatch*)
       ((:pretty *print-pretty*)                   *print-pretty*)
       ((:radix *print-radix*)                     *print-radix*)
       ((:readably *print-readably*)               *print-readably*)
       ((:right-margin *print-right-margin*)       *print-right-margin*)
       ((:stream *standard-output*)                *standard-output*))
  (funcall fun))
  
(defun output-test
  (obj &key
       (fun #'write)
       ((:array *print-array*)                     *print-array*)
       ((:base *print-base*)                       *print-base*)
       ((:case *print-case*)                       *print-case*)
       ((:circle *print-circle*)                   *print-circle*)
       ((:escape *print-escape*)                   *print-escape*)
       ((:gensym *print-gensym*)                   *print-gensym*)
       ((:length *print-length*)                   *print-length*)
       ((:level *print-level*)                     *print-level*)
       ((:lines *print-lines*)                     *print-lines*)
       ((:miser-width *print-miser-width*)         *print-miser-width*)
       ((:pprint-dispatch *print-pprint-dispatch*) *print-pprint-dispatch*)
       ((:pretty *print-pretty*)                   *print-pretty*)
       ((:radix *print-radix*)                     *print-radix*)
       ((:readably *print-readably*)               *print-readably*)
       ((:right-margin *print-right-margin*)       *print-right-margin*)
       ((:stream *standard-output*)                *standard-output*))
  (let ((results (multiple-value-list (funcall fun obj))))
    (assert (= (length results) 1))
    (assert (eql (car results) obj))
    obj))

(defun make-random-key-param (name)
  (rcase (1 nil)
	 (1 `(,name nil))
	 (1 `(,name t))))

(defun make-random-key-integer-or-nil-param (name bound)
  (rcase (1 nil)
	 (1 `(,name nil))
	 (1 `(,name ,(random bound)))))

(defun make-random-write-args ()
  (let* ((arg-lists `(,@(mapcar #'make-random-key-param
					    '(:array :circle :escape :gensym :pretty :radix :readably))
				    ,(rcase (1 nil)
					    (1 `(:base ,(+ 2 (random 35)))))
				    ,(and (coin)
					  `(:case ,(random-from-seq #(:upcase :downcase :capitalize))))
				    ,@(mapcar #'make-random-key-integer-or-nil-param
					      '(:length :level :lines :miser-width :right-margin)
					      '(100 20 50 200 200)))))
		(reduce #'append (random-permute arg-lists) :from-end t)))

(defun filter-unreadable-forms (string)
  "Find #<...> strings and replace with #<>."
  (let ((len (length string))
	(pos 0))
    (loop while (< pos len)
	  do (let ((next (search "#<" string :start2 pos)))
	       (unless next (return string))
	       (let ((end (position #\> string :start next)))
		 (unless end (return string))
		 (setq string
		       (concatenate 'string
				    (subseq string 0 next)
				    "#<>"
				    (subseq string (1+ end)))
		       pos (+ next 3)
		       len (+ len (- next end) 3)))))))
		       

(defmacro def-random-write-test-fun (name write-args test-fn
					  &key
					  (prefix "")
					  (suffix ""))
  `(defun ,name (n &key (size 10))
     (loop
      for args = (make-random-write-args)
      for package = (find-package (random-from-seq #("CL-TEST" "CL-USER" "KEYWORD")))
      for obj = (let ((*random-readable* t))
		  (declare (special *random-readable*))
		  (random-thing (random size)))
      for s1 = (let ((*package* package))
		 (with-output-to-string (s) (apply #'write obj :stream s ,@write-args args)))
      for s2 = (let ((*package* package))
		 (with-output-to-string
		   (*standard-output*)
		   (apply #'output-test obj :fun ,test-fn args)))
      repeat n
      ;; We filter the contents of #<...> forms since they may change with time
      ;; if they contain object addresses.
      unless (string= (filter-unreadable-forms (concatenate 'string ,prefix s1 ,suffix))
		      (filter-unreadable-forms s2))
      collect (list obj s1 s2 args))))

(def-random-write-test-fun random-write-test nil #'write)
(def-random-write-test-fun random-prin1-test (:escape t) #'prin1)
(def-random-write-test-fun random-princ-test (:escape nil :readably nil) #'princ)
(def-random-write-test-fun random-print-test (:escape t) #'print :prefix (string #\Newline) :suffix " ")
(def-random-write-test-fun random-pprint-test (:escape t :pretty t)
  #'(lambda (obj) (assert (null (multiple-value-list (pprint obj)))) obj)
  :prefix (string #\Newline))

(defmacro def-random-write-to-string-test-fun (name write-args test-fn
					  &key
					  (prefix "")
					  (suffix ""))
  `(defun ,name (n)
     (loop
      for args = (make-random-write-args)
      for package = (find-package (random-from-seq #("CL-TEST" "CL-USER" "KEYWORD")))
      for obj = (let ((*random-readable* t))
		  (declare (special *random-readable*))
		  (random-thing (random 10)))
      for s1 = (let ((*package* package))
		 (with-output-to-string (s) (apply #'write obj :stream s ,@write-args args)))
      for s2 = (let ((*package* package))
		 (apply ,test-fn obj args))
      repeat n
      unless (string= (filter-unreadable-forms (concatenate 'string ,prefix s1 ,suffix))
		      (filter-unreadable-forms s2))
      collect (list obj s1 s2))))

(def-random-write-to-string-test-fun random-write-to-string-test nil #'write-to-string)
(def-random-write-to-string-test-fun random-prin1-to-string-test (:escape t)
  #'(lambda (obj &rest args)
      (apply #'funcall-with-print-bindings #'(lambda () (prin1-to-string obj)) args)))
(def-random-write-to-string-test-fun random-princ-to-string-test (:escape nil :readably nil)
  #'(lambda (obj &rest args)
      (apply #'funcall-with-print-bindings #'(lambda () (princ-to-string obj)) args)))

;;; Routines for testing floating point printing

(defun decode-fixed-decimal-string (s)
  "Return a rational equal to the number represented by a decimal floating
   (without exponent).  Trim off leading/trailing spaces."

  (setq s (string-trim " " s))
  (assert (> (length s) 0))
  (let (neg)
    (when (eql (elt s 0) #\-)
      (setq s (subseq s 1))
      (setq neg t))
    ;; Check it's of the form {digits}.{digits}
    (let ((dot-pos (position #\. s)))
      (assert dot-pos)
      (let ((prefix (subseq s 0 dot-pos))
	    (suffix (subseq s (1+ dot-pos))))
	(assert (every #'digit-char-p prefix))
	(assert (every #'digit-char-p suffix))
	(let* ((prefix-len (length prefix))
	       (prefix-integer (if (eql prefix-len 0)
				   0
				 (parse-integer prefix)))
	       (suffix-len (length suffix))
	       (suffix-integer (if (eql suffix-len 0)
				   0
				 (parse-integer suffix)))
	       (magnitude (+ prefix-integer
			     (* suffix-integer (expt 1/10 suffix-len)))))
	  (if neg (- magnitude) magnitude))))))


;;; Macro to define both FORMAT and FORMATTER tests

(defmacro def-format-test (name string args expected-output &optional (num-left 0))
  (assert (symbolp name))
  (let* ((s (symbol-name name))
	 (expected-prefix (string 'format.))
	 (expected-prefix-length (length expected-prefix)))
    (assert (>= (length s) expected-prefix-length))
    (assert (string-equal (subseq s 0 expected-prefix-length)
			  expected-prefix))
    (let* ((formatter-test-name-string
	    (concatenate 'string (string 'formatter.)
			 (subseq s expected-prefix-length)))
	   (formatter-test-name (intern formatter-test-name-string
					(symbol-package name)))
	   (formatter-form (if (stringp string)
			       `(formatter ,string)
			     (list 'formatter (eval string)))))
      `(progn
	 (deftest ,name
	   (with-standard-io-syntax
	    (let ((*print-readably* nil)
		  (*package* (symbol-package 'ABC)))
	      (format nil ,string ,@args)))
	   ,expected-output)
	 (deftest ,formatter-test-name
	   (let ((fn ,formatter-form)
		 (args (list ,@args)))
	     (with-standard-io-syntax
	      (let ((*print-readably* nil)
		    (*package* (symbol-package 'ABC)))
		(with-output-to-string
		  (stream)
		  (let ((tail (apply fn stream args)))
		    ;; FIXME -- Need to check that TAIL really is a tail of ARGS
		    (assert (= (length tail) ,num-left) (tail) "Tail is ~A, length should be ~A"
			    tail ,num-left)
		  )))))
	   ,expected-output)))))

;;; Macro used for an idiom in testing FORMATTER calls

(defmacro formatter-call-to-string (fn &body args)
  (let ((stream (gensym "S")))
    `(with-output-to-string
       (,stream)
       (assert (equal (funcall ,fn ,stream ,@args 'a) '(a))))))
