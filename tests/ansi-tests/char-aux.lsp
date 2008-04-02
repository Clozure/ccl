;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct  5 20:15:55 2002
;;;; Contains: Auxiliary functions for character tests

(in-package :cl-test)

(defun is-ordered-by (seq fn)
  (declare (type function fn))
  (let ((n (length seq)))
    (loop for i from 0 below (1- n)
	  for e = (elt seq i)
	  always
	  (loop for j from (1+ i) below n
		always (funcall fn e (elt seq j))))))

(defun is-antisymmetrically-ordered-by (seq fn)
  (declare (type function fn))
  (and (is-ordered-by seq fn)
       (is-ordered-by (reverse seq) (complement fn))))

(defun is-case-insensitive (fn)
  (when (symbolp fn)
    (assert (fboundp fn))
    (setf fn (symbol-function fn)))
  (assert (typep fn 'function))
  (locally
   (declare (type function fn))
   (loop for c across +code-chars+
	 for c1 = (char-upcase c)
	 for c2 = (if (eql c c1) (char-downcase c) c1)
	 always
	 (loop for d across +code-chars+
	       for d1 = (char-upcase d)
	       for d2 = (if (eql d d1) (char-downcase d) d1)
	       always (equiv (funcall fn c d)
			     (funcall fn c2 d)
			     (funcall fn c d2)
			     (funcall fn c2 d2))))))

(defun equiv (&rest args)
  (declare (dynamic-extent args))
  (cond
   ((null args) t)
   ((car args)
    (loop for e in (cdr args) always e))
   (t (loop for e in (cdr args) never e))))

;;; From character.lsp
(defun char-type-error-check (fn)
  (when (symbolp fn)
    (assert (fboundp fn))
    (setf fn (symbol-function fn)))
  (assert (typep fn 'function))
  (locally
   (declare (type function fn))
   (loop for x in *universe*
	 always (or (characterp x)
		    ;; FIXME -- catch the type error and check that datum
		    ;; is eql to x (and that datum is not in the expected type)
		    (eqt (catch-type-error (funcall fn x)) 'type-error)))))

(defun standard-char.5.body ()
  (loop for i from 0 below (min 65536 char-code-limit)
	always (let ((c (code-char i)))
		 (not (and (typep c 'standard-char)
			   (not (standard-char-p c)))))))

(defun extended-char.3.body ()
  (loop for i from 0 below (min 65536 char-code-limit)
	always (let ((c (code-char i)))
		 (not (and (typep c 'base-char)
			   (typep c 'extended-char)
			   )))))

(defun character.1.body ()
  (loop for i from 0 below (min 65536 char-code-limit)
	always (let ((c (code-char i)))
		 (or (null c)
		     (let ((s (string c)))
		       (and
			(eqlt (character c) c)
			(eqlt (character s) c)
			(eqlt (character (make-symbol s)) c)))))))

(defun character.2.body ()
  (loop for x in *universe*
	when (not (or (characterp x)
		      (and (stringp x) (eqlt (length x) 1))
		      (and (symbolp x) (eqlt (length (symbol-name x)) 1))
		      (let ((c (catch-type-error (character x))))
			(or (eqlt c 'type-error)
			    (let ((s (catch-type-error (string x))))
			      (and (stringp s) (eqlt (my-aref s 0) c)))))))
	do (return x)))

(defun characterp.2.body ()
  (loop for i from 0 below (min 65536 char-code-limit)
	always (let ((c (code-char i)))
		 (or (null c) (characterp c)))))

(defun characterp.3.body ()
  (loop for x in *universe*
	always (let ((p (characterp x))
		     (q (typep x 'character)))
		 (if p (notnot q) (not q)))))

(defun alphanumericp.4.body ()
  (loop for x in *universe*
	always (or (not (characterp x))
		   (if (or (digit-char-p x) (alpha-char-p x))
		       (alphanumericp x)
		     ;; The hyperspec has an example that claims alphanumeric ==
		     ;;  digit-char-p or alpha-char-p, but the text seems to suggest
		     ;;  that there can be numeric characters for which digit-char-p
		     ;;  returns NIL.  Therefore, I've weakened the next line
		     ;; (not (alphanumericp x))
		     t
		     ))))

(defun alphanumericp.5.body ()
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always (or (not (characterp x))
		   (if (or (digit-char-p x) (alpha-char-p x))
		       (alphanumericp x)
		     ;; The hyperspec has an example that claims alphanumeric ==
		     ;;  digit-char-p or alpha-char-p, but the text seems to suggest
		     ;;  that there can be numeric characters for which digit-char-p
		     ;;  returns NIL.  Therefore, I've weakened the next line
		     ;; (not (alphanumericp x))
		     t		     
		     ))))

(defun digit-char.1.body.old ()
  (loop for r from 2 to 36 always
       (loop for i from 0 to 36
	  always (let* ((c (digit-char i r))
			(result
			 (if (>= i r) (null c)
			     (eqlt c (char +extended-digit-chars+ i)))))
		   (unless result
		     (format t "~A ~A ~A~%" r i c))
		   result))))

(defun digit-char.1.body ()
  (loop for r from 2 to 36 nconc
       (loop for i from 0 to 36
	  for c = (digit-char i r)
	  unless (if (>= i r) (null c)
		     (eqlt c (char +extended-digit-chars+ i)))
	  collect (list r i c))))

(defun digit-char-p.1.body ()
  (loop for x in *universe*
	always (not (and (characterp x)
			 (not (alphanumericp x))
			 (digit-char-p x)))))

(defun digit-char-p.2.body ()
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always (or (not x)
		   (not (and (not (alphanumericp x))
			     (digit-char-p x))))))

(defun digit-char-p.3.body ()
  (loop for r from 2 to 35
	always
	(loop for i from r to 35
	      for c = (char +extended-digit-chars+ i)
	      never (or (digit-char-p c r)
			(digit-char-p (char-downcase c) r)))))

(defun digit-char-p.4.body ()
  (loop for r from 2 to 35
	always
	(loop for i from 0 below r
	      for c = (char +extended-digit-chars+ i)
	      always (and (eqlt (digit-char-p c r) i)
			  (eqlt (digit-char-p (char-downcase c) r) i)))))

(defun standard-char-p.2.body ()
  (loop for x in *universe*
	always (or (not (characterp x))
		   (find x +standard-chars+)
		   (not (standard-char-p x)))))

(defun standard-char-p.2a.body ()
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always (or (not (characterp x))
		   (find x +standard-chars+)
		   (not (standard-char-p x)))))

(defun char-upcase.1.body ()
  (loop for x in *universe*
	always
	(or (not (characterp x))
	    (let ((u (char-upcase x))) 
	      (and
	       (or (lower-case-p x) (eqlt u x))
	       (eqlt u (char-upcase u)))))))

(defun char-upcase.2.body ()
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always
	(or (not x)
	    (let ((u (char-upcase x)))
	      (and
	       (or (lower-case-p x) (eqlt u x))
	       (eqlt u (char-upcase u)))))))

(defun char-downcase.1.body ()
  (loop for x in *universe*
	always
	(or (not (characterp x))
	    (let ((u (char-downcase x))) 
	      (and
	       (or (upper-case-p x) (eqlt u x))
	       (eqlt u (char-downcase u)))))))

(defun char-downcase.2.body ()
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always
	(or (not x)
	    (let ((u (char-downcase x)))
	      (and
	       (or (upper-case-p x) (eqlt u x))
	       (eqlt u (char-downcase u)))))))

(defun both-case-p.1.body ()
  (loop for x in *universe*
	always (or (not (characterp x))
		   (if (both-case-p x)
		       (and (graphic-char-p x)
			    (or (upper-case-p x)
				(lower-case-p x)))
		     (not (or (upper-case-p x)
			      (lower-case-p x)))))))

(defun both-case-p.2.body ()
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always (or (not (characterp x))
		   (if (both-case-p x)
		       (and (graphic-char-p x)
			    (or (upper-case-p x)
				(lower-case-p x)))
		     (not (or (upper-case-p x)
			      (lower-case-p x)))))))

(defun char-code.2.body ()
  (loop for i from 0 below (min 65536 char-code-limit)
	for c = (code-char i)
	always (or (not c)
		   (eqlt (char-code c) i))))

(defun char-int.2.fn ()
  (declare (optimize (safety 3) (speed 1) (space 1)))
  (let ((c->i (make-hash-table :test #'equal))
	(i->c (make-hash-table :test #'eql)))
    (flet ((%insert
	    (c)
	    (or (not (characterp c))
		(let* ((i (char-int c))
		       (j (gethash c c->i))
		       (d (gethash i i->c)))
		  (and
		   (or (null j) (eqlt j i))
		   (or (null d) (char= c d))
		   (progn
		     (setf (gethash c c->i) i)
		     (setf (gethash i i->c) c)
		     t))))))
      (or
       (loop for i from 0 below (min (ash 1 16) char-code-limit)
	     unless (%insert (code-char i))
	     collect i)
       (loop for i = (random char-code-limit)
	     repeat 1000
	     unless (%insert (code-char i))
	     collect i)
       (find-if-not #'%insert +standard-chars+)
       (find-if-not #'%insert *universe*)))))

(defun char-name.1.fn ()
  (declare (optimize (safety 3) (speed 1) (space 1)))
  (flet ((%check
	  (c)
	  (or (not (characterp c))
	      (let ((name (char-name c)))
		(or (null name)
		    (and (stringp name)
			 (eqlt c (name-char name))))))))
    (and
     (loop for i from 0 below (min (ash 1 16) char-code-limit)
	   always (%check (code-char i)))
     (every #'%check +standard-chars+)
     (every #'%check *universe*)
     t)))

(defun name-char.1.body ()
  (declare (optimize (safety 3)))
  (loop for x in *universe*
	for s = (catch-type-error (string x))
	always
	(or (eqlt s 'type-error)
	    (let ((c (name-char x)))
	      (or (not c)
		  (characterp c)
		  ;; FIXME The rest of this wasn't reachable
		  #|
		  (let ((name (char-name c)))
		    (declare (type (or null string) name))
		    (and name
			 (string-equal name s)))
		  |#
		  )))))
