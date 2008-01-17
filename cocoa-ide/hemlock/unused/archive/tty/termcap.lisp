;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+CMU (ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;;    Written by Bill Chiles
;;;
;;; Terminal Capability
;;;
;;; This stuff parses a Termcap file and returns a data structure suitable
;;; for initializing a redisplay methods device.
;;;

(in-package :hemlock-internals)



;;;; Interface for device creating code.

(defun get-termcap (name)
  "Look in TERMCAP environment variable for terminal capabilities or a
   file to use.  If it is a file, look for name in it.  If it is a description
   of the capabilities, use it, and don't look for name anywhere.  If TERMCAP
   is undefined, look for name in termcap-file.  An error is signaled if it
   cannot find the terminal capabilities."
  (let ((termcap-env-var (get-termcap-env-var)))
    (if termcap-env-var
	(if (char= (schar termcap-env-var 0) #\/) ; hack for filenamep
	    (with-open-file (s termcap-env-var)
	      (if (find-termcap-entry name s)
		  (parse-fields s)
		  (error "Unknown Terminal ~S in file ~S." name termcap-env-var)))
	    (with-input-from-string (s termcap-env-var)
	      (skip-termcap-names s)
	      (parse-fields s)))
	(with-open-file (s termcap-file)
	  (if (find-termcap-entry name s)
	      (parse-fields s)
	      (error "Unknown Terminal ~S in file ~S." name termcap-file))))))

(declaim (inline termcap))
(defun termcap (name termcap)
  (cdr (assoc name termcap :test #'eq)))



;;;; Finding the termcap entry

(defun find-termcap-entry (name stream)
  (loop
   (let ((end-of-names (lex-termcap-name stream)))
     (when (termcap-found-p name)
       (unless end-of-names (skip-termcap-names stream))
       (return t))
     (when end-of-names
       (unless (skip-termcap-fields stream)
	 (return nil))))))


;;; This buffer is used in LEX-TERMCAP-NAME and PARSE-FIELDS to
;;; do string comparisons and build strings from interpreted termcap
;;; characters, respectively.
;;; 
(defvar *termcap-string-buffer* (make-string 300))
(defvar *termcap-string-index* 0)

(eval-when (:compile-toplevel :execute)

(defmacro init-termcap-string-buffer ()
  `(setf *termcap-string-index* 0))

(defmacro store-char (char)
  `(progn
    (setf (schar *termcap-string-buffer* *termcap-string-index*) ,char)
    (incf *termcap-string-index*)))

(defmacro termcap-string-buffer-string ()
  `(subseq (the simple-string *termcap-string-buffer*)
	   0 *termcap-string-index*))

) ;eval-when


;;; LEX-TERMCAP-NAME gathers characters until the next #\|, which separate
;;; terminal names, or #\:, which terminate terminal names for an entry.
;;; T is returned if the end of the names is reached for the entry.
;;; If we hit and EOF, act like we found a :. 
;;; 
(defun lex-termcap-name (stream)
  (init-termcap-string-buffer)
  (loop
   (let ((char (read-char stream nil #\:)))
     (case char
       (#\Linefeed (init-termcap-string-buffer))
       (#\# (read-line stream nil))
       (#\| (return nil))
       (#\: (return t))
       (t (store-char char))))))

(defun termcap-found-p (name)
  (string= name *termcap-string-buffer* :end2 *termcap-string-index*))

;;; SKIP-TERMCAP-NAMES eats characters until the next #\: which terminates
;;; terminal names for an entry.  Stop also at EOF.
;;; 
(defun skip-termcap-names (stream)
  (loop
   (when (char= (read-char stream nil #\:) #\:)
     (return))))

;;; SKIP-TERMCAP-FIELDS skips the rest of an entry, returning nil if there
;;; are no more entries in the file.  An entry is terminated by a #\:
;;; followed by a #\newline (possibly by eof).
;;; 
(defun skip-termcap-fields (stream)
  (loop
   (multiple-value-bind (line eofp)
			(read-line stream nil)
     (if eofp
	 (return nil)
	 (let ((len (length line)))
	   (declare (simple-string line))
	   (when (and (not (zerop len))
		      (not (char= (schar line 0) #\#))
		      (char= (schar line (1- len)) #\:))
	     (let ((char (read-char stream nil :eof)))
	       (if (eq char :eof)
		   (return nil)
		   (unread-char char stream))
	       (return t))))))))

    

;;;; Defining known capabilities for parsing purposes.

(eval-when (:compile-toplevel :execute :load-toplevel)
(defvar *known-termcaps* ())
) ;eval-when


(eval-when (:compile-toplevel :execute)

;;; DEFTERMCAP makes a terminal capability known for parsing purposes.
;;; Type is one of :string, :number, or :boolean.  Cl-name is an EQ
;;; identifier for the capability.
;;;
(defmacro deftermcap (name type cl-name)
  `(progn (push (list ,name ,type ,cl-name) *known-termcaps*)))

(defmacro termcap-def (name)
  `(cdr (assoc ,name *known-termcaps* :test #'string=)))

(defmacro termcap-def-type (termcap-def)
  `(car ,termcap-def))

(defmacro termcap-def-cl-name (termcap-def)
  `(cadr ,termcap-def))

) ;eval-when


(deftermcap "is" :string :init-string)
(deftermcap "if" :string :init-file)
(deftermcap "ti" :string :init-cursor-motion)
(deftermcap "te" :string :end-cursor-motion)
(deftermcap "al" :string :open-line)
(deftermcap "am" :boolean :auto-margins-p)
(deftermcap "ce" :string :clear-to-eol)
(deftermcap "cl" :string :clear-display)
(deftermcap "cm" :string :cursor-motion)
(deftermcap "co" :number :columns)
(deftermcap "dc" :string :delete-char)
(deftermcap "dm" :string :init-delete-mode)
(deftermcap "ed" :string :end-delete-mode)
(deftermcap "dl" :string :delete-line)
(deftermcap "im" :string :init-insert-mode)
(deftermcap "ic" :string :init-insert-char)
(deftermcap "ip" :string :end-insert-char)
(deftermcap "ei" :string :end-insert-mode)
(deftermcap "li" :number :lines)
(deftermcap "so" :string :init-standout-mode)
(deftermcap "se" :string :end-standout-mode)
(deftermcap "tc" :string :similar-terminal)
(deftermcap "os" :boolean :overstrikes)
(deftermcap "ul" :boolean :underlines)

;;; font related stuff, added by William
(deftermcap "ae" :string :end-alternate-char-set)
(deftermcap "as" :string :start-alternate-char-set)
(deftermcap "mb" :string :start-blinking-attribute)
(deftermcap "md" :string :start-bold-attribute)
(deftermcap "me" :string :end-all-attributes)
(deftermcap "mh" :string :start-half-bright-attribute)
(deftermcap "mk" :string :start-blank-attribute)
(deftermcap "mp" :string :start-protected-attribute)
(deftermcap "mr" :string :start-reverse-video-attribute)
(deftermcap "ue" :string :end-underscore-mode)
(deftermcap "us" :string :start-underscore-mode)


;;;; Parsing an entry.

(defvar *getchar-ungetchar-buffer* nil)

(eval-when (:compile-toplevel :execute)

;;; UNGETCHAR  --  Internal.
;;;
;;; We need this to be able to peek ahead more than one character.
;;; This is used in PARSE-FIELDS and GET-TERMCAP-STRING-CHAR.
;;;
(defmacro ungetchar (char)
  `(push ,char *getchar-ungetchar-buffer*))

;;; GETCHAR  --  Internal.
;;;
;;; This is used in PARSE-FIELDS and GET-TERMCAP-STRING-CHAR.
;;;
(defmacro getchar ()
  `(loop
    (setf char
	  (if *getchar-ungetchar-buffer*
	      (pop *getchar-ungetchar-buffer*)
	      (read-char stream nil :eof)))
    (if (and (characterp char) (char= char #\\))
	(let ((temp (if *getchar-ungetchar-buffer*
			(pop *getchar-ungetchar-buffer*)
			(read-char stream))))
	  (when (char/= temp #\newline)
	    (ungetchar temp)
	    (return char)))
	(return char))))


;;; STORE-FIELD used in PARSE-FIELDS.
;;; 
(defmacro store-field (cl-name value)
  (let ((name (gensym)))
    `(let ((,name ,cl-name))
       (unless (cdr (assoc ,name termcap :test #'eq))
	 (push (cons ,name ,value) termcap)))))

) ;eval-when

;;; PARSE-FIELDS parses a termcap entry.  We start out in the state get-name.
;;; Each name is looked up in *known-termcaps*, and if it is of interest, then
;;; we dispatch to a state to pick up the value of the field; otherwise, eat
;;; the rest of the field to get to the next name.  The name could be present
;;; simply to have the capability negated before the entry indirects to a
;;; similar terminal's capabilities, in which case it is followed by an #\@.
;;; Negated fields are stored with the value :negated since we only store a
;;; field if it does not already have a value -- this is the intent of the
;;; sequencing built into the termcap file.  When we are done, we see if there
;;; is a similar terminal to be parsed, and when we are really done, we replace
;;; all the :negated's with nil's.
;;; 
(defun parse-fields (stream)
  (prog ((termcap-name (make-string 2))
	 (termcap ())
	 char termcap-def)
  GET-NAME
    ;;
    ;; This state expects char to be a #\:.
    (case (getchar)
      ((#\space #\tab)
       (go GET-NAME))
      (#\:
       ;; This is an empty field.
       (go GET-NAME))
      ((#\newline :eof)
       (go MAYBE-DONE))
      (t
       (setf (schar termcap-name 0) char)))
    (setf (schar termcap-name 1) (getchar))
    (setf termcap-def (termcap-def termcap-name))
    (unless termcap-def (go EAT-FIELD))
    (when (char= (getchar) #\@)
      ;; Negation of a capability to be inherited from a similar terminal.
      (store-field (termcap-def-cl-name termcap-def) :negated)
      (go EAT-FIELD))
    (case (termcap-def-type termcap-def)
      (:number (go NUMBER))
      (:boolean (go BOOLEAN))
      (:string (go STRING)))
  NUMBER
    (unless (char= char #\#)
      (error "Bad termcap format -- number field '#' missing."))
    (let ((number 0)
	  digit)
      (loop
       (setf digit (digit-char-p (getchar)))
       (if digit
	   (setf number (+ digit (* number 10)))
	   (if (char= char #\:)
	       (return)
	       (error "Bad termcap format -- number field not : terminated."))))
      (store-field (termcap-def-cl-name termcap-def) number)
      (go GET-NAME))
  BOOLEAN
    (store-field (termcap-def-cl-name termcap-def) t)
    (if (char= char #\:)
	(go GET-NAME)
	(error "Bad termcap format -- boolean field not : terminated."))
  STRING
    (unless (char= char #\=)
      (error "Bad termcap format -- string field '=' missing."))
    ;;
    ;; Eat up any cost of the capability.
    (when (digit-char-p (getchar))
      (let ((dotp nil))
	(loop
	 (case (getchar)
	   ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	   (#\.
	    (when dotp (return))
	    (setf dotp t))
	   (t (when (char= char #\*) (getchar)) ; '*' means a per line cost
	      (return))))))
    ;;
    ;; Collect the characters.
    (let ((normal-string-p (not (eq (termcap-def-cl-name termcap-def)
				    :cursor-motion)))
	  xp cm-info)
      (init-termcap-string-buffer)
      (loop
       (case (setf char (get-termcap-string-char stream char))
	 (#\%
	  (if normal-string-p
	      (store-char #\%)
	      (case (getchar)
		(#\% (store-char #\%))
		((#\d #\2 #\3)
		 (push (if (char= char #\d) 0 (digit-char-p char))
		       cm-info)
		 (push (if xp :y-pad :x-pad) cm-info)
		 (push (termcap-string-buffer-string) cm-info)
		 (push (if xp :string2 :string1) cm-info)
		 (init-termcap-string-buffer)
		 (setf xp t))
		(#\.
		 (push (termcap-string-buffer-string) cm-info)
		 (push (if xp :string2 :string1) cm-info)
		 (init-termcap-string-buffer)
		 (setf xp t))
		(#\+
		 (push (termcap-string-buffer-string) cm-info)
		 (push (if xp :string2 :string1) cm-info)
		 (push (get-termcap-string-char stream (getchar)) cm-info)
		 (push (if xp :y-add-char :x-add-char) cm-info)
		 (init-termcap-string-buffer)
		 (setf xp t))
		(#\>
		 (push (get-termcap-string-char stream (getchar)) cm-info)
		 (push (if xp :y-condx-char :x-condx-char) cm-info)
		 (push (get-termcap-string-char stream (getchar)) cm-info)
		 (push (if xp :y-condx-add-char :x-condx-add-char) cm-info))
		(#\r
		 (push t cm-info)
		 (push :reversep cm-info))
		(#\i
		 (push t cm-info)
		 (push :one-origin cm-info)))))
	 (#\:
	  (store-field (termcap-def-cl-name termcap-def)
		       (cond (normal-string-p (termcap-string-buffer-string))
			     (t (push (termcap-string-buffer-string) cm-info)
				(cons :string3 cm-info))))
	  (return))
	 (t (store-char char)))
       (getchar))
      (go GET-NAME))
  EAT-FIELD
    (loop (when (char= (getchar) #\:) (return)))
    (go GET-NAME)
  MAYBE-DONE
    (let* ((similar-terminal (assoc :similar-terminal termcap :test #'eq))
	   (name (cdr similar-terminal)))
      (when name
	(file-position stream :start)
	(setf (cdr similar-terminal) nil)
	(if (find-termcap-entry name stream)
	    (go GET-NAME)
	    (error "Unknown similar terminal name -- ~S." name))))
    (dolist (ele termcap)
      (when (eq (cdr ele) :negated)
	(setf (cdr ele) nil)))
    (return termcap)))

;;; GET-TERMCAP-STRING-CHAR -- Internal.
;;;
;;; This parses/lexes an ASCII character out of the termcap file and converts
;;; it into the appropriate Common Lisp character.  This is a Common Lisp
;;; character with the same CHAR-CODE code as the ASCII code, so writing the
;;; character to the tty will have the desired effect.  If this function needs
;;; to look ahead to determine any characters, it unreads the character.
;;;
(defun get-termcap-string-char (stream char)
  (case char
    (#\\
     (case (getchar)
       (#\E (code-char 27))
       (#\n (code-char 10))
       (#\r (code-char 13))
       (#\t (code-char 9))
       (#\b (code-char 8))
       (#\f (code-char 12))
       (#\^ #\^)
       (#\\ #\\)
       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	(let ((result 0)
	      (digit (digit-char-p char)))
	  (loop (setf result (+ digit (* 8 result)))
	    (unless (setf digit (digit-char-p (getchar)))
	      (ungetchar char)
	      (return (code-char (ldb (byte 7 0) result)))))))
       (t (error "Bad termcap format -- unknown backslash character."))))
    (#\^
     (code-char (- (char-code (char-upcase (getchar))) 64)))
    (t char)))


;;;; Initialization file string.

(defun get-init-file-string (f)
  (unless (probe-file f)
    (error "File containing terminal initialization string does not exist -- ~S."
	   f))
  (with-open-file (s f)
    (let* ((len (file-length s))
	   (string (make-string len)))
      (dotimes (i len string)
	(setf (schar string i) (read-char s))))))
