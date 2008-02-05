;;; -*- Mode: LISP; Package: Hemlock-Internals -*-

(in-package :hemlock-internals)

(defconstant hemlock-char-code-limit 256)

(defvar *command-line-switches* nil)

(defun default-directory ()
  "Returns the pathname for the default directory.  This is the place where
  a file will be written if no directory is specified.  This may be changed
  with setf."
  (truename #p""))

(defun file-writable (pathname)
  "File-writable accepts a pathname and returns T if the current
  process can write it, and NIL otherwise. Also if the file does
  not exist return T."
  #+(or CMU scl)
  (ext:file-writable pathname)
  #-(or cmu scl)
  (handler-case (let ((io (open pathname
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist nil)))
                  (if io
                      (close io :abort t)
                      ;; more complicate situation:
                      ;; we want test if we can create the file.
                      (let ((io (open pathname
                                      :direction :output
                                      :if-exists nil
                                      :if-does-not-exist :create)))
                        (if io
                            (progn
                              (close io)
                              (delete-file io))
                            t))))
    (file-error (err)
                (declare (ignore err))
                nil)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %sp-byte-blt (src start dest dstart end)
  (declare (type (simple-base-string src dest)))
  (loop for s from start
        for d from dstart below end
        do
        (setf (aref dest d) (aref src s))))

(defun %sp-find-character-with-attribute (string start end table mask)
  ;;(declare (type (simple-array (mod 256) char-code-max) table))
  (declare (simple-string string))
  (declare (fixnum start end))
  "%SP-Find-Character-With-Attribute  String, Start, End, Table, Mask
  The codes of the characters of String from Start to End are used as indices
  into the Table, which is a U-Vector of 8-bit bytes. When the number picked
  up from the table bitwise ANDed with Mask is non-zero, the current
  index into the String is returned. The corresponds to SCANC on the Vax."
  (do ((index start (1+ index)))
      ((= index end) nil)
    (declare (fixnum index))
    (if (/= (logand (aref table (min 255 (char-code (schar string index)))) mask) 0)
	(return index))))

(defun %sp-reverse-find-character-with-attribute (string start end table
							  mask)
  ;;(declare (type (simple-array (mod 256) char-code-max) table))
  (declare (simple-string string))
  (declare (fixnum start end))
  "Like %SP-Find-Character-With-Attribute, only sdrawkcaB."
  (do ((index (1- end) (1- index)))
      ((< index start) nil)
    (declare (fixnum index))
    (if (/= (logand (aref table (min 255 (char-code (aref string index)))) mask) 0)
	(return index))))

(defun %sp-find-character (string start end character)
  "%SP-Find-Character  String, Start, End, Character
  Searches String for the Character from Start to End.  If the character is
  found, the corresponding index into String is returned, otherwise NIL is
  returned."
  (declare (simple-string string)
           (fixnum start end)
           (optimize (speed 3) (safety 0)))
  (do* ((i start (1+ i)))
       ((= i end))
    (declare (fixnum i))
    (when (eq character (schar string i))
      (return i))))

;;;; complete-file

(defun complete-file (pathname &key (defaults *default-pathname-defaults*)
			       ignore-types)
  (let ((files (complete-file-directory pathname defaults)))
    (cond ((null files)
	   (values nil nil))
	  ((null (cdr files))
	   (values (car files) 
		   t))
	  (t
	   (let ((good-files
		  (delete-if #'(lambda (pathname)
				 (and (simple-string-p
				       (pathname-type pathname))
				      (member (pathname-type pathname)
					      ignore-types
					      :test #'string=)))
			     files)))
	     (cond ((null good-files))
		   ((null (cdr good-files))
		    (return-from complete-file
				 (values (car good-files)
					 t)))
		   (t
		    (setf files good-files)))
	     (let ((common (file-namestring (car files))))
	       (dolist (file (cdr files))
		 (let ((name (file-namestring file)))
		   (dotimes (i (min (length common) (length name))
			       (when (< (length name) (length common))
				 (setf common name)))
		     (unless (char= (schar common i) (schar name i))
		       (setf common (subseq common 0 i))
		       (return)))))
	       (values (merge-pathnames common pathname)
		       nil)))))))

;;; COMPLETE-FILE-DIRECTORY-ARG -- Internal.
;;;
(defun complete-file-directory (pathname defaults)
  (let* ((pathname (merge-pathnames pathname (directory-namestring defaults)))
	 (type (pathname-type pathname)))
    (setf pathname
	  (make-pathname :defaults (truename (make-pathname :defaults pathname :name nil :type nil))
			 :name (pathname-name pathname)
			 :type type))
    (delete-if-not (lambda (candidate)
		     (search (namestring pathname) (namestring candidate)))
		   (append
		    #+CLISP 
		    (directory
		     (make-pathname :defaults pathname
				    :name :wild
				    :type nil)) ;gosh!
		    #+CLISP 
		    (directory
		     (make-pathname :defaults pathname
				    :directory (append (pathname-directory pathname) (list "*")) ;gosh gosh!
				    :name nil
				    :type nil))))))

;;; Ambiguous-Files  --  Public
;;;
(defun ambiguous-files (pathname
			&optional (defaults *default-pathname-defaults*))
  "Return a list of all files which are possible completions of Pathname.
   We look in the directory specified by Defaults as well as looking down
   the search list."
  (complete-file-directory pathname defaults))
