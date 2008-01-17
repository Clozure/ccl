;;; -*- Log: hemlock.log; Package: Spell -*-
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
;;;    Designed by Bill Chiles and Rob Maclachlan
;;;
;;; This file contains the code to grow the spelling dictionary in system
;;; space by reading a text file of entries or adding one at a time.  This
;;; code relies on implementation dependent code found in Spell-RT.Lisp.


(in-package "SPELL")


;;;; Converting Flags to Masks

(defconstant flag-names-to-masks
  `((#\V . ,V-mask) (#\N . ,N-mask) (#\X . ,X-mask)
    (#\H . ,H-mask) (#\Y . ,Y-mask) (#\G . ,G-mask)
    (#\J . ,J-mask) (#\D . ,D-mask) (#\T . ,T-mask)
    (#\R . ,R-mask) (#\Z . ,Z-mask) (#\S . ,S-mask)
    (#\P . ,P-mask) (#\M . ,M-mask)))

(defvar *flag-masks*
  (make-array 128 :element-type '(unsigned-byte 16) :initial-element 0)
  "This holds the masks for character flags, which is used when reading
   a text file of dictionary words.  Illegal character flags hold zero.")

(eval-when (:compile-toplevel :execute)
(defmacro flag-mask (char)
  `(aref *flag-masks* (char-code ,char)))
) ;eval-when

(dolist (e flag-names-to-masks)
  (let ((char (car e))
	(mask (cdr e)))
    (setf (flag-mask char) mask)
    (setf (flag-mask (char-downcase char)) mask)))



;;;; String and Hashing Macros

(eval-when (:compile-toplevel :execute)

(defmacro string-table-replace (src-string dst-start length)
  `(sap-replace *string-table* ,src-string 0 ,dst-start (+ ,dst-start ,length)))

;;; HASH-ENTRY is used in SPELL-ADD-ENTRY to find a dictionary location for
;;; adding a new entry.  If a location contains a zero, then it has never been
;;; used, and no entries have ever been "hashed past" it.  If a location
;;; contains SPELL-DELETED-ENTRY, then it once contained an entry that has
;;; since been deleted.
;;;
(defmacro hash-entry (entry entry-len)
  (let ((loop-loc (gensym)) (loc-contents (gensym))
	(hash (gensym)) (loc (gensym)))
    `(let* ((,hash (string-hash ,entry ,entry-len))
	    (,loc (rem ,hash (the fixnum *dictionary-size*)))
	    (,loc-contents (dictionary-ref ,loc)))
       (declare (fixnum ,loc ,loc-contents))
       (if (or (zerop ,loc-contents) (= ,loc-contents spell-deleted-entry))
	   ,loc
	   (hash2-loop (,loop-loc ,loc-contents) ,loc ,hash
	     ,loop-loc nil t)))))

) ;eval-when



;;;; Top Level Stuff

(defun spell-read-dictionary (filename)
  "Add entries to dictionary from lines in the file filename."
  (with-open-file (s filename :direction :input)
    (loop (multiple-value-bind (entry eofp) (read-line s nil nil)
	    (declare (type (or simple-string null) entry))
	    (unless entry (return))
	    (spell-add-entry entry)
	    (if eofp (return))))))


;;; This is used to break up an 18 bit string table index into two parts
;;; for storage in a word descriptor unit.  See the documentation at the
;;; top of Spell-Correct.Lisp.
;;;
(defconstant whole-index-low-byte (byte 16 0))

(defun spell-add-entry (line &optional
			     (word-end (or (position #\/ line :test #'char=)
					   (length line))))
  "Line is of the form \"entry/flag1/flag2\" or \"entry\".  It is parsed and
   added to the spelling dictionary.  Line is desstructively modified."
  (declare (simple-string line) (fixnum word-end))
  (nstring-upcase line :end word-end)
  (when (> word-end max-entry-length)
    (return-from spell-add-entry nil))
  (let ((entry (lookup-entry line word-end)))
    (when entry
      (add-flags (+ entry 2) line word-end)
      (return-from spell-add-entry nil)))
  (let* ((hash-loc (hash-entry line word-end))
	 (string-ptr *string-table-size*)
	 (desc-ptr *descriptors-size*)
	 (desc-ptr+1 (1+ desc-ptr))
	 (desc-ptr+2 (1+ desc-ptr+1)))
    (declare (fixnum string-ptr))
    (when (not hash-loc) (error "Dictionary Overflow!"))
    (when (> 3 *free-descriptor-elements*) (grow-descriptors))
    (when (> word-end *free-string-table-bytes*) (grow-string-table))
    (decf *free-descriptor-elements* 3)
    (incf *descriptors-size* 3)
    (decf *free-string-table-bytes* word-end)
    (incf *string-table-size* word-end)
    (setf (dictionary-ref hash-loc) desc-ptr)
    (setf (descriptor-ref desc-ptr)
	  (dpb (the fixnum (ldb new-hash-byte (string-hash line word-end)))
	       stored-hash-byte
	       word-end))
    (setf (descriptor-ref desc-ptr+1)
	  (ldb whole-index-low-byte string-ptr))
    (setf (descriptor-ref desc-ptr+2)
	  (dpb (the fixnum (ldb whole-index-high-byte string-ptr))
	       stored-index-high-byte
	       0))
    (add-flags desc-ptr+2 line word-end)
    (string-table-replace line string-ptr word-end))
  t)

(defun add-flags (loc line word-end)
  (declare (simple-string line) (fixnum word-end))
  (do ((flag (1+ word-end) (+ 2 flag))
       (line-end (length line)))
      ((>= flag line-end))
    (declare (fixnum flag line-end))
    (let ((flag-mask (flag-mask (schar line flag))))
      (declare (fixnum flag-mask))
      (unless (zerop flag-mask)
	(setf (descriptor-ref loc)
	      (logior flag-mask (descriptor-ref loc)))))))

;;; SPELL-REMOVE-ENTRY destructively uppercases entry in removing it from
;;; the dictionary.  First entry is looked up, and if it is found due to a
;;; flag, the flag is cleared in the descriptor table.  If entry is a root
;;; word in the dictionary (that is, looked up without the use of a flag),
;;; then the root and all its derivitives are deleted by setting its
;;; dictionary location to spell-deleted-entry.
;;; 
(defun spell-remove-entry (entry)
  "Removes entry from the dictionary, so it will be an unknown word.  Entry
   is a simple string and is destructively modified.  If entry is a root
   word, then all words derived with entry and its flags will also be deleted."
  (declare (simple-string entry))
  (nstring-upcase entry)
  (let ((entry-len (length entry)))
    (declare (fixnum entry-len))
    (when (<= 2 entry-len max-entry-length)
      (multiple-value-bind (index flagp)
			   (spell-try-word entry entry-len)
	(when index
	  (if flagp
	      (setf (descriptor-ref (+ 2 index))
		    (logandc2 (descriptor-ref (+ 2 index)) flagp))
	      (let* ((hash (string-hash entry entry-len))
		     (hash-and-len (dpb (the fixnum (ldb new-hash-byte hash))
					stored-hash-byte
					(the fixnum entry-len)))
		     (loc (rem hash (the fixnum *dictionary-size*)))
		     (loc-contents (dictionary-ref loc)))
		(declare (fixnum hash hash-and-len loc))
		(cond ((zerop loc-contents) nil)
		      ((found-entry-p loc-contents entry entry-len hash-and-len)
		       (setf (dictionary-ref loc) spell-deleted-entry))
		      (t
		       (hash2-loop (loop-loc loc-contents) loc hash
				   nil
				   (when (found-entry-p loc-contents entry
							entry-len hash-and-len)
				     (setf (dictionary-ref loop-loc)
					   spell-deleted-entry)
				     (return spell-deleted-entry))))))))))))

(defun spell-root-flags (index)
  "Return the flags associated with the root word corresponding to a
   dictionary entry at index."
  (let ((desc-word (descriptor-ref (+ 2 index)))
	(result ()))
    (declare (fixnum desc-word))
    (dolist (ele flag-names-to-masks result)
      (unless (zerop (logand (the fixnum (cdr ele)) desc-word))
	(push (car ele) result)))))



;;;; Growing Dictionary Structures

;;; GROW-DESCRIPTORS grows the descriptors vector by 10%.
;;;
(defun grow-descriptors ()
  (let* ((old-size (+ (the fixnum *descriptors-size*)
		      (the fixnum *free-descriptor-elements*)))
	 (new-size (truncate (* old-size 1.1)))
	 (new-bytes (* new-size 2))
	 (new-sap (allocate-bytes new-bytes)))
    (declare (fixnum new-size old-size))
    (sap-replace new-sap *descriptors* 0 0
		 (* 2 (the fixnum *descriptors-size*)))
    (deallocate-bytes (system-address *descriptors*) (* 2 old-size))
    (setf *free-descriptor-elements*
	  (- new-size (the fixnum *descriptors-size*)))
    (setf *descriptors* new-sap)))

;;; GROW-STRING-TABLE grows the string table by 10%.
;;;
(defun grow-string-table ()
  (let* ((old-size (+ (the fixnum *string-table-size*)
		      (the fixnum *free-string-table-bytes*)))
	 (new-size (truncate (* old-size 1.1)))
	 (new-sap (allocate-bytes new-size)))
    (declare (fixnum new-size old-size))
    (sap-replace new-sap *string-table* 0 0 *string-table-size*)
    (setf *free-string-table-bytes*
	  (- new-size (the fixnum *string-table-size*)))
    (deallocate-bytes (system-address *string-table*) old-size)
    (setf *string-table* new-sap)))
