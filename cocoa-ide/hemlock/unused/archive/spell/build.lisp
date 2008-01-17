;;; -*- Log: hemlock.log; Package: Spell -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;; **********************************************************************
;;;
;;;    Written by Bill Chiles
;;;    Designed by Bill Chiles and Rob Maclachlan
;;;

;;; This file contains code to build a new binary dictionary file from
;;; text in system space.  This code relies on implementation dependent
;;; code from spell-rt.lisp.  Also, it is expected that spell-corr.lisp
;;; and spell-aug.lisp have been loaded.  In order to compile this file,
;;; you must first compile spell-rt, spell-corr.lisp, and spell-aug.lisp.

;;; The text file must be in the following format:
;;;      entry1/flag1/flag2/flag3
;;;      entry2
;;;      entry3/flag1/flag2/flag3/flag4/flag5.
;;; The flags are single letter indicators of legal suffixes for the entry;
;;; the available flags and their correct use may be found at the beginning
;;; of spell-corr.lisp in the Hemlock sources.  There must be exactly one 
;;; entry per line, and each line must be flushleft.


(in-package "SPELL")

;;; An interesting value when building an initial dictionary.
(defvar *collision-count* 0)

(defvar *new-dictionary*)
(defvar *new-descriptors*)
(defvar *new-string-table*)

(declaim (optimize (debug 3)))


;;;; Constants

;;; This is an upper bound estimate of the number of stored entries in the
;;; dictionary.  It should not be more than 21,845 because the dictionary
;;; is a vector of type '(unsigned-byte 16), and the descriptors' vector
;;; for the entries uses three '(unsigned-byte 16) elements per descriptor
;;; unit.  See the beginning of Spell-Correct.Lisp.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant +max-entry-count-estimate+ 15600)

(defconstant +new-dictionary-size+ 20011)

(defconstant +new-descriptors-size+ (1+ +max-entry-count-estimate+))

(defconstant +max-string-table-length+ (* 10 +max-entry-count-estimate+))

); eval-when


;;;; Hashing

;;; These hashing macros are different from the ones in Spell-Correct.Lisp
;;; simply because we are using separate space and global specials/constants.
;;; Of course, they should be identical, but it doesn't seem worth cluttering
;;; up Spell-Correct with macro generating macros for this file.

;;; Well, we've made them functions now.  we should really clean up the
;;; other macros mentioned above by merging them with these

(declaim (inline hash-increment handle-collision get-hash-index))
(defun hash-increment (hash size)
  (- size 2 (rem hash (- size 2))))

(defun handle-collision (descriptor-table hash location)
  (do* ((incr (hash-increment hash +new-dictionary-size+))
        (collide-location (rem (+ location incr)
                               +new-dictionary-size+)
                          (rem (+ collide-location incr)
                               +new-dictionary-size+)))
       ;; if we've found our way back to where we started, there are
       ;; no free slots available.  indicate failure.
       ((= collide-location location) nil)
    (when (zerop (aref descriptor-table collide-location))
      (return-from handle-collision collide-location))))

(defun get-hash-index (descriptor-table entry entry-length)
  "Finds a suitable position in DESCRIPTOR-TABLE for ENTRY.
   Returns NIL if one cannot be located."
  (let* ((hash (string-hash entry entry-length))
         (location (rem hash +new-dictionary-size+)))
    (cond
      ((not (zerop (aref descriptor-table location)))
       ;; crud.  the desirable spot was already taken.  hunt for another
       (incf *collision-count*)
       (handle-collision descriptor-table hash location))
      (t location))))


;;;; Build-Dictionary

(defun build-dictionary (input output)
  (let* ((descriptors (make-array +new-descriptors-size+))
         (string-table (make-string +max-string-table-length+))
         (descriptor-table (make-array +new-dictionary-size+
                                 :element-type '(unsigned-byte 16)))
         (new-dictionary (make-instance 'dictionary
                                        :string-table string-table
                                        :descriptors descriptors
                                        :descriptor-table descriptor-table)))
    (write-line "Reading dictionary ...")
    (force-output)
    (setf *collision-count* 0)
    (multiple-value-bind (entry-count string-table-length)
			 (read-initial-dictionary input descriptor-table
						  descriptors string-table)
      (write-line "Writing dictionary ...")
      (force-output)
      (write-dictionary output new-dictionary entry-count string-table-length)
      (format t "~D entries processed with ~D collisions."
	      entry-count *collision-count*)
      new-dictionary)))

(defun read-initial-dictionary (f dictionary descriptors string-table)
  (let* ((filename (pathname f))
	 (s (open filename :direction :input :if-does-not-exist nil)))
    (unless s (error "File ~S does not exist." f))
    (multiple-value-prog1
     (let ((descriptor-ptr 1)
	   (string-ptr 0)
	   (entry-count 0))
       (declare (fixnum descriptor-ptr string-ptr entry-count))
       (loop (multiple-value-bind (line eofp) (read-line s nil nil)
	       (declare (type (or null simple-string) line))
	       (unless line (return (values entry-count string-ptr)))
	       (incf entry-count)
	       (when (> entry-count +max-entry-count-estimate+)
		 (error "There are too many entries in text file!~%~
			Please change constants in spell-build.lisp, ~
			recompile the file, and reload it.~%~
			Be sure to understand the constraints of permissible ~
			values."))
	       (let ((flags (or (position #\/ line :test #'char=)
                                (length line))))
		 (declare (fixnum flags))
		 (cond ((> flags +max-entry-length+)
			(format t "Entry ~s too long." (subseq line 0 flags))
			(force-output))
		       (t (let ((new-string-ptr (+ string-ptr flags)))
			    (declare (fixnum new-string-ptr))
			    (when (> new-string-ptr +max-string-table-length+)
			      (error "Spell string table overflow!~%~
				     Please change constants in ~
				     spell-build.lisp, recompile the file, ~
				     and reload it.~%~
				     Be sure to understand the constraints ~
				     of permissible values."))
			    (spell-place-entry line flags
					       dictionary descriptors string-table
					       descriptor-ptr string-ptr)
			    (incf descriptor-ptr)
			    (setf string-ptr new-string-ptr)))))
	       (when eofp (return (values entry-count string-ptr))))))
     (close s))))

(defun word-flags (line word-end)
  (declare (simple-string line) (fixnum word-end))
  (let ((word-flags 0))
    (do ((flag (1+ word-end) (+ 2 flag))
         (line-end (length line)))
        ((>= flag line-end) word-flags)
      (declare (fixnum flag line-end))
      (let ((flag-mask (flag-mask (schar line flag))))
        (declare (fixnum flag-mask))
        (if (zerop flag-mask)
            (format t "Illegal flag ~S on word ~S."
                    (schar line flag) (subseq line 0 word-end))
            (setf word-flags
                  (logior flag-mask word-flags)))))))

(defun spell-place-entry (line word-end dictionary descriptors string-table
			       descriptor-ptr string-ptr)
  (declare (simple-string line string-table)
	   (fixnum word-end descriptor-ptr string-ptr))
  (nstring-upcase line :end word-end)
  (let* ((hash-loc (get-hash-index dictionary line word-end)))
    (unless hash-loc (error "Dictionary Overflow!"))
    (setf (aref dictionary hash-loc) descriptor-ptr)
    (let* ((hash-code (ldb +new-hash-byte+
                           (string-hash line word-end)))
           (descriptor (make-descriptor :hash-code hash-code
                                        :length word-end
                                        :string-index string-ptr)))
      (setf (desc-flags descriptor) (word-flags line word-end)
            (aref descriptors descriptor-ptr) descriptor)
      (replace string-table line :start1 string-ptr :end2 word-end))))