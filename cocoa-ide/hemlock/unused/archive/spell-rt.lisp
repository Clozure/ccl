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
;;;
;;; This file contains system dependent primitives for the spelling checking/
;;; correcting code in Spell-Correct.Lisp, Spell-Augment.Lisp, and
;;; Spell-Build.Lisp.

(defpackage "SPELL"
  (:use "LISP" "EXTENSIONS" "SYSTEM")
  (:export spell-try-word spell-root-word spell-collect-close-words
	   maybe-read-spell-dictionary correct-spelling max-entry-length
	   spell-read-dictionary spell-add-entry spell-root-flags
	   spell-remove-entry))

(in-package "SPELL")


;;;; System Area Referencing and Setting

(eval-when (:compile-toplevel :execute)

;;; MAKE-SAP returns pointers that *dictionary*, *descriptors*, and
;;; *string-table* are bound to.  Address is in the system area.
;;;
(defmacro make-sap (address)
  `(system:int-sap ,address))

(defmacro system-address (sap)
  `(system:sap-int ,sap))


(defmacro allocate-bytes (count)
  `(system:allocate-system-memory ,count))

(defmacro deallocate-bytes (address byte-count)
  `(system:deallocate-system-memory (int-sap ,address) ,byte-count))


(defmacro sapref (sap offset)
  `(system:sap-ref-16 ,sap (* ,offset 2)))

(defsetf sapref (sap offset) (value)
  `(setf (system:sap-ref-16 ,sap (* ,offset 2)) ,value))


(defmacro sap-replace (dst-string src-string src-start dst-start dst-end)
  `(%primitive byte-blt ,src-string ,src-start ,dst-string ,dst-start ,dst-end))

(defmacro string-sapref (sap index)
  `(system:sap-ref-8 ,sap ,index))



;;;; Primitive String Hashing

;;; STRING-HASH employs the instruction SXHASH-SIMPLE-SUBSTRING which takes
;;; an end argument, so we do not have to use SXHASH.  SXHASH would mean
;;; doing a SUBSEQ of entry.
;;;
(defmacro string-hash (string length)
  `(ext:truly-the lisp::index
		  (%primitive sxhash-simple-substring
			      ,string
			      (the fixnum ,length))))

) ;eval-when



;;;; Binary Dictionary File I/O

(defun open-dictionary (f)
  (let* ((filename (ext:unix-namestring f))
	 (kind (unix:unix-file-kind filename)))
    (unless kind (error "Cannot find dictionary -- ~S." filename))
    (multiple-value-bind (fd err)
			 (unix:unix-open filename unix:o_rdonly 0)
      (unless fd
	(error "Opening ~S failed: ~A." filename err))
      (multiple-value-bind (winp dev-or-err) (unix:unix-fstat fd)
	(unless winp (error "Opening ~S failed: ~A." filename dev-or-err))
	fd))))

(defun close-dictionary (fd)
  (unix:unix-close fd))

(defun read-dictionary-structure (fd bytes)
  (let* ((structure (allocate-bytes bytes)))
    (multiple-value-bind (read-bytes err)
			 (unix:unix-read fd structure bytes)
      (when (or (null read-bytes) (not (= bytes read-bytes)))
	(deallocate-bytes (system-address structure) bytes)
	(error "Reading dictionary structure failed: ~A." err))
      structure)))
