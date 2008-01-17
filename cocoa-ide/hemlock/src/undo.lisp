;;; -*- Log: hemlock.log; Package: Hemlock -*-
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
;;; This file contains the implementation of the undo mechanism.

(in-package :hemlock)



;;;; -- Constants --

(defvar undo-name "Undo")



;;;; -- Variables --

(defvar *undo-info* nil
  "Structure containing necessary info to undo last undoable operation.")



;;;; -- Structures --

(defstruct (undo-info (:print-function %print-undo-info)
		      (:constructor %make-undo-info
				    (name method cleanup method-undo buffer))
		      (:copier nil))
  name		; string displayed for user to know what's being undone --
		; typically a command's name.
  (hold-name undo-name)	; holds a name for successive invocations of the
			; "Undo" command.
  method	; closure stored by command that undoes the command when invoked.
  method-undo	; closure stored by command that undoes what method does.
  cleanup	; closure stored by command that cleans up any data for method,
		; such as permanent marks.
  buffer)	; buffer the command was invoked in.

(setf (documentation 'undo-info-name 'function)
      "Return the string indicating what would be undone for given undo info.")
(setf (documentation 'undo-info-method 'function)
      "Return the closure that undoes a command when invoked.")
(setf (documentation 'undo-info-cleanup 'function)
      "Return the closure that cleans up data necessary for an undo method.")
(setf (documentation 'undo-info-buffer 'function)
      "Return the buffer that the last undoable command was invoked in.")
(setf (documentation 'undo-info-hold-name 'function)
      "Return the name being held since the last invocation of \"Undo\"")
(setf (documentation 'undo-info-method-undo 'function)
      "Return the closure that undoes what undo-info-method does.")
      

(defun %print-undo-info (obj s depth)
  (declare (ignore depth))
  (format s "#<Undo Info ~S>" (undo-info-name obj)))



;;;; -- Commands --

(defcommand "Undo" (p)
  "Undo last major change, kill, etc.
   Simple insertions and deletions cannot be undone.  If you change the buffer
   in this way before you undo, you may get slightly wrong results, but this
   is probably still useful."
  "This is not intended to be called in Lisp code."
  (declare (ignore p))
  (if (not *undo-info*) (editor-error "No currently undoable command."))
  (let ((buffer (undo-info-buffer *undo-info*))
	(cleanup (undo-info-cleanup *undo-info*))
	(method-undo (undo-info-method-undo *undo-info*)))
    (if (not (eq buffer (current-buffer)))
	(editor-error "Undo info is for buffer ~S." (buffer-name buffer)))
    (when (prompt-for-y-or-n :prompt (format nil "Undo the last ~A? "
					     (undo-info-name *undo-info*))
			     :must-exist t)
      (funcall (undo-info-method *undo-info*))
      (cond (method-undo
	     (rotatef (undo-info-name *undo-info*)
		      (undo-info-hold-name *undo-info*))
	     (rotatef (undo-info-method *undo-info*)
		      (undo-info-method-undo *undo-info*)))
	    (t (if cleanup (funcall cleanup))
	       (setf *undo-info* nil))))))



;;;; -- Primitives --

(defun save-for-undo (name method
		      &optional cleanup method-undo (buffer (current-buffer)))
  "Stashes information for next \"Undo\" command invocation.  If there is
   an undo-info object, it is cleaned up first."
  (cond (*undo-info*
	 (let ((old-cleanup (undo-info-cleanup *undo-info*)))
	   (if old-cleanup (funcall old-cleanup))
	   (setf (undo-info-name *undo-info*) name)
	   (setf (undo-info-hold-name *undo-info*) undo-name)
	   (setf (undo-info-method *undo-info*) method)
	   (setf (undo-info-method-undo *undo-info*) method-undo)
	   (setf (undo-info-cleanup *undo-info*) cleanup)
	   (setf (undo-info-buffer *undo-info*) buffer)
	   *undo-info*))
	(t (setf *undo-info*
		 (%make-undo-info name method cleanup method-undo buffer)))))



(eval-when (:compile-toplevel :execute)

;;; MAKE-TWIDDLE-REGION-UNDO sets up an undo method that deletes region1,
;;; saving the deleted region and eventually storing it in region2.  After
;;; deleting region1, its start and end are made :right-inserting and
;;; :left-inserting, so it will contain region2 when it is inserted at region1's
;;; end.  This results in a method that takes region1 with permanent marks
;;; into some buffer and results with the contents of region2 in region1 (with
;;; permanent marks into a buffer) and the contents of region1 (from the buffer)
;;; in region2 (a region without marks into any buffer).
;;;
(defmacro make-twiddle-region-undo (region1 region2)
  `#'(lambda ()
       (let* ((tregion (delete-and-save-region ,region1))
	      (mark (region-end ,region1)))
	 (setf (mark-kind (region-start ,region1)) :right-inserting)
	 (setf (mark-kind mark) :left-inserting)
	 (ninsert-region mark ,region2)
	 (setf ,region2 tregion))))

;;; MAKE-DELETE-REGION-UNDO sets up an undo method that deletes region with
;;; permanent marks into a buffer, saving the region in region without any
;;; marks into a buffer, deleting one of the permanent marks, and saving one
;;; permanent mark in the variable mark.  This is designed to work with
;;; MAKE-INSERT-REGION-UNDO, so mark results in the location in a buffer where
;;; region will be inserted if this method is undone.
;;;
(defmacro make-delete-region-undo (region mark)
  `#'(lambda ()
       (let ((tregion (delete-and-save-region ,region)))
	 (delete-mark (region-start ,region))
	 (setf ,mark (region-end ,region))
	 (setf ,region tregion))))

;;; MAKE-INSERT-REGION-UNDO sets up an undo method that inserts region at mark,
;;; saving in the variable region a region with permanent marks in a buffer.
;;; This is designed to work with MAKE-DELETE-REGION-UNDO, so region can later
;;; be deleted.
;;;
(defmacro make-insert-region-undo (region mark)
  `#'(lambda ()
       (let ((tregion (region (copy-mark ,mark :right-inserting) ,mark)))
	 (setf (mark-kind ,mark) :left-inserting)
	 (ninsert-region ,mark ,region)
	 (setf ,region tregion))))

) ;eval-when

;;; MAKE-REGION-UNDO handles three common cases that undo'able commands often
;;; need.  This function sets up three closures via SAVE-FOR-UNDO that do
;;; an original undo, undo the original undo, and clean up any permanent marks
;;; the next time SAVE-FOR-UNDO is called.  Actually, the original undo and
;;; the undo for the original undo setup here are reversible in that each
;;; invocation of "Undo" switches these, so an undo setup by the function is
;;; undo'able, and the undo of the undo is undo'able, and the ....
;;;
;;; :twiddle
;;;    Region has permanent marks into a buffer.  Mark-or-region is a region
;;;    not connected to any buffer.  A first undo deletes region, saving it and
;;;    inserting mark-or-region.  This also sets region around the inserted
;;;    region in the buffer and sets mark-or-region to be the deleted and saved
;;;    region.  Thus the undo and the undo of the undo are the same action.
;;; :insert
;;;    Region is not connected to any buffer.  Mark-or-region is a permanent
;;;    mark into a buffer where region is to be inserted on a first undo, and
;;;    this mark is used to form a region on the first undo that will be
;;;    deleted upon a subsequent undo.  The cleanup method knows mark-or-region
;;;    is a permanent mark into a buffer, but it has to determine if region
;;;    has marks into a buffer because if a subsequent undo does occur, region
;;;    does point into a buffer.
;;; :delete
;;;    Region has permanent marks into a buffer.  Mark-or-region should not
;;;    have been supplied.  A first undo deletes region, saving the deleted
;;;    region in region and creating a permanent mark that indicates where to
;;;    put region back.  The permanent mark is stored in mark-or-region.  The
;;;    cleanup method has to check that mark-or-region is a mark since it won't
;;;    be unless there was a subsequent undo.
;;;
(defun make-region-undo (kind name region &optional mark-or-region)
  (case kind
    (:twiddle
     (save-for-undo name
       (make-twiddle-region-undo region mark-or-region)
       #'(lambda ()
	   (delete-mark (region-start region))
	   (delete-mark (region-end region)))
       (make-twiddle-region-undo region mark-or-region)))
    (:insert
     (save-for-undo name
       (make-insert-region-undo region mark-or-region)
       #'(lambda ()
	   (let ((mark (region-start region)))
	     (delete-mark mark-or-region)
	     (when (line-buffer (mark-line mark))
	       (delete-mark mark)
	       (delete-mark (region-end region)))))
       (make-delete-region-undo region mark-or-region)))
    (:delete
     (save-for-undo name
       (make-delete-region-undo region mark-or-region)
       #'(lambda ()
	   (delete-mark (region-start region))
	   (delete-mark (region-end region))
	   (if (markp mark-or-region) (delete-mark mark-or-region)))
       (make-insert-region-undo region mark-or-region)))))
