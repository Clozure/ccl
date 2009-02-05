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
;;; This file contains Bufed (Buffer Editing) code.
;;;

(in-package :hemlock)



;;;; Representation of existing buffers.

;;; This is the array of buffers in the bufed buffer.  Each element is a cons,
;;; where the CAR is the buffer, and the CDR indicates whether the buffer
;;; should be deleted (t deleted, nil don't).
;;;
(defvar *bufed-buffers* nil)
(defvar *bufed-buffers-end* nil)
;;;
(defmacro bufed-buffer (x) `(car ,x))
(defmacro bufed-buffer-deleted (x) `(cdr ,x))
(defmacro make-bufed-buffer (buffer) `(list ,buffer))


;;; This is the bufed buffer if it exists.
;;;
(defvar *bufed-buffer* nil)

;;; This is the cleanup method for deleting *bufed-buffer*.
;;;
(defun delete-bufed-buffers (buffer)
  (when (eq buffer *bufed-buffer*)
    (setf *bufed-buffer* nil)
    (setf *bufed-buffers* nil)))



;;;; Commands.

(defmode "Bufed" :major-p t
  :documentation
  "Bufed allows the user to quickly save, goto, delete, etc., his buffers.")

(defhvar "Virtual Buffer Deletion"
  "When set, \"Bufed Delete\" marks a buffer for deletion instead of immediately
   deleting it."
  :value t)

(defhvar "Bufed Delete Confirm"
  "When set, \"Bufed\" commands that actually delete buffers ask for
   confirmation before taking action."
  :value t)

(defcommand "Bufed Delete" (p)
  "Delete the buffer.
   Any windows displaying this buffer will display some other buffer."
  "Delete the buffer indicated by the current line.  Any windows displaying this
   buffer will display some other buffer."
  (declare (ignore p))
  (let* ((point (current-point))
	 (buf-info (array-element-from-mark point *bufed-buffers*)))
    (if (and (not (value virtual-buffer-deletion))
	     (or (not (value bufed-delete-confirm))
		 (prompt-for-y-or-n :prompt "Delete buffer? " :default t
				    :must-exist t :default-string "Y")))
	(delete-bufed-buffer (bufed-buffer buf-info))
	(with-writable-buffer (*bufed-buffer*)
	  (setf (bufed-buffer-deleted buf-info) t)
	  (with-mark ((point point))
	    (setf (next-character (line-start point)) #\D))))))

(defcommand "Bufed Undelete" (p)
  "Undelete the buffer.
   Any windows displaying this buffer will display some other buffer."
  "Undelete the buffer.  Any windows displaying this buffer will display some
   other buffer."
  (declare (ignore p))
  (with-writable-buffer (*bufed-buffer*)
    (setf (bufed-buffer-deleted (array-element-from-mark
				 (current-point) *bufed-buffers*))
	  nil)
    (with-mark ((point (current-point)))
      (setf (next-character (line-start point)) #\space))))

(defcommand "Bufed Expunge" (p)
  "Expunge buffers marked for deletion."
  "Expunge buffers marked for deletion."
  (declare (ignore p))
  (expunge-bufed-buffers))

(defcommand "Bufed Quit" (p)
  "Kill the bufed buffer, expunging any buffer marked for deletion."
  "Kill the bufed buffer, expunging any buffer marked for deletion."
  (declare (ignore p))
  (expunge-bufed-buffers)
  (when *bufed-buffer* (delete-buffer-if-possible *bufed-buffer*)))

;;; EXPUNGE-BUFED-BUFFERS deletes the marked buffers in the bufed buffer,
;;; signalling an error if the current buffer is not the bufed buffer.  This
;;; returns t if it deletes some buffer, otherwise nil.  We build a list of
;;; buffers before deleting any because the BUFED-DELETE-HOOK moves elements
;;; around in *bufed-buffers*.
;;;
(defun expunge-bufed-buffers ()
  (unless (eq *bufed-buffer* (current-buffer))
    (editor-error "Not in the Bufed buffer."))
  (let (buffers)
    (dotimes (i *bufed-buffers-end*)
      (let ((buf-info (svref *bufed-buffers* i)))
	(when (bufed-buffer-deleted buf-info)
	  (push (bufed-buffer buf-info) buffers))))
    (if (and buffers
	     (or (not (value bufed-delete-confirm))
		 (prompt-for-y-or-n :prompt "Delete buffers? " :default t
				    :must-exist t :default-string "Y")))
	(dolist (b buffers t) (delete-bufed-buffer b)))))

(defun delete-bufed-buffer (buf)
  (when (and (buffer-modified buf)
	     (prompt-for-y-or-n :prompt (list "~A is modified.  Save it first? "
					      (buffer-name buf))))
    (save-file-command nil buf))
  (delete-buffer-if-possible buf))


(defcommand "Bufed Goto" (p)
  "Change to the buffer."
  "Change to the buffer."
  (declare (ignore p))
  (change-to-buffer
   (bufed-buffer (array-element-from-mark (current-point) *bufed-buffers*))))

(defcommand "Bufed Goto and Quit" (p)
  "Change to the buffer quitting Bufed.
   This supplies a function for \"Generic Pointer Up\" which is a no-op."
  "Change to the buffer quitting Bufed."
  (declare (ignore p))
  (expunge-bufed-buffers)
  (point-to-here-command nil)
  (change-to-buffer
   (bufed-buffer (array-element-from-pointer-pos *bufed-buffers*
		 "No buffer on that line.")))
  (when *bufed-buffer* (delete-buffer-if-possible *bufed-buffer*))
  (supply-generic-pointer-up-function #'(lambda () nil)))

(defcommand "Bufed Save File" (p)
  "Save the buffer."
  "Save the buffer."
  (declare (ignore p))
  (save-file-command
   nil
   (bufed-buffer (array-element-from-mark (current-point) *bufed-buffers*))))

(defcommand "Bufed" (p)
  "Creates a list of buffers in a buffer supporting operations such as deletion
   and selection.  If there already is a bufed buffer, just go to it."
  "Creates a list of buffers in a buffer supporting operations such as deletion
   and selection.  If there already is a bufed buffer, just go to it."
  (declare (ignore p))
  (let ((buf (or *bufed-buffer*
		 (make-buffer "Bufed" :modes '("Bufed")
			      :delete-hook (list #'delete-bufed-buffers)))))

    (unless *bufed-buffer*
      (setf *bufed-buffer* buf)
      (setf *bufed-buffers-end*
	    ;; -1 echo, -1 bufed.
	    (- (length (the list *buffer-list*)) 2))
      (setf *bufed-buffers* (make-array *bufed-buffers-end*))
      (setf (buffer-writable buf) t)
      (with-output-to-mark (s (buffer-point buf))
	(let ((i 0))
	  (do-strings (n b *buffer-names*)
	    (declare (simple-string n))
	    (unless (or (eq b *echo-area-buffer*)
			(eq b buf))
	      (bufed-write-line b n s)
	      (setf (svref *bufed-buffers* i) (make-bufed-buffer b))
	      (incf i)))))
      (setf (buffer-writable buf) nil)
      (setf (buffer-modified buf) nil)
      (let ((fields (buffer-modeline-fields *bufed-buffer*)))
	(setf (cdr (last fields))
	      (list (or (modeline-field :bufed-cmds)
			(make-modeline-field
			 :name :bufed-cmds :width 18
			 :function
			 #'(lambda (buffer window)
			     (declare (ignore buffer window))
			     "  Type ? for help.")))))
	(setf (buffer-modeline-fields *bufed-buffer*) fields))
      (buffer-start (buffer-point buf)))
    (change-to-buffer buf)))

(defun bufed-write-line (buffer name s
		         &optional (buffer-pathname (buffer-pathname buffer)))
  (let ((modified (buffer-modified buffer)))
    (write-string (if modified " *" "  ") s)
    (if buffer-pathname
	(format s "~A  ~A~:[~50T~A~;~]~%"
		(file-namestring buffer-pathname)
		(directory-namestring buffer-pathname)
		(string= (pathname-to-buffer-name buffer-pathname) name)
		name)
	(write-line name s))))


(defcommand "Bufed Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Bufed"))



;;;; Maintenance hooks.

(eval-when (:compile-toplevel :execute)
(defmacro with-bufed-point ((point buffer &optional pos) &rest body)
  (let ((pos (or pos (gensym))))
    `(when (and *bufed-buffers*
		(not (eq *bufed-buffer* ,buffer))
		(not (eq *echo-area-buffer* ,buffer)))
       (let ((,pos (position ,buffer *bufed-buffers* :key #'car
			     :test #'eq :end *bufed-buffers-end*)))
	 (unless ,pos (error "Unknown Bufed buffer."))
	 (let ((,point (buffer-point *bufed-buffer*)))
	   (unless (line-offset (buffer-start ,point) ,pos 0)
	     (error "Bufed buffer not displayed?"))
	   (with-writable-buffer (*bufed-buffer*) ,@body))))))
) ;eval-when


(defun bufed-modified-hook (buffer modified)
  (with-bufed-point (point buffer)
    (setf (next-character (mark-after point)) (if modified #\* #\space))))
;;;
(add-hook buffer-modified-hook 'bufed-modified-hook)

(defun bufed-make-hook (buffer)
  (declare (ignore buffer))
  (when *bufed-buffer* (delete-buffer-if-possible *bufed-buffer*)))
;;;
(add-hook make-buffer-hook 'bufed-make-hook)

(defun bufed-delete-hook (buffer)
  (with-bufed-point (point buffer pos)
    (with-mark ((temp point :left-inserting))
      (line-offset temp 1)
      (delete-region (region point temp)))
    (let ((len-1 (1- *bufed-buffers-end*)))
      (replace *bufed-buffers* *bufed-buffers*
	       :start1 pos :end1 len-1
	       :start2 (1+ pos) :end1 *bufed-buffers-end*)
      (setf (svref *bufed-buffers* len-1) nil)
      (setf *bufed-buffers-end* len-1))))
;;;
(add-hook delete-buffer-hook 'bufed-delete-hook)

(defun bufed-name-hook (buffer name)
  (with-bufed-point (point buffer)
    (with-mark ((temp point :left-inserting))
      (line-offset temp 1)
      (delete-region (region point temp)))
    (with-output-to-mark (s point)
      (bufed-write-line buffer name s))))
;;;
(add-hook buffer-name-hook 'bufed-name-hook)

(defun bufed-pathname-hook (buffer pathname)
  (with-bufed-point (point buffer)
    (with-mark ((temp point :left-inserting))
      (line-offset temp 1)
      (delete-region (region point temp)))
    (with-output-to-mark (s point)
      (bufed-write-line buffer (buffer-name buffer) s pathname))))
;;;
(add-hook buffer-pathname-hook 'bufed-pathname-hook)


;;;; Utilities

(defun array-element-from-pointer-pos (vector &optional
					      (error-msg "Invalid line."))
  (multiple-value-bind (x y window) (last-key-event-cursorpos)
    (declare (ignore x window))
    (when (>= y (length vector))
      (editor-error error-msg))
    (svref vector y)))
