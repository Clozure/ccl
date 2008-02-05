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
;;; Registers for holding text and positions.
;;;
;;; Written by Dave Touretzky.
;;; Modified by Bill Chiles for Hemlock consistency.
;;;
(in-package :hemlock)



;;;; Registers implementation.

;;; Registers are named by characters.  Each register refers to a mark or
;;; a cons of a region and the buffer it came from.
;;; 
(defvar *registers* (make-hash-table))

(defun register-count ()
  (hash-table-count *registers*))

(defun register-value (reg-name)
  (gethash reg-name *registers*))

(defsetf register-value (reg-name) (new-value)
  (let ((name (gensym))
	(value (gensym))
	(old-value (gensym)))
    `(let* ((,name ,reg-name)
	    (,value ,new-value)
	    (,old-value (gethash ,name *registers*)))
       (when (and ,old-value (markp ,old-value))
	 (delete-mark ,old-value))
       (setf (gethash ,name *registers*) ,value))))

(defun prompt-for-register (&optional (prompt "Register: ") must-exist)
  (let ((reg-name (prompt-for-key-event :prompt prompt)))
    (unless (or (not must-exist) (gethash reg-name *registers*))
      (editor-error "Register ~A is empty." reg-name))
    reg-name))

     
(defmacro do-registers ((name value &optional sorted) &rest body)
  (if sorted
      (let ((sorted-regs (gensym))
	    (reg (gensym)))
	`(let ((,sorted-regs nil))
	   (declare (list ,sorted-regs))
	   (maphash #'(lambda (,name ,value)
			(push (cons ,name ,value) ,sorted-regs))
		    *registers*)
	   (setf ,sorted-regs (sort ,sorted-regs #'char-lessp :key #'car))
	   (dolist (,reg ,sorted-regs)
	     (let ((,name (car ,reg))
		   (,value (cdr ,reg)))
	       ,@body))))
      `(maphash #'(lambda (,name ,value)
		    ,@body)
		*registers*)))


;;; Hook to clean things up if a buffer is deleted while registers point to it.
;;; 
(defun flush-reg-references-to-deleted-buffer (buffer)
  (do-registers (name value)
    (etypecase value
      (mark (when (eq (mark-buffer value) buffer)
	      (free-register name)))
      (cons (free-register-value value buffer)))))
;;;
(add-hook delete-buffer-hook 'flush-reg-references-to-deleted-buffer)


(defun free-register (name)
  (let ((value (register-value name)))
    (when value (free-register-value value)))
  (remhash name *registers*))

(defun free-register-value (value &optional buffer)
  (etypecase value
    (mark
     (when (or (not buffer) (eq (mark-buffer value) buffer))
       (delete-mark value)))
    (cons
     (when (and buffer (eq (cdr value) buffer))
       (setf (cdr value) nil)))))



;;;; Commands.

;;; These commands all stash marks and regions with marks that point into some
;;; buffer, and they assume that the register values have the same property.
;;; 

(defcommand "Save Position" (p)
  "Saves the current location in a register.  Prompts for register name."
  (declare (ignore p))
  (let ((reg-name (prompt-for-register)))
    (setf (register-value reg-name)
	  (copy-mark (current-point) :left-inserting))))

(defcommand "Jump to Saved Position" (p)
  "Moves the point to a location previously saved in a register."
  (declare (ignore p))
  (let* ((reg-name (prompt-for-register "Jump to Register: " t))
	 (val (register-value reg-name)))
    (unless (markp val)
      (editor-error "Register ~A does not hold a location." reg-name))
    (unless (eq (mark-buffer val) (current-buffer))
      (hemlock-ext:raise-buffer-view (mark-buffer val)
                                     #'(lambda ()
                                         (move-mark (current-point) val))))))

(defcommand "Kill Register" (p)
  "Kill a register.  Prompts for the name."
  (declare (ignore p))
  (free-register (prompt-for-register "Register to kill: ")))

(defcommand "List Registers" (p)
  "Lists all registers in a pop-up window."
  "Lists all registers in a pop-up window."
  (declare (ignore p))
  (with-pop-up-display (f :height (* 2 (register-count)))
    (do-registers (name val :sorted)
      (write-string "Reg " f)
      (write-string (pretty-key-string name) f)
      (write-string ":  " f)
      (etypecase val
	(mark
	 (let* ((line (mark-line val))
		(buff (line-buffer line))
		(len (line-length line)))
	   (format f "Line ~S, col ~S in buffer ~A~%   ~A~:[~;...~]~%"
		   (count-lines (region (buffer-start-mark buff) val))
		   (mark-column val)
		   (buffer-name buff)
		   (subseq (line-string line) 0 (min 61 len))
		   (> len 60))))
	(cons
	 (let* ((str (region-to-string (car val)))
		(nl (position #\newline str :test #'char=))
		(len (length str))
		(buff (cdr val)))
	   (declare (simple-string str))
	   (format f "Text~@[ from buffer ~A~]~%   ~A~:[~;...~]~%"
		   (if buff (buffer-name buff))
		   (subseq str 0 (if nl (min 61 len nl) (min 61 len)))
		   (> len 60))))))))

(defcommand "Put Register" (p)
  "Copies a region into a register.  Prompts for register name."
  "Copies a region into a register.  Prompts for register name."
  (declare (ignore p))
  (let ((region (current-region)))
    ;; Bind the region before prompting in case the region isn't active.
    (setf (register-value (prompt-for-register))
	  (cons (copy-region region) (current-buffer)))))

(defcommand "Get Register" (p)
  "Copies a region from a register to the current point."
  "Copies a region from a register to the current point."
  (declare (ignore p))
  (let* ((reg-name (prompt-for-register "Register from which to get text: " t))
	 (val (register-value reg-name)))
    (unless (and (consp val) (regionp (car val)))
      (editor-error "Register ~A does not hold a region." reg-name))
    (let ((point (current-point)))
      (push-new-buffer-mark point)
      (insert-region point (car val))))
  (setf (last-command-type) :ephemerally-active))
