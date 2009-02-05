;;; -*- Package: Hemlock; Log: hemlock.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(hemlock-ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;; This file contains code for processing input to and output from slaves
;;; using typescript streams.  It maintains the stuff that hacks on the
;;; typescript buffer and maintains its state.
;;;
;;; Written by William Lott.
;;;

(in-package :hemlock)


(defhvar "Input Wait Alarm"
  "When non-nil, the user is informed when a typescript buffer goes into
   an input wait, and it is not visible.  Legal values are :message,
   :loud-message (the default), and nil."
  :value :loud-message)



;;;; Structures.

(defstruct (ts-data
	    (:print-function
	     (lambda (ts s d)
	       (declare (ignore ts d))
	       (write-string "#<TS Data>" s)))
	    (:constructor
	     make-ts-data (buffer
			   &aux
			   (fill-mark (copy-mark (buffer-end-mark buffer)
						 :right-inserting)))))
  buffer		      ; The buffer we are in
  stream		      ; Stream in the slave.
  wire			      ; Wire to slave
  server		      ; Server info struct.
  fill-mark		      ; Mark where output goes.  This is actually the
			      ;   "Buffer Input Mark" which is :right-inserting,
			      ;   and we make sure it is :left-inserting for
			      ;   inserting output.
  )


;;;; Output routines.

;;; TS-BUFFER-OUTPUT-STRING --- internal interface.
;;;
;;; Called by the slave to output stuff in the typescript.  Can also be called
;;; by other random parts of hemlock when they want to output stuff to the
;;; buffer.  Since this is called for value from the slave, we have to be
;;; careful about what values we return, so the result can be sent back.  It is
;;; called for value only as a synchronization thing.
;;;
;;; Whenever the output is gratuitous, we want it to go behind the prompt.
;;; When it's gratuitous, and we're not at the line-start, then we can output
;;; it normally, but we also make sure we end the output in a newline for
;;; visibility's sake.
;;;
(defun ts-buffer-output-string (ts string &optional gratuitous-p)
  "Outputs STRING to the typescript described with TS. The output is inserted
   before the fill-mark and the current input."
  (when (hemlock.wire:remote-object-p ts)
    (setf ts (hemlock.wire:remote-object-value ts)))
  (hemlock-ext:without-interrupts
    (let ((mark (ts-data-fill-mark ts)))
      (cond ((and gratuitous-p (not (start-line-p mark)))
	     (with-mark ((m mark :left-inserting))
	       (line-start m)
	       (insert-string m string)
	       (unless (start-line-p m)
		 (insert-character m #\newline))))
	    (t
	     (setf (mark-kind mark) :left-inserting)
	     (insert-string mark string)
	     (when (and gratuitous-p (not (start-line-p mark)))
	       (insert-character mark #\newline))
	     (setf (mark-kind mark) :right-inserting)))))
  (values))

;;; TS-BUFFER-FINISH-OUTPUT --- internal interface.
;;;
;;; Redisplays the windows. Used by ts-stream in order to finish-output.
;;;
(defun ts-buffer-finish-output (ts)
  (declare (ignore ts))
  (redisplay)
  nil)

;;; TS-BUFFER-CHARPOS --- internal interface.
;;;
;;; Used by ts-stream in order to find the charpos.
;;; 
(defun ts-buffer-charpos (ts)
  (mark-charpos (ts-data-fill-mark (if (hemlock.wire:remote-object-p ts)
				       (hemlock.wire:remote-object-value ts)
				       ts))))

;;; TS-BUFFER-LINE-LENGTH --- internal interface.
;;;
;;; Used by ts-stream to find out the line length.  Returns the width of the
;;; first window, or 80 if there are no windows.
;;; 
(defun ts-buffer-line-length (ts)
  (let* ((ts (if (hemlock.wire:remote-object-p ts)
		 (hemlock.wire:remote-object-value ts)
		ts))
	 (window (car (buffer-windows (ts-data-buffer ts)))))
    (if window
	(window-width window)
	80))) ; Seems like a good number to me.


;;;; Input routines

(defun ts-buffer-ask-for-input (remote)
  (let* ((ts (hemlock.wire:remote-object-value remote))
	 (buffer (ts-data-buffer ts)))
    (unless (buffer-windows buffer)
      (let ((input-wait-alarm
	     (if (hemlock-bound-p 'input-wait-alarm
				  :buffer buffer)
	       (variable-value 'input-wait-alarm
			       :buffer buffer)
	       (variable-value 'input-wait-alarm
			       :global))))
	(when input-wait-alarm
	  (when (eq input-wait-alarm :loud-message)
	    (beep))
	  (message "Waiting for input in buffer ~A."
		   (buffer-name buffer))))))
  nil)

(defun ts-buffer-clear-input (ts)
  (let* ((ts (if (hemlock.wire:remote-object-p ts)
		 (hemlock.wire:remote-object-value ts)
		 ts))
	 (buffer (ts-data-buffer ts))
	 (mark (ts-data-fill-mark ts)))
    (unless (mark= mark (buffer-end-mark buffer))
      (with-mark ((start mark))
	(line-start start)
	(let ((prompt (region-to-string (region start mark)))
	      (end (buffer-end-mark buffer)))
	  (unless (zerop (mark-charpos end))
	    (insert-character end #\Newline))
	  (insert-string end "[Input Cleared]")
	  (insert-character end #\Newline)
	  (insert-string end prompt)
	  (move-mark mark end)))))
  nil)

(defun ts-buffer-set-stream (ts stream)
  (let ((ts (if (hemlock.wire:remote-object-p ts)
		(hemlock.wire:remote-object-value ts)
		ts)))
    (setf (ts-data-stream ts) stream)
    (hemlock.wire:remote (ts-data-wire ts)
      (ts-stream-set-line-length stream (ts-buffer-line-length ts))))
  nil)


;;;; Typescript mode.

(defun setup-typescript (buffer)
  (let ((ts (make-ts-data buffer)))
    (defhvar "Current Package"
      "The package used for evaluation of Lisp in this buffer."
      :buffer buffer
      :value nil)

    (defhvar "Typescript Data"
      "The ts-data structure for this buffer"
      :buffer buffer
      :value ts)
    
    (defhvar "Buffer Input Mark"
      "Beginning of typescript input in this buffer."
      :value (ts-data-fill-mark ts)
      :buffer buffer)
    
    (defhvar "Interactive History"
      "A ring of the regions input to the Hemlock typescript."
      :buffer buffer
      :value (make-ring (value interactive-history-length)))
    
    (defhvar "Interactive Pointer"
      "Pointer into the Hemlock typescript input history."
      :buffer buffer
      :value 0)
    
    (defhvar "Searching Interactive Pointer"
      "Pointer into \"Interactive History\"."
      :buffer buffer
      :value 0)))

(defmode "Typescript"
  :setup-function #'setup-typescript
  :documentation "The Typescript mode is used to interact with slave lisps.")


;;; TYPESCRIPTIFY-BUFFER -- Internal interface.
;;;
;;; Buffer creation code for eval server connections calls this to setup a
;;; typescript buffer, tie things together, and make some local Hemlock
;;; variables.
;;;
(defun typescriptify-buffer (buffer server wire)
  (setf (buffer-minor-mode buffer "Typescript") t)
  (let ((info (variable-value 'typescript-data :buffer buffer)))
    (setf (ts-data-server info) server)
    (setf (ts-data-wire info) wire)
    (defhvar "Server Info"
      "Server-info structure for this buffer."
      :buffer buffer :value server)
    (defhvar "Current Eval Server"
      "The Server-Info object for the server currently used for evaluation and
       compilation."
      :buffer buffer :value server)
    info))

(defun ts-buffer-wire-died (ts)
  (setf (ts-data-stream ts) nil)
  (setf (ts-data-wire ts) nil)
  (buffer-end (ts-data-fill-mark ts) (ts-data-buffer ts))
  (ts-buffer-output-string ts (format nil "~%~%Slave died!~%")))

(defun unwedge-typescript-buffer ()
  (typescript-slave-to-top-level-command nil)
  (buffer-end (current-point) (current-buffer)))

(defhvar "Unwedge Interactive Input Fun"
  "Function to call when input is confirmed, but the point is not past the
   input mark."
  :value #'unwedge-typescript-buffer
  :mode "Typescript")

(defhvar "Unwedge Interactive Input String"
  "String to add to \"Point not past input mark.  \" explaining what will
   happen if the the user chooses to be unwedged."
  :value "Cause the slave to throw to the top level? "
  :mode "Typescript")

;;; TYPESCRIPT-DATA-OR-LOSE -- internal
;;;
;;; Return the typescript-data for the current buffer, or die trying.
;;; 
(defun typescript-data-or-lose ()
  (if (hemlock-bound-p 'typescript-data)
      (let ((ts (value typescript-data)))
	(if ts
	    ts
	    (editor-error "Can't find the typescript data?")))
      (editor-error "Not in a typescript buffer.")))

(defcommand "Confirm Typescript Input" (p)
  "Send the current input to the slave typescript."
  "Send the current input to the slave typescript."
  (declare (ignore p))
  (let ((ts (typescript-data-or-lose)))
    (let ((input (get-interactive-input)))
      (when input
	(let ((string (region-to-string input)))
	  (declare (simple-string string))
	  (insert-character (current-point) #\NewLine)
	  (hemlock.wire:remote (ts-data-wire ts)
	    (ts-stream-accept-input (ts-data-stream ts)
				    (concatenate 'simple-string
						 string
						 (string #\newline))))
	  (hemlock.wire:wire-force-output (ts-data-wire ts))
	  (buffer-end (ts-data-fill-mark ts)
		      (ts-data-buffer ts)))))))
  
(defcommand "Typescript Slave Break" (p)
  "Interrupt the slave Lisp process associated with this interactive buffer,
   causing it to invoke BREAK."
  "Interrupt the slave Lisp process associated with this interactive buffer,
   causing it to invoke BREAK."
  (declare (ignore p))
  (send-oob-to-slave "B"))

(defcommand "Typescript Slave to Top Level" (p)
  "Interrupt the slave Lisp process associated with this interactive buffer,
   causing it to throw to the top level REP loop."
  "Interrupt the slave Lisp process associated with this interactive buffer,
   causing it to throw to the top level REP loop."
  (declare (ignore p))
  (send-oob-to-slave "T"))

(defcommand "Typescript Slave Status" (p)
  "Interrupt the slave and cause it to print status information."
  "Interrupt the slave and cause it to print status information."
  (declare (ignore p))
  (send-oob-to-slave "S"))

#+NIL
(defun send-oob-to-slave (string)
  (let* ((ts (typescript-data-or-lose))
	 (wire (ts-data-wire ts))
	 (socket (hemlock.wire:wire-fd wire)))
    (unless socket
      (editor-error "The slave is no longer alive."))
    (error "SEND-OOB-TO-SLAVE seeks an implementation.")
    #+NIL
    (hemlock-ext:send-character-out-of-band socket (schar string 0))))
