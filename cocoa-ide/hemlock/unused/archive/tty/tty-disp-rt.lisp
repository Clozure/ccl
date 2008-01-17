;;; -*- Log: hemlock.log; Package: hemlock-internals -*-
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
;;;    Written by Bill Chiles.
;;;

(in-package :hemlock-internals)


;;;; Terminal init and exit methods.

(defvar *hemlock-input-handler*)

(defun init-tty-device (device)
  (setf *hemlock-input-handler*
	(system:add-fd-handler 0 :input #'get-editor-tty-input))
  (standard-device-init)
  (device-write-string (tty-device-init-string device))
  (redisplay-all))

(defun exit-tty-device (device)
  (cursor-motion device 0 (1- (tty-device-lines device)))
  ;; Can't call the clear-to-eol method since we don't have a hunk to
  ;; call it on, and you can't count on the bottom hunk being the echo area.
  ;; 
  (if (tty-device-clear-to-eol-string device)
      (device-write-string (tty-device-clear-to-eol-string device))
      (dotimes (i (tty-device-columns device)
		  (cursor-motion device 0 (1- (tty-device-lines device))))
	(tty-write-char #\space)))
  (device-write-string (tty-device-cm-end-string device))
  (when (device-force-output device)
    (funcall (device-force-output device)))
  (when *hemlock-input-handler*
    (system:remove-fd-handler *hemlock-input-handler*)
    (setf *hemlock-input-handler* nil))
  (standard-device-exit))


;;;; Get terminal attributes:

(defvar *terminal-baud-rate* nil)
(declaim (type (or (unsigned-byte 24) null) *terminal-baud-rate*))

;;; GET-TERMINAL-ATTRIBUTES  --  Interface
;;;
;;;    Get terminal attributes from Unix.  Return as values, the lines,
;;; columns and speed.  If any value is inaccessible, return NIL for that
;;; value.  We also sleazily cache the speed in *terminal-baud-rate*, since I
;;; don't want to figure out how to get my hands on the TTY-DEVICE at the place
;;; where I need it.  Currently, there really can only be one TTY anyway, since
;;; the buffer is in a global.
;;;
(defun get-terminal-attributes (&optional (fd 1))
  (alien:with-alien ((winsize (alien:struct unix:winsize))
                     #-(or glibc2 bsd)
		     (sgtty (alien:struct unix:sgttyb))
                     #+bsd ; termios
		     (tios (alien:struct unix:termios)))
    (let ((size-win (unix:unix-ioctl fd unix:TIOCGWINSZ
				     (alien:alien-sap winsize)))
          #-(or glibc2 bsd)
	  (speed-win (unix:unix-ioctl fd unix:TIOCGETP
				      (alien:alien-sap sgtty)))
	  #+bsd
	  (speed-win (unix:unix-tcgetattr fd (alien:alien-sap tios))))
      (flet ((frob (val)
	       (if (and size-win (not (zerop val)))
		   val
		   nil)))
	(values
	 (frob (alien:slot winsize 'unix:ws-row))
	 (frob (alien:slot winsize 'unix:ws-col))
         #-(or glibc2 bsd)
	 (and speed-win
	      (setq *terminal-baud-rate*
		    (svref unix:terminal-speeds
			   (alien:slot sgtty 'unix:sg-ospeed))))
	 #+bsd
	 (and speed-win
	      (setq *terminal-baud-rate* (unix:unix-cfgetospeed tios)))
         #+glibc2
         4800)))))


;;;; Output routines and buffering.

(defconstant redisplay-output-buffer-length 256)

(defvar *redisplay-output-buffer*
  (make-string redisplay-output-buffer-length))
(declaim (simple-string *redisplay-output-buffer*))

(defvar *redisplay-output-buffer-index* 0)
(declaim (fixnum *redisplay-output-buffer-index*))

;;; WRITE-AND-MAYBE-WAIT  --  Internal
;;;
;;;    Write the first Count characters in the redisplay output buffer.  If
;;; *terminal-baud-rate* is set, then sleep for long enough to allow the
;;; written text to be displayed.  We multiply by 10 to get the baud-per-byte
;;; conversion, which assumes 7 character bits + 1 start bit + 2 stop bits, no
;;; parity.
;;;
(defun write-and-maybe-wait (count)
  (declare (fixnum count))
  (unix:unix-write 1 *redisplay-output-buffer* 0 count)
  (let ((speed *terminal-baud-rate*))
    (when speed
      (sleep (/ (* (float count) 10.0) (float speed))))))


;;; TTY-WRITE-STRING blasts the string into the redisplay output buffer.
;;; If the string overflows the buffer, then segments of the string are
;;; blasted into the buffer, dumping the buffer, until the last piece of
;;; the string is stored in the buffer.  The buffer is always dumped if
;;; it is full, even if the last piece of the string just fills the buffer.
;;; 
(defun tty-write-string (string start length)
  (declare (fixnum start length))
  (let ((buffer-space (- redisplay-output-buffer-length
			 *redisplay-output-buffer-index*)))
    (declare (fixnum buffer-space))
    (cond ((<= length buffer-space)
	   (let ((dst-index (+ *redisplay-output-buffer-index* length)))
	     (%primitive byte-blt string start *redisplay-output-buffer*
			 *redisplay-output-buffer-index* dst-index)
	     (cond ((= length buffer-space)
		    (write-and-maybe-wait redisplay-output-buffer-length)
		    (setf *redisplay-output-buffer-index* 0))
		   (t
		    (setf *redisplay-output-buffer-index* dst-index)))))
	  (t
	   (let ((remaining (- length buffer-space)))
	     (declare (fixnum remaining))
	     (loop
	      (%primitive byte-blt string start *redisplay-output-buffer*
			  *redisplay-output-buffer-index*
			  redisplay-output-buffer-length)
	      (write-and-maybe-wait redisplay-output-buffer-length)
	      (when (< remaining redisplay-output-buffer-length)
		(%primitive byte-blt string (+ start buffer-space)
			    *redisplay-output-buffer* 0 remaining)
		(setf *redisplay-output-buffer-index* remaining)
		(return t))
	      (incf start buffer-space)
	      (setf *redisplay-output-buffer-index* 0)
	      (setf buffer-space redisplay-output-buffer-length)
	      (decf remaining redisplay-output-buffer-length)))))))


;;; TTY-WRITE-CHAR stores a character in the redisplay output buffer,
;;; dumping the buffer if it becomes full.
;;; 
(defun tty-write-char (char)
  (setf (schar *redisplay-output-buffer* *redisplay-output-buffer-index*)
	char)
  (incf *redisplay-output-buffer-index*)
  (when (= *redisplay-output-buffer-index* redisplay-output-buffer-length)
    (write-and-maybe-wait redisplay-output-buffer-length)
    (setf *redisplay-output-buffer-index* 0)))


;;; TTY-FORCE-OUTPUT dumps the redisplay output buffer.  This is called
;;; out of terminal device structures in multiple places -- the device
;;; exit method, random typeout methods, out of tty-hunk-stream methods,
;;; after calls to REDISPLAY or REDISPLAY-ALL.
;;; 
(defun tty-force-output ()
  (unless (zerop *redisplay-output-buffer-index*)
    (write-and-maybe-wait *redisplay-output-buffer-index*)
    (setf *redisplay-output-buffer-index* 0)))


;;; TTY-FINISH-OUTPUT simply dumps output.
;;;
(defun tty-finish-output (device window)
  (declare (ignore window))
  (let ((force-output (device-force-output device)))
    (when force-output
      (funcall force-output))))



;;;; Screen image line hacks.

(defmacro replace-si-line (dst-string src-string src-start dst-start dst-end)
  `(%primitive byte-blt ,src-string ,src-start ,dst-string ,dst-start ,dst-end))
