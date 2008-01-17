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
;;; Some stuff to make streams that write out to terminal hunks.
;;;
;;; Written by Bill Chiles.
;;;
;;; This code is VERY similar to that in Pane-Stream.Lisp.  The biggest
;;; (if only) difference is in TTY-HUNK-STREAM-NEWLINE.
;;;

(in-package "HEMLOCK-INTERNALS")



;;;; Constants

(defconstant tty-hunk-width-limit 200)



;;;; Structures

;;; Tty-Hunk streams are inherently buffered by line.

(defstruct (stream-hunk (:print-function %print-device-hunk)
			(:include tty-hunk))
  (width 0 :type fixnum)
  (point-x 0 :type fixnum)
  (point-y 0 :type fixnum)
  (buffer "" :type simple-string))

(defstruct (tty-hunk-output-stream (:include sys:lisp-stream
					     (out #'hunk-out)
					     (sout #'hunk-sout)
					     (misc #'hunk-misc))
				   (:constructor
				    make-tty-hunk-output-stream ()))
  (hunk (make-stream-hunk :buffer (make-string tty-hunk-width-limit))))



;;;; Tty-hunk-output-stream methods

;;; HUNK-OUT puts a character into a hunk-stream buffer.  If the character
;;; makes the current line wrap, or if the character is a newline, then
;;; call TTY-HUNK-NEWLINE.
;;;
(defun hunk-out (stream character)
  (let* ((hunk (tty-hunk-output-stream-hunk stream))
	 (x (stream-hunk-point-x hunk)))
    (declare (fixnum x))
    (cond ((char= character #\newline)
	   (tty-hunk-stream-newline hunk)
	   (return-from hunk-out nil))
	  ((= x (the fixnum (stream-hunk-width hunk)))
	   (setf x 0)
	   (tty-hunk-stream-newline hunk)))
    (setf (schar (stream-hunk-buffer hunk) x) character)
    (incf (stream-hunk-point-x hunk))))

;;; HUNK-MISC, when finishing or forcing output, only needs to blast
;;; out the buffer at y from 0 to x since these streams are inherently
;;; line buffered.  Currently, these characters will be blasted out again
;;; since there isn't a separate buffer index from point-x, and we can't
;;; set point-x to zero since we haven't a newline.
;;; 
(defun hunk-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    (:charpos
     (let ((hunk (tty-hunk-output-stream-hunk stream)))
       (values (stream-hunk-point-x hunk) (stream-hunk-point-y hunk))))
    ((:finish-output :force-output)
     (let* ((hunk (tty-hunk-output-stream-hunk stream))
	    (device (device-hunk-device hunk)))
       (funcall (tty-device-display-string device)
		hunk 0 (stream-hunk-point-y hunk) (stream-hunk-buffer hunk)
		0 (stream-hunk-point-x hunk))
       (when (device-force-output device)
	 (funcall (device-force-output device)))))
    (:line-length
     (stream-hunk-width (tty-hunk-output-stream-hunk stream)))
    (:element-type 'base-char)))

;;; HUNK-SOUT writes a byte-blt's a string to a hunk-stream's buffer.
;;; When newlines are found, recurse on the substrings delimited by start,
;;; end, and newlines.  If the string causes line wrapping, then we break
;;; the string up into line-at-a-time segments calling TTY-HUNK-STREAM-NEWLINE.
;;; 
(defun hunk-sout (stream string start end)
  (declare (fixnum start end))
  (let* ((hunk (tty-hunk-output-stream-hunk stream))
	 (buffer (stream-hunk-buffer hunk))
	 (x (stream-hunk-point-x hunk))
	 (dst-end (+ x (- end start)))
	 (width (stream-hunk-width hunk))
	 (newlinep (%sp-find-character string start end #\newline)))
    (declare (fixnum x dst-end width))
    (cond (newlinep
	   (let ((previous start) (current newlinep))
	     (declare (fixnum previous))
	     (loop (when (null current)
		     (hunk-sout stream string previous end)
		     (return))
		   (hunk-sout stream string previous current)
		   (tty-hunk-stream-newline hunk)
		   (setf previous (the fixnum (1+ (the fixnum current))))
		   (setf current
			 (%sp-find-character string previous end #\newline)))))
	  ((> dst-end width)
	   (let ((new-start (+ start (- width x))))
	     (declare (fixnum new-start))
	     (%primitive byte-blt string start buffer x width)
	     (setf (stream-hunk-point-x hunk) width)
	     (tty-hunk-stream-newline hunk)
	     (do ((idx (+ new-start width) (+ idx width))
		  (prev new-start idx))
		 ((>= idx end)
		  (let ((dst-end (- end prev)))
		    (%primitive byte-blt string prev buffer 0 dst-end)
		    (setf (stream-hunk-point-x hunk) dst-end)))
	       (declare (fixnum prev idx))
	       (%primitive byte-blt string prev buffer 0 width)
	       (setf (stream-hunk-point-x hunk) width)
	       (tty-hunk-stream-newline hunk))))
	  (t
	   (%primitive byte-blt string start buffer x dst-end)
	   (setf (stream-hunk-point-x hunk) dst-end)))))

;;; TTY-HUNK-STREAM-NEWLINE is the only place we display lines and affect
;;; point-y.  We also blast out the buffer in HUNK-MISC.
;;; 
(defun tty-hunk-stream-newline (hunk)
  (let* ((device (device-hunk-device hunk))
	 (force-output-fun (device-force-output device))
	 (y (stream-hunk-point-y hunk)))
    (declare (fixnum y))
    (when (= y (the fixnum (device-hunk-position hunk)))
      (funcall (tty-device-display-string device) hunk 0 y "--More--" 0 8)
      (when force-output-fun (funcall force-output-fun))
      (wait-for-more)
      (funcall (tty-device-clear-to-eow device) hunk 0 0)
      (setf (stream-hunk-point-y hunk) 0)
      (setf y 0))
    (funcall (tty-device-display-string device)
	     hunk 0 y (stream-hunk-buffer hunk) 0 (stream-hunk-point-x hunk))
    (when force-output-fun (funcall force-output-fun))
    (setf (stream-hunk-point-x hunk) 0)
    (incf (stream-hunk-point-y hunk))))
