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
;;; Some stuff to make streams that write out on bitmap hunks.
;;;
;;; Written by Rob MacLachlan.
;;; Modified by Bill Chiles to run under X on the IBM RT.
;;;
(in-package "HEMLOCK-INTERNALS")


;;; These streams have an associated bitmap-hunk that is used for its
;;; font-family, foreground and background color, and X window pointer.
;;; The hunk need not be associated with any Hemlock window, and the low
;;; level painting routines that use hunk dimensions are not used for
;;; output.  Only BITMAP-HUNK-WRITE-STRING is used.  The hunk is not
;;; registered for any event service, so resizing the associated X window
;;; does not invoke the exposed/changed handler in Bit-Screen.Lisp; also, the
;;; hunk's input and changed handler slots are not set.
;;;
(defstruct (bitmap-hunk-output-stream (:include sys:lisp-stream
						(out #'bitmap-hunk-out)
						(sout #'bitmap-hunk-sout)
						(misc #'bitmap-hunk-misc))
				      (:constructor
				       make-bitmap-hunk-output-stream (hunk)))
  hunk			; bitmap-hunk we display on.
  (cursor-x 0)		; Character position of output cursor.
  (cursor-y 0)
  (buffer (make-string hunk-width-limit) :type simple-string)
  (old-bottom 0))	; # of lines of scrolling before next "--More--" prompt.

;;; Bitmap-Hunk-Stream-Newline  --  Internal
;;;
;;;    Flush the stream's output buffer and then move the cursor down
;;; or scroll the window up if there is no room left.
;;;
(defun bitmap-hunk-stream-newline (stream)
  (let* ((hunk (bitmap-hunk-output-stream-hunk stream))
	 (height (bitmap-hunk-char-height hunk))
	 (y (bitmap-hunk-output-stream-cursor-y stream)))
    (when (zerop (bitmap-hunk-output-stream-old-bottom stream))
      (hunk-write-string hunk 0 y "--More--" 0 8)
      (let ((device (device-hunk-device hunk)))
	(when (device-force-output device)
	  (funcall (device-force-output device))))
      (wait-for-more)
      (hunk-clear-lines hunk y 1)
      (setf (bitmap-hunk-output-stream-old-bottom stream) (1- height)))
    (hunk-write-string hunk 0 y (bitmap-hunk-output-stream-buffer stream) 0 
		       (bitmap-hunk-output-stream-cursor-x stream))
    (setf (bitmap-hunk-output-stream-cursor-x stream) 0)
    (decf (bitmap-hunk-output-stream-old-bottom stream))
    (incf y)
    (when (= y height)
      (decf y)
      (hunk-copy-lines hunk 1 0 y)
      (hunk-clear-lines hunk y 1))
    (setf (bitmap-hunk-output-stream-cursor-y stream) y)))

;;; Bitmap-Hunk-Misc  --  Internal
;;;
;;;    This is the misc method for bitmap-hunk-output-streams.  It just
;;; writes out the contents of the buffer, and does the element type.
;;;
(defun bitmap-hunk-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    (:charpos
     (values (bitmap-hunk-output-stream-cursor-x stream)
	     (bitmap-hunk-output-stream-cursor-y stream)))
    ((:finish-output :force-output)
     (hunk-write-string (bitmap-hunk-output-stream-hunk stream)
			0 (bitmap-hunk-output-stream-cursor-y stream) 
			(bitmap-hunk-output-stream-buffer stream) 0
			(bitmap-hunk-output-stream-cursor-x stream))
     (let ((device (device-hunk-device (bitmap-hunk-output-stream-hunk stream))))
       (when (device-force-output device)
	 (funcall (device-force-output device)))))
    (:line-length
     (bitmap-hunk-char-width (bitmap-hunk-output-stream-hunk stream)))
    (:element-type 'base-char)))


;;; Bitmap-Hunk-Out  --  Internal
;;;
;;;    Throw a character in a bitmap-hunk-stream's buffer.  If we wrap or hit a 
;;; newline then call bitmap-hunk-stream-newline.
;;;
(defun bitmap-hunk-out (stream character)
  (let ((hunk (bitmap-hunk-output-stream-hunk stream))
	(x (bitmap-hunk-output-stream-cursor-x stream)))
    (cond ((char= character #\newline)
	   (bitmap-hunk-stream-newline stream)
	   (return-from bitmap-hunk-out nil))
	  ((= x (bitmap-hunk-char-width hunk))
	   (setq x 0)
	   (bitmap-hunk-stream-newline stream)))
    (setf (schar (bitmap-hunk-output-stream-buffer stream) x) character)
    (setf (bitmap-hunk-output-stream-cursor-x stream) (1+ x))))


;;; Bitmap-Hunk-Sout  --  Internal
;;;
;;;    Write a string out to a bitmap-hunk, calling ourself recursively if the
;;; string contains newlines.
;;;
(defun bitmap-hunk-sout (stream string start end)
  (let* ((hunk (bitmap-hunk-output-stream-hunk stream))
	 (buffer (bitmap-hunk-output-stream-buffer stream))
	 (x (bitmap-hunk-output-stream-cursor-x stream))
	 (dst-end (+ x (- end start)))
	 (width (bitmap-hunk-char-width hunk)))
    (cond ((%primitive find-character string start end #\newline)
	   (do ((current (%primitive find-character string start end #\newline)
			 (%primitive find-character string (1+ current)
				     end #\newline))
		(previous start (1+ current)))
	       ((null current)
		(bitmap-hunk-sout stream string previous end))
	     (bitmap-hunk-sout stream string previous current)
	     (bitmap-hunk-stream-newline stream)))
	  ((> dst-end width)
	   (let ((new-start (+ start (- width x))))
	     (%primitive byte-blt string start buffer x width)
	     (setf (bitmap-hunk-output-stream-cursor-x stream) width)
	     (bitmap-hunk-stream-newline stream)
	     (do ((idx (+ new-start width) (+ idx width))
		  (prev new-start idx))
		 ((>= idx end)
		  (let ((dst-end (- end prev)))
		    (%primitive byte-blt string prev buffer 0 dst-end)
		    (setf (bitmap-hunk-output-stream-cursor-x stream) dst-end)))
	       (%primitive byte-blt string prev buffer 0 width)
	       (setf (bitmap-hunk-output-stream-cursor-x stream) width)
	       (bitmap-hunk-stream-newline stream))))
	  (t
	   (%primitive byte-blt string start buffer x dst-end)
	   (setf (bitmap-hunk-output-stream-cursor-x stream) dst-end)))))
