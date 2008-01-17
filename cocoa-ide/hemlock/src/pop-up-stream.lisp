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
;;; This file contatins the stream operations for pop-up-displays.
;;;
;;; Written by Blaine Burks.
;;;

(in-package :hemlock-internals)



;;;; Line-buffered Stream Methods.

;; ###GB we want a more optimized interface

(defmethod stream-write-char ((stream random-typeout-stream) char)
  (with-slots (line-buffered-p) stream
    (cond (line-buffered-p
           (insert-character (random-typeout-stream-mark stream) char)
           (when (and (char= char #\newline)
                      (not (random-typeout-stream-no-prompt stream)))
             (funcall (device-random-typeout-line-more
                       (device-hunk-device
                        (window-hunk (random-typeout-stream-window stream))))
                      stream 1)))
          (t
           (insert-character (random-typeout-stream-mark stream) char)))))             

(defmethod stream-write-string ((stream random-typeout-stream) string &optional start end)
  (setf start (or start 0))
  (setf end (or end (length string)))
  (unless (and (eql start 0) (eql end (length string)))
    (setq string (subseq string start end)))
  (with-slots (line-buffered-p) stream
    (cond (line-buffered-p
           (insert-string (random-typeout-stream-mark stream) string)
           (unless (random-typeout-stream-no-prompt stream)
             (let ((count (count #\newline string)))
               (when count
                 (funcall (device-random-typeout-line-more
                           (device-hunk-device
                            (window-hunk (random-typeout-stream-window stream))))
                          stream count)))))
          (t
           (insert-string (random-typeout-stream-mark stream) string)))))

(defmethod stream-finish-output ((stream random-typeout-stream))
  (with-slots (line-buffered-p) stream
    (cond (line-buffered-p
           (random-typeout-redisplay (random-typeout-stream-window stream)))
          (t
           nil))))

(defmethod stream-force-output ((stream random-typeout-stream))
  (stream-finish-output stream))

(defmethod stream-line-column ((stream random-typeout-stream))
  (mark-charpos (random-typeout-stream-mark stream)))

;;; Bitmap line-buffered support.

;;; UPDATE-BITMAP-LINE-BUFFERED-STREAM is called when anything is written to
;;; a line-buffered-random-typeout-stream on the bitmap.  It does a lot of
;;; checking to make sure that strings of characters longer than the width of
;;; the window don't screw us.  The code is a little wierd, so a brief
;;; explanation is below.
;;;
;;; The more-mark is how we tell when we will next need to more.  Each time
;;; we do a more-prompt, we point the mark at the last visible character in
;;; the random typeout window.  That way, when the mark is no longer
;;; DISPLAYED-P, we know it's time to do another more prompt.
;;;
;;; If the buffer-end-mark is DISPLAYED-P, then we return, only redisplaying
;;; if there was at least one newline in the last batch of output.  If we
;;; haven't done a more prompt yet (indicated by a value of T for
;;; first-more-p), then since we know the end of the buffer isn't visible, we
;;; need to do a more-prompt.  If neither of the first two tests returns T,
;;; then we can only need to do a more-prompt if our more-mark has scrolled
;;; off the top of the screen.  If it hasn't, everything is peechy-keen, so
;;; we scroll the screen one line and redisplay.
;;;
(defun update-bitmap-line-buffered-stream (stream newline-count)
  (let* ((window (random-typeout-stream-window stream))
	 (count 0))
    (when (plusp newline-count) (random-typeout-redisplay window))
    (loop
      (cond ((no-text-past-bottom-p window)
	     (return))
	    ((or (random-typeout-stream-first-more-p stream)
		 (not (displayed-p (random-typeout-stream-more-mark stream)
				   window)))
	     (do-bitmap-more-prompt stream)
	     (return))
	    (t
	     (scroll-window window 1)
	     (random-typeout-redisplay window)))
      (when (= (incf count) newline-count) (return)))))

;;; NO-TEXT-PAST-BOTTOM-P determines whether there is text left to be displayed
;;; in the random-typeout window.  It does this by first making sure there is a
;;; line past the WINDOW-DISPLAY-END of the window.  If there is, this line
;;; must be empty, and BUFFER-END-MARK must be on this line.  The final test is
;;; that the window-end is displayed within the window.  If it is not, then the
;;; last line wraps past the end of the window, and there is text past the
;;; bottom.
;;;
;;; Win-end is bound after the call to DISPLAYED-P because it updates the
;;; window's image moving WINDOW-DISPLAY-END.  We want this updated value for
;;; the display end.
;;;
(defun no-text-past-bottom-p (window)
  (let* ((window-end (window-display-end window))
	 (window-end-displayed-p (displayed-p window-end window)))
    (with-mark ((win-end window-end))
      (let ((one-after-end (line-offset win-end 1)))
	(if one-after-end
	    (and (empty-line-p win-end)
		 (same-line-p win-end (buffer-end-mark (window-buffer window)))
		 window-end-displayed-p)
	    window-end-displayed-p)))))

(defun reset-more-mark (stream)
  (let* ((window (random-typeout-stream-window stream))
	 (more-mark (random-typeout-stream-more-mark stream))
	 (end (window-display-end window)))
    (move-mark more-mark end)
    (unless (displayed-p end window) (character-offset more-mark -1))))





