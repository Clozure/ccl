;;; -*- Package: hemlock; Log: hemlock.log -*-
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
;;;   This is an italicized comment.

(in-package :hemlock)

(defun delete-line-italic-marks (line)
  (dolist (m (hi::line-marks line))
    (when (and (hi::fast-font-mark-p m)
	       (eql (hi::font-mark-font m) 1))
      (delete-font-mark m))))

(defun set-comment-font (region font)
  (do ((line (mark-line (region-start region))
	     (line-next line))
       (end (line-next (mark-line (region-end region)))))
      ((eq line end))
    (delete-line-italic-marks line)
    (let ((pos (position #\; (the simple-string (line-string line)))))
      (when pos
	(font-mark line pos font :left-inserting)))))

(defun delete-italic-marks-region (region)
  (do ((line (mark-line (region-start region))
	     (line-next line))
       (end (line-next (mark-line (region-end region)))))
      ((eq line end))
    (delete-line-italic-marks line)))


(defmode "Italic"
  :setup-function
  #'(lambda (buffer) (set-comment-font (buffer-region buffer) 1))
  :cleanup-function
  #'(lambda (buffer) (delete-italic-marks-region (buffer-region buffer))))

(define-file-option "Italicize Comments" (buffer value)
  (declare (ignore value))
  (setf (buffer-minor-mode buffer "Italic") t))

(defcommand "Italic Comment Mode" (p)
  "Toggle \"Italic\" mode in the current buffer.  When in \"Italic\" mode,
  semicolon comments are displayed in an italic font."
  "Toggle \"Italic\" mode in the current buffer."
  (declare (ignore p))
  (setf (buffer-minor-mode (current-buffer) "Italic")
	(not (buffer-minor-mode (current-buffer) "Italic"))))


(defcommand "Start Italic Comment" (p)
  "Italicize the text in this comment."
  "Italicize the text in this comment."
  (declare (ignore p))
  (let* ((point (current-point))
	 (pos (mark-charpos point))
	 (line (mark-line point)))
    (delete-line-italic-marks line)
    (insert-character point #\;)
    (font-mark
     line
     (or (position #\; (the simple-string (line-string line))) pos)
     1
     :left-inserting)))

(bind-key "Start Italic Comment" #k";" :mode "Italic")
