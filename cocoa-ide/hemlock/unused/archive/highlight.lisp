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
;;; Highlighting paren and some other good stuff.
;;;
;;; Written by Bill Chiles and Jim Healy.
;;;

(in-package :hemlock)



;;;; Open parens.

(defhvar "Highlight Open Parens"
  "When non-nil, causes open parens to be displayed in a different font when
   the cursor is directly to the right of the corresponding close paren."
  :value nil)

(defhvar "Open Paren Finder Function"
  "Should be a function that takes a mark for input and returns either NIL
   if the mark is not after a close paren, or two (temporary) marks
   surrounding the corresponding open paren."
  :value 'lisp-open-paren-finder-function)


(defvar *open-paren-font-marks* nil
  "The pair of font-marks surrounding the currently highlighted open-
   paren or nil if there isn't one.")

(defvar *open-paren-highlight-font* 2
  "The index into the font-map for the open paren highlighting font.")


;;; MAYBE-HIGHLIGHT-OPEN-PARENS is a redisplay hook that matches parens by
;;; highlighting the corresponding open-paren after a close-paren is
;;; typed.
;;; 
(defun maybe-highlight-open-parens (window)
  (declare (ignore window))
  (when (value highlight-open-parens)
    (if (and (value highlight-active-region) (region-active-p))
	(kill-open-paren-font-marks)
	(multiple-value-bind
	    (start end)
	    (funcall (value open-paren-finder-function)
		     (current-point))
	  (if (and start end)
	      (set-open-paren-font-marks start end)
	      (kill-open-paren-font-marks))))))
;;;
(add-hook redisplay-hook 'maybe-highlight-open-parens)

(defun set-open-paren-font-marks (start end)
  (if *open-paren-font-marks*
      (flet ((maybe-move (dst src)
	       (unless (mark= dst src)
		 (move-font-mark dst src))))
	(declare (inline maybe-move))
	(maybe-move (region-start *open-paren-font-marks*) start)
	(maybe-move (region-end *open-paren-font-marks*) end))
      (let ((line (mark-line start)))
	(setf *open-paren-font-marks*
	      (region
	       (font-mark line (mark-charpos start)
			  *open-paren-highlight-font*)
	       (font-mark line (mark-charpos end) 0))))))

(defun kill-open-paren-font-marks ()
  (when *open-paren-font-marks*
    (delete-font-mark (region-start *open-paren-font-marks*))
    (delete-font-mark (region-end *open-paren-font-marks*))
    (setf *open-paren-font-marks* nil)))




;;;; Active regions.

(defvar *active-region-font-marks* nil)
(defvar *active-region-highlight-font* 3
  "The index into the font-map for the active region highlighting font.")


;;; HIGHLIGHT-ACTIVE-REGION is a redisplay hook for active regions.
;;; Since it is too hard to know how the region may have changed when it is
;;; active and already highlighted, if it does not check out to being exactly
;;; the same, we just delete all the font marks and make new ones.  When
;;; the current window is the echo area window, just pretend everything is
;;; okay; this keeps the region highlighted while we're in there.
;;;
(defun highlight-active-region (window)
  (unless (eq window *echo-area-window*)
    (when (value highlight-active-region)
      (cond ((region-active-p)
	     (cond ((not *active-region-font-marks*)
		    (set-active-region-font-marks))
		   ((check-active-region-font-marks))
		   (t (kill-active-region-font-marks)
		      (set-active-region-font-marks))))
	    (*active-region-font-marks*
	     (kill-active-region-font-marks))))))
;;;
(add-hook redisplay-hook 'highlight-active-region)

(defun set-active-region-font-marks ()
  (flet ((stash-a-mark (m &optional (font *active-region-highlight-font*))
	   (push (font-mark (mark-line m) (mark-charpos m) font)
		 *active-region-font-marks*)))
    (let* ((region (current-region nil nil))
	   (start (region-start region))
	   (end (region-end region)))
      (with-mark ((mark start))
	(unless (mark= mark end)
	  (loop
	    (stash-a-mark mark)
	    (unless (line-offset mark 1 0) (return))
	    (when (mark>= mark end) (return)))
	  (unless (start-line-p end) (stash-a-mark end 0))))))
  (setf *active-region-font-marks* (nreverse *active-region-font-marks*)))

(defun kill-active-region-font-marks ()
  (dolist (m *active-region-font-marks*)
    (delete-font-mark m))
  (setf *active-region-font-marks* nil))

;;; CHECK-ACTIVE-REGION-FONT-MARKS returns t if the current region is the same
;;; as that what is highlighted on the screen.  This assumes
;;; *active-region-font-marks* is non-nil.  At the very beginning, our start
;;; mark must not be at the end; it must be at the first font mark; and the
;;; font marks must be in the current buffer.  We don't make font marks if the
;;; start is at the end, so if this is the case, then they just moved together.
;;; We return nil in this case to kill all the font marks and make new ones, but
;;; no new ones will be made.
;;;
;;; Sometimes we hack the font marks list and return t because we can easily
;;; adjust the highlighting to be correct.  This keeps all the font marks from
;;; being killed and re-established.  In the loop, if there are no more font
;;; marks, we either ended a region already highlighted on the next line down,
;;; or we have to revamp the font marks.  Before returning here, we see if the
;;; region ends one more line down at the beginning of the line.  If this is
;;; true, then the user is simply doing "Next Line" at the beginning of the
;;; line.
;;;
;;; Each time through the loop we look at the top font mark, move our roving
;;; mark down one line, and see if they compare.  If they are not equal, the
;;; region may still be the same as that highlighted on the screen.  If this
;;; is the last font mark, not at the beginning of the line, and it is at the
;;; region's end, then this last font mark is in the middle of a line somewhere
;;; changing the font from the highlighting font to the default font.  Return
;;; t.
;;;
;;; If our roving mark is not at the current font mark, but it is at or after
;;; the end of the active region, then the end of the active region has moved
;;; before its previous location.
;;;
;;; Otherwise, move on to the next font mark.
;;;
;;; If our roving mark never moved onto a next line, then the buffer ends on the
;;; previous line, and the last font mark changes from the highlighting font to
;;; the default font.
;;;
(defun check-active-region-font-marks ()
  (let* ((region (current-region nil nil))
	 (end (region-end region)))
    (with-mark ((mark (region-start region)))
      (let ((first-active-mark (car *active-region-font-marks*))
	    (last-active-mark (last *active-region-font-marks*)))
	(if (and (mark/= mark end)
		 (eq (current-buffer)
		     (line-buffer (mark-line first-active-mark)))
		 (mark= first-active-mark mark))
	    (let ((marks (cdr *active-region-font-marks*)))
	      (loop
		(unless marks
		  (let ((res (and (line-offset mark 1 0)
				  (mark= mark end))))
		    (when (and (not res)
			       (line-offset mark 1 0)
			       (mark= mark end)
			       (start-line-p (car last-active-mark)))
		      (setf (cdr last-active-mark)
			    (list (font-mark (line-previous (mark-line mark))
					     0
					     *active-region-highlight-font*)))
		      (return t))
		    (return res)))
		(let ((fmark (car marks)))
		  (if (line-offset mark 1 0)
		      (cond ((mark/= mark fmark)
			     (return (and (not (cdr marks))
					  (not (start-line-p fmark))
					  (mark= fmark end))))
			    ((mark>= mark end)
			     (return nil))
			    (t (setf marks (cdr marks))))

		      (return (and (not (cdr marks))
				   (not (start-line-p fmark))
				   (mark= fmark end))))))))))))

