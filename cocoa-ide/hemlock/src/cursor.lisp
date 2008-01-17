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
;;;    Written by Rob MacLachlan
;;;
;;; Cursor: Routines for cursor positioning and recentering
;;;
(in-package :hemlock-internals)


;;;; Mark-To-Cursorpos
;;;
;;; Since performance analysis showed that HALF of the time in the editor
;;; was being spent in this function, I threw all of the tricks in the
;;; book at it to try and make it tenser.
;;;
;;; The algorithm is roughly as follows:
;;;
;;;    1) Eliminate the annoying boundry condition of the mark being
;;; off the end of the window, if it is return NIL now.
;;;    2) If the charpos is on or immediately after the last character
;;; in the line, then find the last dis-line on which the line is
;;; displayed.  We know that the mark is at the end of this dis-line
;;; because it is known to be on the screen.  X position is trivially
;;; derived from the dis-line-length.
;;;    3) Call Real-Line-Length or Cached-Real-Line-Length to get the
;;; X position and number of times wrapped.

(declaim (special *the-sentinel*))

(eval-when (:compile-toplevel :execute)
;;; find-line
;;;
;;;    Find a dis-line which line is displayed on which starts before
;;; charpos, setting ypos and dis-line to the dis-line and it's index.
;;; Offset is expected to be the mark-charpos of the display-start for
;;; the window initially, and is set to offset within line that
;;; Dis-Line begins.  Charpos is the mark-charpos of the mark we want
;;; to find.  Check if same as *redisplay-favorite-line* and then scan
;;; if not.
;;;
(defmacro find-line (line offset charpos ypos dis-lines dis-line)
  (declare (ignore charpos))
  `(cond
    ;; No lines at all, fail.
    ((eq ,dis-lines *the-sentinel*) nil)
    ;; On the first line, offset is already set, so just set dis-line and
    ;; ypos and fall through.
    ((eq (dis-line-line (car ,dis-lines)) ,line)
     (setq ,dis-line ,dis-lines  ,ypos 0))
    ;; Look farther down. 
    ((do ((l (cdr ,dis-lines) (cdr l)))
	 ((eq l *the-sentinel*))
       (when (eq (dis-line-line (car l)) ,line)
	 (setq ,dis-line l  ,ypos (dis-line-position (car l)) ,offset 0)
	 (return t))))
    (t
     (error "Horrible flaming lossage, Sorry Man."))))

;;; find-last 
;;;
;;;    Find the last dis-line on which line is displayed, set ypos and 
;;; dis-line.
;;;
(defmacro find-last (line ypos dis-line)
  `(do ((trail ,dis-line dl)
	(dl (cdr ,dis-line) (cdr dl)))
       ((not (eq (dis-line-line (car dl)) ,line))
	(setq ,dis-line (car trail)  ,ypos (dis-line-position ,dis-line)))))

;;; find-charpos
;;;
;;;    Special-Case mark at end of line, if not punt out to real-line-length 
;;; function.  Return the correct values.
;;;
(defmacro find-charpos (line offset charpos length ypos dis-line width
			     fun chars)
  (declare (ignore chars))
  `(cond
    ((= ,charpos ,length)
     (find-last ,line ,ypos ,dis-line)
     (values (min (dis-line-length ,dis-line) (1- ,width)) ,ypos))
    ((= ,charpos (1- ,length))
     (multiple-value-bind (x dy)
			  (,fun ,line (1- ,width) ,offset ,charpos)
       (if (and (not (zerop dy)) (zerop x))
	   (values (1- ,width) (1- (+ ,ypos dy)))
	   (values x (+ ,ypos dy)))))
    (t
     (multiple-value-bind (x dy)
			  (,fun ,line (1- ,width) ,offset ,charpos)
	  (values x (+ ,ypos dy))))))

); eval-when

;;; real-line-length 
;;;
;;;    Return as values the X position and the number of times wrapped if
;;; one to display the characters from Start to End of Line starting at an
;;; X position of 0 wrapping Width wide.
;;; %SP-Find-Character-With-Attribute is used to find charaters 
;;; with funny representation much as in Compute-Line-Image.
;;;
(defun real-line-length (line width start end)
  (declare (fixnum width start end))
  (do ((xpos 0)
       (ypos 0)
       (chars (line-chars line))
       (losing 0)
       (dy 0))
      ((= start end) (values xpos ypos))
    (declare (fixnum xpos ypos dy) (simple-string chars)
	     (type (or fixnum null) losing))
    (setq losing (%fcwa chars start end losing-char))
    (when (null losing)
      (multiple-value-setq (dy xpos) (truncate (+ xpos (- end start)) width))
      (return (values xpos (+ ypos dy))))
    (multiple-value-setq (dy xpos) (truncate (+ xpos (- losing start)) width))
    (setq ypos (+ ypos dy)  start losing)
    (do ((last (or (%fcwa chars start end winning-char) end)) str)
	((= start last))
      (declare (fixnum last))
      (setq str (get-rep (schar chars start)))
      (incf start)
      (unless (simple-string-p str) (setq str (funcall str xpos)))
      (multiple-value-setq (dy xpos) (truncate (+ xpos (strlen str)) width))
      (setq ypos (+ ypos dy)))))

;;; cached-real-line-length
;;;
;;;    The same as Real-Line-Length, except does it for the cached line.
;;; the line argument is ignored, but present to make the arglists the
;;; same.
;;;
(defun cached-real-line-length (line width start end)
  (declare (fixnum width start end) (ignore line))
  (let ((offset (- (current-right-open-pos) (current-left-open-pos)))
	(bound 0))
    (declare (fixnum offset bound))
    (cond
     ((>= start (current-left-open-pos))
      (setq start (+ start offset)  bound (setq end (+ end offset))))
     ((> end (current-left-open-pos))
      (setq bound (current-left-open-pos)  end (+ end offset)))
     (t
      (setq bound end)))
    
    (do ((xpos 0)
	 (ypos 0)
	 (losing 0)
	 (dy 0))
	(())
      (declare (fixnum xpos ypos dy)
	       (type (or fixnum null) losing))
      (when (= start bound)
	(when (= start end) (return (values xpos ypos)))
	(setq start (current-right-open-pos)  bound end))
      (setq losing (%fcwa (current-open-chars) start bound losing-char))
      (cond
       (losing
	(multiple-value-setq (dy xpos)
	  (truncate (+ xpos (- losing start)) width))
	(setq ypos (+ ypos dy)  start losing)
	(do ((last (or (%fcwa (current-open-chars) start bound winning-char) bound)) str)
	    ((= start last))
	  (declare (fixnum last))
	  (setq str (get-rep (schar (current-open-chars) start)))
	  (incf start)
	  (unless (simple-string-p str) (setq str (funcall str xpos)))
	  (multiple-value-setq (dy xpos)
	    (truncate (+ xpos (strlen str)) width))
	  (setq ypos (+ ypos dy))))
       (t
	(multiple-value-setq (dy xpos)
	  (truncate (+ xpos (- bound start)) width))
	(setq ypos (+ ypos dy)  start bound))))))


;;; Dis-Line-Offset-Guess  --  Internal
;;;
;;;    Move Mark by Offset display lines.  The mark is assumed to be at the
;;; beginning of a display line, and we attempt to leave it at one.  We assume
;;; all characters print one wide.  Width is the width of the window we are
;;; displaying in.
;;;
(defun dis-line-offset-guess (mark offset width)
  (let ((w (1- width)))
    (if (minusp offset)
	(dotimes (i (- offset) t)
	  (let ((pos (mark-charpos mark)))
	    (if (>= pos w)
		(character-offset mark (- w))
		(let ((prev (line-previous (mark-line mark))))
		  (unless prev (return nil))
		  (multiple-value-bind
		      (lines chars)
		      (truncate (line-length prev) w)
		    (move-to-position mark
				      (cond ((zerop lines) 0)
					    ((< chars 2)
					     (* w (1- lines)))
					    (t
					     (* w lines)))
				      prev))))))
	(dotimes (i offset t)
	  (let ((left (- (line-length (mark-line mark))
			 (mark-charpos mark))))
	    (if (> left width)
		(character-offset mark w)
		(unless (line-offset mark 1 0)
		  (return nil))))))))

;;; maybe-recenter-window  --  Internal
;;;
;;;     Update the dis-lines for Window and recenter if the point is off
;;; the screen.
;;;
(defun maybe-recenter-window (window)
  (unless (%displayed-p (buffer-point (window-buffer window)) window)
    (center-window window (buffer-point (window-buffer window)))
    t))

;;; center-window  --  Public
;;;
;;;    Try to move the start of window so that Mark is on a line in the 
;;; center.
;;;
(defun center-window (window mark)
  "Adjust the start of Window so that Mark is displayed on the center line."
  (let ((height (window-height window))
	(start (window-display-start window)))
    (move-mark start mark)
    (unless (dis-line-offset-guess start (- (truncate height 2))
				   (window-width window))
      (move-mark start (buffer-start-mark (window-buffer window))))
    (update-window-image window)
    ;; If that doesn't work, panic and make the start the point.
    (unless (%displayed-p mark window)
      (move-mark start mark)
      (update-window-image window))))


;;; %Displayed-P  --  Internal
;;;
;;;    If Mark is within the displayed bounds in Window, then return true,
;;; otherwise false.  We assume the window image is up to date.
;;;
(defun %displayed-p (mark window)
  (let ((start (window-display-start window))
	(end (window-display-end window)))
    (not (or (mark< mark start) (mark> mark end)
	     (if (mark= mark end)
		 (let ((ch (next-character end)))
		   (and ch (char/= ch #\newline)))
		 nil)))))


;;; Displayed-p  --  Public
;;;
;;;    Update the window image and then check if the mark is displayed.
;;;
(defun displayed-p (mark window)
  "Return true if Mark is displayed on Window, false otherwise."
  (maybe-update-window-image window)
  (%displayed-p mark window))


;;; scroll-window  --  Public
;;;
;;;    This is not really right, since it uses dis-line-offset-guess.
;;; Probably if there is any screen overlap then we figure it out
;;; exactly.
;;;


;;; Mark-Column  --  Public
;;;
;;;    Find the X position of a mark supposing that it were displayed
;;; in an infinitely wide screen.
;;;
(defun mark-column (mark)
  "Find the X position at which Mark would be displayed if it were on
  an infinitely wide screen.  This takes into account tabs and control
  characters."
  (let ((charpos (mark-charpos mark))
	(line (mark-line mark)))
    (if (current-open-line-p line)
	(values (cached-real-line-length line 10000 0 charpos))
	(values (real-line-length line 10000 0 charpos)))))

;;; Find-Position  --  Internal
;;;
;;;    Return the charpos which corresponds to the specified X position
;;; within Line.  If there is no such position between Start and End then
;;; rutne NIL.
;;;
(defun find-position (line position start end width)
  (do* ((cached (current-open-line-p line))
	(lo start)
	(hi (1- end))
	(probe (truncate (+ lo hi) 2) (truncate (+ lo hi) 2)))
       ((> lo hi)
	(if (= lo end) nil hi))
    (let ((val (if cached
		   (cached-real-line-length line width start probe)
		   (real-line-length line width start probe))))
      (cond ((= val position) (return probe))
	    ((< val position) (setq lo (1+ probe)))
	    (t (setq hi (1- probe)))))))

;;; Cursorpos-To-Mark  --  Public
;;;
;;;    Find the right dis-line, then zero in on the correct position
;;; using real-line-length.
;;;
(defun cursorpos-to-mark (x y window)
  (check-type window window)
  (let ((width (window-width window))
	(first (window-first-line window)))
    (when (>= x width)
      (return-from cursorpos-to-mark nil))
    (do* ((prev first dl)
	  (dl (cdr first) (cdr dl))
	  (ppos (mark-charpos (window-display-start window))
		(if (eq (dis-line-line (car dl)) (dis-line-line (car prev)))
		    (dis-line-end (car prev)) 0)))
	((eq dl *the-sentinel*)
	 (copy-mark (window-display-end window) :temporary))
      (when (= (dis-line-position (car dl)) y)
	(let* ((line (dis-line-line (car dl)))
	       (end (dis-line-end (car dl))))
	  (return (mark line (or (find-position line x ppos end width) end))))))))

;;; Move-To-Column  --  Public
;;;
;;;    Just look up the charpos using find-position...
;;;
(defun move-to-column (mark column &optional (line (mark-line mark)))
  "Move Mark to the specified Column on Line.  This function is analogous
  to Move-To-Position, but it deals with the physical screen position
  as returned by Mark-Column; the mark is moved to before the character
  which would be displayed in Column if the line were displayed on
  an infinitely wide screen.  If the column specified is greater than
  the column of the last character, then Nil is returned and the mark
  is not modified."
  (let ((res (find-position line column 0 (line-length line) 10000)))
    (if res
	(move-to-position mark res line))))
