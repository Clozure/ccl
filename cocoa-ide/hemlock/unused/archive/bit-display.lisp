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
;;;    Modified by Bill Chiles to run under X on IBM RT's.
;;;

(in-package :hemlock-internals)


;;; prepare-window-for-redisplay  --  Internal
;;;
;;;    Called by make-window to do whatever redisplay wants to set up
;;; a new window.
;;;
(defun prepare-window-for-redisplay (window)
  (setf (window-old-lines window) 0))



;;;; Dumb window redisplay.

;;; DUMB-WINDOW-REDISPLAY redraws an entire window using dumb-line-redisplay.
;;; This assumes the cursor has been lifted if necessary.
;;;
(defun dumb-window-redisplay (window)
  (let* ((hunk (window-hunk window))
	 (first (window-first-line window)))
    (hunk-reset hunk)
    (do ((i 0 (1+ i))
	 (dl (cdr first) (cdr dl)))
	((eq dl *the-sentinel*)
	 (setf (window-old-lines window) (1- i)))
      (dumb-line-redisplay hunk (car dl)))
    (setf (window-first-changed window) *the-sentinel*
	  (window-last-changed window) first)
    (when (window-modeline-buffer window)
      (hunk-replace-modeline hunk)
      (setf (dis-line-flags (window-modeline-dis-line window))
	    unaltered-bits))
    (setf (bitmap-hunk-start hunk) (cdr (window-first-line window)))))


;;; DUMB-LINE-REDISPLAY is used when the line is known to be cleared already.
;;;
(defun dumb-line-redisplay (hunk dl)
  (hunk-write-line hunk dl)
  (setf (dis-line-flags dl) unaltered-bits (dis-line-delta dl) 0))



;;;; Smart window redisplay.

;;; We scan through the changed dis-lines, and condense the information
;;; obtained into five categories: Unchanged lines moved down, unchanged
;;; lines moved up, lines that need to be cleared, lines that are in the
;;; same place (but changed), and new or moved-and-changed lines to write.
;;; Each such instance of a thing that needs to be done is remembered be
;;; throwing needed information on a stack specific to the thing to be
;;; done.  We cannot do any of these things right away because each may
;;; confict with the previous.
;;; 
;;; Each stack is represented by a simple-vector big enough to hold the
;;; worst-case number of entries and a pointer to the next free entry.  The
;;; pointers are local variables returned from COMPUTE-CHANGES and used by
;;; SMART-WINDOW-REDISPLAY.  Note that the order specified in these tuples
;;; is the order in which they were pushed.
;;; 
(defvar *display-down-move-stack* (make-array (* hunk-height-limit 2))
  "This is the vector that we stash info about which lines moved down in
  as (Start, End, Count) triples.")
(defvar *display-up-move-stack* (make-array (* hunk-height-limit 2))
  "This is the vector that we stash info about which lines moved up in
  as (Start, End, Count) triples.")
(defvar *display-erase-stack* (make-array hunk-height-limit)
  "This is the vector that we stash info about which lines need to be erased
  as (Start, Count) pairs.")
(defvar *display-write-stack* (make-array hunk-height-limit)
  "This is the vector that we stash dis-lines in that need to be written.")
(defvar *display-rewrite-stack* (make-array hunk-height-limit)
  "This is the vector that we stash dis-lines in that need to be written.
  with clear-to-end.")

;;; Accessor macros to push and pop on the stacks:
;;;
(eval-when (:compile-toplevel :execute)

(defmacro spush (thing stack stack-pointer)
  `(progn
    (setf (svref ,stack ,stack-pointer) ,thing)
    (incf ,stack-pointer)))

(defmacro spop (stack stack-pointer)
  `(svref ,stack (decf ,stack-pointer)))

(defmacro snext (stack stack-pointer)
  `(prog1 (svref ,stack ,stack-pointer) (incf ,stack-pointer)))

); eval-when


;;; SMART-WINDOW-REDISPLAY only re-writes lines which may have been changed,
;;; and updates them with smart-line-redisplay if not very much has changed.
;;; Lines which have moved are copied.  We must be careful not to redisplay
;;; the window with the cursor down since it is not guaranteed to be out of
;;; the way just because we are in redisplay; LIFT-CURSOR is called just before
;;; the screen may be altered, and it takes care to know whether the cursor
;;; is lifted already or not.  At the end, if the cursor had been down,
;;; DROP-CURSOR puts it back; it doesn't matter if LIFT-CURSOR was never called
;;; since it does nothing if the cursor is already down.
;;; 
(defun smart-window-redisplay (window)
  ;; This isn't actually called --GB
  (let* ((hunk (window-hunk window))
	 (liftp (and (eq *cursor-hunk* hunk) *cursor-dropped*)))
    (when (bitmap-hunk-trashed hunk)
      (when liftp (lift-cursor))
      (dumb-window-redisplay window)
      (when liftp (drop-cursor))
      (return-from smart-window-redisplay nil))
    (let ((first-changed (window-first-changed window))
	  (last-changed (window-last-changed window)))
      ;; Is there anything to do?
      (unless (eq first-changed *the-sentinel*)
	(when liftp (lift-cursor))
	(if (and (eq first-changed last-changed)
		 (zerop (dis-line-delta (car first-changed))))
	    ;; One line changed.
	    (smart-line-redisplay hunk (car first-changed))
	    ;; More than one line changed.
	    (multiple-value-bind (up down erase write rewrite)
				 (compute-changes first-changed last-changed)
	      (do-down-moves hunk down)
	      (do-up-moves hunk up)
	      (do-erases hunk erase)
	      (do-writes hunk write)
	      (do-rewrites hunk rewrite)))
	;; Set the bounds so we know we displayed...
	(setf (window-first-changed window) *the-sentinel*
	      (window-last-changed window) (window-first-line window))))
    ;;
    ;; Clear any extra lines at the end of the window.
    (let ((pos (dis-line-position (car (window-last-line window)))))
      (when (< pos (window-old-lines window))
	(when liftp (lift-cursor))
	(hunk-clear-lines hunk (1+ pos) (- (window-height window) pos 1)))
      (setf (window-old-lines window) pos))
    ;;
    ;; Update the modeline if needed.
    (when (window-modeline-buffer window)
      (when (/= (dis-line-flags (window-modeline-dis-line window))
		unaltered-bits)
	(hunk-replace-modeline hunk)
	(setf (dis-line-flags (window-modeline-dis-line window))
	      unaltered-bits)))
    ;;
    (setf (bitmap-hunk-start hunk) (cdr (window-first-line window)))
    (when liftp (drop-cursor))))

;;; COMPUTE-CHANGES is used once in smart-window-redisplay, and it scans
;;; through the changed dis-lines in a window, computes the changes needed
;;; to bring the screen into corespondence, and throws the information
;;; needed to do the change onto the apropriate stack.  The pointers into
;;; the stacks (up, down, erase, write, and rewrite) are returned.
;;; 
;;; The algorithm is as follows:
;;; 1] If the line is moved-and-changed or new then throw the line on
;;; the write stack and increment the clear count.  Repeat until no more
;;; such lines are found.
;;; 2] If the line is moved then flush any pending clear, find how many
;;; consecutive lines are moved the same amount, and put the numbers
;;; on the correct move stack.
;;; 3] If the line is changed and unmoved throw it on a write stack.
;;; If a clear is pending throw it in the write stack and bump the clear
;;; count, otherwise throw it on the rewrite stack.
;;; 4] The line is unchanged, do nothing.
;;;
(defun compute-changes (first-changed last-changed)
  (let* ((dl first-changed)
	 (flags (dis-line-flags (car dl)))
	 (up 0) (down 0) (erase 0) (write 0) (rewrite 0) ;return values.
	 (clear-count 0)
	 prev clear-start)
    (declare (fixnum up down erase write rewrite clear-count))
    (loop
      (cond
       ;; Line moved-and-changed or new.
       ((> flags moved-bit)
	(when (zerop clear-count)
	  (setq clear-start (dis-line-position (car dl))))
	(loop
	  (setf (dis-line-delta (car dl)) 0)
	  (spush (car dl) *display-write-stack* write)
	  (incf clear-count)
	  (setq prev dl  dl (cdr dl)  flags (dis-line-flags (car dl)))
	  (when (<= flags moved-bit) (return nil))))
       ;; Line moved, unchanged.
       ((= flags moved-bit)
	(unless (zerop clear-count)
	  (spush clear-count *display-erase-stack* erase)
	  (spush clear-start *display-erase-stack* erase)
	  (setq clear-count 0))
	(do ((delta (dis-line-delta (car dl)))
	     (end (dis-line-position (car dl)))
	     (count 1 (1+ count)))
	    (())
	  (setf (dis-line-delta (car dl)) 0
		(dis-line-flags (car dl)) unaltered-bits)
	  (setq prev dl  dl (cdr dl)  flags (dis-line-flags (car dl)))
	  (when (or (/= (dis-line-delta (car dl)) delta) (/= flags moved-bit))
	    ;; We push in different order because we pop in different order.
	    (cond
	     ((minusp delta)
	      (spush (- end delta) *display-up-move-stack* up)
	      (spush end *display-up-move-stack* up)
	      (spush count *display-up-move-stack* up))
	     (t
	      (spush count *display-down-move-stack* down)
	      (spush end *display-down-move-stack* down)
	      (spush (- end delta) *display-down-move-stack* down)))
	    (return nil))))
       ;; Line changed, unmoved.
       ((= flags changed-bit)
	(cond ((zerop clear-count)
	       (spush (car dl) *display-rewrite-stack* rewrite))
	      (t
	       (spush (car dl) *display-write-stack* write)
	       (incf clear-count)))
	(setq prev dl  dl (cdr dl)  flags (dis-line-flags (car dl))))
       ;; Line unmoved, unchanged.
       (t
	(unless (zerop clear-count)
	  (spush clear-count *display-erase-stack* erase)
	  (spush clear-start *display-erase-stack* erase)
	  (setq clear-count 0))
	(setq prev dl  dl (cdr dl)  flags (dis-line-flags (car dl)))))
     
     (when (eq prev last-changed)
       ;; If done flush any pending clear.
       (unless (zerop clear-count)
	 (spush clear-count *display-erase-stack* erase)
	 (spush clear-start *display-erase-stack* erase))
       (return (values up down erase write rewrite))))))

(defun do-up-moves (hunk up)
  (do ((i 0))
      ((= i up))
    (hunk-copy-lines hunk (snext *display-up-move-stack* i)
		     (snext *display-up-move-stack* i)
		     (snext *display-up-move-stack* i))))

(defun do-down-moves (hunk down)
  (do ()
      ((zerop down))
    (hunk-copy-lines hunk (spop *display-down-move-stack* down)
		     (spop *display-down-move-stack* down)
		     (spop *display-down-move-stack* down))))

(defun do-erases (hunk erase)
  (do ()
      ((zerop erase))
    (hunk-clear-lines hunk (spop *display-erase-stack* erase)
		      (spop *display-erase-stack* erase))))

(defun do-writes (hunk write)
  (do ((i 0))
      ((= i write))
    (dumb-line-redisplay hunk (snext *display-write-stack* i))))

(defun do-rewrites (hunk rewrite)
  (do ()
      ((zerop rewrite))
    (smart-line-redisplay hunk (spop *display-rewrite-stack* rewrite))))


;;; SMART-LINE-REDISPLAY is called when the screen is mostly the same,
;;; clear to eol after we write it to avoid annoying flicker.
;;;
(defun smart-line-redisplay (hunk dl)
  (hunk-replace-line hunk dl)
  (setf (dis-line-flags dl) unaltered-bits (dis-line-delta dl) 0))
