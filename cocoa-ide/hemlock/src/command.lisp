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
;;; This file contains the definitions for the basic Hemlock commands.
;;;

(in-package :hemlock)

;;; utility for collapsing selections from movement commands
;;; returns a true value if a selection was collapsed, false otherwise

(defun collapse-if-selection (&key (direction :forward))
  (assert (memq direction '(:backward :forward))()
          "collapse-if-selection requires a :direction argument equal to either :backward or :forward")
  (let ((b (current-buffer)))
    (if (hi::%buffer-current-region-p b)
        (let* ((point (buffer-point b))
               (region (current-region)))
          ;; Deactivate the region
          (ecase direction
            ((:backward) (move-mark point (region-start region)))
            ((:forward) (move-mark point (region-end region))))
          (setf (hi::buffer-region-active b) nil)
          point)
        nil)))

;;; Make a mark for buffers as they're consed:

(defun hcmd-new-buffer-hook-fun (buff)
  (setf (hi::buffer-%mark buff) (copy-mark (buffer-point buff) :right-inserting)))

(add-hook make-buffer-hook #'hcmd-new-buffer-hook-fun)
(dolist (buff *buffer-list*) (hcmd-new-buffer-hook-fun buff))




;;;; Simple character manipulation:

(defcommand "Self Insert" (p)
  "Insert the last character typed.
  With prefix argument insert the character that many times."
  "Implements ``Self Insert'', calling this function is not meaningful."
  (let ((char (last-char-typed)))
    (unless char (editor-error "Can't insert that character."))
    (if (and p (> p 1))
	(insert-string
	 (current-point-for-insertion)
	 (make-string p :initial-element char))
	(insert-character (current-point-for-insertion) char))))

(defcommand "Quoted Insert" (p)
  "Causes the next character typed to be inserted in the current
   buffer, even if would normally be interpreted as an editor command."
  (declare (ignore p))
  (setf (hi::hemlock-view-quote-next-p hi::*current-view*) t))

(defcommand "Native Quoted Insert" (p)
  "Causes the next character typed to be processed by the native text-handling
   facility."
  (declare (ignore p))
  (setf (hi::hemlock-view-quote-next-p hi::*current-view*) :native))


(defcommand "Forward Character" (p)
    "Move the point forward one character, collapsing the selection.
   With prefix argument move that many characters, with negative argument
   go backwards."
    "Move the point of the current buffer forward p characters, collapsing the selection."
  (or (collapse-if-selection :direction :forward)
      (let* ((p (cond
                  (p p)
                  ((hi::%buffer-current-region-p hi::*current-buffer*) 0)
                  (t 1)))
             (point (current-point-collapsing-selection)))
        (cond ((character-offset point p))
              ((= p 1)
               (editor-error "No next character."))
              ((= p -1)
               (editor-error "No previous character."))
              (t
               (if (plusp p)
                   (buffer-end point)
                   (buffer-start point))
               (editor-error "Not enough characters."))))))

(defcommand "Select Forward Character" (p)
  "Move the point forward one character, extending the selection.
   With prefix argument move that many characters, with negative argument
   go backwards."
  "Move the point of the current buffer forward p characters, extending the selection."
  (let* ((p (or p 1)))
    (if (< p 0)
      (select-backward-character-command (- p))
      (let* ((point (current-point-for-selection-end)))
        (cond ((character-offset point p))
              ((= p 1)
               (editor-error "No next character."))
              (t
               (buffer-end point)
               (editor-error "Not enough characters.")))))))

(defcommand "Backward Character" (p)
    "Move the point backward one character, collapsing the selection.
  With prefix argument move that many characters backward."
    "Move the point p characters backward, collapsing the selection."
  (or (collapse-if-selection :direction :backward)
      (forward-character-command (if p (- p) -1))))

(defcommand "Select Backward Character" (p)
  "Move the point backward one character, extending the selection.
  With prefix argument move that many characters backward."
  "Move the point p characters backward, extending the selection."
  (let* ((p (or p 1)))
    (if (< p 0)
      (select-forward-character-command (- p))
      (let* ((point (current-point-for-selection-start)))
        (cond ((character-offset point (- p)))
              ((= p 1)
               (editor-error "No previous character."))
              (t
               (buffer-start point)
               (editor-error "Not enough characters.")))))))

#|
(defcommand "Delete Next Character" (p)
  "Deletes the character to the right of the point.
  With prefix argument, delete that many characters to the right
  (or left if prefix is negative)."
  "Deletes p characters to the right of the point."
  (unless (delete-characters (current-point) (or p 1))
    (buffer-end (current-point))
    (editor-error "No next character.")))

(defcommand "Delete Previous Character" (p)
  "Deletes the character to the left of the point.
  With prefix argument, delete that many characters to the left 
  (or right if prefix is negative)."
  "Deletes p characters to the left of the point."
  (unless (delete-characters (current-point) (if p (- p) -1))
    (editor-error "No previous character.")))
|#

(defcommand "Delete Next Character" (p)
  "Deletes the character to the right of the point.
   With prefix argument, delete that many characters to the right
  (or left if prefix is negative)."
  "Deletes p characters to the right of the point."
  (let* ((point (current-point-for-deletion)))
    (when point
      (cond ((kill-characters point (or p 1)))
	    ((and p (minusp p))
	     (editor-error "Not enough previous characters."))
	    (t
	     (editor-error "Not enough next characters."))))))

(defcommand "Delete Previous Character" (p)
  "Deletes the character to the left of the point.
   Will push characters from successive deletes on to the kill ring."
  "Deletes the character to the left of the point.
   Will push characters from successive deletes on to the kill ring."
  (delete-next-character-command (- (or p 1))))

(defcommand "Transpose Characters" (p)
  "Exchanges the characters on either side of the point and moves forward
  With prefix argument, does this that many times.  A negative prefix
  argument causes the point to be moved backwards instead of forwards."
  "Exchanges the characters on either side of the point and moves forward."
  (let ((arg (or p 1))
	(point (current-point-unless-selection)))
    (when point
      (dotimes (i (abs arg))
        (when (minusp arg) (mark-before point))
        (let ((prev (previous-character point))
              (next (next-character point)))

          (cond ((not prev) (editor-error "No previous character."))
                ((not next) (editor-error "No next character."))
                (t
                 (setf (previous-character point) next)
                 (setf (next-character point) prev))))
        (when (plusp arg) (mark-after point))))))

;;;; Word hacking commands:

;;; WORD-OFFSET 
;;;
;;;    Move a mark forward/backward some words.
;;;
(defun word-offset (mark offset)
  "Move Mark by Offset words."
  (if (minusp offset)
      (do ((cnt offset (1+ cnt)))
	  ((zerop cnt) mark)
	(cond
	 ((null (reverse-find-attribute mark :word-delimiter #'zerop))
	  (return nil))
	 ((reverse-find-attribute mark :word-delimiter))
	 (t
	  (move-mark
	   mark (buffer-start-mark (mark-buffer mark))))))
      (do ((cnt offset (1- cnt)))
	  ((zerop cnt) mark)
	(cond
	 ((null (find-attribute mark :word-delimiter #'zerop))
	  (return nil))
	 ((null (find-attribute mark :word-delimiter))
	  (return nil))))))

(defcommand "Forward Word" (p)
    "Moves forward one word, collapsing the selection.
  With prefix argument, moves the point forward over that many words."
    "Moves the point forward p words, collapsing the selection."
  (or (collapse-if-selection :direction :forward)
      (let* ((point (current-point-collapsing-selection)))
        (cond ((word-offset point (or p 1)))
              ((and p (minusp p))
               (buffer-start point)
               (editor-error "No previous word."))
              (t
               (buffer-end point)
               (editor-error "No next word."))))))

(defcommand "Select Forward Word" (p)
  "Moves forward one word, extending the selection.
  With prefix argument, moves the point forward over that many words."
  "Moves the point forward p words, extending the selection."
  (let* ((p (or p 1)))
    (if (< p 0)
      (select-backward-word-command (- p))
      (let* ((point (current-point-for-selection-end)))
        (cond ((word-offset point p))
              (t
               (buffer-end point)
               (editor-error "No next word.")))))))

(defcommand "Backward Word" (p)
  "Moves forward backward word.
  With prefix argument, moves the point back over that many words."
  "Moves the point backward p words."
  (or (collapse-if-selection :direction :backward)
   (forward-word-command (- (or p 1)))))

(defcommand "Select Backward Word" (p)
  "Moves forward backward word, extending the selection.
  With prefix argument, moves the point back over that many words."
  "Moves the point backward p words, extending the selection."
  (let* ((p (or p 1)))
    (if (< p 0)
      (select-forward-word-command (- p))
      (let* ((point (current-point-for-selection-start)))
        (cond ((word-offset point (- p)))
              (t
               (buffer-start point)
               (editor-error "No previous word.")))))))



;;;; Moving around:

(defun set-target-column (mark)
  (if (eq (last-command-type) :line-motion)
    (hi::hemlock-target-column hi::*current-view*)
    (setf (hi::hemlock-target-column hi::*current-view*) (mark-column mark))))

(defhvar "Next Line Inserts Newlines"
    "If true, causes the \"Next Line\" command to insert newlines when
     moving past the end of the buffer."
  :value nil)


(defcommand "Next Line" (p)
  "Moves the point to the next line, collapsing the selection.
   With prefix argument, moves the point that many lines down (or up if
   the prefix is negative)."
  "Moves the down p lines, collapsing the selection."
  (let* ((point (current-point-collapsing-selection))
	 (target (set-target-column point))
         (count (or p 1)))
    (unless (line-offset point count)
      (cond ((and (not p) (value next-line-inserts-newlines))
             (when (same-line-p point (buffer-end-mark (current-buffer)))
               (line-end point))
             (insert-character point #\newline))
            ((minusp count)
             (buffer-start point)
             (editor-error "No previous line."))
            (t
             (buffer-end point)
             (editor-error "No next line."))))
    (unless (move-to-position point target) (line-end point))
    (setf (last-command-type) :line-motion)))

(defcommand "Select Next Line" (p)
  "Moves the point to the next line, extending the selection.
  With prefix argument, moves the point that many lines down (or up if
  the prefix is negative)."
  "Moves the down p lines, extending the selection." 
  (if (and p (< p 0))
    (select-previous-line-command (- p))
    (let* ((point (current-point-for-selection-end))
           (target (set-target-column point))
           (count (or p 1))
           (seek-target t))
      (unless (line-offset point count)
        (cond ((and (not p) (value next-line-inserts-newlines))
               (when (same-line-p point (buffer-end-mark (current-buffer)))
                 (line-end point))
               (insert-character point #\newline))
              ((minusp count)
               (buffer-start point)
               (setq seek-target nil))
              (t
               (buffer-end point)
               (setq seek-target nil))))
      (when seek-target
        (unless (move-to-position point target) (line-end point)))
      (setf (last-command-type) :line-motion))))


(defcommand "Previous Line" (p)
  "Moves the point to the previous line, collapsing the selection.
  With prefix argument, moves the point that many lines up (or down if
  the prefix is negative)."
  "Moves the point up p lines, collapsing the selection."
  (next-line-command (- (or p 1))))

(defcommand "Select Previous Line" (p)
  "Moves the point to the previous line, collapsing the selection.
  With prefix argument, moves the point that many lines up (or down if
  the prefix is negative)."
  "Moves the point up p lines, collapsing the selection."
  (let* ((p (or p 1)))
    (if (< p 0)
      (select-next-line-command (- p))
      (let* ((point (current-point-for-selection-start))
             (target (set-target-column point)))
        (if (line-offset point (- p))
          (unless (move-to-position point target) (line-end point))
          (buffer-start point))
        (setf (last-command-type) :line-motion)))))

(defcommand "Mark to End of Buffer" (p)
  "Sets the current region from point to the end of the buffer."
  "Sets the current region from point to the end of the buffer."
  (declare (ignore p))
  (buffer-end (push-new-buffer-mark (current-point) t)))

(defcommand "Mark to Beginning of Buffer" (p)
  "Sets the current region from the beginning of the buffer to point."
  "Sets the current region from the beginning of the buffer to point."
  (declare (ignore p))
  (buffer-start (push-new-buffer-mark (current-point) t)))

(defcommand "Beginning of Buffer" (p)
  "Moves the point to the beginning of the current buffer, collapsing the selection."
  "Moves the point to the beginning of the current buffer, collapsing the selection."
  (declare (ignore p))
  (let ((point (current-point-collapsing-selection)))
    (push-new-buffer-mark point)
    (buffer-start point)))

(defcommand "End of Buffer" (p)
  "Moves the point to the end of the current buffer."
  "Moves the point to the end of the current buffer."
  (declare (ignore p))
  (let ((point (current-point-collapsing-selection)))
    (push-new-buffer-mark point)
    (buffer-end point)))

(defcommand "Beginning of Line" (p)
  "Moves the point to the beginning of the current line, collapsing the selection.
  With prefix argument, moves the point to the beginning of the prefix'th
  next line."
  "Moves the point down p lines and then to the beginning of the line, collapsing the selection."
  (let ((point (current-point-collapsing-selection)))
    (unless (line-offset point (if p p 0)) (editor-error "No such line."))
    (line-start point)))

(defcommand "Select to Beginning of Line" (p)
    "Moves the point to the beginning of the current line, extending the selection.
  With prefix argument, moves the point to the beginning of the prefix'th
  next line."
    "Moves the point down p lines and then to the beginning of the line, extending the selection."
  (let ((point (current-point-for-selection-start)))
    (unless (line-offset point (if p p 0)) (editor-error "No such line."))
    (line-start point)))

(defcommand "End of Line" (p)
  "Moves the point to the end of the current line, collapsing the selection.
  With prefix argument, moves the point to the end of the prefix'th next line."
  "Moves the point down p lines and then to the end of the line, collapsing the selection."
  (let ((point (current-point-collapsing-selection)))
    (unless (line-offset point (if p p 0)) (editor-error "No such line."))
    (line-end point)))

(defcommand "Select to End of Line" (p)
  "Moves the point to the end of the current line, extending the selection.
  With prefix argument, moves the point to the end of the prefix'th next line."
  "Moves the point down p lines and then to the end of the line, extending the selection."
  (let ((point (current-point-for-selection-end)))
    (unless (line-offset point (if p p 0)) (editor-error "No such line."))
    (line-end point)))

(defhvar "Scroll Overlap"
  "The \"Scroll Window\" commands leave this much overlap between screens."
  :value 2)

(defhvar "Scroll Redraw Ratio"
  "This is a ratio of \"inserted\" lines to the size of a window.  When this
   ratio is exceeded, insert/delete line terminal optimization is aborted, and
   every altered line is simply redrawn as efficiently as possible.  For example,
   setting this to 1/4 will cause scrolling commands to redraw the entire window
   instead of moving the bottom two lines of the window to the top (typically
   3/4 of the window is being deleted upward and inserted downward, hence a
   redraw); however, commands line \"New Line\" and \"Open Line\" will still
   efficiently, insert a line moving the rest of the window's text downward."
  :value nil)

(defcommand "Scroll Window Down" (p)
  "Move down one screenfull.
  With prefix argument scroll down that many lines."
  "If P is NIL then scroll Window, which defaults to the current
  window, down one screenfull.  If P is supplied then scroll that
  many lines."
  (if p
    (set-scroll-position :lines-down p)
    (set-scroll-position :page-down)))

(defcommand "Page Down" (p)
  "Move down one screenfull, without changing the selection."
  "Ignores prefix argument"
  (declare (ignore p))
  (set-scroll-position :view-page-down))

(defcommand "Scroll Window Up" (p)
  "Move up one screenfull.
  With prefix argument scroll up that many lines."
  "If P is NIL then scroll Window, which defaults to the current
  window, up one screenfull.  If P is supplied then scroll that
  many lines."
  (if p
    (set-scroll-position :lines-up p)
    (set-scroll-position :page-up)))

(defcommand "Page Up" (p)
  "Move up one screenfull, without changing the selection."
  "Ignores prefix argument."
  (declare (ignore p))
  (set-scroll-position :view-page-up))

;;;; Kind of miscellaneous commands:

(defcommand "Refresh Screen" (p)
  "Refreshes everything in the window, centering current line.
With prefix argument, puts moves current line to top of window"
  (if p
    (set-scroll-position :line (current-point))
    (set-scroll-position :center-selection)))


(defcommand "Extended Command" (p)
  "Prompts for and executes an extended command."
  "Prompts for and executes an extended command.  The prefix argument is
  passed to the command."
  (let* ((name (prompt-for-keyword :tables (list *command-names*)
				   :prompt "Extended Command: "
				   :help "Name of a Hemlock command"))
	 (function (command-function (getstring name *command-names*))))
    (funcall function p)))

(defhvar "Universal Argument Default"
  "Default value for \"Universal Argument\" command."
  :value 4)

(defstruct (prefix-argument-state (:conc-name "PS-"))
  sign
  multiplier
  read-some-digit-p
  ;; This is NIL if haven't started and don't have a universal argument, else a number
  result
  ;; This is cleared by prefix-argument-resetting-state (called at the start of each
  ;; command) and can be set by a command to avoid the state being reset at
  ;; the end of the command.
  set-p)

(defun prefix-argument-resetting-state (&optional (ps (current-prefix-argument-state)))
  "Fetches the prefix argument and uses it up, i.e. marks it as not being set"
  (unless (ps-set-p ps)
    (setf (ps-sign ps) 1
	  (ps-multiplier ps) 1
	  (ps-read-some-digit-p ps) nil
	  (ps-result ps) nil))
  (setf (ps-set-p ps) nil) ;; mark it for death unless explicitly revived.
  (when (ps-result ps)
    (* (ps-sign ps)
       (if (ps-read-some-digit-p ps)
	 (ps-result ps)
	 (expt (value universal-argument-default) (ps-multiplier ps))))))

(defun note-prefix-argument-set (ps)
  (assert (ps-result ps))
  (setf (ps-set-p ps) t)
  (message (with-output-to-string (s)
	     (dotimes (i (ps-multiplier ps))
	       (write-string "C-U " s))
	     (cond ((ps-read-some-digit-p ps)
		    (format s "~d" (* (ps-sign ps) (ps-result ps))))
		   ((< (ps-sign ps) 0)
		    (write-string "-" s))))))

(defcommand "Universal Argument" (p)
  "Sets prefix argument for next command.
   Typing digits, regardless of any modifier keys, specifies the argument.
   Optionally, you may first type a sign (- or +).  While typing digits, if you
   type C-U or C-u, the digits following the C-U form a number this command
   multiplies by the digits preceding the C-U.  The default value for this
   command and any number following a C-U is the value of \"Universal Argument
   Default\"."
  (declare (ignore p)) ;; we operate on underlying state instead
  (let ((ps (current-prefix-argument-state)))
    (if (ps-result ps)
      (incf (ps-multiplier ps))
      (setf (ps-result ps) 0))
    (note-prefix-argument-set ps)))

(defcommand "Argument Digit" (p)
  "This command is equivalent to invoking \"Universal Argument\" and typing
   the key used to invoke this command.  It waits for more digits and a
   command to which to give the prefix argument."
  (declare (ignore p)) ;; we operate on underlying state instead
  (let* ((ps (current-prefix-argument-state))
	 (key-event (last-key-event-typed))
	 (stripped-key-event (make-key-event key-event))
	 (char (key-event-char stripped-key-event))
	 (digit (if char (digit-char-p char))))
    (when (null (ps-result ps))
      (setf (ps-result ps) 0))
    (case char
      (#\-
       (when (ps-read-some-digit-p ps) ;; could just insert it up front...
	 (editor-error "Must type minus sign first."))
       (setf (ps-sign ps) (- (ps-sign ps))))
      (#\+
       (when (ps-read-some-digit-p ps) ;; could just insert it up front...
	 (editor-error "Must type plus sign first.")))
      (t
       (unless digit
	 (editor-error "Argument Digit must be bound to a digit!"))
       (setf (ps-read-some-digit-p ps) t)
       (setf (ps-result ps) (+ digit (* (ps-result ps) 10)))))
    (note-prefix-argument-set ps)))

(defcommand "Digit" (p)
  "With a numeric argument, this command extends the argument.
   Otherwise it does self insert"
  (if p
    (argument-digit-command p)
    (self-insert-command p)))
