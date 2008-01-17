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


;;; Make a mark for buffers as they're consed:

(defun hcmd-new-buffer-hook-fun (buff)
  (let ((ring (make-ring 10 #'delete-mark)))
    (defhvar "Buffer Mark Ring" 
      "This variable holds this buffer's mark ring."
      :buffer buff
      :value ring)
    (setf (hi::buffer-%mark buff) (copy-mark (buffer-point buff) :right-inserting))))

(add-hook make-buffer-hook #'hcmd-new-buffer-hook-fun)
(dolist (buff *buffer-list*) (hcmd-new-buffer-hook-fun buff))







;;;; Simple character manipulation:

(defcommand "Self Insert" (p)
  "Insert the last character typed.
  With prefix argument insert the character that many times."
  "Implements ``Self Insert'', calling this function is not meaningful."
  (let ((char (hemlock-ext:key-event-char *last-key-event-typed*)))
    (unless char (editor-error "Can't insert that character."))
    (if (and p (> p 1))
	(insert-string
	 (current-point-for-insertion)
	 (make-string p :initial-element char))
	(insert-character (current-point-for-insertion) char))))

(defcommand "Quoted Insert" (p)
  "Causes the next character typed to be inserted in the current
   buffer, even if would normally be interpreted as an editor command."
  "Reads a key-event from *editor-input* and inserts it at the point."
  (declare (ignore p))
  (hi::enable-self-insert hi::*editor-input*))

(defcommand "Forward Character" (p)
  "Move the point forward one character, collapsing the selection.
   With prefix argument move that many characters, with negative argument
   go backwards."
  "Move the point of the current buffer forward p characters, collapsing the selection."
  (let* ((p (or p 1))
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
	   (editor-error "Not enough characters.")))))

(defcommand "Select Forward Character" (p)
  "Move the point forward one character, extending the selection.
   With prefix argument move that many characters, with negative argument
   go backwards."
  "Move the point of the current buffer forward p characters, extending the selection."
  (let* ((p (or p 1))
         (point (current-point-extending-selection)))
    (cond ((character-offset point p))
	  ((= p 1)
	   (editor-error "No next character."))
	  ((= p -1)
	   (editor-error "No previous character."))
	  (t
	   (if (plusp p)
	       (buffer-end point)
	       (buffer-start point))
	   (editor-error "Not enough characters.")))))

(defcommand "Backward Character" (p)
  "Move the point backward one character, collapsing the selection.
  With prefix argument move that many characters backward."
  "Move the point p characters backward, collapsing the selection."
  (forward-character-command (if p (- p) -1)))

(defcommand "Select Backward Character" (p)
  "Move the point backward one character, extending the selection.
  With prefix argument move that many characters backward."
  "Move the point p characters backward, extending the selection."
  (select-forward-character-command (if p (- p) -1)))

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
	   mark (buffer-start-mark (line-buffer (mark-line mark)))))))
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
  (let* ((point (current-point-collapsing-selection)))
    (cond ((word-offset point (or p 1)))
          ((and p (minusp p))
           (buffer-start point)
           (editor-error "No previous word."))
          (t
           (buffer-end point)
           (editor-error "No next word.")))))

(defcommand "Select Forward Word" (p)
  "Moves forward one word, extending the selection.
  With prefix argument, moves the point forward over that many words."
  "Moves the point forward p words, extending the selection."
  (let* ((point (current-point-extending-selection)))
    (cond ((word-offset point (or p 1)))
          ((and p (minusp p))
           (buffer-start point)
           (editor-error "No previous word."))
          (t
           (buffer-end point)
           (editor-error "No next word.")))))

(defcommand "Backward Word" (p)
  "Moves forward backward word.
  With prefix argument, moves the point back over that many words."
  "Moves the point backward p words."
  (forward-word-command (- (or p 1))))

(defcommand "Select Backward Word" (p)
  "Moves forward backward word, extending the selection.
  With prefix argument, moves the point back over that many words."
  "Moves the point backward p words, extending the selection."
  (select-forward-word-command (- (or p 1))))



;;;; Moving around:

(defvar *target-column* 0)

(defun set-target-column (mark)
  (if (eq (last-command-type) :line-motion)
      *target-column*
      (setq *target-column* (mark-column mark))))

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
	 (target (set-target-column point)))
    (unless (line-offset point (or p 1))
      (when (value next-line-inserts-newlines)
        (cond ((not p)
               (when (same-line-p point (buffer-end-mark (current-buffer)))
                 (line-end point))
               (insert-character point #\newline))
              ((minusp p)
               (buffer-start point)
               (editor-error "No previous line."))
              (t
               (buffer-end point)
               (when p (editor-error "No next line."))))))
    (unless (move-to-column point target) (line-end point))
    (setf (last-command-type) :line-motion)))

(defcommand "Select Next Line" (p)
  "Moves the point to the next line, extending the selection.
   With prefix argument, moves the point that many lines down (or up if
   the prefix is negative)."
  "Moves the down p lines, extendin the selection."
  (let* ((point (current-point-extending-selection))
	 (target (set-target-column point)))
    (unless (line-offset point (or p 1))
      (when (value next-line-inserts-newlines)
        (cond ((not p)
               (when (same-line-p point (buffer-end-mark (current-buffer)))
                 (line-end point))
               (insert-character point #\newline))
              ((minusp p)
               (buffer-start point)
               (editor-error "No previous line."))
              (t
               (buffer-end point)
               (when p (editor-error "No next line."))))))
    (unless (move-to-column point target) (line-end point))
    (setf (last-command-type) :line-motion)))


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
  (select-next-line-command (- (or p 1))))

(defcommand "Mark to End of Buffer" (p)
  "Sets the current region from point to the end of the buffer."
  "Sets the current region from point to the end of the buffer."
  (declare (ignore p))
  (push-buffer-mark (buffer-end (copy-mark (current-point))) t))

(defcommand "Mark to Beginning of Buffer" (p)
  "Sets the current region from the beginning of the buffer to point."
  "Sets the current region from the beginning of the buffer to point."
  (declare (ignore p))
  (push-buffer-mark (buffer-start (copy-mark (current-point))) t))

(defcommand "Beginning of Buffer" (p)
  "Moves the point to the beginning of the current buffer, collapsing the selection."
  "Moves the point to the beginning of the current buffer, collapsing the selection."
  (declare (ignore p))
  (let ((point (current-point-collapsing-selection)))
    (push-buffer-mark (copy-mark point))
    (buffer-start point)))

(defcommand "End of Buffer" (p)
  "Moves the point to the end of the current buffer."
  "Moves the point to the end of the current buffer."
  (declare (ignore p))
  (let ((point (current-point-collapsing-selection)))
    (push-buffer-mark (copy-mark point))
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
  (let ((point (current-point-extending-selection)))
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
  (let ((point (current-point-extending-selection)))
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

(defcommand "Scroll Window Down" (p &optional (window (current-window)))
  "Move down one screenfull.
  With prefix argument scroll down that many lines."
  "If P is NIL then scroll Window, which defaults to the current
  window, down one screenfull.  If P is supplied then scroll that
  many lines."
  (scroll-window window (or p :page-down)))

(defcommand "Scroll Window Up" (p &optional (window (current-window)))
  "Move up one screenfull.
  With prefix argument scroll up that many lines."
  "If P is NIL then scroll Window, which defaults to the current
  window, up one screenfull.  If P is supplied then scroll that
  many lines."
  (scroll-window window (if p (- p) :page-up)))

(defcommand "Scroll Next Window Down" (p)
  "Do a \"Scroll Window Down\" on the next window."
  "Do a \"Scroll Window Down\" on the next window."
  (let ((win (next-window (current-window))))
    (when (eq win (current-window)) (editor-error "Only one window."))
    (scroll-window-down-command p win)))

(defcommand "Scroll Next Window Up" (p)
  "Do a \"Scroll Window Up\" on the next window."
  "Do a \"Scroll Window Up\" on the next window."
  (let ((win (next-window (current-window))))
    (when (eq win (current-window)) (editor-error "Only one window."))
    (scroll-window-up-command p win)))



;;;; Kind of miscellaneous commands:

;;; "Refresh Screen" may not be right with respect to wrapping lines in
;;; the case where an argument is supplied due the use of
;;; WINDOW-DISPLAY-START instead of SCROLL-WINDOW, but using the latter
;;; messed with point and did other hard to predict stuff.
;;; 
(defcommand "Refresh Screen" (p)
  "Refreshes everything in the window, centering current line."
  "Refreshes everything in the window, centering current line."
  (declare (ignore p))
  (center-text-pane (current-window)))



;;;
(defun reset-window-display-recentering (window &optional buffer)
  (declare (ignore buffer))
  (setf (window-display-recentering window) nil))
;;;
(add-hook window-buffer-hook #'reset-window-display-recentering)


(defcommand "Extended Command" (p)
  "Prompts for and executes an extended command."
  "Prompts for and executes an extended command.  The prefix argument is
  passed to the command."
  (let* ((name (prompt-for-keyword (list *command-names*)
				   :prompt "Extended Command: "
				   :help "Name of a Hemlock command"))
	 (function (command-function (getstring name *command-names*))))
    (funcall function p)))

(defhvar "Universal Argument Default"
  "Default value for \"Universal Argument\" command."
  :value 4)

(defcommand "Universal Argument" (p)
  "Sets prefix argument for next command.
  Typing digits, regardless of any modifier keys, specifies the argument.
  Optionally, you may first type a sign (- or +).  While typing digits, if you
  type C-U or C-u, the digits following the C-U form a number this command
  multiplies by the digits preceding the C-U.  The default value for this
  command and any number following a C-U is the value of \"Universal Argument
  Default\"."
  "You probably don't want to use this as a function."
  (declare (ignore p))
  (clear-echo-area)
  (write-string "C-U " *echo-area-stream*)
  (let* ((key-event (get-key-event hi::*editor-input*))
	 (char (hemlock-ext:key-event-char key-event)))
    (if char
	(case char
	  (#\-
	   (write-char #\- *echo-area-stream*)
	   (universal-argument-loop (get-key-event hi::*editor-input*) -1))
	  (#\+
	   (write-char #\+ *echo-area-stream*)
	   (universal-argument-loop (get-key-event hi::*editor-input*) -1))
	  (t
	   (universal-argument-loop key-event 1)))
	(universal-argument-loop key-event 1))))

(defcommand "Negative Argument" (p)
  "This command is equivalent to invoking \"Universal Argument\" and typing
   a minus sign (-).  It waits for more digits and a command to which to give
   the prefix argument."
  "Don't call this as a function."
  (when p (editor-error "Must type minus sign first."))
  (clear-echo-area)
  (write-string "C-U -" *echo-area-stream*)
  (universal-argument-loop (get-key-event hi::*editor-input*) -1))

(defcommand "Argument Digit" (p)
  "This command is equivalent to invoking \"Universal Argument\" and typing
   the digit used to invoke this command.  It waits for more digits and a
   command to which to give the prefix argument."
  "Don't call this as a function."
  (declare (ignore p))
  (clear-echo-area)
  (write-string "C-U " *echo-area-stream*)
  (universal-argument-loop *last-key-event-typed* 1))

(defun universal-argument-loop (key-event sign &optional (multiplier 1))
  (flet ((prefix (sign multiplier read-some-digit-p result)
	   ;; read-some-digit-p and (zerop result) are not
	   ;; equivalent if the user invokes this and types 0.
	   (* sign multiplier
	      (if read-some-digit-p
		  result
		  (value universal-argument-default)))))
    (let* ((stripped-key-event (if key-event (hemlock-ext:make-key-event key-event)))
	   (char (hemlock-ext:key-event-char stripped-key-event))
	   (digit (if char (digit-char-p char)))
	   (result 0)
	   (read-some-digit-p nil))
      (loop
	(cond (digit
	       (setf read-some-digit-p t)
	       (write-char char *echo-area-stream*)
	       (setf result (+ digit (* 10 result)))
	       (setf key-event (get-key-event hi::*editor-input*))
	       (setf stripped-key-event (if key-event
					    (hemlock-ext:make-key-event key-event)))
	       (setf char (hemlock-ext:key-event-char stripped-key-event))
	       (setf digit (if char (digit-char-p char))))
	      ((or (eq key-event #k"C-u") (eq key-event #k"C-U"))
	       (write-string " C-U " *echo-area-stream*)
	       (universal-argument-loop
		(get-key-event hi::*editor-input*) 1
		(prefix sign multiplier read-some-digit-p result))
	       (return))
	      (t
	       (unget-key-event key-event hi::*editor-input*)
	       (setf (prefix-argument)
		     (prefix sign multiplier read-some-digit-p result))
	       (return))))))
  (setf (last-command-type) (last-command-type)))
