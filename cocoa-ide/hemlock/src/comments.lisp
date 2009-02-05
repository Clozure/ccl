;;; -*- Log: Hemlock.Log; Package: Hemlock -*-
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
;;;    Written by Bill Chiles
;;;
;;; This file contains the implementation of comment commands.

(in-package hemlock)



;;;; -- Variables --

(defhvar "Comment Column"
  "Colmun to start comments in."
  :value 0)

(defhvar "Comment Start"
  "String that indicates the start of a comment."
  :value nil)

(defhvar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :value nil)

(defhvar "Comment Begin"
  "String that is inserted to begin a comment."
  :value nil)


;;;; -- Internal Specials --

;;; For the search pattern state specials, we just use " " as the comment
;;; start and end if none exist, so we are able to make search patterns.
;;; This is reasonable since any use of these will cause the patterns to be
;;; made consistent with the actual start and end strings.

(defvar *comment-start-pattern*
  (new-search-pattern :string-insensitive :forward (or (value comment-start) " "))
  "Search pattern to keep around for looking for comment starts.")

(defvar *last-comment-start*
  (or (value comment-start) " ")
  "Previous comment start used to make *comment-start-pattern*.")

(defvar *comment-end-pattern*
  (new-search-pattern :string-insensitive :forward (or (value comment-end) " "))
  "Search pattern to keep around for looking for comment ends.")

(defvar *last-comment-end*
  (or (value comment-end) " ")
  "Previous comment end used to make *comment-end-pattern*.")


(eval-when (:compile-toplevel :execute)
(defmacro get-comment-pattern (string kind) ;kind is either :start or :end
  (let (pattern-var last-var)
    (cond ((eq kind :start)
	   (setf pattern-var '*comment-start-pattern*)
	   (setf last-var '*last-comment-start*))
	  (t (setf pattern-var '*comment-end-pattern*)
	     (setf last-var '*last-comment-end*)))
    `(cond ((string= (the simple-string ,string) (the simple-string ,last-var))
	    ,pattern-var)
	   (t (setf ,last-var ,string)
	      (new-search-pattern :string-insensitive :forward
				  ,string ,pattern-var)))))
) ;eval-when



;;;;  -- Commands --

(defcommand "Set Comment Column" (p)
  "Set Comment Column to current column or argument.
   If argument is provided use its absolute value."
  "Set Comment Column to current column or argument.
   If argument is provided use its absolute value."
  (let ((new-column (or (and p (abs p))
			(mark-column (current-point)))))
    (defhvar "Comment Column" "This buffer's column to start comments."
      :value new-column  :buffer (current-buffer))
    (message "Comment Column = ~D" new-column)))


(defcommand "Indent for Comment" (p)
  "Move to or create a comment.  Moves to the start of an existing comment
   and indents it to start in Comment Column.  An existing double semicolon
   comment is aligned like a line of code.  An existing triple semicolon
   comment or any that start in column 0 is not moved.  With argument,
   aligns any comments on the next argument lines but does not create any.
   If characters extend past comment column, a space is added before
   starting comment."
  "Create comment or move to beginning of existing one aligning it."
  (let* ((column (value comment-column))
	 (start (value comment-start))
	 (begin (value comment-begin))
	 (end (value comment-end)))
    (unless (stringp start) (editor-error "No comment start string -- ~S." start))
    (indent-for-comment (current-point) column start begin end (or p 1))))


(defcommand "Up Comment Line" (p)
  "Equivalent to Previous Line followed by Indent for Comment (C-P ALT-;)."
  "Equivalent to Previous Line followed by Indent for Comment (C-P ALT-;)."
  (let ((column (value comment-column))
	(start (value comment-start))
	(begin (value comment-begin))
	(end (value comment-end)))
    (unless (stringp start) (editor-error "No comment start string -- ~S." start))
    (change-comment-line (current-point) column start
			 begin end (or (and p (- p)) -1))))

(defcommand "Down Comment Line" (p)
  "Equivalent to Next Line followed by Indent for Comment (C-N ALT-;)."
  "Equivalent to Next Line followed by Indent for Comment (C-N ALT-;)."
  (let ((column (value comment-column))
	(start (value comment-start))
	(begin (value comment-begin))
	(end (value comment-end)))
    (unless (stringp start) (editor-error "No comment start string -- ~S." start))
    (change-comment-line (current-point) column start begin end (or p 1))))


(defcommand "Kill Comment" (p)
  "Kills the comment (if any) on the current line.
   With argument, applies to specified number of lines, and moves past them."
  "Kills the comment (if any) on the current line.
   With argument, applies to specified number of lines, and moves past them."
  (let ((start (value comment-start)))
    (when start
      (if (not (stringp start))
	  (editor-error "Comment start not string or nil -- ~S." start))
      (kill-comment (current-point) start (or p 1)))))


(defcommand "Indent New Comment Line" (p)
  "Inserts comment end and then starts a comment on a new line.
   The indentation and number of additional comment-start characters are
   copied from the previous line's comment.  Acts like Linefeed, when done
   while not inside a comment, assuming a comment is the last thing on a line."
  "complete a current comment and start another a new line, copying indentation
   and start characters.  If no comment, call Linefeed command."
  (let ((start (value comment-start))
	(begin (value comment-begin))
	(end (value comment-end))
	(point (current-point)))
    (with-mark ((tmark point :left-inserting))
      (if start
	  (cond ((not (stringp start))
		 (editor-error "Comment start not string or nil -- ~S." start))
		((and (to-line-comment tmark start) (mark> point tmark))
		 (with-mark ((emark tmark))
		   (let ((endp (if end (to-comment-end emark end))))
		     (cond ((and endp (mark= emark point))
			    (insert-string point end)
			    (indent-new-comment-line point tmark start begin end))
			   ((and endp
				 (character-offset emark endp)
				 (mark>= point emark))
			    (indent-new-line-command p))
			   (t (delete-horizontal-space point)
			      (if end (insert-string point end))
			      (indent-new-comment-line point tmark
						       start begin end))))))
		(t (indent-new-line-command p)))
	  (indent-new-line-command p)))))



;;;; -- Support Routines --

(eval-when (:compile-toplevel :execute)
(defmacro %do-comment-lines ((var number) mark1 &rest forms)
  (let ((next-line-p (gensym)))
    `(do ((,var (if (plusp ,number) ,number 0) (1- ,var))
	  (,next-line-p t))
	 ((or (zerop ,var) (not ,next-line-p))
	  (zerop ,var))
       ,@forms
       (setf ,next-line-p (line-offset ,mark1 1)))))
) ;eval-when


;;; CHANGE-COMMENT-LINE closes any comment on the current line, deleting
;;; an empty comment.  After offsetting by lines, a comment is either
;;; aligned or created.
(defun change-comment-line (mark column start begin end lines)
  (with-mark ((tmark1 mark :left-inserting)
	      (tmark2 mark))
    (let ((start-len (to-line-comment mark start))
	  end-len)
      (when start-len
	(if end
	    (setf end-len (to-comment-end (move-mark tmark1 mark) end))
	    (line-end tmark1))
	(character-offset (move-mark tmark2 mark) start-len)
	(find-attribute tmark2 :whitespace #'zerop)
	(cond ((mark>= tmark2 tmark1)
	       (if end-len (character-offset tmark1 end-len))
	       ;; even though comment is blank, the line might not be blank
	       ;; after it in languages that have comment terminators.
	       (when (blank-after-p tmark1)
		 (reverse-find-attribute mark :whitespace #'zerop)
		 (if (not (same-line-p mark tmark1))
		     (line-start mark (mark-line tmark1)))
		 (delete-region (region mark tmark1))))
	      ((and end (not end-len)) (insert-string tmark1 end))))
      (if (line-offset mark lines)
	  (indent-for-comment mark column start begin end 1)
	  (editor-error)))))


(defun indent-for-comment (mark column start begin end times)
  (with-mark ((tmark mark :left-inserting))
    (if (= times 1)
	(let ((start-len (to-line-comment tmark start)))
	  (cond (start-len
		 (align-comment tmark start start-len column)
		 (character-offset (move-mark mark tmark) start-len))
		(t (comment-line mark column start begin end))))
	(unless (%do-comment-lines (n times) mark
		  (let ((start-len (to-line-comment mark start)))
		    (if start-len (align-comment mark start start-len column))))
	  (buffer-end mark)
	  (editor-error)))))


;;; KILL-COMMENT assumes a comment is the last thing on a line, so it does
;;; not deal with comment-end.  The Tao of EMACS.
(defun kill-comment (mark start times)
  (with-mark ((tmark mark :left-inserting))
    (if (= times 1)
	(when (to-line-comment mark start)
	  (with-mark ((u-start mark)
		      (u-end (line-end (move-mark tmark mark))))
	    (rev-scan-char u-start :whitespace nil)
	    (let ((undo-region (copy-region (region u-start u-end))))
	      (kill-ring-push (delete-and-save-region (region mark tmark)))
	      (delete-horizontal-space mark)
	      (make-region-undo :insert "Kill Comment" undo-region
				(copy-mark mark :left-inserting)))))
	(let* ((kill-region (delete-and-save-region (region mark tmark)))
	       (insert-mark (region-end kill-region))
	       ;; don't delete u-start and u-end since undo stuff handles that.
	       (u-start (line-start (copy-mark mark :left-inserting)))
	       (u-end (copy-mark mark :left-inserting))
	       (undo-region (copy-region (region u-start
						 (if (line-offset u-end times)
						     (line-start u-end)
						     (buffer-end u-end)))))
	       (n-times-p
		(%do-comment-lines (n times) mark
		  (when (to-line-comment mark start)
		    (line-end (move-mark tmark mark))
		    (ninsert-region insert-mark
				    (delete-and-save-region (region mark tmark)))
		    (insert-character insert-mark #\newline)
		    (delete-horizontal-space mark)))))
	  (kill-ring-push kill-region)
	  (make-region-undo :twiddle "Kill Comment"
			    (region u-start u-end) undo-region)
	  (unless n-times-p
	    (buffer-end mark)
	    (editor-error))))))

(defun comment-line (point column start begin end)
  (let* ((open (or begin start))
	 (open-len (length (the simple-string open)))
	 (end-len (if end (length (the simple-string end)) 0))
	 (insert-len (+ open-len end-len)))
    (line-end point)
    (insert-string point open)
    (if end (insert-string point end))
    (character-offset point (- insert-len))
    (adjust-comment point column)
    (character-offset point open-len)))


(eval-when (:compile-toplevel :execute)
(defmacro count-extra-last-chars (mark start-len start-char)
  (let ((count (gensym))
	(tmark (gensym)))
    `(with-mark ((,tmark ,mark))
       (character-offset ,tmark ,start-len)
       (do ((,count 0 (1+ ,count)))
	   ((char/= (next-character ,tmark) ,start-char) ,count)
	 (mark-after ,tmark)))))
)


;;; ALIGN-COMMENT sets a comment starting at mark to start in column
;;; column.  If the comment starts at the beginning of the line, it is not
;;; moved.  If the comment start is a single character and duplicated, then
;;; it is indented as if it were code, and if it is triplicated, it is not
;;; moved.  If the comment is to be moved to column, then we check to see
;;; if it is already there and preceded by whitespace.

(defun align-comment (mark start start-len column)
  (unless (start-line-p mark)
    (case (count-extra-last-chars mark start-len (schar start (1- start-len)))
      (1 (funcall (value indent-function) mark))
      (2 )
      (t (if (or (/= (mark-column mark) column)
		 (zerop (character-attribute
			 :whitespace (previous-character mark))))
	     (adjust-comment mark column))))))


;;; ADJUST-COMMENT moves the comment starting at mark to start in column
;;; column, inserting a space if the line extends past column.
(defun adjust-comment (mark column)
  (delete-horizontal-space mark)
  (let ((current-column (mark-column mark))
	(spaces-per-tab (value spaces-per-tab))
	tabs spaces next-tab-pos)
    (cond ((= current-column column)
	   (if (/= column 0) (insert-character mark #\space)))
	  ((> current-column column) (insert-character mark #\space))
	  (t (multiple-value-setq (tabs spaces)
	       (floor current-column spaces-per-tab))
	     (setf next-tab-pos
		   (if (zerop spaces)
		       current-column
		       (+ current-column (- spaces-per-tab spaces))))
	     (cond ((= next-tab-pos column)
		    (insert-character mark #\tab))
		   ((> next-tab-pos column)
		    (dotimes (i (- column current-column))
		      (insert-character mark #\space)))
		   (t (multiple-value-setq (tabs spaces)
			(floor (- column next-tab-pos) spaces-per-tab))
		      (dotimes (i (if (= current-column next-tab-pos)
				      tabs
				      (1+ tabs)))
			(insert-character mark #\tab))
		      (dotimes (i spaces)
			(insert-character mark #\space))))))))


;;; INDENT-NEW-COMMENT-LINE makes a new line at point starting a comment
;;; in the same way as the one at start-mark.
(defun indent-new-comment-line (point start-mark start begin end)
  (new-line-command nil)
  (insert-string point (gen-comment-prefix start-mark start begin))
  (if end
      (when (not (to-comment-end (move-mark start-mark point) end))
	(insert-string start-mark end)
	(if (mark= start-mark point)
	    ;; This occurs when nothing follows point on the line and
	    ;; both marks are left-inserting.
	    (character-offset
	     point (- (length (the simple-string end))))))))


;;; GEN-COMMENT-PREFIX returns a string suitable for beginning a line
;;; with a comment lined up with mark and starting the same as the comment
;;; immediately following mark.  This is used in the auto filling stuff too.
(defun gen-comment-prefix (mark start begin)
  (let* ((start-len (length (the simple-string start)))
	 (last-char (schar start (1- start-len)))
	 (extra-start-chars (count-extra-last-chars mark start-len last-char))
	 (spaces-per-tab (value spaces-per-tab))
	 (begin-end (if begin
			(subseq begin start-len (length (the simple-string begin)))
			"")))
    (multiple-value-bind (tabs spaces) (floor (mark-column mark) spaces-per-tab)
      (concatenate 'simple-string
		   (make-string tabs :initial-element #\tab)
		   (make-string spaces :initial-element #\space)
		   start
		   (make-string extra-start-chars :initial-element last-char)
		   begin-end))))


;;; TO-LINE-COMMENT moves mark to the first comment start character on its
;;; line if there is a comment and returns the length of start, otherwise
;;; nil is returned.  Start must be a string.  This is used by the auto
;;; filling stuff too.
(defun to-line-comment (mark start)
  (with-mark ((tmark mark))
    (line-start tmark)
    (let ((start-len (find-pattern tmark (get-comment-pattern start :start))))
      (when (and start-len (same-line-p mark tmark))
	(move-mark mark tmark)
	start-len))))


;;; TO-COMMENT-END moves mark to the first comment end character on its
;;; line if end is there and returns the length of comment end, otherwise
;;; mark is moved to the end of the line returning nil.  This is used by
;;; the auto filling stuff too.
(defun to-comment-end (mark end)
  (with-mark ((tmark mark))
    (let ((end-len (find-pattern tmark (get-comment-pattern end :end))))
      (cond ((and end-len (same-line-p mark tmark))
	     (move-mark mark tmark)
	     end-len)
	    (t (line-end mark) nil)))))
