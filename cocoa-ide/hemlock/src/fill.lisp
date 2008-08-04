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
;;;    Written by Bill Chiles
;;;
;;; This file contains the implementation of Auto Fill Mode.  Also,
;;;   paragraph and region filling stuff is here.
;;;

(in-package :hemlock)


;;; Fill Mode should be defined with some transparent bindings (linefeed and
;;; return) but with some that are not (space), so until this is possible, we
;;; kludge this effect by altering Auto Fill Linefeed and Auto Fill Return.
(defmode "Fill")



;;;; -- Variables --

(defhvar "Fill Column"
  "Used to determine at what column to force text to the next line."
  :value 75)

(defhvar "Fill Prefix"
  "String to put before each line when filling."
  :value ())

(defhvar "Auto Fill Space Indent"
  "When non-nil, uses \"Indent New Comment Line\" to break lines instead of
   \"New Line\".  However, if there is a fill prefix, it is still preferred."
  :value nil)



;;;; -- New Attributes --

(defattribute "Paragraph Delimiter"
  "is a character that delimits a paragraph by beginning a line."
  '(mod 2)
  0)


;;; (setf (character-attribute :paragraph-delimiter #\@) 1)
;;; (setf (character-attribute :paragraph-delimiter #\\) 1)
;;; (setf (character-attribute :paragraph-delimiter #\/) 1)
;;; (setf (character-attribute :paragraph-delimiter #\-) 1)
;;; (setf (character-attribute :paragraph-delimiter #\') 1)
;;; (setf (character-attribute :paragraph-delimiter #\.) 1)
;;;    These are useful for making certain text formatting command lines
;;; delimit paragraphs.  Anyway, this is what EMACS documentation states,
;;; and #\' and #\. are always paragraph delimiters (don't ask me).

(setf (character-attribute :paragraph-delimiter #\space) 1)
(setf (character-attribute :paragraph-delimiter #\linefeed) 1)
(setf (character-attribute :paragraph-delimiter
			   #+CMU #\formfeed #+(or sbcl EXCL CLISP Clozure) #\page) 1)
(setf (character-attribute :paragraph-delimiter #\tab) 1)
(setf (character-attribute :paragraph-delimiter #\newline) 1)



(defattribute "Sentence Closing Char"
  "is a delimiting character that may follow a sentence terminator
   such as quotation marks and parentheses."
  '(mod 2)
  0)


(setf (character-attribute :sentence-closing-char #\") 1)
(setf (character-attribute :sentence-closing-char #\') 1)
(setf (character-attribute :sentence-closing-char #\)) 1)
(setf (character-attribute :sentence-closing-char #\]) 1)
(setf (character-attribute :sentence-closing-char #\|) 1)
(setf (character-attribute :sentence-closing-char #\>) 1)


;;;; -- Commands --

(defcommand "Auto Fill Mode" (p)
  "Breaks lines between words at the right margin.
   A positive argument turns Fill mode on, while zero or a negative
   argument turns it off.  With no arguments, it is toggled.  When space
   is typed, text that extends past the right margin is put on the next
   line.  The right column is controlled by Fill Column."
  "Determine if in Fill mode or not and set the mode accordingly."
  (setf (buffer-minor-mode (current-buffer) "Fill")
	(if p
	    (plusp p)
	    (not (buffer-minor-mode (current-buffer) "Fill")))))


;;; This command should not have a transparent binding since it sometimes does
;;; not insert a spaces, and transparency would propagate to "Self Insert".
(defcommand "Auto Fill Space" (p)
  "Insert space and a CRLF if text extends past margin.
   If arg is 0, then may break line but will not insert the space.
   If arg is positive, then inserts that many spaces without filling."
  "Insert space and CRLF if text extends past margin.
   If arg is 0, then may break line but will not insert the space.
   If arg is positive, then inserts that many spaces without filling."
  (let ((point (current-point)))
    (check-fill-prefix (value fill-prefix) (value fill-column) point)
    (cond ((and p (plusp p))
	   (dotimes (x p) (insert-character point #\space)))
	  ((and p (zerop p)) (%auto-fill-space point nil))
	  (t (%auto-fill-space point t)))))


(defcommand "Auto Fill Linefeed" (p)
  "Does an immediate CRLF inserting Fill Prefix if it exists."
  "Does an immediate CRLF inserting Fill Prefix if it exists."
  (let ((point (current-point)))
    (check-fill-prefix (value fill-prefix) (value fill-column) point)
    (%auto-fill-space point nil)
    ;; The remainder of this function should go away when
    ;; transparent key bindings are per binding instead of
    ;; per mode.
    (multiple-value-bind (command t-bindings)
			 (get-command #k"Linefeed" :current)
      (declare (ignore command)) ;command is this one, so don't invoke it
      (dolist (c t-bindings) (invoke-command c p)))
    (indent-new-line-command nil)))



(defcommand "Auto Fill Return" (p)
  "Does an Auto Fill Space with a prefix argument of 0
   followed by a newline."
  "Does an Auto Fill Space with a prefix argument of 0
   followed by a newline."
  (let ((point (current-point)))
    (check-fill-prefix (value fill-prefix) (value fill-column) point)
    (%auto-fill-space point nil)
    ;; The remainder of this function should go away when
    ;; transparent key bindings are per binding instead of
    ;; per mode.
    (multiple-value-bind (command t-bindings)
			 (get-command #k"Return" :current)
      (declare (ignore command)) ;command is this one, so don't invoke it
      (dolist (c t-bindings) (invoke-command c p)))
    (new-line-command nil)))



(defcommand "Fill Paragraph" (p)
  "Fill this or next paragraph.
   Point stays fixed, but text may move past it due to filling.
   A paragraph is delimited by a blank line, a line beginning with a
   special character (@,\,-,',and .), or it is begun with a line with at
   least one whitespace character starting it.  Prefixes are ignored or
   skipped over before determining if a line starts or delimits a
   paragraph."
  "Fill this or next paragraph.
   Point stays fixed, but text may move past it due to filling."
  (let* ((prefix (value fill-prefix))
	 (prefix-len (length prefix))
	 (column (if p (abs p) (value fill-column)))
	 (point (current-point)))
    (with-mark ((m point))
      (let ((paragraphp (paragraph-offset m 1)))
	(unless (or paragraphp
		    (and (last-line-p m)
			 (end-line-p m)
			 (not (blank-line-p (mark-line m)))))
	  (editor-error))
	;;
	;; start and end get deleted by the undo cleanup function
	(let ((start (copy-mark m :right-inserting))
	      (end (copy-mark m :left-inserting)))
	  (%fill-paragraph-start start prefix prefix-len)
	  (let* ((region (region start end))
		 (undo-region (copy-region region)))
	    (fill-region region prefix column)
	    (make-region-undo :twiddle "Fill Paragraph" region undo-region)))))))


(defcommand "Fill Region" (p)
  "Fill text from point to mark."
  "Fill text from point to mark."
  (let* ((region (current-region))
	 (prefix (value fill-prefix))
	 (column (if p (abs p) (value fill-column))))
    (check-fill-prefix prefix column (current-point))
    (fill-region-by-paragraphs region prefix column)))



(defcommand "Set Fill Column" (p)
  "Set Fill Column to current column or argument.
   If argument is provided use its absolute value."
  "Set Fill Column to current column or argument.
   If argument is provided use its absolute value."
  (let ((new-column (or (and p (abs p))
			(mark-column (current-point)))))
    (defhvar "Fill Column" "This buffer's fill column"
      :value new-column  :buffer (current-buffer))
    (message "Fill Column = ~D" new-column)))


(defcommand "Set Fill Prefix" (p) 
  "Define Fill Prefix from the current line.
   All of the current line up to point is the prefix.  This may be
   turned off by placing point at the beginning of a line when setting."
  "Define Fill Prefix from the current line.
   All of the current line up to point is the prefix.  This may be
   turned off by placing point at the beginning of a line when setting."
  (declare (ignore p))
  (let ((point (current-point)))
    (with-mark ((mark point))
      (line-start mark)
      (let ((val (if (mark/= mark point) (region-to-string (region mark point)))))
	(defhvar "Fill Prefix" "This buffer's fill prefix"
	  :value val  :buffer (current-buffer))
	(message "Fill Prefix now ~:[empty~;~:*~S~]" val)))))

#+cmucl
(eval-when (:compile-toplevel)
  (declaim (optimize (speed 2)))); turn off byte compilation.

;;;; -- Auto Filling --

;;;      %AUTO-FILL-SPACE takes a point and an argument indicating
;;; whether it should insert a space or not.  If point is past Fill
;;; Column then text is filled. Usually  the else clause of the if
;;; will be executed.  If the then clause is executed, then the first
;;; branch of the COND will usually be executed.  The first branch
;;; handles the case of the end of a word extending past Fill Column
;;; while the second handles whitespace preceded by non-whitespace
;;; extending past the Fill Column.  The last branch is for those who
;;; like to whitespace out a blank line.

(defun %auto-fill-space (point insertp)
  "Insert space, but CRLF if text extends past margin.
   If arg is 0, then may break line but will not insert the space.
   If arg is positive, then inserts that many spaces without filling."
  (if (> (mark-column point) (value fill-column))
      (with-mark ((mark1 point :left-inserting))
	(let ((not-all-blank (reverse-find-attribute mark1 :whitespace #'zerop))
	      (prefix (value fill-prefix))
	      (column (value fill-column)))
	  (cond ((and not-all-blank (mark= point mark1))
		 (%auto-fill-word-past-column point mark1 insertp prefix column))
		((and not-all-blank (same-line-p mark1 point))
		 (delete-region (region mark1 point))
		 (if (> (mark-column point) column)
		     (%auto-fill-word-past-column point mark1 insertp prefix column)
		     (%filling-set-next-line point nil prefix)))
		(t
		 (line-start mark1 (mark-line point))
		 (delete-region (region mark1 point))
		 (%filling-set-next-line point nil prefix)))))
      (if insertp (insert-character point #\space))))



;;;      %AUTO-FILL-WORD-PAST-COLUMN takes a point, a second mark that is
;;; mark= at the end of some word, and an indicator of whether a space
;;; should be inserted or not.  First, point is moved before the previous
;;; "word."  If the word is effectively the only word on the line, it
;;; should not be moved down to the next line as it will leave a blank
;;; line.  The third branch handles when the typing began in the middle of
;;; some line (that is, right in front of some word).  Note that the else
;;; clause is the usual case.

(defun %auto-fill-word-past-column (point mark1 insertp prefix column)
  (let ((point-moved-p (reverse-find-attribute point :whitespace)))
    (with-mark ((mark2 point :left-inserting))
      (cond ((or (not point-moved-p)
		 (%auto-fill-blank-before-p point prefix))
	     (move-mark point mark1)
	     (%filling-set-next-line point nil prefix))
	    ((%auto-fill-line-as-region-p point mark2 column)
	     (if (and insertp
		      (not (or (end-line-p mark1)
			       (whitespace-attribute-p (next-character mark1)))))
		 (insert-character mark1 #\space))
	     (auto-fill-line-as-region point (move-mark mark2 point) prefix column)
	     (move-mark point mark1)
	     (if (and insertp (end-line-p point))
		 (insert-character point #\space)))
	    ((not (or (end-line-p mark1)
		      (whitespace-attribute-p (next-character mark1))))
	     (insert-character mark1 #\space)
	     (%filling-set-next-line point nil prefix)
	     (mark-after point)
	     (%auto-fill-clean-previous-line mark1 mark2))
	    (t
	     (%filling-set-next-line point insertp prefix)
	     (%auto-fill-clean-previous-line mark1 mark2))))))



;;; AUTO-FILL-LINE-AS-REGION basically grabs a line as a region and fills
;;; it.  However, it knows about comments and makes auto filling a comment
;;; line as a region look the same as a typical "back up a word and break
;;; the line."  When there is a comment, then region starts where the
;;; comment starts instead of the beginning of the line, but the presence
;;; of a prefix overrides all this.

(defun auto-fill-line-as-region (point mark prefix column)
  (let* ((start (value comment-start))
	 (begin (value comment-begin))
	 (end (value comment-end)))
    (line-start mark)
    (cond ((and (not prefix) start (to-line-comment mark start))
	   (fill-region (region mark (line-end point))
			(gen-comment-prefix mark start begin)
			column)
	   (when end
	     (line-start point)
	     (do ()
		 ((mark>= mark point))
	       (if (not (to-comment-end mark end)) (insert-string mark end))
	       (line-offset mark 1 0))))	   
	  (t (fill-region (region mark (line-end point)) prefix column)))))



(defun %auto-fill-blank-before-p (point prefix)
  "is true if whitespace only precedes point except for the prefix."
  (or (blank-before-p point)
      (with-mark ((temp point))
	(reverse-find-attribute temp :whitespace #'zerop)
	(<= (mark-column temp) (length prefix)))))



;;;      %AUTO-FILL-LINE-AS-REGION-P determines if the line point and mark2
;;; sit on is so long that it might as well be filled as if it were a
;;; region.  Mark2 is mark= to point at the beginning of the last word on
;;; the line and is then moved over the whitespace before point.  If the
;;; word end prior the last word on the line is on the same line and not
;;; before column, then fill the line as a region.

(defun %auto-fill-line-as-region-p (point mark2 column)
  (reverse-find-attribute mark2 :whitespace #'zerop)
  (and (same-line-p mark2 point)
       (> (mark-column mark2) column)))



(defun %auto-fill-clean-previous-line (mark1 mark2)
  (when (line-offset mark1 -1)
    (line-end mark1)
    (move-mark mark2 mark1)
    (unless (and (reverse-find-attribute mark1 :whitespace #'zerop)
		 (same-line-p mark1 mark2))
      (line-start mark1 (mark-line mark2)))
    (delete-region (region mark1 mark2))))



;;; %FILLING-SET-NEXT-LINE gets a new blank line and sets it up with the
;;; prefix and places the point correctly.  The argument point must alias
;;; (current-point).

(defun %filling-set-next-line (point insertp prefix)
  (cond ((and (value auto-fill-space-indent) (not prefix))
	 (indent-new-comment-line-command nil))
	(t (new-line-command nil)
	   (if prefix (insert-string point prefix))))
  (if (not (find-attribute point :whitespace)) (line-end point))
  (if insertp (insert-character point #\space)))



;;;; -- Paragraph Filling --


;;;      %FILL-PARAGRAPH-START takes a mark that has just been moved
;;; forward over some paragraph.  After moving to the beginning of it, we
;;; place the mark appropriately for filling the paragraph as a region.

(defun %fill-paragraph-start (mark prefix prefix-len)
  (paragraph-offset mark -1)
  (skip-prefix-if-here mark prefix prefix-len)
  (if (text-blank-line-p mark)
      (line-offset mark 1 0)
      (line-start mark)))



;;;; -- Region Filling --


;;;      FILL-REGION-BY-PARAGRAPHS finds paragraphs and uses region filling
;;; primitives to fill them.  Tmark2 is only used for the first paragraph; we
;;; need a mark other than start in case start is in the middle of a paragraph
;;; instead of between two.
;;;
(defun fill-region-by-paragraphs (region &optional
					 (prefix (value fill-prefix))
					 (column (value fill-column)))
  "Finds paragraphs in region and fills them as distinct regions using
   FILL-REGION."
  (with-mark ((start (region-start region) :left-inserting))
    (with-mark ((tmark1 start :left-inserting)
		(tmark2 start :left-inserting)) ;only used for first para.
      (let ((region (region (copy-mark (region-start region)) ;deleted by undo.
			    (copy-mark (region-end region))))
	    (undo-region (copy-region region))
	    (end (region-end region))
	    (prefix-len (length prefix))
	    (paragraphp (paragraph-offset tmark1 1)))
	(when paragraphp
	  (%fill-paragraph-start (move-mark tmark2 tmark1) prefix prefix-len)
	  (if (mark>= tmark2 start) (move-mark start tmark2))
	  (cond ((mark>= tmark1 end)
		 (fill-region-aux start end prefix prefix-len column))
		(t
		 (fill-region-aux start tmark1 prefix prefix-len column)
		 (do ((paragraphp (mark-paragraph start tmark1)
				  (mark-paragraph start tmark1)))
		     ((not paragraphp))
		   (if (mark> start end)
		       (return)
		       (cond ((mark>= tmark1 end)
			      (fill-region-aux start end prefix
					       prefix-len column)
			      (return))
			     (t (fill-region-aux start tmark1
						 prefix prefix-len column))))))))
	(make-region-undo :twiddle "Fill Region" region undo-region)))))

(defun fill-region (region &optional
			   (prefix (value fill-prefix))
			   (column (value fill-column)))
  "Fills a region using the given prefix and column."
  (let ((prefix (if (and prefix (string= prefix "")) () prefix)))
    (with-mark ((start (region-start region) :left-inserting))
      (check-fill-prefix prefix column start)
      (fill-region-aux start (region-end region)
		       prefix (length prefix) column))))



;;;      FILL-REGION-AUX grinds over a region between fill-mark and
;;; end-mark deleting blank lines and filling lines.  For each line, the
;;; extra whitespace between words is collapsed to one space, and at the
;;; end and beginning of the line it is deleted.  We do not return after
;;; realizing that fill-mark is after end-mark if the line needs to be
;;; broken; it may be the case that there are several filled line lengths
;;; of material before end-mark on the current line.

(defun fill-region-aux (fill-mark end-mark prefix prefix-len column)
  (if (and (start-line-p fill-mark) prefix)
      (fill-region-prefix-line fill-mark prefix prefix-len))
  (with-mark ((mark1 fill-mark :left-inserting)
	      (cmark fill-mark :left-inserting))
    (do ((collapse-p t))
	(nil)
      (line-end fill-mark)
      (line-start (move-mark mark1 fill-mark))
      (skip-prefix-if-here mark1 prefix prefix-len)
      (cond ((mark>= fill-mark end-mark)
	     (if (mark= fill-mark end-mark)
		 (fill-region-clear-eol fill-mark))
	     (cond ((> (mark-column end-mark) column)
		    (when collapse-p
		      (fill-region-collapse-whitespace cmark end-mark)
		      (setf collapse-p nil))
		    (fill-region-break-line fill-mark prefix
					    prefix-len end-mark column))
		   (t (return))))
	    ((blank-after-p mark1)
	     (fill-region-delete-blank-lines fill-mark end-mark prefix prefix-len)
	     (cond ((mark< fill-mark end-mark)
		    (if prefix
			(fill-region-prefix-line fill-mark prefix prefix-len))
		    (fill-region-clear-bol fill-mark)
		    (move-mark cmark fill-mark))
		   (t (return)))
	     (setf collapse-p t))
	    (t (fill-region-clear-eol fill-mark)
	       (if collapse-p (fill-region-collapse-whitespace cmark fill-mark))
	       (cond ((> (mark-column fill-mark) column)
		      (fill-region-break-line fill-mark prefix
					      prefix-len end-mark column)
		      (setf collapse-p nil))
		     (t (fill-region-get-next-line fill-mark column
						   prefix prefix-len end-mark)
			(move-mark cmark fill-mark)
			(setf collapse-p t))))))
    (move-mark fill-mark end-mark)))



;;;      FILL-REGION-BREAK-LINE breaks lines as close to the low side
;;; column as possible.  The first branch handles a word lying across
;;; column while the second takes care of whitespace passing column.  If
;;; FILL-REGION-WORD-PAST-COLUMN encountered a single word stretching over
;;; column, it would leave an extra opened line that needs to be cleaned up
;;; or filled up.

(defun fill-region-break-line (fill-mark prefix prefix-length
					  end-mark column)
  (with-mark ((mark1 fill-mark :left-inserting))
    (move-to-position mark1 column)
    (cond ((not (whitespace-attribute-p (next-character mark1)))
	   (if (not (find-attribute mark1 :whitespace))
	       (line-end mark1))
	   (move-mark fill-mark mark1)
	   (if (eq (fill-region-word-past-column fill-mark mark1 prefix)
		   :handled-oversized-word)
	       (if (mark>= fill-mark end-mark)
		   (delete-characters (line-start fill-mark)
				      prefix-length)
		   (delete-characters fill-mark 1))))
	  (t (move-mark fill-mark mark1)
	     (unless (and (reverse-find-attribute mark1 :whitespace #'zerop)
			  (same-line-p mark1 fill-mark))
	       (line-start mark1 (mark-line fill-mark)))
	     ;; forward find must move mark because of cond branch we are in.
	     (find-attribute fill-mark :whitespace #'zerop)
	     (unless (same-line-p mark1 fill-mark)
	       (line-end fill-mark (mark-line mark1)))
	     (delete-region (region mark1 fill-mark))
	     (insert-character fill-mark #\newline)
	     (if prefix (insert-string fill-mark prefix))))))



;;;      FILL-REGION-WORD-PAST-COLUMN takes a point and a second mark that
;;; is mark= at the end of some word.  First, point is moved before the
;;; previous "word."  If the word is effectively the only word on the line,
;;; it should not be moved down to the next line as it will leave a blank
;;; line.

(defun fill-region-word-past-column (point mark1 prefix)
  (with-mark ((mark2 (copy-mark point :left-inserting)))
    (let ((point-moved-p (reverse-find-attribute point :whitespace))
	  (hack-for-fill-region :handled-normal-case))
      (cond ((or (not point-moved-p)
		 (%auto-fill-blank-before-p point prefix))
	     (setf hack-for-fill-region :handled-oversized-word)
	     (move-mark point mark1)
	     (fill-region-set-next-line point prefix))
	    (t (fill-region-set-next-line point prefix)
	       (%auto-fill-clean-previous-line mark1 mark2)))
      hack-for-fill-region)))

(defun fill-region-set-next-line (point prefix)
  (insert-character point #\newline)
  (if prefix (insert-string point prefix))
  (if (not (find-attribute point :whitespace)) (line-end point)))



;;;      FILL-REGION-GET-NEXT-LINE gets another line when the current one
;;; is short of the fill column.  It cleans extraneous whitespace from the
;;; beginning of the next line to fill.  To save typical redisplay the
;;; length of the first word is added to the ending column of the current
;;; line to see if it extends past the fill column; if it does, then the
;;; fill-mark is left on the new line instead of merging the new line with
;;; the current one.  The fill-mark is left after a prefix (if there is one)
;;; on a new line, before the first word brought up to the current line, or
;;; after the end mark.

(defun fill-region-get-next-line (fill-mark column prefix prefix-len end-mark)
  (let ((prev-end-pos (mark-column fill-mark))
	(two-spaces-p (fill-region-insert-two-spaces-p fill-mark)))
    (with-mark ((tmark fill-mark :left-inserting))
      (fill-region-find-next-line fill-mark prefix prefix-len end-mark)
      (move-mark tmark fill-mark)
      (cond ((mark< fill-mark end-mark)
	     (skip-prefix-if-here tmark prefix prefix-len)
	     (fill-region-clear-bol tmark)
	     (let ((beginning-pos (mark-column tmark)))
	       (find-attribute tmark :whitespace)
	       (cond ((> (+ prev-end-pos (if two-spaces-p 2 1)
			    (- (mark-column tmark) beginning-pos))
			 column)
		      (if prefix
			  (fill-region-prefix-line fill-mark prefix prefix-len)))
		     (t
		      (if (and prefix
			       (%line-has-prefix-p fill-mark prefix prefix-len))
			  (delete-characters fill-mark prefix-len))
		      (delete-characters fill-mark -1)
		      (insert-character fill-mark #\space)
		      (if two-spaces-p (insert-character fill-mark #\space))))))
	    (t
	     (mark-after fill-mark))))))



;;;      FILL-REGION-FIND-NEXT-LINE finds the next non-blank line, modulo
;;; fill prefixes, and deletes the intervening lines.  Fill-mark is left at
;;; the beginning of the next line.

(defun fill-region-find-next-line (fill-mark prefix prefix-len end-mark)
  (line-offset fill-mark 1 0)
  (when (mark< fill-mark end-mark)
    (skip-prefix-if-here fill-mark prefix prefix-len)
    (if (blank-after-p fill-mark)
	(fill-region-delete-blank-lines fill-mark end-mark prefix prefix-len)
	(line-start fill-mark))))



;;;      FILL-REGION-DELETE-BLANK-LINES deletes the blank line mark is on
;;; and all successive blank lines.  Mark is left at the beginning of the
;;; first non-blank line by virtue of its placement and region deletions.

(defun fill-region-delete-blank-lines (mark end-mark prefix prefix-len)
  (line-start mark)
  (with-mark ((tmark mark :left-inserting))
    (do ((linep (line-offset tmark 1 0) (line-offset tmark 1 0)))
	((not linep)
	 (move-mark tmark end-mark)
	 (delete-region (region mark tmark)))
      (skip-prefix-if-here tmark prefix prefix-len)
      (when (mark>= tmark end-mark)
	(move-mark tmark end-mark)
	(delete-region (region mark tmark))
	(return))
      (unless (blank-after-p tmark)
	(line-start tmark)
	(delete-region (region mark tmark))
	(return)))))



;;;      FILL-REGION-CLEAR-BOL clears the initial whitespace on a line
;;; known to be non-blank.  Note that the fill prefix is not considered, so
;;; the mark must have been moved over it already if there is one.

(defun fill-region-clear-bol (mark)
  (with-mark ((tmark mark :left-inserting))
    (find-attribute tmark :whitespace #'zerop)
    (unless (mark= mark tmark)
      (delete-region (region mark tmark)))))



;;;      FILL-REGION-COLLAPSE-WHITESPACE deletes extra whitespace between
;;; blocks of non-whitespace characters from mark1 to mark2.  Tabs are
;;; converted into a single space.  Mark2 must be on the same line as mark1
;;; since there is no concern of newlines, prefixes on a new line, blank
;;; lines between blocks of non-whitespace characters, etc.

(defun fill-region-collapse-whitespace (mark1 mark2)
  (with-mark ((tmark mark1 :left-inserting))
    ;; skip whitespace at beginning of line or single space between words
    (find-attribute mark1 :whitespace #'zerop)
    (unless (mark>= mark1 mark2)
      (do ()
	  (nil)
	(if (not (find-attribute mark1 :whitespace)) ;not end of buffer
	    (return))
	(if (mark>= mark1 mark2) (return))
	(if (char/= (next-character mark1) #\space)
	    ;; since only on one line, must be tab or space
	    (setf (next-character mark1) #\space))
	(move-mark tmark mark1)
	(if (mark= (mark-after mark1) mark2) (return))
	(let ((char (next-character mark1)))
	  (when (and (fill-region-insert-two-spaces-p tmark)
		     (char= char #\space))
	    ;; if at the end of a sentence, don't blow away the second space
	    (if (mark= (mark-after mark1) mark2)
		(return)
		(setf char (next-character mark1))))
	  (when (whitespace-attribute-p char) ;more whitespace than necessary
	    (find-attribute (move-mark tmark mark1) :whitespace #'zerop)
	    (if (mark>= tmark mark2) (move-mark tmark mark2))
	    (delete-region (region mark1 tmark))))))))



;;;      FILL-REGION-CLEAR-EOL must check the result of
;;; REVERSE-FIND-ATTRIBUTE because if fill-mark did not move, then we are
;;; only whitespace away from the beginning of the buffer.

(defun fill-region-clear-eol (fill-mark)
  (with-mark ((mark1 fill-mark :left-inserting))
    (unless (and (reverse-find-attribute mark1 :whitespace #'zerop)
		 (same-line-p mark1 fill-mark))
      (line-start mark1 (mark-line fill-mark)))
    (delete-region (region mark1 fill-mark))))



(defun fill-region-prefix-line (fill-mark prefix prefix-length)
  (if (%line-has-prefix-p fill-mark prefix prefix-length)
      (character-offset fill-mark prefix-length)
      (insert-string fill-mark prefix)))



(defun %line-has-prefix-p (mark prefix prefix-length)
  (declare (simple-string prefix))
  (if (>= (line-length (mark-line mark)) prefix-length)
      (string= prefix (the simple-string (line-string (mark-line mark)))
	       :end2 prefix-length)))



;;;      FILL-REGION-INSERT-TWO-SPACES-P returns true if a sentence
;;; terminator is followed by any number of "closing characters" such as
;;; ",',),etc.  If there is a sentence terminator at the end of the current
;;; line, it must be assumed to be the end of a sentence as opposed to an
;;; abbreviation.  Why?  Because EMACS does, and besides, what would Lisp
;;; code be without heuristics.

(defun fill-region-insert-two-spaces-p (mark)
  (do ((n 0 (1+ n)))
      ((not (sentence-closing-char-attribute-p (previous-character mark)))
       (cond ((sentence-terminator-attribute-p (previous-character mark))
	      (character-offset mark n))
	     (t (character-offset mark n) nil)))
    (mark-before mark)))



(defun check-fill-prefix (prefix column mark)
  (when prefix
    (insert-character mark #\newline)
    (insert-character mark #\newline)
    (mark-before mark)
    (insert-string mark prefix)
    (let ((pos (mark-column mark)))
      (declare (simple-string prefix))
      (mark-after mark)
      (delete-characters mark (- (+ (length prefix) 2)))
      (if (>= pos column)
	  (editor-error
	   "The fill prefix length is longer than the fill column.")))))
