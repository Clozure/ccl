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
;;; This file contains searching and replacing commands.
;;;

(in-package :hemlock)



;;;; Some global state.

(defvar *last-search-string* () "Last string searched for.")
(defvar *last-search-pattern*
  (new-search-pattern :string-insensitive :forward "Foo")
  "Search pattern we keep around so we don't cons them all the time.")
(defvar *search-wrapped-p* nil "True if search wrapped")

(defhvar "String Search Ignore Case"
  "When set, string searching commands use case insensitive."
  :value t)

(defun get-search-pattern (string direction)
  (declare (simple-string string))
  (when (zerop (length string)) (editor-error))
  (setq *last-search-string* string)
  (setq *last-search-pattern*
	(new-search-pattern (if (value string-search-ignore-case)
				:string-insensitive
				:string-sensitive)
			    direction string *last-search-pattern*)))



;;;; Vanilla searching.

(defcommand "Forward Search" (p &optional string)
  "Do a forward search for a string.
  Prompt for the string and leave the point after where it is found."
  "Searches for the specified String in the current buffer."
  (declare (ignore p))
  (if (not string)
      (setq string (prompt-for-string :prompt "Search: "
				      :default *last-search-string*
				      :help "String to search for")))
  (let* ((pattern (get-search-pattern string :forward))
	 (point (current-point))
	 (mark (copy-mark point))
	 ;; find-pattern moves point to start of match, and returns is # chars matched
	 (won (find-pattern point pattern)))
    (cond (won (move-mark mark point)
	       (character-offset point won)
               (push-buffer-mark mark t)
	       (hi::note-selection-set-by-search))
	  (t (delete-mark mark)
	     (editor-error)))
    (clear-echo-area)))

(defcommand "Reverse Search" (p &optional string)
  "Do a backward search for a string.
   Prompt for the string and leave the point before where it is found."
  "Searches backwards for the specified String in the current buffer."
  (declare (ignore p))
  (if (not string)
      (setq string (prompt-for-string :prompt "Reverse Search: "
				      :default *last-search-string* 
				      :help "String to search for")))
  (let* ((pattern (get-search-pattern string :backward))
	 (point (current-point))
	 (mark (copy-mark point))
	 (won (find-pattern point pattern)))
    (cond (won (move-mark mark point)
	       (character-offset mark won)
	       (push-buffer-mark mark t)
	       (hi::note-selection-set-by-search))
	  (t (delete-mark mark)
	     (editor-error)))
    (clear-echo-area)))



;;;; Incremental searching.

(defun i-search-pattern (string direction)
  (setq *last-search-pattern*
	(new-search-pattern (if (value string-search-ignore-case)
				:string-insensitive
				:string-sensitive)
			    direction string *last-search-pattern*)))

;;;      %I-SEARCH-ECHO-REFRESH refreshes the echo buffer for incremental
;;; search.
;;;
(defun %i-search-echo-refresh (string direction failure)
  (when (interactive)
    (clear-echo-area)
    (format *echo-area-stream* 
	    "~:[~;Failing ~]~:[~;Overwrapped ~]~:[Reverse I-Search~;I-Search~]: ~A"
	    failure *search-wrapped-p* (eq direction :forward) string)))

(defcommand "Incremental Search" (p)
  "Searches for input string as characters are provided.
  These are the default I-Search command characters:  ^Q quotes the
  next character typed.  Backspace cancels the last character typed.  ^S
  repeats forward, and ^R repeats backward.  ^R or ^S with empty string
  either changes the direction or yanks the previous search string.
  Escape exits the search unless the string is empty.  Escape with 
  an empty search string calls the non-incremental search command.  
  Other control characters cause exit and execution of the appropriate 
  command.  If the search fails at some point, ^G and backspace may be 
  used to backup to a non-failing point; also, ^S and ^R may be used to
  look the other way.  ^W extends the search string to include the the word 
  after the point. ^G during a successful search aborts and returns
  point to where it started."
  "Search for input string as characters are typed in.
  It sets up for the recursive searching and checks return values."
  (declare (ignore p))
  (setf (last-command-type) nil)
  (%i-search-echo-refresh "" :forward nil)
  (let* ((*search-wrapped-p* nil)
	 (point (current-point))
	 (save-start (copy-mark point :temporary)))
    (with-mark ((here point))
      (when (eq (catch 'exit-i-search
		  (%i-search "" point here :forward nil))
		:control-g)
	(move-mark point save-start)
	(invoke-hook abort-hook)
	(editor-error))
      (if (region-active-p)
	  (delete-mark save-start)
	  (push-buffer-mark save-start)))))


(defcommand "Reverse Incremental Search" (p)
  "Searches for input string as characters are provided.
  These are the default I-Search command characters:  ^Q quotes the
  next character typed.  Backspace cancels the last character typed.  ^S
  repeats forward, and ^R repeats backward.  ^R or ^S with empty string
  either changes the direction or yanks the previous search string.
  Altmode exits the search unless the string is empty.  Altmode with 
  an empty search string calls the non-incremental search command.  
  Other control characters cause exit and execution of the appropriate 
  command.  If the search fails at some point, ^G and backspace may be 
  used to backup to a non-failing point; also, ^S and ^R may be used to
  look the other way.  ^G during a successful search aborts and returns
  point to where it started."
  "Search for input string as characters are typed in.
  It sets up for the recursive searching and checks return values."
  (declare (ignore p))
  (setf (last-command-type) nil)
  (%i-search-echo-refresh "" :backward nil)
  (let* ((*search-wrapped-p* nil)
	 (point (current-point))
	 (save-start (copy-mark point :temporary)))
    (with-mark ((here point))
      (when (eq (catch 'exit-i-search
		  (%i-search "" point here :backward nil))
		:control-g)
	(move-mark point save-start)
	(invoke-hook abort-hook)
	(editor-error))
      (if (region-active-p)
	  (delete-mark save-start)
	  (push-buffer-mark save-start)))))

;;;      %I-SEARCH recursively (with support functions) searches to provide
;;; incremental searching.  There is a loop in case the recursion is ever
;;; unwound to some call.  curr-point must be saved since point is clobbered
;;; with each recursive call, and the point must be moved back before a
;;; different letter may be typed at a given call.  In the CASE at :cancel
;;; and :control-g, if the string is not null, an accurate pattern for this
;;; call must be provided when %I-SEARCH-CHAR-EVAL is called a second time
;;; since it is possible for ^S or ^R to be typed.
;;;
(defun %i-search (string point trailer direction failure)
  (do* ((curr-point (copy-mark point :temporary))
        (curr-trailer (copy-mark trailer :temporary)))
       (nil)
    (let* ((next-key-event (recursive-get-key-event hi::*editor-input* t))
	   (val (%i-search-char-eval next-key-event string point trailer
                                 direction failure))
	   (empty-string-p (zerop (length string))))
      (case val	
        (:mouse-exit
         (clear-echo-area)
         (throw 'exit-i-search nil))
        (:cancel
         (%i-search-echo-refresh string direction failure)
         (unless empty-string-p
           (i-search-pattern string direction))) ;sets *last-search-pattern*
        (:return-cancel ;backspace was typed
	 (if empty-string-p
	     (beep)
	     (return :cancel)))
        (:control-g
         (when failure (return :control-g))
         (%i-search-echo-refresh string direction nil)
         (unless empty-string-p
           (i-search-pattern string direction)))) ;*last-search-pattern*
      (move-mark point curr-point)
      (move-mark trailer curr-trailer))))

;;;      %I-SEARCH-CHAR-EVAL evaluates the last character typed and takes
;;; necessary actions.
;;;
(defun %i-search-char-eval (key-event string point trailer direction failure)
  (declare (simple-string string))
  (cond ((let ((character (key-event-char key-event)))
	   (and character (standard-char-p character)))
	 (%i-search-printed-char key-event string point trailer
				 direction failure))
	((or (logical-key-event-p key-event :forward-search)
	     (logical-key-event-p key-event :backward-search))
	 (%i-search-control-s-or-r key-event string point trailer
				   direction failure))
	((logical-key-event-p key-event :cancel) :return-cancel)
	((logical-key-event-p key-event :extend-search-word)
	 (with-mark ((end point))
	   (word-offset end 1)
	   (let ((extension (region-to-string (region point end))))
	     (%i-search-extend-string string extension point trailer direction failure))))	     
	((logical-key-event-p key-event :abort)
	 (unless failure
	   (clear-echo-area)
	   (message "Search aborted.")
	   (throw 'exit-i-search :control-g))
	 :control-g)
	((logical-key-event-p key-event :quote)
	 (%i-search-printed-char (get-key-event hi::*editor-input* t)
				 string point trailer direction failure))
	((and (zerop (length string)) (logical-key-event-p key-event :exit))
	 (if (eq direction :forward)
	     (forward-search-command nil)
	     (reverse-search-command nil))
	 (throw 'exit-i-search nil))
	(t
	 (unless (logical-key-event-p key-event :exit)
	   (unget-key-event key-event hi::*editor-input*))
	 (unless (zerop (length string))
	   (setf *last-search-string* string))
	 (throw 'exit-i-search nil))))

;;;      %I-SEARCH-CONTROL-S-OR-R handles repetitions in the search.  Note
;;; that there cannot be failure in the last COND branch: since the direction
;;; has just been changed, there cannot be a failure before trying a new
;;; direction.
;;;
(defun %i-search-control-s-or-r (key-event string point trailer
					   direction failure)
  (let ((forward-direction-p (eq direction :forward))
	(forward-character-p (logical-key-event-p key-event :forward-search)))
    (cond ((zerop (length string))
	   (%i-search-empty-string point trailer direction forward-direction-p
				   forward-character-p))
	  ((eq forward-direction-p forward-character-p) ;keep searching in the same direction
	   (cond ((eq failure :first-failure)
		  (cond (forward-direction-p
			 (buffer-start point)
			 (buffer-start trailer)
			 (character-offset trailer (length string)))
			(t
			 (buffer-end point)
			 (buffer-end trailer)))
		  (push-buffer-mark (copy-mark point))
		  (let ((*search-wrapped-p* t))
		    (%i-search-echo-refresh string direction nil)
		    (%i-search-find-pattern string point trailer direction)))
		  (failure
		   (%i-search string point trailer direction t))
		  (t
		   (%i-search-find-pattern string point (move-mark trailer point)
					   direction))))
	  (t
	   (let ((new-direction (if forward-character-p :forward :backward)))
	     (%i-search-echo-refresh string new-direction nil)
	     (i-search-pattern string new-direction) ;sets *last-search-pattern*
	     (%i-search-find-pattern string point (move-mark trailer point)
				     new-direction))))))


;;;      %I-SEARCH-EMPTY-STRING handles the empty string case when a ^S
;;; or ^R is typed.  If the direction and character typed do not agree,
;;; then merely switch directions.  If there was a previous string, search
;;; for it, else flash at the guy.
;;;
(defun %i-search-empty-string (point trailer direction forward-direction-p
				     forward-character-p)
  (cond ((eq forward-direction-p (not forward-character-p))
	 (let ((direction (if forward-character-p :forward :backward)))
	   (%i-search-echo-refresh "" direction nil)
	   (%i-search "" point trailer direction nil)))
	(*last-search-string*
	 (%i-search-echo-refresh *last-search-string* direction nil)
	 (i-search-pattern *last-search-string* direction) ;sets *last-search-pattern*
	 (%i-search-find-pattern *last-search-string* point trailer direction))
	(t (beep))))


;;;      %I-SEARCH-PRINTED-CHAR handles the case of standard character input.
;;; If the direction is backwards, we have to be careful not to MARK-AFTER
;;; the end of the buffer or to include the next character at the beginning
;;; of the search.
;;;
(defun %i-search-printed-char (key-event string point trailer direction failure)
  (let ((tchar (hemlock-ext:key-event-char key-event)))
    (unless tchar (editor-error "Not a text character -- ~S" (key-event-char
							      key-event)))
    (when (interactive)
      (insert-character (buffer-point *echo-area-buffer*) tchar)
      (force-output *echo-area-stream*))
    (let ((new-string (concatenate 'simple-string string (string tchar))))
      (i-search-pattern new-string direction) ;sets *last-search-pattern*
      (cond (failure (%i-search new-string point trailer direction failure))
	    ((and (eq direction :backward) (next-character trailer))
	     (%i-search-find-pattern new-string point (mark-after trailer)
				     direction))
	    (t
	     (%i-search-find-pattern new-string point trailer direction))))))

(defun %i-search-extend-string (string extension point trailer direction failure)
  (when (interactive)
    (insert-string (buffer-point *echo-area-buffer*) extension)
    (force-output *echo-area-stream*))
  (let ((new-string (concatenate 'simple-string string extension)))
    (i-search-pattern new-string direction) ;sets *last-search-pattern*
    (cond (failure (%i-search new-string point trailer direction failure))
	  ((and (eq direction :backward) (next-character trailer))
	   (%i-search-find-pattern new-string point (mark-after trailer)
				   direction))
	  (t
	   (%i-search-find-pattern new-string point trailer direction)))))


;;;      %I-SEARCH-FIND-PATTERN takes a pattern for a string and direction
;;; and finds it, updating necessary pointers for the next call to %I-SEARCH.
;;; If the search failed, tell the user and do not move any pointers.
;;;
(defun %i-search-find-pattern (string point trailer direction)
  (let ((found-offset (find-pattern trailer *last-search-pattern*)))
    (cond (found-offset
	    (cond ((eq direction :forward)
		   (character-offset (move-mark point trailer) found-offset))
		  (t
		   (move-mark point trailer)
		   (character-offset trailer found-offset)))
	    (push-buffer-mark (copy-mark trailer) t)
	    (hi::note-selection-set-by-search)
	    (%i-search string point trailer direction nil))
	  (t
	   (%i-search-echo-refresh string direction t)
	   (if (interactive)
	       (beep)
	       (editor-error "I-Search failed."))
	   (%i-search string point trailer direction :first-failure)))))



;;;; Replacement commands:

(defcommand "Replace String" (p &optional
				(target (prompt-for-string
					 :prompt "Replace String: "
					 :help "Target string"
					 :default *last-search-string*))
				(replacement (prompt-for-string
					      :prompt "With: "
					      :help "Replacement string")))
  "Replaces the specified Target string with the specified Replacement
   string in the current buffer for all occurrences after the point or within
   the active region, depending on whether it is active."
  "Replaces the specified Target string with the specified Replacement
   string in the current buffer for all occurrences after the point or within
   the active region, depending on whether it is active.  The prefix argument
   may limit the number of replacements."
  (multiple-value-bind (ignore count)
		       (query-replace-function p target replacement
					       "Replace String" t)
    (declare (ignore ignore))
    (message "~D Occurrences replaced." count)))

(defcommand "Query Replace" (p &optional
			       (target (prompt-for-string
					:prompt "Query Replace: "
					:help "Target string"
					:default *last-search-string*))
			       (replacement (prompt-for-string
					     :prompt "With: "
					     :help "Replacement string")))
  "Replaces the Target string with the Replacement string if confirmation
   from the keyboard is given.  If the region is active, limit queries to
   occurrences that occur within it, otherwise use point to end of buffer."
  "Replaces the Target string with the Replacement string if confirmation
   from the keyboard is given.  If the region is active, limit queries to
   occurrences that occur within it, otherwise use point to end of buffer.
   A prefix argument may limit the number of queries."
  (let ((mark (copy-mark (current-point))))
    (multiple-value-bind (ignore count)
			 (query-replace-function p target replacement
						 "Query Replace")
      (declare (ignore ignore))
      (message "~D Occurrences replaced." count))
    (push-buffer-mark mark)))


(defhvar "Case Replace"
  "If this is true then \"Query Replace\" will try to preserve case when
  doing replacements."
  :value t)

(defstruct (replace-undo (:constructor make-replace-undo (mark region)))
  mark
  region)

(setf (documentation 'replace-undo-mark 'function)
      "Return the mark where a replacement was made.")
(setf (documentation 'replace-undo-region 'function)
      "Return region deleted due to replacement.")

(defvar *query-replace-undo-data* nil)

;;; REPLACE-THAT-CASE replaces a string case-sensitively.  Lower, Cap and Upper
;;; are the original, capitalized and uppercase replacement strings.  Mark is a
;;; :left-inserting mark after the text to be replaced.  Length is the length
;;; of the target string.  If dumb, then do a simple replace.  This pushes
;;; an undo information structure into *query-replace-undo-data* which
;;; QUERY-REPLACE-FUNCTION uses.
;;;
(defun replace-that-case (lower cap upper mark length dumb)
  (character-offset mark (- length))
  (let ((insert (cond (dumb lower)
		      ((upper-case-p (next-character mark))
		       (mark-after mark)
		       (prog1 (if (upper-case-p (next-character mark)) upper cap)
			      (mark-before mark)))
		      (t lower))))
    (with-mark ((undo-mark1 mark :left-inserting)
		(undo-mark2 mark :left-inserting))
      (character-offset undo-mark2 length)
      (push (make-replace-undo
	     ;; Save :right-inserting, so the INSERT-STRING at mark below
	     ;; doesn't move the copied mark the past replacement.
	     (copy-mark mark :right-inserting)
	     (delete-and-save-region (region undo-mark1 undo-mark2)))
	    *query-replace-undo-data*))
    (insert-string mark insert)))

;;; QUERY-REPLACE-FUNCTION does the work for the main replacement commands:
;;; "Query Replace", "Replace String", "Group Query Replace", "Group Replace".
;;; Name is the name of the command for undoing purposes.  If doing-all? is
;;; true, this replaces all ocurrences for the non-querying commands.  This
;;; returns t if it completes successfully, and nil if it is aborted.  As a
;;; second value, it returns the number of replacements.
;;;
;;; The undo method, before undo'ing anything, makes all marks :left-inserting.
;;; There's a problem when two replacements are immediately adjacent, such as
;;;    foofoo
;;; replacing "foo" with "bar".  If the marks were still :right-inserting as
;;; REPLACE-THAT-CASE makes them, then undo'ing the first replacement would
;;; bring the two marks together due to the DELETE-CHARACTERS.  Then inserting
;;; the region would move the second replacement's mark to be before the first
;;; replacement.
;;;
(defun query-replace-function (count target replacement name
			       &optional (doing-all? nil))
  (declare (simple-string replacement))
  (let ((replacement-len (length replacement))
	(*query-replace-undo-data* nil))
    (when (and count (minusp count))
      (editor-error "Replacement count is negative."))
    (get-search-pattern target :forward)
    (unwind-protect
	(query-replace-loop (get-count-region) (or count -1) target replacement
			    replacement-len (current-point) doing-all?)
      (let ((undo-data (nreverse *query-replace-undo-data*)))
	(save-for-undo name
	  #'(lambda ()
	      (dolist (ele undo-data)
		(setf (mark-kind (replace-undo-mark ele)) :left-inserting))
	      (dolist (ele undo-data)
		(let ((mark (replace-undo-mark ele)))
		  (delete-characters mark replacement-len)
		  (ninsert-region mark (replace-undo-region ele)))))
	  #'(lambda ()
	      (dolist (ele undo-data)
		(delete-mark (replace-undo-mark ele)))))))))

;;; QUERY-REPLACE-LOOP is the essence of QUERY-REPLACE-FUNCTION.  The first
;;; value is whether we completed all replacements, nil if we aborted.  The
;;; second value is how many replacements occurred.
;;;
(defun query-replace-loop (region count target replacement replacement-len
			   point doing-all?)
  (with-mark ((last-found point)
	      ;; Copy REGION-END before moving point to REGION-START in case
	      ;; the end is point.  Also, make it permanent in case we make
	      ;; replacements on the last line containing the end.
	      (stop-mark (region-end region) :left-inserting))
    (move-mark point (region-start region))
    (let ((length (length target))
	  (cap (string-capitalize replacement))
	  (upper (string-upcase replacement))
	  (dumb (not (and (every #'(lambda (ch) (or (not (both-case-p ch))
						    (lower-case-p ch)))
				 (the string replacement))
			  (value case-replace)))))
      (values
       (loop
	 (let ((won (find-pattern point *last-search-pattern*)))
	   (when (or (null won) (zerop count) (mark> point stop-mark))
	     (character-offset (move-mark point last-found) replacement-len)
	     (return t))
	   (decf count)
	   (move-mark last-found point)
	   (character-offset point length)
	   (if doing-all?
	       (replace-that-case replacement cap upper point length dumb)
	       (command-case
		   (:prompt
		    "Query replace: "
		    :help "Type one of the following single-character commands:"
		    :change-window nil :bind key-event)
		 (:yes "Replace this occurrence."
		       (replace-that-case replacement cap upper point length
					  dumb))
		 (:no "Don't replace this occurrence, but continue.")
		 (:do-all "Replace this and all remaining occurrences."
			  (replace-that-case replacement cap upper point length
					     dumb)
			  (setq doing-all? t))
		 (:do-once "Replace this occurrence, then exit."
			   (replace-that-case replacement cap upper point length
					      dumb)
			   (return nil))
		 (:recursive-edit
		  "Go into a recursive edit at the current position."
		  (do-recursive-edit)
		  (get-search-pattern target :forward))
		 (:exit "Exit immediately."
			(return nil))
		 (t (unget-key-event key-event hi::*editor-input*)
		    (return nil))))))
       (length (the list *query-replace-undo-data*))))))



;;;; Occurrence searching.

(defcommand "List Matching Lines" (p &optional string)
  "Prompts for a search string and lists all matching lines after the point or
   within the current-region, depending on whether it is active or not.
   With an argument, lists p lines before and after each matching line."
  "Prompts for a search string and lists all matching lines after the point or
   within the current-region, depending on whether it is active or not.
   With an argument, lists p lines before and after each matching line."
  (unless string
    (setf string (prompt-for-string :prompt "List Matching: "
				    :default *last-search-string*
				    :help "String to search for")))
  (let ((pattern (get-search-pattern string :forward))
	(matching-lines nil)
	(region (get-count-region)))
    (with-mark ((mark (region-start region))
		(end-mark (region-end region)))
      (loop
	(when (or (null (find-pattern mark pattern)) (mark> mark end-mark))
	  (return))
	(setf matching-lines
	      (nconc matching-lines (list-lines mark (or p 0))))
	(unless (line-offset mark 1 0)
	  (return))))
    (with-pop-up-display (s :height (length matching-lines) :title (format nil "Lines matching ~s" string))
      (dolist (line matching-lines)
	(write-line line s)))))

;;; LIST-LINES creates a lists of strings containing (num) lines before the
;;; line that the point is on, the line that the point is on, and (num)
;;; lines after the line that the point is on. If (num) > 0, a string of
;;; dashes will be added to make life easier for List Matching Lines.
;;; 
(defun list-lines (mark num)
  (if (<= num 0)
      (list (line-string (mark-line mark)))
      (with-mark ((mark mark)
		  (beg-mark mark))
	(unless (line-offset beg-mark (- num))
	  (buffer-start beg-mark))
	(unless (line-offset mark num)
	  (buffer-end mark))
	(let ((lines (list "--------")))
	  (loop
	    (push (line-string (mark-line mark)) lines)
	    (when (same-line-p mark beg-mark)
	      (return lines))
	    (line-offset mark -1))))))

(defcommand "Delete Matching Lines" (p &optional string)
  "Deletes all lines that match the search pattern using delete-region. If
   the current region is active, limit the search to it. The argument is
   ignored."
  "Deletes all lines that match the search pattern using delete-region. If
   the current region is active, limit the search to it. The argument is
   ignored."
  (declare (ignore p))
  (unless string
    (setf string (prompt-for-string :prompt "Delete Matching: "
				    :default *last-search-string*
				    :help "String to search for")))
  (let* ((region (get-count-region))
	 (pattern (get-search-pattern string :forward))
	 (start-mark (region-start region))
	 (end-mark (region-end region)))
    (with-mark ((bol-mark start-mark :left-inserting)
		(eol-mark start-mark :right-inserting))
      (loop
	(unless (and (find-pattern bol-mark pattern) (mark< bol-mark end-mark))
	  (return))
	(move-mark eol-mark bol-mark)
	(line-start bol-mark)
	(unless (line-offset eol-mark 1 0)
	  (buffer-end eol-mark))
	(delete-region (region bol-mark eol-mark))))))

(defcommand "Delete Non-Matching Lines" (p &optional string)
  "Deletes all lines that do not match the search pattern using delete-region.
   If the current-region is active, limit the search to it. The argument is
   ignored."
  "Deletes all lines that do not match the search pattern using delete-region.
   If the current-region is active, limit the search to it. The argument is
   ignored."
  (declare (ignore p))
  (unless string
    (setf string (prompt-for-string :prompt "Delete Non-Matching:"
				    :default *last-search-string*
				    :help "String to search for")))
  (let* ((region (get-count-region))
	 (start-mark (region-start region))
	 (stop-mark (region-end region))
	 (pattern (get-search-pattern string :forward)))
    (with-mark ((beg-mark start-mark :left-inserting)
		(end-mark start-mark :right-inserting))
      (loop
	(move-mark end-mark beg-mark)
	(cond ((and (find-pattern end-mark pattern) (mark< end-mark stop-mark))
	       (line-start end-mark)
	       (delete-region (region beg-mark end-mark))
	       (unless (line-offset beg-mark 1 0)
		 (return)))
	      (t
	       (delete-region (region beg-mark stop-mark))
	       (return)))))))

(defcommand "Count Occurrences" (p &optional string)
  "Prompts for a search string and counts occurrences of it after the point or
   within the current-region, depending on whether it is active or not. The
   argument is ignored."
  "Prompts for a search string and counts occurrences of it after the point or
   within the current-region, depending on whether it is active or not. The
   argument is ignored."
  (declare (ignore p))
  (unless string
    (setf string (prompt-for-string
		  :prompt "Count Occurrences: "
		  :default *last-search-string*
		  :help "String to search for")))
  (message "~D occurrence~:P"
	   (count-occurrences-region (get-count-region) string)))

(defun count-occurrences-region (region string)
  (let ((pattern (get-search-pattern string :forward))
	(end-mark (region-end region)))
    (let ((occurrences 0))
      (with-mark ((mark (region-start region)))
	(loop
	  (let ((won (find-pattern mark pattern)))
	    (when (or (null won) (mark> mark end-mark))
	      (return))
	    (incf occurrences)
	    (character-offset mark won))))
      occurrences)))
