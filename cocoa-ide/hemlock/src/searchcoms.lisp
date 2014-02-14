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

(defparameter *isearch-is-global* nil "True if incremental search string is not per-buffer but global.")

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


(defun note-current-selection-set-by-search ()
  (hemlock-ext:note-selection-set-by-search (current-buffer)))

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
	       (note-current-selection-set-by-search))
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
	       (note-current-selection-set-by-search))
	  (t (delete-mark mark)
	     (editor-error)))
    (clear-echo-area)))



;;;; Replacement commands:

(defmode "Query/Replace" :precedence :highest
  :documentation "Type one of the following single-character commands:"
  ;; Make anything that's not otherwise overridden exit query/replace
  :default-command "Query/Replace Exit and Redo")

(add-hook abort-hook 'abort-query/replace-mode)

(defhvar "Case Replace"
  "If this is true then \"Query Replace\" will try to preserve case when
  doing replacements."
  :value t)

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
  (let ((qrs (query/replace-init :count p :target target :replacement replacement
                                 :undo-name "Replace String")))
    (query/replace-all qrs)
    (query/replace-finish qrs)))

(defun current-query-replace-state ()
  (or (value query/replace-state)
      (error "Query/Replace command invoked outside Query Replace")))

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
  (let* ((buffer (current-buffer))
         (qrs (query/replace-init :count p :target target :replacement replacement
                                  :undo-name "Query Replace")))
    (setf (buffer-minor-mode (current-buffer) "Query/Replace") t)
    (unless (hemlock-bound-p 'query/replace-state :buffer buffer)
      (defhvar "Query/Replace State"
        "Internal variable containing current state of Query/Replace"
        :buffer buffer))
    (setf (value query/replace-state) qrs)
    (query/replace-find-next qrs)))

(defstruct (replace-undo (:constructor make-replace-undo (mark region)))
  mark
  region)

(setf (documentation 'replace-undo-mark 'function)
      "Return the mark where a replacement was made.")
(setf (documentation 'replace-undo-region 'function)
      "Return region deleted due to replacement.")


(defstruct (query-replace-state (:conc-name "QRS-"))
  count
  target
  replacement
  undo-name
  dumb-p
  upper
  cap
  start-mark
  last-found
  stop-mark
  undo-data)

(defun query/replace-init (&key count target replacement undo-name)
  (when (and count (minusp count))
    (editor-error "Replacement count is negative."))
  (let* ((point (current-point))
         (region (get-count-region))
         (start-mark (copy-mark (region-start region) :temporary))
         (end-mark (copy-mark (region-end region) :left-inserting)))
    (move-mark point start-mark)
    (get-search-pattern target :forward)
    (make-query-replace-state
     :count (or count -1)
     :target target
     :replacement replacement
     :undo-name undo-name
     :dumb-p (not (and (every #'(lambda (ch) (or (not (both-case-p ch))
                                                 (lower-case-p ch)))
                              (the string replacement))
                       (value case-replace)))
     :upper (string-upcase replacement)
     :cap (string-capitalize replacement)
     :start-mark start-mark
     :last-found (copy-mark start-mark :temporary)
     :stop-mark end-mark
     :undo-data nil)))


(defun query/replace-find-next (qrs &key (interactive t))
  (let* ((point (current-point))
         (won (and (not (zerop (qrs-count qrs)))
		   (find-pattern point *last-search-pattern* (qrs-stop-mark qrs)))))
    (if won
      (progn
	(decf (qrs-count qrs))
	(move-mark (qrs-last-found qrs) (current-point))
	(character-offset point (length (qrs-target qrs)))
	(when interactive
	  (message "Query Replace (type ? for help): "))
	T)
      (progn
	(when interactive
	  (end-query/replace-mode))
	nil))))

(defun query/replace-replace (qrs)
  (let* ((replacement (qrs-replacement qrs))
         (point (current-point))
         (length (length (qrs-target qrs))))
    (with-mark ((undo-mark1 point :left-inserting)
		(undo-mark2 point :left-inserting))
      (character-offset undo-mark1 (- length))
      (let ((string (cond ((qrs-dumb-p qrs) replacement)
			  ((upper-case-p (next-character undo-mark1))
			   (prog2
			    (mark-after undo-mark1)
			    (if (upper-case-p (next-character undo-mark1))
			      (qrs-upper qrs)
			      (qrs-cap qrs))
			    (mark-before undo-mark1)))
			  (t replacement))))
	(push (make-replace-undo
               ;; Save :right-inserting, so the INSERT-STRING at mark below
               ;; doesn't move the copied mark the past replacement.
               (copy-mark undo-mark1 :right-inserting)
               (delete-and-save-region (region undo-mark1 undo-mark2)))
              (qrs-undo-data qrs))
	(insert-string point string)))))

(defun query/replace-all (qrs)
  (loop
    while (query/replace-find-next qrs :interactive nil)
    do (query/replace-replace qrs)))

(defun query/replace-finish (qrs &key (report t))
  (let* ((undo-data (nreverse (qrs-undo-data qrs)))
	 (count (length undo-data))
	 (replacement-len (length (qrs-replacement qrs))))
    (save-for-undo (qrs-undo-name qrs)
      #'(lambda ()
          (dolist (ele undo-data)
            (setf (mark-kind (replace-undo-mark ele)) :left-inserting))
          (dolist (ele undo-data)
            (let ((mark (replace-undo-mark ele)))
              (delete-characters mark replacement-len)
              (ninsert-region mark (replace-undo-region ele)))))
      #'(lambda ()
          (dolist (ele undo-data)
            (delete-mark (replace-undo-mark ele)))))
    (unless (mark= (current-point) (qrs-start-mark qrs))
      (push-buffer-mark (qrs-start-mark qrs)))
    (delete-mark (qrs-stop-mark qrs))
    (when report
      (message "~D occurrence~:P replaced." count))))


(defun abort-query/replace-mode ()
  (when (buffer-minor-mode (current-buffer) "Query/Replace")
    (end-query/replace-mode :report nil)))

(defun end-query/replace-mode (&key (report t))
  (let* ((qrs (current-query-replace-state)))
    (query/replace-finish qrs :report report)
    (setf (buffer-minor-mode (current-buffer) "Query/Replace") nil)))

(defcommand "Query/Replace This" (p)
  "Replace this occurence"
  (declare (ignore p))
  (let ((qrs (current-query-replace-state)))
    (query/replace-replace qrs)
    (query/replace-find-next qrs)))

(defcommand "Query/Replace Skip" (p)
  "Don't replace this occurence, but continue"
  (declare (ignore p))
  (let ((qrs (current-query-replace-state)))
    (query/replace-find-next qrs)))

(defcommand "Query/Replace All" (p)
  "Replace this and all remaining occurences"
  (declare (ignore p))
  (let ((qrs (current-query-replace-state)))
    (query/replace-replace qrs)
    (query/replace-all qrs))
  (end-query/replace-mode))

(defcommand "Query/Replace Last" (p)
  "Replace this occurrence, then exit"
  (declare (ignore p))
  (let ((qrs (current-query-replace-state)))
    (query/replace-replace qrs))
  (end-query/replace-mode))

(defcommand "Query/Replace Exit" (p)
  "Exit Query Replace mode"
  (declare (ignore p))
  (end-query/replace-mode))

(defcommand "Query/Replace Abort" (p)
  "Abort Query/Replace mode"
  (declare (ignore p))
  (abort-current-command "Query/Replace aborted"))

(defcommand "Query/Replace Help" (p)
  "Describe Query/Replace commands"
  (describe-mode-command p "Query/Replace"))

;; The transparent-p flag takes care of executing the key normally when we're done,
;; as long as we don't take a non-local exit.
(defcommand ("Query/Replace Exit and Redo" :transparent-p t) (p)
  "Exit Query Replace and then execute the key normally"
  (declare (ignore p))
  (end-query/replace-mode))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *def-search-string* (coerce '(#\newline #\() 'string))

(defcommand "List Definitions" (p)
  "List definitions in the buffer, or in the current region if there is one"
  (declare (ignore p))
  (show-list-definitions-window (current-view)))

;;; TODO: It could be nice to track and raise an existing window. That
;;; could mean keeping track of each window per buffer and region, or just
;;; keeping one per buffer and updating it if the region changes. As it
;;; works now, a new window will be opened each time.
(defun show-list-definitions-window (view)
  (let* ((region (if (region-active-p)
                   (current-region)
                   (buffer-region (current-buffer))))
         (definitions (collect-definition-lines region)))
    (flet ((defn-action (defn)
             (gui::execute-in-gui (lambda ()
                                    (hemlock-ext:select-view view)
                                    (hi::handle-hemlock-event view 
                                      (lambda ()
                                        ;; TODO: only leave mark if we're far away, or maybe if last command
                                        ;; was not list-definitions...
                                        (destructuring-bind (line-text posn type) defn
					  (declare (ignore type))
                                          (or (move-to-definition posn line-text t)
                                              (loud-message "Could find definition"))))))))
           (defn-printer (defn stream)
             (write-string (car defn) stream)))
      (hemlock-ext:open-sequence-dialog
       :title (format nil "Definitions in ~s" (buffer-name (current-buffer)))
       :sequence definitions
       :action #'defn-action
       :printer #'defn-printer))))

(defun collect-definition-lines (&optional (region (buffer-region (current-buffer))))
  (let* ((pattern (new-search-pattern :string-sensitive :forward *def-search-string*))
         (end (region-end region)))
    (with-mark ((mark (region-start region)))
      ;; TODO: doesn't find the definition on very first line.  LTRAB.
      (loop
        until (or (null (find-pattern mark pattern)) (mark> mark end))
        as line = (mark-line (mark-after mark))
        collect (list (line-string line) (hi::get-line-origin line) (get-definition-type mark))
        while (let ((next (line-next line)))
                (when next
                  (setf (mark-line mark) next)
                  (setf (mark-charpos mark) 0)))))))

(defun get-definition-type (mark)
  (let ((buffer (mark-buffer mark)))
    (mark-after mark)
    (let ((str (symbol-at-mark mark)))
      (when str
        (multiple-value-bind (sym error)
                             (let* ((*package* (ccl:require-type (or (buffer-package buffer) *package*) 'package)))
                               (ignore-errors (values (read-from-string str))))
          (if error
            (intern (string-upcase str) *package*)
            sym))))))

(defun move-to-definition (posn line-text &optional (leave-mark t))
  (flet ((ssearch (mark string direction)
           (find-pattern mark (new-search-pattern :string-insensitive
                                                  direction
                                                  string))))
    (declare (inline ssearch))
    (with-mark ((mark (current-point)))
      (or (move-to-absolute-position mark posn) (buffer-end mark))
      (when (or (ssearch mark line-text :forward)
                (ssearch mark line-text :backward))
        (if leave-mark
          (move-point-leaving-mark mark)
          (move-mark (current-point-collapsing-selection) mark))))))


;; Interface for getting this functionality outside of the editor.
;; Returns a list of (string number symbol) where string is the first line of the definition,
;; number is the absolute position in the buffer of the start of the line, and symbol is the
;; definition type (eg. DEFUN, DEFVAR, HI:DEFCOMMAND, etc).
(defun definitions-in-document (ns-doc)
  (gui::execute-in-buffer (gui::hemlock-buffer ns-doc) #'collect-definition-lines))
