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
;;;		     Hemlock Word Abbreviation Mode
;;;		          by Jamie W. Zawinski
;;;		           24 September 1985
;;;
(in-package :hemlock)

;;;; These Things are Here:

;;; C-X C-A    Add Mode Word Abbrev 
;;;               Define a mode abbrev for the word before point.
;;; C-X +      Add Global Word Abbrev 
;;;               Define a global abbrev for the word before point.
;;; C-X C-H    Inverse Add Mode Word Abbrev
;;;               Define expansion for mode abbrev before point.
;;; C-X -      Inverse Add Global Word Abbrev
;;;               Define expansion for global abbrev before point.
;;; Alt Space  Abbrev Expand Only
;;;               Expand abbrev without inserting anything.
;;; M-'        Word Abbrev Prefix Mark
;;;               Mark a prefix to be glued to an abbrev following.
;;; C-X U      Unexpand Last Word
;;;               Unexpands last abbrev or undoes C-X U.

;;; List Word Abbrevs                 Shows definitions of all word abbrevs.
;;; Edit Word Abbrevs                 Lets you edit the definition list directly.
;;; Read Word Abbrev File <filename>  Define word abbrevs from a definition file.
;;; Write Word Abbrev File            Make a definition file from current abbrevs.

;;; Make Word Abbrev <abbrev><expansion><mode> More General form of C-X C-A, etc.
;;; Delete All Word Abbrevs                      Wipes them all.
;;; Delete Mode Word Abbrev                      Kills all Mode abbrev.
;;; Delete Global Word Abbrev                    Kills all Global abbrev.

;;; Insert Word Abbrevs          Inserts a list of current definitions in the
;;;                                format that Define Word Abbrevs uses.
;;; Define Word Abbrevs          Defines set of abbrevs from a definition list in 
;;;                                the buffer.
;;; Word Abbrev Apropos <string> Shows definitions containing <string> in abbrev,
;;;                                definition, or mode.

;;; Append Incremental Word Abbrev File           Appends to a file changed abbrev
;;;                                                 definitions since last dumping.

(defmode "Abbrev" :major-p nil :transparent-p t :precedence 2.0)


(defvar *global-abbrev-table* (make-hash-table :test #'equal)
  "Hash table holding global abbrev definitions.")

(defhvar "Abbrev Pathname Defaults"
  "Holds the name of the last Abbrev-file written."
  :value (pathname "abbrev.defns"))

(defvar *new-abbrevs* ()
 "holds a list of abbrevs (and their definitions and modes) changed since saving.")


;;; C-X C-H    Inverse Add Mode Word Abbrev 
;;;               Define a mode expansion for the word before point.

(defcommand "Inverse Add Mode Word Abbrev" (p)
  "Defines a mode word abbrev expansion for the word before the point."
  "Defines a mode word abbrev expansion for the word before the point."
  (declare (ignore p))
  (let ((word (prev-word 1 (current-point)))
	(mode (buffer-major-mode (current-buffer))))
    (make-word-abbrev-command nil word nil mode)))


;;; C-X C-A    Add Mode Word Abbrev
;;;               Define mode abbrev for word before point.

(defcommand "Add Mode Word Abbrev" (p)
  "Defines a mode word abbrev for the word before the point.
  With a positive argument, uses that many preceding words as the expansion.
  With a zero argument, uses the region as the expansion.  With a negative
  argument, prompts for a word abbrev to delete in the current mode."
  "Defines or deletes a mode word abbrev."
  (if (and p (minusp p))
      (delete-mode-word-abbrev-command nil)
      (let* ((val (if (eql p 0)
		      (region-to-string (current-region nil))
		      (prev-word (or p 1) (current-point))))
	     (mode (buffer-major-mode (current-buffer))))
	(make-word-abbrev-command nil nil val mode))))



;;; C-X -    Inverse Add Global Word Abbrev
;;;               Define global expansion for word before point.

(defcommand "Inverse Add Global Word Abbrev" (p)
  "Defines a Global expansion for the word before point."
  "Defines a Global expansion for the word before point."
  (declare (ignore p))
  (let ((word (prev-word 1 (current-point))))
    (make-word-abbrev-command nil word nil "Global")))



;;; C-X +      Add Global Word Abbrev
;;;               Define global Abbrev for word before point.

(defcommand "Add Global Word Abbrev" (p)
  "Defines a global word abbrev for the word before the point.
  With a positive argument, uses that many preceding words as the expansion.
  With a zero argument, uses the region as the expansion.  With a negative
  argument, prompts for a global word abbrev to delete."
  "Defines or deletes a global word abbrev."
  (if (and p (minusp p))
      (delete-global-word-abbrev-command nil)
      (let ((val (if (eql p 0)
		     (region-to-string (current-region nil))
		     (prev-word (or p 1) (current-point)))))
	(make-word-abbrev-command nil nil val "Global"))))


;;;; Defining Abbrevs

;;; Make Word Abbrev <abbrev><expansion><mode>  More General form of C-X C-A, etc.

(defvar *global-abbrev-string-table*
  (make-string-table :initial-contents '(("Global" . nil))))

(defcommand "Make Word Abbrev" (p &optional abbrev expansion mode)
  "Defines an arbitrary word abbreviation.
  Prompts for abbrev, expansion, and mode."
  "Makes Abbrev be a word abbreviation for Expansion when in Mode.  If
  mode is \"Global\" then make a global abbrev."
  (declare (ignore p))
  (unless mode
    (setq mode
	  (prompt-for-keyword
	   (list *mode-names* *global-abbrev-string-table*)
	   :prompt "Mode of abbrev to add: "
	   :default "Global"
	   :help 
	   "Type the mode of the Abbrev you want to add, or confirm for Global.")))
  (let ((globalp (string-equal mode "Global")))
    (unless (or globalp (mode-major-p mode))
      (editor-error "~A is not a major mode." mode))
    (unless abbrev
      (setq abbrev
	    (prompt-for-string
	     :trim t
	     :prompt
	     (list "~A abbreviation~@[ of ~S~]: " mode expansion)
	     :help
	     (list "Define a ~A word abbrev." mode))))
    (when (zerop (length abbrev))
      (editor-error "Abbreviation must be at least one character long."))
    (unless (every #'(lambda (ch)
		       (zerop (character-attribute :word-delimiter ch)))
		   (the simple-string abbrev))
      (editor-error "Word Abbrevs must be a single word."))
    (unless expansion
      (setq expansion
	    (prompt-for-string
	     :prompt (list "~A expansion for ~S: " mode abbrev)
	     :help (list "Define the ~A expansion of ~S." mode abbrev))))
    (setq abbrev (string-downcase abbrev))
    (let* ((table (cond (globalp *global-abbrev-table*)
			((hemlock-bound-p 'Mode-Abbrev-Table :mode mode)
			 (variable-value 'Mode-Abbrev-Table :mode mode))
			(t
			 (let ((new (make-hash-table :test #'equal)))
			   (defhvar "Mode Abbrev Table"
			     "Hash Table of Mode Abbrevs"
			     :value new :mode mode)
			   new))))
	   (old (gethash abbrev table)))
      (when (or (not old)
		(prompt-for-y-or-n
		 :prompt
		 (list "Current ~A definition of ~S is ~S.~%Redefine?"
		       mode abbrev old)
		 :default t
		 :help (list "Redefine the expansion of ~S." abbrev)))
	(setf (gethash abbrev table) expansion)
	(push (list abbrev expansion (if globalp nil mode))
	      *new-abbrevs*)))))


;;; Alt Space  Abbrev Expand Only
;;;               Expand abbrev without inserting anything.

(defcommand "Abbrev Expand Only" (p)
  "This command expands the word before point into its abbrev definition 
  (if indeed it has one)."
  "This command expands the word before point into its abbrev definition 
  (if indeed it has one)."
  (declare (ignore p))
  (let* ((word (prev-word 1 (current-point)))
	 (glob (gethash (string-downcase word) *global-abbrev-table*))
	 (mode (if (hemlock-bound-p 'Mode-Abbrev-Table)
		   (gethash (string-downcase word)
			    (value Mode-Abbrev-Table))))
	 (end-word (reverse-find-attribute (copy-mark (current-point)
						      :right-inserting)
					   :word-delimiter #'zerop))
	 (result (if mode mode glob)))
    (when (or mode glob)
      (delete-characters end-word (- (length word)))
      (cond ((equal word (string-capitalize word))
	     (setq result (string-capitalize result)))
	    ((equal word (string-upcase word))
	     (setq result (string-upcase result))))
      (insert-string end-word result)
      (unless (hemlock-bound-p 'last-expanded)
	(defhvar "last expanded"
            "Holds a mark, the last expanded abbrev, and its expansion in a list."
            :buffer (current-buffer)))
      (setf (value last-expanded)
	    (list (copy-mark (current-point) :right-inserting)
		  word result)))
    (delete-mark end-word))
  (when (and (hemlock-bound-p 'prefix-mark)
	     (value prefix-mark))
    (delete-characters (value prefix-mark) 1)
    (delete-mark (value prefix-mark))
    (setf (value prefix-mark) nil)))



;;; This function returns the n words immediately before the mark supplied.

(defun prev-word (n mark)
  (let* ((mark-1 (reverse-find-attribute (copy-mark mark :temporary)
					 :word-delimiter #'zerop))
	 (mark-2 (copy-mark mark-1)))
    (dotimes (x n (region-to-string (region mark-2 mark-1)))
      (reverse-find-attribute (mark-before mark-2) :word-delimiter))))



;;; M-'        Word Abbrev Prefix Mark
;;;               Mark a prefix to be glued to an abbrev following.

;;; When "Abbrev Expand Only" expands the abbrev (because #\- is an expander)
;;; it will see that prefix-mark is non-nil, and will delete the #\- immediately
;;; after prefix-mark.

(defcommand "Word Abbrev Prefix Mark" (p)
  "Marks a prefix to be glued to an abbrev following." 
  "Marks a prefix to be glued to an abbrev following."
  (declare (ignore p))
  (unless (hemlock-bound-p 'prefix-mark)
    (defhvar "prefix mark"
             "Holds a mark (or not) pointing to the current Prefix Mark."
             :buffer (current-buffer)))
  (when (value prefix-mark)
    (delete-mark (value prefix-mark)))
  (setf (value prefix-mark) (copy-mark (current-point) :right-inserting))
  (insert-character (value prefix-mark) #\-))


;;; C-X U     Unexpand Last Word
;;;              Unexpands last abbrev or undoes last C-X U.

(defcommand "Unexpand Last Word" (p)
  "Undoes the last abbrev expansion, or undoes \"Unexpand Last Word\".
  Only one abbrev may be undone."
  "Undoes the last abbrev expansion, or undoes \"Unexpand Last Word\"."
  (declare (ignore p))
  (unless (or (not (hemlock-bound-p 'last-expanded))
	      (value last-expanded))
    (editor-error "Nothing to Undo."))
  (let ((mark (car (value last-expanded)))
	(word1 (second (value last-expanded)))
	(word2 (third (value last-expanded))))
    (unless (string= word2
		     (region-to-string
		      (region (character-offset (copy-mark mark :temporary)
						(- (length word2)))
			      mark)))
      (editor-error "The last expanded Abbrev has been altered in the text."))
    (delete-characters mark (- (length word2)))
    (insert-string mark word1)
    (character-offset mark (length word1))
    (setf (value last-expanded) (list mark word2 word1))))

 
  
;;; Delete Mode Word Abbrev                       Kills some Mode abbrevs.

(defcommand "Delete Mode Word Abbrev"
	    (p &optional abbrev
	       (mode (buffer-major-mode (current-buffer))))
  "Prompts for a word abbrev and deletes the mode expansion in the current mode.
  If called with a prefix argument, deletes all word abbrevs define in the
  current mode."
  "Deletes Abbrev in Mode, or all abbrevs in Mode if P is true."
  (let ((boundp (hemlock-bound-p 'Mode-Abbrev-Table :mode mode)))
    (if p
	(when boundp
	  (delete-variable 'Mode-Abbrev-Table :mode mode))
	(let ((down
	       (string-downcase
		(or abbrev
		    (prompt-for-string
		     :prompt (list "~A abbrev to delete: " mode)
		     :help
 (list "Give the name of a ~A mode word abbrev to delete." mode)
		     :trim t))))
	      (table (and boundp (variable-value 'mode-abbrev-table :mode mode))))
	  (unless (and table (gethash down table))
	    (editor-error "~S is not the name of an abbrev in ~A mode."
			  down mode))
	  (remhash down table)))))


;;; Delete Global Word Abbrevs                    Kills some Global abbrevs.

(defcommand "Delete Global Word Abbrev" (p &optional abbrev)
  "Prompts for a word abbrev and delete the global expansion.
  If called with a prefix argument, deletes all global abbrevs."
  "Deletes the global word abbreviation named Abbrev.  If P is true,
  deletes all global abbrevs."
  (if p
      (setq *global-abbrev-table* (make-hash-table :test #'equal))
      (let ((down 
	     (string-downcase
	      (or abbrev
		  (prompt-for-string
		   :prompt "Global abbrev to delete: "
		   :help "Give the name of a global word abbrev to delete."
		   :trim t)))))
	(unless (gethash down *global-abbrev-table*)
	  (editor-error "~S is not the name of a global word abbrev." down))
	(remhash down *global-abbrev-table*))))
	
;;; Delete All Word Abbrevs                       Wipes them all.

(defcommand "Delete All Word Abbrevs" (p)
  "Deletes all currently defined Word Abbrevs"
  "Deletes all currently defined Word Abbrevs"
  (declare (ignore p))
  (Delete-Global-Word-Abbrev-Command 1)
  (Delete-Mode-Word-Abbrev-Command 1))


;;;; Abbrev I/O

;;; List Word Abbrevs                 Shows definitions of all word abbrevs.

(defcommand "List Word Abbrevs" (p)
  "Lists all of the currently defined Word Abbrevs."
  "Lists all of the currently defined Word Abbrevs."
  (word-abbrev-apropos-command p ""))


;;; Word Abbrev Apropos <string> Shows definitions containing <string> in abbrev,
;;;                                definition, or mode.

(defcommand "Word Abbrev Apropos" (p &optional search-string)
  "Lists all of the currently defined Word Abbrevs which contain a given string
  in their abbrev. definition, or mode."
  "Lists all of the currently defined Word Abbrevs which contain a given string
  in their abbrev. definition, or mode."
  (declare (ignore p))
  (unless search-string
    (setq search-string
	  (string-downcase
	   (prompt-for-string
	    :prompt "Apropos string: "
	    :help "The string to search word abbrevs and definitions for."))))
  (multiple-value-bind (count mode-tables) (count-abbrevs)
    (with-pop-up-display (s :height (min (1+ count) 30))
      (unless (zerop (hash-table-count *global-abbrev-table*))
	(maphash #'(lambda (key val)
		     (when (or (search search-string (string-downcase key))
			       (search search-string (string-downcase val)))
		       (write-abbrev key val nil s t)))
		 *global-abbrev-table*))
      (dolist (modename mode-tables)
	(let ((table (variable-value 'Mode-Abbrev-Table :mode modename)))
	  (if (search search-string (string-downcase modename))
	      (maphash #'(lambda (key val)
			   (write-abbrev key val modename s t))
		       table)
	      (maphash #'(lambda (key val)
			   (when (or (search search-string (string-downcase key))
				     (search search-string (string-downcase val)))
			     (write-abbrev key val modename s t)))
		       table))))
      (terpri s))))



(defun count-abbrevs ()
  (let* ((count (hash-table-count *global-abbrev-table*))
	 (mode-tables nil))
    (do-strings (which x *mode-names*)
      (declare (ignore x))
      (when (hemlock-bound-p 'Mode-Abbrev-Table :mode which)
	(let ((table-count (hash-table-count (variable-value 'Mode-Abbrev-Table
							     :mode which))))
	  (unless (zerop table-count)
	    (incf count table-count)
	    (push which mode-tables)))))
    (values count mode-tables)))


;;; Edit Word Abbrevs                 Lets you edit the definition list directly.

(defcommand "Edit Word Abbrevs" (p)
  "Allows direct editing of currently defined Word Abbrevs."
  "Allows direct editing of currently defined Word Abbrevs."
  (declare (ignore p))
  (when (getstring "Edit Word Abbrevs" *buffer-names*)
    (delete-buffer (getstring "Edit Word Abbrevs" *buffer-names*)))
  (let ((old-buf (current-buffer))
	(new-buf (make-buffer "Edit Word Abbrevs")))
    (change-to-buffer new-buf)
    (unwind-protect
      (progn
       (insert-word-abbrevs-command nil)
       (do-recursive-edit)
       (unless (equal #\newline (previous-character (buffer-end (current-point))))
	 (insert-character (current-point) #\newline))
       (delete-all-word-abbrevs-command nil)
       (define-word-abbrevs-command nil))
      (progn
       (change-to-buffer old-buf)
       (delete-buffer new-buf)))))



;;; Insert Word Abbrevs          Inserts a list of current definitions in the
;;;                                format that Define Word Abbrevs uses.

(defcommand "Insert Word Abbrevs" (p)
  "Inserts into the current buffer a list of all currently defined abbrevs in the
  format used by \"Define Word Abbrevs\"."
  "Inserts into the current buffer a list of all currently defined abbrevs in the
  format used by \"Define Word Abbrevs\"."
  
  (declare (ignore p))
  (multiple-value-bind (x mode-tables)
		       (count-abbrevs)
    (declare (ignore x))
    (with-output-to-mark (stream (current-point) :full)
      (maphash #'(lambda (key val)
		   (write-abbrev key val nil stream))
	       *global-abbrev-table*)
      
      (dolist (mode mode-tables)
	(let ((modename (if (listp mode) (car mode) mode)))
	  (maphash #'(lambda (key val)
		       (write-abbrev key val modename stream))
		   (variable-value 'Mode-Abbrev-Table :mode modename)))))))



;;; Define Word Abbrevs          Defines set of abbrevs from a definition list in 
;;;                                the buffer.

(defcommand "Define Word Abbrevs" (p)
  "Defines Word Abbrevs from the definition list in the current buffer.  The 
  definition list must be in the format produced by \"Insert Word Abbrevs\"."
  "Defines Word Abbrevs from the definition list in the current buffer.  The
  definition list must be in the format produced by \"Insert Word Abbrevs\"."
  
  (declare (ignore p))
  (with-input-from-region (file (buffer-region (current-buffer)))
    (read-abbrevs file)))


;;; Read Word Abbrev file <filename>   Define word abbrevs from a definition file.

;;; Ignores all lines less than 4 characters, i.e. blankspace or errors. That is
;;; the minimum number of characters possible to define an abbrev.  It thinks the 
;;; current abbrev "wraps" if there is no #\" at the end of the line or there are
;;; two #\"s at the end of the line (unless that is the entire definition string,
;;; i.e, a null-abbrev).

;;; The format of the Abbrev files is 
;;;
;;;                   ABBREV<tab><tab>"ABBREV DEFINITION"
;;;
;;; for Global Abbrevs, and
;;;
;;;                   ABBREV<tab>(MODE)<tab>"ABBREV DEFINITION"
;;;
;;; for Modal Abbrevs.  
;;; Double-quotes contained within the abbrev definition are doubled.  If the first
;;; line of an abbrev definition is not closed by a single double-quote, then
;;; the subsequent lines are read in until a single double-quote is found.

(defcommand "Read Word Abbrev File" (p &optional filename)
  "Reads in a file of previously defined abbrev definitions."
  "Reads in a file of previously defined abbrev definitions."
  (declare (ignore p))
  (setf (value abbrev-pathname-defaults)
	(if filename
	    filename
	    (prompt-for-file
	     :prompt "Name of abbrev file: "
	     :help "The name of the abbrev file to load."
	     :default (value abbrev-pathname-defaults)
	     :must-exist nil)))
  (with-open-file (file (value abbrev-pathname-defaults) :direction :input
			:element-type 'base-char :if-does-not-exist :error)
    (read-abbrevs file)))


;;; Does the actual defining of abbrevs from a given stream, expecting tabs and
;;; doubled double-quotes.

(defun read-abbrevs (file)
  (do ((line (read-line file nil nil)
	     (read-line file nil nil)))
      ((null line))
    (unless (< (length line) 4)
      (let* ((tab (position #\tab line))
	     (tab2 (position #\tab line :start (1+ tab)))
	     (abbrev (subseq line 0 tab))
	     (modename (subseq line (1+ tab) tab2))
	     (expansion (do* ((last (1+ (position #\" line))
				    (if found (min len (1+ found)) 0))
			      (len (length line))
			      (found (if (position #\" line :start last)
					 (1+ (position #\" line :start last)))
				     (if (position #\" line :start last)
					 (1+ (position #\" line :start last))))
			      (expansion (subseq line last (if found found len))
					 (concatenate 'simple-string expansion
						      (subseq line last
							      (if found found
								  len)))))
			     ((and (or (null found) (= found len))
				   (equal #\" (char line (1- len)))
				   (or (not (equal #\" (char line (- len 2))))
				       (= (- len 3) tab2)))
			      (subseq expansion 0 (1- (length expansion))))
			  
			  (when (null found)
			    (setq line (read-line file nil nil)
				  last 0
				  len (length line)
				  found (if (position #\" line)
					    (1+ (position #\" line)))
				  expansion (format nil "~A~%~A" expansion
						    (subseq line 0 (if found
								       found
								       0))))))))
	
	(cond ((equal modename "")
	       (setf (gethash abbrev *global-abbrev-table*)
		     expansion))
	      (t (setq modename (subseq modename 1 (1- (length modename))))
		 (unless (hemlock-bound-p 'Mode-Abbrev-Table
					  :mode modename)
		   (defhvar "Mode Abbrev Table"
    			    "Hash Table of Mode Abbrevs"
    			    :value (make-hash-table :test #'equal)
  			    :mode modename))
		 (setf (gethash abbrev (variable-value
					'Mode-Abbrev-Table :mode modename))
		       expansion)))))))


;;; Write Word Abbrev File            Make a definition file from current abbrevs.

(defcommand "Write Word Abbrev File" (p &optional filename)
  "Saves the currently defined Abbrevs to a file."
  "Saves the currently defined Abbrevs to a file."
  (declare (ignore p))
  (unless filename
    (setq filename
	  (prompt-for-file
	   :prompt "Write abbrevs to file: "
	   :default (value abbrev-pathname-defaults)
	   :help "Name of the file to write current abbrevs to."
	   :must-exist nil)))
  (with-open-file (file filename :direction :output
			:element-type 'base-char :if-exists :supersede
			:if-does-not-exist :create)
    (multiple-value-bind (x mode-tables) (count-abbrevs)
      (declare (ignore x))
      (maphash #'(lambda (key val)
		   (write-abbrev key val nil file))
	       *global-abbrev-table*)
      
      (dolist (modename mode-tables)
	(let ((mode (if (listp modename) (car modename) modename)))
	  (maphash #'(lambda (key val)
		       (write-abbrev key val mode file))
		   (variable-value 'Mode-Abbrev-Table :mode mode))))))
  (let ((tn (truename filename)))
    (setf (value abbrev-pathname-defaults) tn)
    (message "~A written." (namestring tn))))



;;; Append to Word Abbrev File          Appends to a file changed abbrev 
;;;                                     definitions since last dumping.

(defcommand "Append to Word Abbrev File" (p &optional filename)
  "Appends Abbrevs defined or redefined since the last save to a file."
  "Appends Abbrevs defined or redefined since the last save to a file."
  (declare (ignore p))
  (cond
   (*new-abbrevs*
    (unless filename
      (setq filename
	    (prompt-for-file
	     :prompt
	     "Append incremental abbrevs to file: "
	     :default (value abbrev-pathname-defaults)
	     :must-exist nil
	     :help "Filename to append recently defined Abbrevs to.")))
    (write-incremental :append filename))
   (t
    (message "No Abbrev definitions have been changed since the last write."))))


(defun write-incremental (mode filename)
  (with-open-file (file filename :direction :output
			:element-type 'base-char
			:if-exists mode :if-does-not-exist :create)
    (dolist (def *new-abbrevs*)
      (let ((abb (car def))
	    (val (second def))
	    (mode (third def)))
	(write-abbrev abb val mode file))))
  (let ((tn (truename filename)))
    (setq *new-abbrevs* nil)
    (setf (value abbrev-pathname-defaults) tn)
    (message "~A written." (namestring tn))))


;;; Given an Abbrev, expansion, mode (nil for Global), and stream, this function
;;; writes to the stream with doubled double-quotes and stuff.
;;; If the flag is true, then the output is in a pretty format (like "List Word
;;; Abbrevs" uses), otherwise output is in tabbed format (like "Write Word 
;;; Abbrev File" uses).

(defun write-abbrev (abbrev expansion modename file &optional flag)
  (if flag
      (if modename
	  (format file "~5t~A~20t(~A)~35t\"" abbrev modename); pretty format
	  (format file "~5t~A~35t\"" abbrev))                ; pretty format
      (cond (modename
	     (write-string abbrev file)
	     (write-char #\tab file)
	     (format file "(~A)" modename)                   ; "~A<tab>(~A)<tab>\""
	     (write-char #\tab file)
	     (write-char #\" file))
	    (t
	     (write-string abbrev file)
	     (write-char #\tab file)                         ; "~A<tab><tab>\""
	     (write-char #\tab file)
	     (write-char #\" file))))
  (do* ((prev 0 found)
	(found (position #\" expansion)
	       (position #\" expansion :start found)))
       ((not found)
	(write-string expansion file :start prev)
	(write-char #\" file)
	(terpri file))
    (incf found)
    (write-string expansion file :start prev :end found)
    (write-char #\" file)))


(defcommand "Abbrev Mode" (p)
  "Put current buffer in Abbrev mode." 
  "Put current buffer in Abbrev mode."  
  (declare (ignore p))
  (setf (buffer-minor-mode (current-buffer) "Abbrev")
	(not (buffer-minor-mode (current-buffer) "Abbrev"))))
