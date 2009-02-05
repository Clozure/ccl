;;; -*- Log: hemlock.log; Package: Hemlock -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;;    Written by Bill Chiles and Rob Maclachlan.
;;;
;;; This file contains the code to implement commands using the spelling
;;; checking/correcting stuff in Spell-Corr.Lisp and the dictionary
;;; augmenting stuff in Spell-Augment.Lisp.

(in-package "HEMLOCK")



(defstruct (spell-info (:print-function print-spell-info)
		       (:constructor make-spell-info (pathname)))
  pathname	;Dictionary file.
  insertions)	;Incremental insertions for this dictionary.

(defun print-spell-info (obj str n)
  (declare (ignore n))
  (let ((pn (spell-info-pathname obj)))
    (format str "#<Spell Info~@[ ~S~]>"
	    (and pn (namestring pn)))))


(defattribute "Spell Word Character"
  "One if the character is one that is present in the spell dictionary,
  zero otherwise.")

(do-alpha-chars (c :both)
  (setf (character-attribute :spell-word-character c) 1))
(setf (character-attribute :spell-word-character #\') 1)


(defvar *spelling-corrections* (make-hash-table :test #'equal)
  "Mapping from incorrect words to their corrections.")

(defvar *ignored-misspellings* (make-hash-table :test #'equal)
  "A hashtable with true values for words that will be quietly ignored when
  they appear.")

(defhvar "Spell Ignore Uppercase"
  "If true, then \"Check Word Spelling\" and \"Correct Buffer Spelling\" will
  ignore unknown words that are all uppercase.  This is useful for
  abbreviations and cryptic formatter directives."
  :value nil)



;;;; Basic Spelling Correction Command (Esc-$ in EMACS)

(defcommand "Check Word Spelling" (p)
  "Check the spelling of the previous word and offer possible corrections
   if the word in unknown. To add words to the dictionary from a text file see
   the command \"Augment Spelling Dictionary\"."
  "Check the spelling of the previous word and offer possible correct
   spellings if the word is known to be misspelled."
  (declare (ignore p))
  (spell:maybe-read-spell-dictionary)  
  (let* ((region (spell-previous-word (current-point) nil))
	 (word (if region
		   (region-to-string region)
		   (editor-error "No previous word.")))
	 (folded (string-upcase word)))
    (message "Checking spelling of ~A." word)
    (unless (check-out-word-spelling word folded)
      (get-word-correction (region-start region) word folded))))


;;;; Auto-Spell mode:

(defhvar "Check Word Spelling Beep"
  "If true, \"Auto Check Word Spelling\" will beep when an unknown word is
   found."
  :value t)

(defhvar "Correct Unique Spelling Immediately"
  "If true, \"Auto Check Word Spelling\" will immediately attempt to correct any
   unknown word, automatically making the correction if there is only one
   possible."
  :value t)


(defhvar "Default User Spelling Dictionary"
  "This is the pathname of a dictionary to read the first time \"Spell\" mode
   is entered in a given editing session.  When \"Set Buffer Spelling
   Dictionary\" or the \"dictionary\" file option is used to specify a
   dictionary, this default one is read also.  It defaults to nil."
  :value nil)

(defvar *default-user-dictionary-read-p* nil)

(defun maybe-read-default-user-spelling-dictionary ()
  (let ((default-dict (value default-user-spelling-dictionary)))
    (when (and default-dict (not *default-user-dictionary-read-p*))
      (spell:maybe-read-spell-dictionary)
      (spell:spell-read-dictionary (truename default-dict))
      (setf *default-user-dictionary-read-p* t))))


(defmode "Spell"
  :transparent-p t :precedence 1.0 :setup-function 'spell-mode-setup)

(defun spell-mode-setup (buffer)
  (defhvar "Buffer Misspelled Words"
    "This variable holds a ring of marks pointing to misspelled words."
    :buffer buffer  :value (make-ring 10 #'delete-mark))
  (maybe-read-default-user-spelling-dictionary))

(defcommand "Auto Spell Mode" (p)
  "Toggle \"Spell\" mode in the current buffer.  When in \"Spell\" mode,
  the spelling of each word is checked after it is typed."
  "Toggle \"Spell\" mode in the current buffer."
  (declare (ignore p))
  (setf (buffer-minor-mode (current-buffer) "Spell")
	(not (buffer-minor-mode (current-buffer) "Spell"))))


(defcommand "Auto Check Word Spelling" (p)
  "Check the spelling of the previous word and display a message in the echo
   area if the word is not in the dictionary.  To add words to the dictionary
   from a text file see the command \"Augment Spelling Dictionary\".  If a
   replacement for an unknown word has previously been specified, then the
   replacement will be made immediately.  If \"Correct Unique Spelling
   Immediately\" is true, then this command will immediately correct words
   which have a unique correction.  If there is no obvious correction, then we
   place the word in a ring buffer for access by the \"Correct Last Misspelled
   Word\" command.  If \"Check Word Spelling Beep\" is true, then this command
   beeps when an unknown word is found, in addition to displaying the message."
  "Check the spelling of the previous word, making obvious corrections, or
  queuing the word in buffer-misspelled-words if we are at a loss."
  (declare (ignore p))
  (unless (eq (last-command-type) :spell-check)
    (spell:maybe-read-spell-dictionary)
    (let ((region (spell-previous-word (current-point) t)))
      (when region
	(let* ((word (nstring-upcase (region-to-string region)))
	       (len (length word)))
	  (declare (simple-string word))
	  (when (and (<= 2 len spell:max-entry-length)
		     (not (spell:spell-try-word word len)))
	    (let ((found (gethash word *spelling-corrections*))
		  (save (region-to-string region)))
	      (cond (found
		     (undoable-replace-word (region-start region) save found)
		     (message "Corrected ~S to ~S." save found)
		     (when (value check-word-spelling-beep) (beep)))
		    ((and (value spell-ignore-uppercase)
			  (every #'upper-case-p save))
		     (unless (gethash word *ignored-misspellings*)
		       (setf (gethash word *ignored-misspellings*) t)
		       (message "Ignoring ~S." save)))
		    (t
		     (let ((close (spell:spell-collect-close-words word)))
		       (cond ((and close
				   (null (rest close))
				   (value correct-unique-spelling-immediately))
			      (let ((fix (first close)))
				(undoable-replace-word (region-start region)
						       save fix)
				(message "Corrected ~S to ~S." save fix)))
			     (t
			      (ring-push (copy-mark (region-end region)
						    :right-inserting)
					 (value buffer-misspelled-words))
			      (let ((nclose
				     (do ((i 0 (1+ i))
					  (words close (cdr words))
					  (nwords () (cons (list i (car words))
							   nwords)))
					 ((null words) (nreverse nwords)))))
				(message
				 "Word ~S not found.~
				  ~@[  Corrections:~:{ ~D=~A~}~]"
				 save nclose)))))
		     (when (value check-word-spelling-beep) (beep))))))))))
  (setf (last-command-type) :spell-check))

(defcommand "Correct Last Misspelled Word" (p)
  "Fix a misspelling found by \"Auto Check Word Spelling\".  This prompts for
   a single character command to determine which action to take to correct the
   problem."
  "Prompt for a single character command to determine how to fix up a
   misspelling detected by Check-Word-Spelling-Command."
  (declare (ignore p))
  (spell:maybe-read-spell-dictionary)
  (do ((info (value spell-information)))
      ((sub-correct-last-misspelled-word info))))

(defun sub-correct-last-misspelled-word (info)
  (let* ((missed (value buffer-misspelled-words))
	 (region (cond ((zerop (ring-length missed))
			(editor-error "No recently misspelled word."))
		       ((spell-previous-word (ring-ref missed 0) t))
		       (t (editor-error "No recently misspelled word."))))
	 (word (region-to-string region))
	 (folded (string-upcase word))
	 (point (current-point))
	 (save (copy-mark point))
	 (res t))
    (declare (simple-string word))
    (unwind-protect
      (progn
       (when (check-out-word-spelling word folded)
	 (delete-mark (ring-pop missed))
	 (return-from sub-correct-last-misspelled-word t))
       (move-mark point (region-end region))
       (command-case (:prompt "Action: "
		      :change-window nil
 :help "Type a single character command to do something to the misspelled word.")
	 (#\c "Try to find a correction for this word."
	  (unless (get-word-correction (region-start region) word folded)
	    (reprompt)))
	 (#\i "Insert this word in the dictionary."
	  (spell:spell-add-entry folded)
	  (push folded (spell-info-insertions info))
	  (message "~A inserted in the dictionary." word))
	 (#\r "Prompt for a word to replace this word with."
	  (let ((s (prompt-for-string :prompt "Replace with: "
				      :default word
 :help "Type a string to replace occurrences of this word with.")))
	    (delete-region region)
	    (insert-string point s)
	    (setf (gethash folded *spelling-corrections*) s)))
	 (:cancel "Ignore this word and go to the previous misspelled word."
	  (setq res nil))
	 (:recursive-edit
	  "Go into a recursive edit and leave when it exits."
	  (do-recursive-edit))
	 ((:exit #\q) "Exit and forget about this word.")
	 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	  "Choose this numbered word as the correct spelling."
	  (let ((num (digit-char-p (ext:key-event-char *last-key-event-typed*)))
		(close-words (spell:spell-collect-close-words folded)))
	    (cond ((> num (length close-words))
		   (editor-error "Choice out of range."))
		  (t (let ((s (nth num close-words)))
		       (setf (gethash folded *spelling-corrections*) s)
		       (undoable-replace-word (region-start region)
					      word s)))))))
       (delete-mark (ring-pop missed))
       res)
      (move-mark point save)
      (delete-mark save))))

(defhvar "Spelling Un-Correct Prompt for Insert"
  "When this is set, \"Undo Last Spelling Correction\" will prompt before
   inserting the old word into the dictionary."
  :value nil)

(defcommand "Undo Last Spelling Correction" (p)
  "Undo the last incremental spelling correction.
   The \"correction\" is replaced with the old word, and the old word is
   inserted in the dictionary.  When \"Spelling Un-Correct Prompt for Insert\"
   is set, the user is asked about inserting the old word.  Any automatic
   replacement for the old word is eliminated."
  "Undo the last incremental spelling correction, nuking any undesirable
   side-effects."
  (declare (ignore p))
  (unless (hemlock-bound-p 'last-spelling-correction-mark)
    (editor-error "No last spelling correction."))
  (let ((mark (value last-spelling-correction-mark))
	(words (value last-spelling-correction-words)))
    (unless words
      (editor-error "No last spelling correction."))
    (let* ((new (car words))
	   (old (cdr words))
	   (folded (string-upcase old)))
      (declare (simple-string old new folded))
      (remhash folded *spelling-corrections*)
      (delete-characters mark (length new))
      (insert-string mark old)
      (setf (value last-spelling-correction-words) nil)
      (when (or (not (value spelling-un-correct-prompt-for-insert))
		(prompt-for-y-or-n
		 :prompt (list "Insert ~A into spelling dictionary? " folded)
		 :default t
		 :default-string "Y"))
	(push folded (spell-info-insertions (value spell-information)))
	(spell:maybe-read-spell-dictionary)
	(spell:spell-add-entry folded)
	(message "Added ~S to spelling dictionary." old)))))


;;; Check-Out-Word-Spelling  --  Internal
;;;
;;;    Return Nil if Word is a candidate for correction, otherwise
;;; return T and message as to why it isn't.
;;;
(defun check-out-word-spelling (word folded)
  (declare (simple-string word))
  (let ((len (length word)))
      (cond ((= len 1)
	     (message "Single character words are not in the dictionary.") t)
	    ((> len spell:max-entry-length)
	     (message "~A is too long for the dictionary." word) t)
	    (t
	     (multiple-value-bind (idx flagp) (spell:spell-try-word folded len)
	       (when idx
		 (message "Found it~:[~; because of ~A~]." flagp
			  (spell:spell-root-word idx))
		 t))))))

;;; Get-Word-Correction  --  Internal
;;;
;;;    Find all known close words to the either unknown or incorrectly
;;; spelled word we are checking.  Word is the unmunged word, and Folded is
;;; the uppercased word.  Mark is a mark which points to the beginning of
;;; the offending word.  Return True if we successfully corrected the word.
;;;
(defun get-word-correction (mark word folded)
  (let ((close-words (spell:spell-collect-close-words folded)))
    (declare (list close-words))
    (if close-words
	(with-pop-up-display (s :height 3)
	  (do ((i 0 (1+ i))
	       (words close-words (cdr words)))
	      ((null words))
	    (format s "~36R=~A " i (car words)))
	  (finish-output s)
	  (let* ((key-event (prompt-for-key-event
			     :prompt "Correction choice: "))
		 (num (digit-char-p (ext:key-event-char key-event) 36)))
	    (cond ((not num) (return-from get-word-correction nil))
		  ((> num (length close-words))
		   (editor-error "Choice out of range."))
		  (t
		   (let ((s (nth num close-words)))
		     (setf (gethash folded *spelling-corrections*) s)
		     (undoable-replace-word mark word s)))))
	  (return-from get-word-correction t))
	(with-pop-up-display (s :height 1)
	  (write-line "No corrections found." s)
	  nil))))


;;; Undoable-Replace-Word  --  Internal
;;;
;;;    Like Spell-Replace-Word, but makes annotations in buffer local variables
;;; so that "Undo Last Spelling Correction" can undo it.
;;;
(defun undoable-replace-word (mark old new)
  (unless (hemlock-bound-p 'last-spelling-correction-mark)
    (let ((buffer (current-buffer)))
      (defhvar "Last Spelling Correction Mark"
	"This variable holds a park pointing to the last spelling correction."
	:buffer buffer  :value (copy-mark (buffer-start-mark buffer)))
      (defhvar "Last Spelling Correction Words"
	"The replacement done for the last correction: (new . old)."
	:buffer buffer  :value nil)))
  (move-mark (value last-spelling-correction-mark) mark)
  (setf (value last-spelling-correction-words) (cons new old))
  (spell-replace-word mark old new))


;;;; Buffer Correction

(defvar *spell-word-characters*
  (make-array char-code-limit :element-type 'bit  :initial-element 0)
  "Characters that are legal in a word for spelling checking purposes.")

(do-alpha-chars (c :both)
  (setf (sbit *spell-word-characters* (char-code c)) 1))
(setf (sbit *spell-word-characters* (char-code #\')) 1)


(defcommand "Correct Buffer Spelling" (p)
  "Correct spelling over whole buffer.  A log of the found misspellings is
   kept in the buffer \"Spell Corrections\".  For each unknown word the
   user may accept it, insert it in the dictionary, correct its spelling
   with one of the offered possibilities, replace the word with a user
   supplied word, or go into a recursive edit.  Words may be added to the
   dictionary in advance from a text file (see the command \"Augment
   Spelling Dictionary\")."
  "Correct spelling over whole buffer."
  (declare (ignore p))
  (clrhash *ignored-misspellings*)
  (let* ((buffer (current-buffer))
	 (log (or (make-buffer "Spelling Corrections")
		  (getstring "Spelling Corrections" *buffer-names*)))
	 (point (buffer-end (buffer-point log)))
	 (*standard-output* (make-hemlock-output-stream point))
	 (window (or (car (buffer-windows log)) (make-window point))))
    (format t "~&Starting spelling checking of buffer ~S.~2%"
	    (buffer-name buffer))
    (spell:maybe-read-spell-dictionary)
    (correct-buffer-spelling buffer window)
    (delete-window window)
    (close *standard-output*)))

;;; CORRECT-BUFFER-SPELLING scans through buffer a line at a time, grabbing the
;;; each line's string and breaking it up into words using the
;;; *spell-word-characters* mask.  We try the spelling of each word, and if it
;;; is unknown, we call FIX-WORD and resynchronize when it returns.
;;;
(defun correct-buffer-spelling (buffer window)
  (do ((line (mark-line (buffer-start-mark buffer)) (line-next line))
       (info (if (hemlock-bound-p 'spell-information :buffer buffer)
		 (variable-value 'spell-information :buffer buffer)
		 (value spell-information)))
       (mask *spell-word-characters*)
       (word (make-string spell:max-entry-length)))
      ((null line))
    (declare (simple-bit-vector mask) (simple-string word))
    (block line
      (let* ((string (line-string line))
	     (length (length string)))
	(declare (simple-string string))
	(do ((start 0 (or skip-apostrophes end))
	     (skip-apostrophes nil nil)
	     end)
	    (nil)
	  ;;
	  ;; Find word start.
	  (loop
	    (when (= start length) (return-from line))
	    (when (/= (bit mask (char-code (schar string start))) 0) (return))
	    (incf start))
	  ;;
	  ;; Find the end.
	  (setq end (1+ start))
	  (loop
	    (when (= end length) (return))
	    (when (zerop (bit mask (char-code (schar string end)))) (return))
	    (incf end))
	  (multiple-value-setq (end skip-apostrophes)
	    (correct-buffer-word-end string start end))
	  ;;
	  ;; Check word.
	  (let ((word-len (- end start)))
	    (cond
	     ((= word-len 1))
	     ((> word-len spell:max-entry-length)
	      (format t "Not checking ~S -- too long for dictionary.~2%"
		      word))
	     (t
	      ;;
	      ;; Copy the word and uppercase it.
	      (do* ((i (1- end) (1- i))
		    (j (1- word-len) (1- j)))
		   ((zerop j)
		    (setf (schar word 0) (char-upcase (schar string i))))
		(setf (schar word j) (char-upcase (schar string i))))
	      (unless (spell:spell-try-word word word-len)
		(move-to-position (current-point) start line)
		(fix-word (subseq word 0 word-len) (subseq string start end)
			  window info)
		(let ((point (current-point)))
		  (setq end (mark-charpos point)
			line (mark-line point)
			string (line-string line)
			length (length string))))))))))))

;;; CORRECT-BUFFER-WORD-END takes a line string from CORRECT-BUFFER-SPELLING, a
;;; start, and a end.  It places end to exclude from the word apostrophes used
;;; for quotation marks, possessives, and funny plurals (e.g., A's and AND's).
;;; Every word potentially can be followed by "'s", and any clown can use the
;;; `` '' Scribe ligature.  This returns the value to use for end of the word
;;; and the value to use as the end when continuing to find the next word in
;;; string.
;;;
(defun correct-buffer-word-end (string start end)
  (cond ((and (> (- end start) 2)
	      (char= (char-upcase (schar string (1- end))) #\S)
	      (char= (schar string (- end 2)) #\'))
	 ;; Use roots of possessives and funny plurals (e.g., A's and AND's).
	 (values (- end 2) end))
	(t
	 ;; Maybe backup over apostrophes used for quotation marks.
	 (do ((i (1- end) (1- i)))
	     ((= i start) (values end end))
	   (when (char/= (schar string i) #\')
	     (return (values (1+ i) end)))))))

;;; Fix-Word  --  Internal
;;;
;;;    Handles the case where the word has a known correction.  If is does
;;; not then call Correct-Buffer-Word-Not-Found.  In either case, the
;;; point is left at the place to resume checking.
;;;
(defun fix-word (word unfolded-word window info)
  (declare (simple-string word unfolded-word))
  (let ((correction (gethash word *spelling-corrections*))
	(mark (current-point)))
    (cond (correction
	   (format t "Replacing ~S with ~S.~%" unfolded-word correction)
	   (spell-replace-word mark unfolded-word correction))
	  ((and (value spell-ignore-uppercase)
		(every #'upper-case-p unfolded-word))
	   (character-offset mark (length word))
	   (unless (gethash word *ignored-misspellings*)
	     (setf (gethash word *ignored-misspellings*) t)
	     (format t "Ignoring ~S.~%" unfolded-word)))
	  (t
	   (correct-buffer-word-not-found word unfolded-word window info)))))

(defun correct-buffer-word-not-found (word unfolded-word window info)
  (declare (simple-string word unfolded-word))
  (let* ((close-words (spell:spell-collect-close-words word))
	 (close-words-len (length (the list close-words)))
	 (mark (current-point))
	 (wordlen (length word)))
    (format t "Unknown word: ~A~%" word)
    (cond (close-words
	   (format t "~[~;A~:;Some~]~:* possible correction~[~; is~:;s are~]: "
		   close-words-len)
	   (if (= close-words-len 1)
	       (write-line (car close-words))
	       (let ((n 0))
		 (dolist (w close-words (terpri))
		   (format t "~36R=~A " n w)
		   (incf n)))))
	  (t
	   (write-line "No correction possibilities found.")))
    (let ((point (buffer-point (window-buffer window))))
      (unless (displayed-p point window)
	(center-window window point)))
    (command-case
       (:prompt "Action: "
        :help "Type a single letter command, or help character for help."
        :change-window nil)
      (#\i "Insert unknown word into dictionary for future lookup."
	 (spell:spell-add-entry word)
	 (push word (spell-info-insertions info))
	 (format t "~S added to dictionary.~2%" word))
      (#\c "Correct the unknown word with possible correct spellings."
	 (unless close-words
	   (write-line "There are no possible corrections.")
	   (reprompt))
	 (let ((num (if (= close-words-len 1) 0
			(digit-char-p (ext:key-event-char
				       (prompt-for-key-event
					:prompt "Correction choice: "))
				      36))))
	   (unless num (reprompt))
	   (when (> num close-words-len)
	     (beep)
	     (write-line "Response out of range.")
	     (reprompt))
	   (let ((choice (nth num close-words)))
	     (setf (gethash word *spelling-corrections*) choice)
	     (spell-replace-word mark unfolded-word choice)))
	 (terpri))
      (#\a "Accept the word as correct (that is, ignore it)."
	 (character-offset mark wordlen))
      (#\r "Replace the unknown word with a supplied replacement."
	 (let ((s (prompt-for-string
		   :prompt "Replacement Word: "
		   :default unfolded-word
		   :help "String to replace the unknown word with.")))
	   (setf (gethash word *spelling-corrections*) s)
	   (spell-replace-word mark unfolded-word s))
	 (terpri))
      (:recursive-edit
       "Go into a recursive edit and resume correction where the point is left."
       (do-recursive-edit)))))

;;; Spell-Replace-Word  --  Internal
;;;
;;;    Replaces Old with New, starting at Mark.  The case of Old is used
;;; to derive the new case.
;;;
(defun spell-replace-word (mark old new)
  (declare (simple-string old new))
  (let ((res (cond ((lower-case-p (schar old 0))
		    (string-downcase new))
		   ((lower-case-p (schar old 1))
		    (let ((res (string-downcase new)))
		      (setf (char res 0) (char-upcase (char res 0)))
		      res))
		   (t
		    (string-upcase new)))))
    (with-mark ((m mark :left-inserting))
      (delete-characters m (length old))
      (insert-string m res))))


;;;; User Spelling Dictionaries.

(defvar *pathname-to-spell-info* (make-hash-table :test #'equal)
  "This maps dictionary files to spelling information.")

(defhvar "Spell Information"
  "This is the information about a spelling dictionary and its incremental
   insertions."
  :value (make-spell-info nil))

(define-file-option "Dictionary" (buffer file)
  (let* ((dict (merge-pathnames
		file
		(make-pathname :defaults (buffer-default-pathname buffer)
			       :type "dict")))
	 (dictp (probe-file dict)))
    (if dictp
	(set-buffer-spelling-dictionary-command nil dictp buffer)
	(loud-message "Couldn't find dictionary ~A." (namestring dict)))))

;;; SAVE-DICTIONARY-ON-WRITE is on the "Write File Hook" in buffers with
;;; the "dictionary" file option.
;;; 
(defun save-dictionary-on-write (buffer)
  (when (hemlock-bound-p 'spell-information :buffer buffer)
    (save-spelling-insertions
     (variable-value 'spell-information :buffer buffer))))


(defcommand "Save Incremental Spelling Insertions" (p)
  "Append incremental spelling dictionary insertions to a file.  The file
   is prompted for unless \"Set Buffer Spelling Dictionary\" has been
   executed in the buffer."
  "Append incremental spelling dictionary insertions to a file."
  (declare (ignore p))
  (let* ((info (value spell-information))
	 (file (or (spell-info-pathname info)
		   (value default-user-spelling-dictionary)
		   (prompt-for-file
		    :prompt "Dictionary File: "
		    :default (dictionary-name-default)
		    :must-exist nil
		    :help
 "Name of the dictionary file to append dictionary insertions to."))))
    (save-spelling-insertions info file)
    (let* ((ginfo (variable-value 'spell-information :global))
	   (insertions (spell-info-insertions ginfo)))
      (when (and insertions
		 (prompt-for-y-or-n
		  :prompt
		  `("Global spelling insertions exist.~%~
		     Save these to ~A also? "
		    ,(namestring file)
		  :default t
		  :default-string "Y"))
	(save-spelling-insertions ginfo file))))))

(defun save-spelling-insertions (info &optional
				      (name (spell-info-pathname info)))
  (when (spell-info-insertions info)
    (with-open-file (stream name
			    :direction :output :element-type 'base-char
			    :if-exists :append :if-does-not-exist :create)
      (dolist (w (spell-info-insertions info))
	(write-line w stream)))
    (setf (spell-info-insertions info) ())
    (message "Incremental spelling insertions for ~A written."
	     (namestring name))))

(defcommand "Set Buffer Spelling Dictionary" (p &optional file buffer)
  "Prompts for the dictionary file to associate with the current buffer.
   If this file has not been read for any other buffer, then it is read.
   Incremental spelling insertions from this buffer can be appended to
   this file with \"Save Incremental Spelling Insertions\"."
  "Sets the buffer's spelling dictionary and reads it if necessary."
  (declare (ignore p))
  (maybe-read-default-user-spelling-dictionary)
  (let* ((file (truename (or file
			     (prompt-for-file
			      :prompt "Dictionary File: "
			      :default (dictionary-name-default)
			      :help
 "Name of the dictionary file to add into the current dictionary."))))
	 (file-name (namestring file))
	 (spell-info-p (gethash file-name *pathname-to-spell-info*))
	 (spell-info (or spell-info-p (make-spell-info file)))
	 (buffer (or buffer (current-buffer))))
    (defhvar "Spell Information"
      "This is the information about a spelling dictionary and its incremental
       insertions."
      :value spell-info :buffer buffer)
    (add-hook write-file-hook 'save-dictionary-on-write)
    (unless spell-info-p
      (setf (gethash file-name *pathname-to-spell-info*) spell-info)
      (read-spelling-dictionary-command nil file))))

(defcommand "Read Spelling Dictionary" (p &optional file)
  "Adds entries to the dictionary from a file in the following format:
   
      entry1/flag1/flag2/flag3
      entry2
      entry3/flag1/flag2/flag3/flag4/flag5.

   The flags are single letter indicators of legal suffixes for the entry;
   the available flags and their correct use may be found at the beginning
   of spell-correct.lisp in the Hemlock sources.  There must be exactly one 
   entry per line, and each line must be flushleft."
  "Add entries to the dictionary from a text file in a specified format."
  (declare (ignore p))
  (spell:maybe-read-spell-dictionary)
  (spell:spell-read-dictionary
   (or file
       (prompt-for-file
	:prompt "Dictionary File: "
	:default (dictionary-name-default)
	:help
	"Name of the dictionary file to add into the current dictionary."))))

(defun dictionary-name-default ()
  (make-pathname :defaults (buffer-default-pathname (current-buffer))
		 :type "dict"))

(defcommand "Add Word to Spelling Dictionary" (p)
  "Add the previous word to the spelling dictionary."
  "Add the previous word to the spelling dictionary."
  (declare (ignore p))
  (spell:maybe-read-spell-dictionary)
  (let ((word (region-to-string (spell-previous-word (current-point) nil))))
    ;;
    ;; SPELL:SPELL-ADD-ENTRY destructively uppercases word.
    (when (spell:spell-add-entry word)
      (message "Word ~(~S~) added to the spelling dictionary." word)
      (push word (spell-info-insertions (value spell-information))))))

(defcommand "Remove Word from Spelling Dictionary" (p)
  "Prompts for word to remove from the spelling dictionary."
  "Prompts for word to remove from the spelling dictionary."
   (declare (ignore p))
  (spell:maybe-read-spell-dictionary)
  (let* ((word (prompt-for-string
		:prompt "Word to remove from spelling dictionary: "
		:trim t))
	 (upword (string-upcase word)))
    (declare (simple-string word))
    (multiple-value-bind (index flagp)
			 (spell:spell-try-word upword (length word))
      (unless index
	(editor-error "~A not in dictionary." upword))
      (if flagp
	  (remove-spelling-word upword)
	  (let ((flags (spell:spell-root-flags index)))
	    (when (or (not flags)
		      (prompt-for-y-or-n
		       :prompt
 `("Deleting ~A also removes words formed from this root and these flags: ~%  ~
    ~S.~%~
    Delete word anyway? "
   ,word ,flags)
		       :default t
		       :default-string "Y"))
	      (remove-spelling-word upword)))))))

;;; REMOVE-SPELLING-WORD removes the uppercase word word from the spelling
;;; dictionary and from the spelling informations incremental insertions list.
;;; 
(defun remove-spelling-word (word)
  (let ((info (value spell-information)))
    (spell:spell-remove-entry word)
    (setf (spell-info-insertions info)
	  (delete word (spell-info-insertions info) :test #'string=))))

(defcommand "List Incremental Spelling Insertions" (p)
  "Display the incremental spelling insertions for the current buffer's
   associated spelling dictionary file."
  "Display the incremental spelling insertions for the current buffer's
   associated spelling dictionary file."
  (declare (ignore p))
  (let* ((info (value spell-information))
	 (file (spell-info-pathname info))
	 (insertions (spell-info-insertions info)))
    (declare (list insertions))
    (with-pop-up-display (s :height (1+ (length insertions)))
      (if file
	  (format s "Incremental spelling insertions for dictionary ~A:~%"
		  (namestring file))
	  (write-line "Global incremental spelling insertions:" s))
      (dolist (w insertions)
	(write-line w s)))))



;;;; Utilities for above stuff.

;;; SPELL-PREVIOUS-WORD returns as a region the current or previous word, using
;;; the spell word definition.  If there is no such word, return nil.  If end-p
;;; is non-nil, then mark ends the word even if there is a non-delimiter
;;; character after it.
;;;
;;; Actually, if mark is between the first character of a word and a
;;; non-spell-word characer, it is considered to be in that word even though
;;; that word is after the mark.  This is because Hemlock's cursor is always
;;; displayed over the next character, so users tend to think of a cursor
;;; displayed on the first character of a word as being in that word instead of
;;; before it.
;;;
(defun spell-previous-word (mark end-p)
  (with-mark ((point mark)
	      (mark mark))
    (cond ((or end-p
	       (zerop (character-attribute :spell-word-character
					   (next-character point))))
	   (unless (reverse-find-attribute mark :spell-word-character)
	     (return-from spell-previous-word nil))
	   (move-mark point mark)
	   (reverse-find-attribute point :spell-word-character #'zerop))
	  (t
	   (find-attribute mark :spell-word-character #'zerop)
	   (reverse-find-attribute point :spell-word-character #'zerop)))
    (cond ((and (> (- (mark-charpos mark) (mark-charpos point)) 2)
		(char= (char-upcase (previous-character mark)) #\S)
		(char= (prog1 (previous-character (mark-before mark))
			 (mark-after mark))
		       #\'))
	   ;; Use roots of possessives and funny plurals (e.g., A's and AND's).
	   (character-offset mark -2))
	  (t
	   ;; Maybe backup over apostrophes used for quotation marks.
	   (loop
	     (when (mark= point mark) (return-from spell-previous-word nil))
	     (when (char/= (previous-character mark) #\') (return))
	     (mark-before mark))))
    (region point mark)))
