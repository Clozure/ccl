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
;;; Echo area commands.
;;;
;;; Written by Rob MacLachlan and Skef Wholey.
;;;
(in-package :hemlock)

(defhvar "Beep on Ambiguity"
  "If non-NIL, beep when completion of a parse is ambiguous."
  :value t)

(defhvar "Ignore File Types"
  "File types to ignore when trying to complete a filename."
  :value
  (list "fasl" "pmaxf" "sparcf" "rtf" "hpf" "axpf" "sgif" "err"
	"x86f" "lbytef"	"core" "trace"	    ; Lisp
	"BAK" "CKP"			    ; Backups & Checkpoints
	"PS" "ps" "press" "otl" "dvi" "toc" ; Formatting
	"bbl" "lof" "idx" "lot" "aux"	    ; Formatting
	"mo" "elc"			    ; Other editors
	"bin" "lbin"			    ; Obvious binary extensions.
	"o" "a" "aout" "out"		    ; UNIXY stuff
	"bm" "onx" "snf"		    ; X stuff
	"UU" "uu" "arc" "Z" "gz" "tar"	    ; Binary encoded files
	))


;;; Field separator characters separate fields for TOPS-20 ^F style 
;;; completion.
(defattribute "Parse Field Separator"
  "A value of 1 for this attribute indicates that the corresponding character
  should be considered to be a field separator by the prompting commands.")
(setf (character-attribute :parse-field-separator #\space) 1)


;;; Find-All-Completions  --  Internal
;;;
;;;    Return as a list of all the possible completions of String in the
;;; list of string-tables Tables.
;;;
(defun find-all-completions (string tables)
  (do ((table tables (cdr table))
       (res () 
	    (merge 'list (find-ambiguous string (car table)) 
		   res #'string-lessp)))
      ((null table) res)))

(defcommand "Help on Parse" (p)
  "Display help for parse in progress.
  If there are a limited number of options then display them."
  "Display the *Parse-Help* and any possibly completions of the current
  input."
  (declare (ignore p))
  (let ((help (typecase *parse-help*
		(list (unless *parse-help* (error "There is no parse help."))
		      (apply #'format nil *parse-help*))
		(string *parse-help*)
		(t (error "Parse help is not a string or list: ~S" *parse-help*))))
	(input (region-to-string *parse-input-region*)))
    (cond
     ((eq *parse-type* :keyword)
      (let ((strings (find-all-completions input *parse-string-tables*)))
	(with-pop-up-display (s :title "input help" :height (+ (length strings) 2))
	  (write-line help s)
	  (cond (strings
		 (write-line "Possible completions of what you have typed:" s)
		 (dolist (string strings)
		   (write-line string s)))
		(t
		 (write-line 
 "There are no possible completions of what you have typed." s))))))
     ((and (eq *parse-type* :file) (not (zerop (length input))))
      (let ((pns (ambiguous-files (region-to-string *parse-input-region*)
				  *parse-default*)))
	(declare (list pns))
	(with-pop-up-display(s :title "Completion help" :height (+ (length pns) 2))
	  (write-line help s)
	  (cond (pns
		 (write-line "Possible completions of what you have typed:" s)
		 (let ((width (- (window-width (current-window)) 27)))
		   (dolist (pn pns)
		     (let* ((dir (directory-namestring pn))
			    (len (length dir)))
		       (unless (<= len width)
			 (let ((slash (position #\/ dir
						:start (+ (- len width) 3))))
			   (setf dir
				 (if slash
				     (concatenate 'string "..."
						  (subseq dir slash))
				     "..."))))
		       (format s " ~A~25T ~A~%"
			       (file-namestring pn) dir)))))
		(t
		 (write-line 
 "There are no possible completions of what you have typed." s))))))
     (t
      (with-mark ((m (buffer-start-mark *echo-area-buffer*) :left-inserting))
	(insert-string m help)
	(insert-character m #\newline))))))

(defun file-completion-action (typein)
  (declare (simple-string typein))
  (when (zerop (length typein)) (editor-error))
  (multiple-value-bind
      (result win)
      (hemlock-ext:complete-file typein
                                 :defaults (directory-namestring *parse-default*)
                                 :ignore-types (value ignore-file-types))
    (when result
      (delete-region *parse-input-region*)
      (insert-string (region-start *parse-input-region*)
		     (namestring result)))
    (when (and (not win) (value beep-on-ambiguity))
      (editor-error))))

(defcommand "Complete Keyword" (p)
  "Trys to complete the text being read in the echo area as a string in
  *parse-string-tables*"
  "Complete the keyword being parsed as far as possible.
  If it is ambiguous and ``Beep On Ambiguity'' true beep."
  (declare (ignore p))
  (let ((typein (region-to-string *parse-input-region*)))
    (declare (simple-string typein))
    (case *parse-type*
      (:keyword
       (multiple-value-bind
	   (prefix key value field ambig)
	   (complete-string typein *parse-string-tables*)
	 (declare (ignore value field))
	 (when prefix
	   (delete-region *parse-input-region*)
	   (insert-string (region-start *parse-input-region*) prefix)
	   (when (eq key :ambiguous)
	     (let ((point (current-point)))
	       (move-mark point (region-start *parse-input-region*))
	       (unless (character-offset point ambig)
		 (buffer-end point)))))
	 (when (and (or (eq key :ambiguous) (eq key :none))
		    (value beep-on-ambiguity))
	   (editor-error))))
      (:file
       (file-completion-action typein))
      (t
       (editor-error "Cannot complete input for this prompt.")))))

(defun field-separator-p (x)
  (plusp (character-attribute :parse-field-separator x)))

(defcommand "Complete Field" (p)
  "Complete a field in a parse.
  Fields are defined by the :field separator attribute,
  the text being read in the echo area as a string in *parse-string-tables*"
  "Complete a field in a keyword.
  If it is ambiguous and ``Beep On Ambiguity'' true beep.  Fields are
  separated by characters having a non-zero :parse-field-separator attribute,
  and this command should only be bound to characters having that attribute."
  (let ((typein (region-to-string *parse-input-region*)))
    (declare (simple-string typein))
    (case *parse-type*
      (:string
       (self-insert-command p))
      (:file
       (file-completion-action typein))
      (:keyword
       (let ((point (current-point)))
	 (unless (blank-after-p point)
	   (insert-character point
			     (hemlock-ext:key-event-char *last-key-event-typed*))))
       (multiple-value-bind
	   (prefix key value field ambig)
	   (complete-string typein *parse-string-tables*)
	 (declare (ignore value ambig))
	 (when (eq key :none) (editor-error "No possible completion."))
	 (delete-region *parse-input-region*)
	 (let ((new-typein (if (and (eq key :unique) (null field))
			       (subseq prefix 0 field)
			       (concatenate 'string
					    (subseq prefix 0 field)
					    (string
					     (hemlock-ext:key-event-char
					      *last-key-event-typed*))))))
	   (insert-string (region-start *parse-input-region*) new-typein))))
      (t
       (editor-error "Cannot complete input for this prompt.")))))


(defvar *echo-area-history* (make-ring 10)
  "This ring-buffer contains strings which were previously input in the
  echo area.")

(defvar *echo-history-pointer* 0
  "This is our current position to the ring during a historical exploration.")

(defcommand "Confirm Parse" (p)
  "Terminate echo-area input.
  If the input is invalid then an editor-error will signalled."
  "If no input has been given, exits the recursive edit with the default,
  otherwise calls the verification function."
  (declare (ignore p))
  (let* ((string (region-to-string *parse-input-region*))
	 (empty (zerop (length string))))
    (declare (simple-string string))
    (if empty
	(when *parse-default* (setq string *parse-default*))
	(when (or (zerop (ring-length *echo-area-history*))
		  (string/= string (ring-ref *echo-area-history* 0)))
	  (ring-push string *echo-area-history*)))
    (multiple-value-bind (res flag)
			 (funcall *parse-verification-function* string)
      (unless (or res flag) (editor-error))
      (exit-recursive-edit res))))

(defcommand "Previous Parse" (p)
  "Rotate the echo-area history forward.
  If current input is non-empty and different from what is on the top
  of the ring then push it on the ring before inserting the new input."
  "Pop the *echo-area-history* ring buffer."
  (let ((length (ring-length *echo-area-history*))
	(p (or p 1)))
    (when (zerop length) (editor-error))
    (cond
     ((eq (last-command-type) :echo-history)
      (let ((base (mod (+ *echo-history-pointer* p) length)))
	(delete-region *parse-input-region*)
	(insert-string (region-end *parse-input-region*)
		       (ring-ref *echo-area-history* base))
	(setq *echo-history-pointer* base)))
     (t
      (let ((current (region-to-string *parse-input-region*))
	    (base (mod (if (minusp p) p (1- p)) length)))
	(delete-region *parse-input-region*)
	(insert-string (region-end *parse-input-region*)
		       (ring-ref *echo-area-history* base))	
	(when (and (plusp (length current))
		   (string/= (ring-ref *echo-area-history* 0) current))
	  (ring-push current *echo-area-history*)
	  (incf base))
	(setq *echo-history-pointer* base))))
    (setf (last-command-type) :echo-history)))

(defcommand "Next Parse" (p)
  "Rotate the echo-area history backward.
  If current input is non-empty and different from what is on the top
  of the ring then push it on the ring before inserting the new input."
  "Push the *echo-area-history* ring buffer."
  (previous-parse-command (- (or p 1))))

(defcommand "Illegal" (p)
  "This signals an editor-error.
  It is useful for making commands locally unbound."
  "Just signals an editor-error."
  (declare (ignore p))
  (editor-error))

(add-hook window-buffer-hook
	  #'(lambda (window new-buff)
	      (when (and (eq window *echo-area-window*)
			 (not (eq new-buff *echo-area-buffer*)))
		(editor-error "Can't change echo area window."))))

(defcommand "Beginning Of Parse" (p)
  "Moves to immediately after the prompt when in the echo area."
  "Move the point of the echo area buffer to *parse-starting-mark*."
  (declare (ignore p))
  (move-mark (buffer-point *echo-area-buffer*) *parse-starting-mark*))

(defcommand "Echo Area Delete Previous Character" (p)
  "Delete the previous character.
  Don't let the luser rub out the prompt."
  "Signal an editor-error if we would nuke the prompt,
  otherwise do a normal delete."
  (with-mark ((tem (buffer-point *echo-area-buffer*)))
    (unless (character-offset tem (- (or p 1))) (editor-error))
    (when (mark< tem *parse-starting-mark*) (editor-error))
    (delete-previous-character-command p)))

(defcommand "Echo Area Kill Previous Word" (p)
  "Kill the previous word.
  Don't let the luser rub out the prompt."
  "Signal an editor-error if we would mangle the prompt, otherwise
  do a normal kill-previous-word."
  (with-mark ((tem (buffer-point *echo-area-buffer*)))
    (unless (word-offset tem (- (or p 1))) (editor-error))
    (when (mark< tem *parse-starting-mark*) (editor-error))
    (kill-previous-word-command p)))

(declaim (special *kill-ring*))

(defcommand "Kill Parse" (p)
  "Kills any input so far."
  "Kills *parse-input-region*."
  (declare (ignore p))
  (if (end-line-p (current-point))
      (kill-region *parse-input-region* :kill-backward)
      (ring-push (delete-and-save-region *parse-input-region*)
		 *kill-ring*)))

(defcommand "Insert Parse Default" (p)
  "Inserts the default for the parse in progress.
  The text is inserted at the point."
  "Inserts *parse-default* at the point of the *echo-area-buffer*.
  If there is no default an editor-error is signalled."
  (declare (ignore p))
  (unless *parse-default* (editor-error))
  (insert-string (buffer-point *echo-area-buffer*) *parse-default*))

(defcommand "Echo Area Backward Character" (p)
  "Go back one character.
  Don't let the luser move into the prompt."
  "Signal an editor-error if we try to go into the prompt, otherwise
  do a backward-character command."
  (backward-character-command p)
  (when (mark< (buffer-point *echo-area-buffer*) *parse-starting-mark*)
    (beginning-of-parse-command ())
    (editor-error)))

(defcommand "Echo Area Backward Word" (p)
  "Go back one word.
  Don't let the luser move into the prompt."
  "Signal an editor-error if we try to go into the prompt, otherwise
  do a backward-word command."
  (backward-word-command p)
  (when (mark< (buffer-point *echo-area-buffer*) *parse-starting-mark*)
    (beginning-of-parse-command ())
    (editor-error)))
