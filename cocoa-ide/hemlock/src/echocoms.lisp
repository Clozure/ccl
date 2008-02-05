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
  (list "fasl" "cfsl" "dfsl" "cfasl"
	"pmaxf" "sparcf" "rtf" "hpf" "axpf" "sgif" "err"
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

(defun get-parse-input-string (eps)
  (region-to-string (eps-parse-input-region eps)))

(defun replace-parse-input-string (eps string)
  (delete-region (eps-parse-input-region eps))
  (insert-string (eps-parse-starting-mark eps) string))

(defcommand "Help on Parse" (p)
  "Display help for parse in progress.
  If there are a limited number of options then display them."
  "Display the *Parse-Help* and any possibly completions of the current
  input."
  (declare (ignore p))
  (let* ((eps (current-echo-parse-state))
	 (raw-help (eps-parse-help eps))
	 (help (typecase raw-help
		 (null (error "There is no parse help."))
		 (list (apply #'format nil raw-help))
		 (string raw-help)
		 (t (error "Parse help is not a string or list: ~S" raw-help))))
	 (input (get-parse-input-string eps)))
    (cond
     ((eq (eps-parse-type eps) :keyword)
      (let ((strings (find-all-completions input (eps-parse-string-tables eps))))
	(with-pop-up-display (s :title "input help" :height (+ (length strings) 2))
	  (write-line help s)
	  (cond (strings
		 (write-line "Possible completions of what you have typed:" s)
		 (dolist (string strings)
		   (write-line string s)))
		(t
		 (write-line "There are no possible completions of what you have typed." s))))))
     ((and (eq (eps-parse-type eps) :file) (not (zerop (length input))))
      (let ((pns (ambiguous-files input (eps-parse-default eps))))
	(declare (list pns))
	(with-pop-up-display(s :title "Completion help" :height (+ (length pns) 2))
	  (write-line help s)
	  (cond (pns
		 (write-line "Possible completions of what you have typed:" s)
		 (let ((width 55))
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
		 (write-line  "There are no possible completions of what you have typed." s))))))
     (t
      (with-pop-up-display (s :title "input help" :height 2)
	(write-line help s))))))

(defun file-completion-action (eps typein)
  (declare (simple-string typein))
  (when (zerop (length typein)) (editor-error))
  (multiple-value-bind
      (result win)
      (complete-file typein
		     :defaults (directory-namestring (eps-parse-default eps))
		     :ignore-types (value ignore-file-types))
    (when result
      (replace-parse-input-string eps (namestring result)))
    (when (and (not win) (value beep-on-ambiguity))
      (editor-error))))

(defcommand "Complete Keyword" (p)
  "Trys to complete the text being read in the echo area as a string in
  *parse-string-tables*"
  "Complete the keyword being parsed as far as possible.
  If it is ambiguous and ``Beep On Ambiguity'' true beep."
  (declare (ignore p))
  (let* ((eps (current-echo-parse-state))
	 (typein (get-parse-input-string eps)))
    (declare (simple-string typein))
    (case (eps-parse-type eps)
      (:keyword
       (multiple-value-bind (prefix key value field ambig)
			    (complete-string typein (eps-parse-string-tables eps))
	 (declare (ignore value field))
	 (when prefix
	   (replace-parse-input-string eps prefix)
	   (when (eq key :ambiguous)
	     (let ((point (current-point)))
	       (move-mark point (eps-parse-starting-mark eps))
	       (unless (character-offset point ambig)
		 (buffer-end point)))))
	 (when (and (or (eq key :ambiguous) (eq key :none))
		    (value beep-on-ambiguity))
	   (editor-error))))
      (:file
       (file-completion-action eps typein))
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
  (let* ((eps (current-echo-parse-state))
	 (typein (get-parse-input-string eps)))
    (declare (simple-string typein))
    (case (eps-parse-type eps)
      (:string
       (self-insert-command p))
      (:file
       (file-completion-action eps typein))
      (:keyword
       (let ((point (current-point)))
	 (unless (blank-after-p point)
	   (insert-character point (last-char-typed))))
       (multiple-value-bind
	   (prefix key value field ambig)
	   (complete-string typein (eps-parse-string-tables eps))
	 (declare (ignore value ambig))
	 (when (eq key :none) (editor-error "No possible completion."))
	 (let ((new-typein (if (and (eq key :unique) (null field))
			       (subseq prefix 0 field)
			       (concatenate 'string
					    (subseq prefix 0 field)
					    (string (last-char-typed))))))
	   (replace-parse-input-string eps new-typein))))
      (t
       (editor-error "Cannot complete input for this prompt.")))))


;;; *** TODO: this needs to be view-local
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
  (let* ((eps (current-echo-parse-state))
	 (string (get-parse-input-string eps))
	 (empty (zerop (length string))))
    (declare (simple-string string))
    (if empty
	(when (eps-parse-default eps) (setq string (eps-parse-default eps)))
	(when (or (zerop (ring-length *echo-area-history*))
		  (string/= string (ring-ref *echo-area-history* 0)))
	  (ring-push string *echo-area-history*)))
    (multiple-value-bind (vals flag)
			 (funcall (eps-parse-verification-function eps) eps string)
      ;; flag is to distinguish vals=() to return 0 values vs vals=nil because invalid.
      (unless (or vals flag) (editor-error))
      (exit-echo-parse eps vals))))

(defcommand "Previous Parse" (p)
  "Rotate the echo-area history forward.
  If current input is non-empty and different from what is on the top
  of the ring then push it on the ring before inserting the new input."
  "Pop the *echo-area-history* ring buffer."
  (let* ((eps (current-echo-parse-state))
	 (length (ring-length *echo-area-history*))
	 (p (or p 1)))
    (when (zerop length) (editor-error))
    (cond
     ((eq (last-command-type) :echo-history)
      (let ((base (mod (+ *echo-history-pointer* p) length)))
	(replace-parse-input-string eps (ring-ref *echo-area-history* base))
	(setq *echo-history-pointer* base)))
     (t
      (let ((current (get-parse-input-string eps))
	    (base (mod (if (minusp p) p (1- p)) length)))
	(replace-parse-input-string eps (ring-ref *echo-area-history* base))
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

(defcommand "Beginning Of Parse" (p)
  "Moves to immediately after the prompt when in the echo area."
  "Move the point of the echo area buffer to *parse-starting-mark*."
  (declare (ignore p))
  (let* ((eps (current-echo-parse-state))
	 (start (eps-parse-starting-mark eps)))
    (move-mark (current-point) start)))

(defcommand "Echo Area Delete Previous Character" (p)
  "Delete the previous character, up to the prompt."
  (let* ((eps (current-echo-parse-state))
	 (start (eps-parse-starting-mark eps)))
    (with-mark ((tem (current-point)))
      (unless (character-offset tem (- (or p 1))) (editor-error))
      (when (mark< tem start) (editor-error))
      (delete-previous-character-command p))))

(defcommand "Echo Area Kill Previous Word" (p)
  "Kill the previous word, up to the prompt."
  (let* ((eps (current-echo-parse-state))
	 (start (eps-parse-starting-mark eps)))
    (with-mark ((tem (current-point)))
      (unless (word-offset tem (- (or p 1))) (editor-error))
      (when (mark< tem start) (editor-error))
      (kill-previous-word-command p))))

(declaim (special *kill-ring*))

(defcommand "Kill Parse" (p)
  "Kills any input so far."
  "Kills *parse-input-region*."
  (declare (ignore p))
  (let* ((eps (current-echo-parse-state)))
    (if (end-line-p (current-point))
      (kill-region (eps-parse-input-region eps) :kill-backward)
      (ring-push (delete-and-save-region (eps-parse-input-region eps))
		 *kill-ring*))))

(defcommand "Insert Parse Default" (p)
  "Inserts the default for the parse in progress.
  The text is inserted at the point."
  (declare (ignore p))
  (let* ((eps (current-echo-parse-state))
	 (default (eps-parse-default eps)))
    (unless default (editor-error))
    (insert-string (current-point) default)))

(defcommand "Echo Area Backward Character" (p)
  "Go back one character.
   Don't let the luser move into the prompt."
  "Signal an editor-error if we try to go into the prompt, otherwise
   do a backward-character command."
  (let* ((eps (current-echo-parse-state))
	 (start (eps-parse-starting-mark eps))
	 (point (current-point)))
    (when (mark<= point start)
      (editor-error))
    (backward-character-command p)
    (when (mark< point start)
      (beginning-of-parse-command nil))))

(defcommand "Echo Area Backward Word" (p)
  "Go back one word.
  Don't let the luser move into the prompt."
  "Signal an editor-error if we try to go into the prompt, otherwise
  do a backward-word command."
  (let* ((eps (current-echo-parse-state))
	 (start (eps-parse-starting-mark eps))
	 (point (current-point)))
    (when (mark<= point start)
      (editor-error))
    (backward-word-command p)
    (when (mark< point start)
      (beginning-of-parse-command nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun append-key-name (key-event)
  (let ((point (current-point)))
    (insert-string point (pretty-key-string key-event t))
    (insert-character point #\Space)))

(defcommand "Key Input Handler" (p)
  "Internal command to handle input during y-or-n or key-event prompting"
  (declare (ignore p))
  (let* ((eps (current-echo-parse-state))
         (key-event (last-key-event-typed)))
    (multiple-value-bind (res exit-p)
                         (funcall (eps-parse-verification-function eps) eps key-event)
      #+GZ (log-debug "Key Input Hander: res: ~s exit-p ~s" res exit-p)
      (cond (exit-p
	     (unless (eq exit-p :confirmed)
	       (append-key-name key-event))
             (exit-echo-parse eps res))
            ((eq res :abort)
             (abort-to-toplevel))
            ((eq res :help)
             (help-on-parse-command nil))
            ((eq res :error)
             (beep))
	    ((eq res :ignore)
	     nil)
	    (t
	     (append-key-name key-event))))))

