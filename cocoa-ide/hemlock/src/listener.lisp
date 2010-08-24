;;; -*- Log: hemlock.log; Package: Hemlock -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Listener mode, dervived (sort of) from Hemlock's "Eval" mode.
;;;

(in-package :hemlock)


(defmacro in-lisp (&body body)
  "Evaluates body inside HANDLE-LISP-ERRORS.  *package* is bound to the package
   named by \"Current Package\" if it is non-nil."
  (let ((name (gensym)) (package (gensym)))
    `(handle-lisp-errors
      (let* ((,name (variable-value 'current-package :buffer (current-buffer)))
	     (,package (and ,name (find-package ,name))))
	(progv (if ,package '(*package*)) (if ,package (list ,package))
	  ,@body)))))


(defun package-name-change-hook (name kind where new-value)
  (declare (ignore name new-value))
  (if (eq kind :buffer)
    (hi::note-modeline-change where)))

(define-file-option "Package" (buffer value)
  (let* ((thing (handler-case (read-from-string value t)
                  (error () (editor-error "Bad package file option value"))))
         (name
          (cond
           ((or (stringp thing) (symbolp thing))
            (string thing))
           ((and (consp thing) ;; e.g. Package: (foo :use bar)
                 (or (stringp (car thing)) (symbolp (car thing))))
            (string (car thing)))
           (t
            (message "Ignoring \"package:\" file option ~a" thing)
            nil))))
    (when name
      (ignore-errors (let* ((*package* *package*))
                       (apply 'ccl::old-in-package (if (atom thing) (list thing) thing)))))
    (defhvar "Current Package"
      "The package used for evaluation of Lisp in this buffer."
      :buffer buffer
      :value (or name (package-name *package*))
      :hooks (list 'package-name-change-hook))
    (defhvar "Default Package"
      "The buffer's default package."
      :buffer buffer
      :value (or name (package-name *package*)))))
      


;;;; Listener Mode Interaction.



(defun setup-listener-mode (buffer)
  (let ((point (buffer-point buffer)))
    (setf (buffer-minor-mode buffer "Listener") t)
    (setf (buffer-minor-mode buffer "Editor") t)
    (setf (buffer-major-mode buffer) "Lisp")
    (buffer-end point)
    (defhvar "Current Package"
      "This variable holds the name of the package currently used for Lisp
       evaluation and compilation.  If it is Nil, the value of *Package* is used
       instead."
      :value nil
      :buffer buffer)
    (unless (hemlock-bound-p 'buffer-input-mark :buffer buffer)
      (defhvar "Buffer Input Mark"
	"Mark used for Listener Mode input."
	:buffer buffer
	:value (copy-mark point :right-inserting))
      (defhvar "Buffer Output Mark"
	"Mark used for Listener Mode output."
	:buffer buffer
	:value (copy-mark point :left-inserting))
      (defhvar "Interactive History"
	"A ring of the regions input to an interactive mode (Eval or Typescript)."
	:buffer buffer
	:value (make-ring (value interactive-history-length)))
      (defhvar "Interactive Pointer"
	"Pointer into \"Interactive History\"."
	:buffer buffer
	:value 0)
      (defhvar "Searching Interactive Pointer"
	"Pointer into \"Interactive History\"."
	:buffer buffer
	:value 0)
      (defhvar "Input Regions"
        "Input region history list."
        :buffer buffer
        :value nil)
      (defhvar "Current Input Font Region"
          "Current font region, for listener input"
        :buffer buffer
        :value nil)
      (defhvar "Current Output Font Region"
          "Current font region, for listener output"
        :buffer buffer
        :value nil)
      (defhvar "Grabbed Input Start Mark"
        "Mark start of grabbed input"
        :buffer buffer
        :value (copy-mark point :right-inserting))
      )
    (let* ((input-mark (variable-value 'buffer-input-mark :buffer buffer)))
      (when (hemlock-ext:read-only-listener-p)
	(setf (hi::buffer-protected-region buffer)
	      (region (buffer-start-mark buffer) input-mark)))
      (move-mark input-mark point)
      (append-font-regions buffer))))

(defmode "Listener" :major-p nil :setup-function #'setup-listener-mode)

(declaim (special hi::*listener-input-style* hi::*listener-output-style*))

(defun append-font-regions (buffer)
  (let* ((end (region-end (buffer-region buffer))))
    (setf (variable-value 'current-output-font-region :buffer buffer)
          (hi::new-font-region buffer end end hi::*listener-output-style*))
    (let* ((input (hi::new-font-region buffer end end hi::*listener-input-style*)))
      (hi::activate-buffer-font-region buffer input)
      (setf (variable-value 'current-input-font-region :buffer buffer) input))))

(defun append-buffer-output (buffer string)
  (let* ((output-region (variable-value 'current-output-font-region
                                        :buffer buffer))
         (output-end (region-end output-region)))
    (hi::with-active-font-region (buffer output-region)
      (with-mark ((output-mark output-end :left-inserting))
        ;(setf (mark-charpos output-mark) 0)
        (insert-string output-mark string))
      (move-mark (variable-value 'buffer-input-mark :buffer buffer)
                 output-end))))



(defparameter *listener-modeline-fields*
  (list	(modeline-field :package)
	(modeline-field :modes)
	(modeline-field :process-info)))
  
(defun listener-mode-lisp-mode-hook (buffer on)
  "Turn on Lisp mode when we go into Listener Mode."
  (when on
    (setf (buffer-major-mode buffer) "Lisp")))
;;;
(add-hook listener-mode-hook 'listener-mode-lisp-mode-hook)





(defvar lispbuf-eof '(nil))

(defun skip-line-comment (mark)
  ;; return t if we skipped a comment, nil otherwise
  (let ((cstart (to-line-comment mark ";")))
    (if cstart
        (progn (to-comment-end mark (string #\newline))
               t)
        nil)))

(defun balanced-expressions-in-region (region)
  "Return true if there's at least one syntactically well-formed S-expression
between the region's start and end, and if there are no ill-formed expressions in that region."
  ;; It helps to know that END-MARK immediately follows a #\newline.
  (let* ((start-mark (region-start region))
         (end-mark (region-end region))
         (end-line (mark-line end-mark))
         (end-charpos (mark-charpos end-mark)))
    (with-mark ((m start-mark))
      (pre-command-parse-check m)
      (when (form-offset m 1)
        (let* ((skip-whitespace t))
          (loop
            (let* ((current-line (mark-line m))
                   (current-charpos (mark-charpos m)))
              (when (and (eq current-line end-line)
                         (eql current-charpos end-charpos))
                (return t))
              (if skip-whitespace
                (progn
                  (scan-char m :whitespace nil)
                  (setq skip-whitespace nil))
                (progn
                  (pre-command-parse-check m)
                  (unless (or (form-offset m 1)
                              (skip-line-comment m))
                    (return nil))
                  (setq skip-whitespace t))))))))))
               
#| old version
(defcommand "Confirm Listener Input" (p)
  "Evaluate Listener Mode input between point and last prompt."
  (declare (ignore p))
  (let* ((input-region (get-interactive-input))
         (r (if input-region
              (region (copy-mark (region-start input-region))
                      (copy-mark (region-end input-region) :right-inserting)))))

    (when input-region
      (insert-character (current-point-for-insertion) #\NewLine)
      (when (or (input-stream-reading-line
                 (top-listener-input-stream))
                (balanced-expressions-in-region input-region))
        (let* ((string (region-to-string input-region)))
          (push (cons r nil) (value input-regions))
          (move-mark (value buffer-input-mark) (current-point))
          (append-font-regions (current-buffer))
          (hemlock-ext:send-string-to-listener (current-buffer) string))))))
|#

(defun point-at-prompt-p ()
  (with-mark ((input-mark (value buffer-input-mark))
              (end-mark (value buffer-input-mark)))
    (buffer-end end-mark)
    (and (mark>= (current-point) input-mark)
         (mark>= end-mark (current-point)))))

(defun send-input-region-to-lisp ()
  (let* ((input-mark (value buffer-input-mark))
         (end-mark (region-end (buffer-region (current-buffer))))
         (input-region (region input-mark end-mark))
         (r (if input-region
                (region (copy-mark (region-start input-region))
                        (copy-mark (region-end input-region) :right-inserting)))))
    (when input-region
      (if (or (input-stream-reading-line
               (top-listener-input-stream))
              (balanced-expressions-in-region input-region))
        ;; complete expression: send it to lisp
        (let* ((string (region-to-string input-region))
               (ring (value interactive-history)))
          (when (and (or (zerop (ring-length ring))
                         (string/= string (region-to-string (ring-ref ring 0))))
                     (> (length string) (value minimum-interactive-input-length)))
            (ring-push (copy-region input-region) ring))
          (insert-character (region-end input-region) #\NewLine)
          (push (cons r nil) (value input-regions))
          (set-charprop-value (region-start input-region) :font-weight :bold
                              :end (region-end input-region))
          (move-mark (value buffer-input-mark) (current-point))
          (append-font-regions (current-buffer))
          (hemlock-ext:send-string-to-listener (current-buffer) (concatenate 'string string '(#\Newline)))
          (buffer-end (current-point)))
        ;; incomplete expression: enter a newline
        (progn
          (insert-character (current-point-for-insertion) #\NewLine))))))

(defun copy-region-to-input (region)
  (let* ((region-string (when region (region-to-string region)))
         (input-mark (value buffer-input-mark))
         (end-mark (region-end (buffer-region (current-buffer))))
         (input-region (region input-mark end-mark)))
    (with-mark ((input-mark (value buffer-input-mark)))
      (move-mark (current-point) input-mark)
      (delete-region input-region)
      (insert-string (current-point) region-string)
      (buffer-end (current-point)))))

(defun find-backward-form (mark)
  (let ((start (copy-mark mark))
        (end (copy-mark mark)))
    (block finding
      (or (form-offset start -1) (return-from finding nil))
      (or (form-offset end -1) (return-from finding nil))
      (or (form-offset end 1) (return-from finding nil))
      (region start end))))

(defun find-forward-form (mark)
  (let ((start (copy-mark mark))
        (end (copy-mark mark)))
    (block finding
      (or (form-offset start 1) (return-from finding nil))
      (or (form-offset end 1) (return-from finding nil))
      (or (form-offset start -1) (return-from finding nil))
      (region start end))))

(defun region= (r1 r2)
  (multiple-value-bind (r1-start r1-end)(region-bounds r1)
    (multiple-value-bind (r2-start r2-end)(region-bounds r2)
      (and (mark= r1-start r2-start)
           (mark= r1-end r2-end)))))

;;; find the start or end of the nearest lisp form. return a region if
;;; one is found, nil otherwise. try for a commonsense result.
(defun mark-nearest-form (mark)
  (let* ((backward-region (find-backward-form mark))
         (forward-region (find-forward-form mark)))
    (if backward-region
        (if forward-region
            ;; if we're in the middle of a token, then the backward
            ;; and forward regions will be the same
            (if (region= backward-region forward-region)
                backward-region
                ;; not equal, so figure out which is closer
                (let* ((mark-pos (mark-absolute-position mark))
                       (backward-dist (abs (- mark-pos (mark-absolute-position (region-end backward-region)))))
                       (forward-dist (abs (- (mark-absolute-position (region-start forward-region)) mark-pos))))
                  (if (< forward-dist backward-dist)
                      forward-region
                      backward-region)))
            backward-region)
        forward-region)))

(defun copy-expression-at-point-to-input ()
  (let* ((nearest-form-region (mark-nearest-form (current-point))))
    (if nearest-form-region
        (copy-region-to-input nearest-form-region)
        (beep))))

(defcommand "Confirm Listener Input" (p)
    "Evaluate Listener Mode input between point and last prompt."
  (declare (ignore p))
  (if (point-at-prompt-p)
    (progn
      (if (eq (character-attribute :lisp-syntax (previous-character (buffer-end-mark (current-buffer)))) :char-quote)
        (let* ((point (current-point))) 
          (buffer-end point)
          (insert-character point #\newline))
        (send-input-region-to-lisp)))
      (if (region-active-p)
          (let ((selected-region (current-region nil nil)))
            (copy-region-to-input selected-region))
          (let ((prior-region (input-region-containing-mark (current-point) (value input-regions))))
            (if prior-region
                (copy-region-to-input prior-region)
                (copy-expression-at-point-to-input))))))


(defparameter *pop-string* ":POP
" "what you have to type to exit a break loop")

(defcommand "POP or Delete Forward" (p)
  "Send :POP if input-mark is at buffer's end, else delete forward character."
  (let* ((input-mark (value buffer-input-mark))
         (point (current-point-for-deletion)))
    (when point
      (if (and (null (next-character point))
	       (null (next-character input-mark)))
        (hemlock-ext:send-string-to-listener (current-buffer) *pop-string*)
        (delete-next-character-command p)))))


;;;; General interactive commands used in eval and typescript buffers.

(defhvar "Interactive History Length"
  "This is the length used for the history ring in interactive buffers.
   It must be set before turning on the mode."
  :value 200)

(defun input-region-containing-mark (m history-list)
  (dolist (pair history-list)
    (let* ((actual (car pair))
           (start (region-start actual))
           (end (region-end actual)))
      (when (and (mark>= m start)
                 (mark<= m end))        ; sic: inclusive
        (return (or (cdr pair) (setf (cdr pair) (copy-region actual))))))))


(defun get-interactive-input ()
  "Tries to return a region.  When the point is not past the input mark, and
   the user has \"Unwedge Interactive Input Confirm\" set, the buffer is
   optionally fixed up, and nil is returned.  Otherwise, an editor error is
   signalled.  When a region is returned, the start is the current buffer's
   input mark, and the end is the current point moved to the end of the buffer."
  (let ((point (current-point))
        (mark (value buffer-input-mark)))
    (cond
      ((mark>= point mark)
       (buffer-end point)
       (let* ((input-region (region mark point))
              (string (region-to-string input-region))
              (ring (value interactive-history)))
         (when (and (or (zerop (ring-length ring))
                        (string/= string (region-to-string (ring-ref ring 0))))
                    (> (length string) (value minimum-interactive-input-length)))
           (ring-push (copy-region input-region) ring))
         input-region))
      (t
       (let* ((region (input-region-containing-mark point (value input-regions ))))
         (buffer-end point)
         (if region
             (progn
               (delete-region (region mark point))
               (insert-region point region))
             (beep))
         nil)))))


(defhvar "Minimum Interactive Input Length"
  "When the number of characters in an interactive buffer exceeds this value,
   it is pushed onto the interactive history, otherwise it is lost forever."
  :value 0)


(defvar *previous-input-search-string* "ignore")

(defvar *previous-input-search-pattern*
  ;; Give it a bogus string since you can't give it the empty string.
  (new-search-pattern :string-insensitive :forward "ignore"))

(defun get-previous-input-search-pattern (string)
  (if (string= *previous-input-search-string* string)
      *previous-input-search-pattern*
      (new-search-pattern :string-insensitive :forward 
			  (setf *previous-input-search-string* string)
			  *previous-input-search-pattern*)))

(defcommand "Search Previous Interactive Input" (p)
  "Search backward through the interactive history using the current input as
   a search string.  Consecutive invocations repeat the previous search."
  (declare (ignore p))
  (let* ((mark (value buffer-input-mark))
	 (ring (value interactive-history))
	 (point (current-point))
	 (just-invoked (eq (last-command-type) :searching-interactive-input)))
    (when (mark<= point mark)
      (editor-error "Point not past input mark."))
    (when (zerop (ring-length ring))
      (editor-error "No previous input in this buffer."))
    (unless just-invoked
      (get-previous-input-search-pattern (region-to-string (region mark point))))
    (let ((found-it (find-previous-input ring just-invoked)))
      (unless found-it 
	(editor-error "Couldn't find ~a." *previous-input-search-string*))
      (delete-region (region mark point))
      (insert-region point (ring-ref ring found-it))
      (setf (value searching-interactive-pointer) found-it))
  (setf (last-command-type) :searching-interactive-input)))

(defun find-previous-input (ring againp)
  (let ((ring-length (ring-length ring))
	(base (if againp
		  (+ (value searching-interactive-pointer) 1)
		  0)))
      (loop
	(when (= base ring-length)
	  (if againp
	      (setf base 0)
	      (return nil)))
	(with-mark ((m (region-start (ring-ref ring base))))
	  (when (find-pattern m *previous-input-search-pattern*)
	    (return base)))
	(incf base))))

(defcommand "Previous Interactive Input" (p)
  "Insert the previous input in an interactive mode (Listener or Typescript).
   If repeated, keep rotating the history.  With prefix argument, rotate
   that many times."
  "Pop the *interactive-history* at the point."
  (let* ((point (current-point))
	 (mark (value buffer-input-mark))
	 (ring (value interactive-history))
	 (length (ring-length ring))
	 (p (or p 1)))
    (when (or (mark< point mark) (zerop length)) (editor-error "Can't get command history"))
    (cond
     ((eq (last-command-type) :interactive-history)
      (let ((base (mod (+ (value interactive-pointer) p) length))
            (grab-start (value grabbed-input-start-mark)))
	(delete-region (region grab-start point))
	(insert-region grab-start (ring-ref ring base))
	(setf (value interactive-pointer) base)))
     (t
      (let ((base (mod (if (minusp p) p (1- p)) length))
            (region (copy-region (region mark (buffer-end-mark (current-buffer)))))
            (grab-start (move-mark (value grabbed-input-start-mark) point)))
        (when (mark/= (region-start region) (region-end region))
          (ring-push region ring)
          (incf base))
	(insert-region grab-start (ring-ref ring base))
	(setf (value interactive-pointer) base)))))
  (setf (last-command-type) :interactive-history))

(defcommand "Next Interactive Input" (p)
  "Rotate the interactive history backwards.  The region is left around the
   inserted text.  With prefix argument, rotate that many times."
  (previous-interactive-input-command (- (or p 1))))

(defcommand "Kill Interactive Input" (p)
  "Kill any input to an interactive mode (Listener or Typescript)."
  (declare (ignore p))
  (let ((point (buffer-point (current-buffer)))
	(mark (value buffer-input-mark)))
    (when (mark< point mark) (editor-error))
    (kill-region (region mark point) :kill-backward)))

(defcommand "Interactive Beginning of Line" (p)
  "If on line with current prompt, go to after it, otherwise do what
  \"Beginning of Line\" always does."
  "Go to after prompt when on prompt line."
  (let ((mark (value buffer-input-mark))
	(point (current-point)))
    (if (and (same-line-p point mark) (or (not p) (= p 1)))
	(move-mark point mark)
	(beginning-of-line-command p))))

(defcommand "Reenter Interactive Input" (p)
  "Copies the form to the left of point to be after the interactive buffer's
   input mark.  When the current region is active, it is copied instead."
  (declare (ignore p))
  (unless (hemlock-bound-p 'buffer-input-mark)
    (editor-error "Not in an interactive buffer."))
  (let ((point (current-point)))
    (let ((region (if (region-active-p)
		      ;; Copy this, so moving point doesn't affect the region.
		      (copy-region (current-region))
		      (with-mark ((start point)
				  (end point))
			(pre-command-parse-check start)
			(unless (form-offset start -1)
			  (editor-error "Not after complete form."))
			(region (copy-mark start) (copy-mark end))))))
      (buffer-end point)
      (push-new-buffer-mark point)
      (insert-region point region)
      (setf (last-command-type) :ephemerally-active))))



;;; Other stuff.

(defmode "Editor" :hidden t)

(defcommand "Editor Mode" (p)
  "Toggle \"Editor\" mode in the current buffer.  
  When in editor mode, most lisp compilation and evaluation commands
  manipulate the editor process instead of the current eval server."
  (declare (ignore p))
  (setf (buffer-minor-mode (current-buffer) "Editor")
	(not (buffer-minor-mode (current-buffer) "Editor"))))

(define-file-option "Editor" (buffer value)
  (declare (ignore value))
  (setf (buffer-minor-mode buffer "Editor") t))



(defun defun-region (mark)
  "This returns a region around the current or next defun with respect to mark.
   Mark is not used to form the region.  If there is no appropriate top level
   form, this signals an editor-error.  This calls PRE-COMMAND-PARSE-CHECK."
  (with-mark ((start mark)
	      (end mark))
    (pre-command-parse-check start)
    (cond ((not (mark-top-level-form start end))
	   (editor-error "No current or next top level form."))
	  (t (region start end)))))

(defun current-form-region (&optional (error t))
  (if (region-active-p)
    (current-region)
    (let ((point (current-point)))
      (pre-command-parse-check point)
      (or (form-region-at-mark point)
          (and error (editor-error "No current expression"))))))

(defun eval-region (region
		    &key
		    (package (variable-value 'current-package :buffer (current-buffer)))
		    (path (buffer-pathname (current-buffer))))
  (ccl::application-ui-operation ccl:*application*
                                 :eval-selection
                                 (list package
                                       path
                                       (region-to-string region)
                                       (mark-absolute-position (region-start region)))))


(defcommand "Editor Execute Defun" (p)
  "Executes the current or next top-level form in the editor Lisp."
  (declare (ignore p))
  (if (region-active-p)
    (eval-region (current-region))
    (eval-region (defun-region (current-point)))))

(defcommand "Editor Execute Expression" (p)
  "Executes the current region in the editor Lisp."
  (declare (ignore p))
  (eval-region (current-form-region)))

(defcommand "Editor Re-evaluate Defvar" (p)
  "Evaluate the current or next top-level form if it is a DEFVAR.  Treat the
   form as if the variable is not bound."
  (declare (ignore p))
  (with-input-from-region (stream (defun-region (current-point)))
    (clear-echo-area)
    (in-lisp
     (let ((form (read stream)))
       (unless (eq (car form) 'defvar) (editor-error "Not a DEFVAR."))
       (makunbound (cadr form))
       (message "Evaluation returned ~S" (eval form))))))

(defun macroexpand-expression (expander)
  (in-lisp
   (let* ((region (current-form-region))
          (expr (with-input-from-region (s region)
                  (read s))))
     (let* ((*print-pretty* t)
            (expansion (funcall expander expr)))
       (format t "~&~s~&" expansion)))))

(defcommand "Editor Macroexpand-1 Expression" (p)
  "Show the macroexpansion of the current expression in the null environment.
   With an argument, use MACROEXPAND instead of MACROEXPAND-1."
  (macroexpand-expression (if p 'macroexpand 'macroexpand-1)))

(defcommand "Editor Macroexpand Expression" (p)
  "Show the macroexpansion of the current expression in the null environment.
   With an argument, use MACROEXPAND-1 instead of MACROEXPAND."
  (macroexpand-expression (if p 'macroexpand-1 'macroexpand)))


(defcommand "Editor Evaluate Expression" (p)
  "Prompt for an expression to evaluate in the editor Lisp."
  (declare (ignore p))
  (in-lisp
   (multiple-value-call #'message "=> ~@{~#[~;~S~:;~S, ~]~}"
     (eval (prompt-for-expression
	    :prompt "Editor Eval: "
	    :help "Expression to evaluate")))))

(defcommand "Editor Evaluate Buffer" (p)
  "Evaluates the text in the current buffer in the editor Lisp."
  (declare (ignore p))
  (message "Evaluating buffer in the editor ...")
  (with-input-from-region (stream (buffer-region (current-buffer)))
    (in-lisp
     (do ((object (read stream nil lispbuf-eof) 
                  (read stream nil lispbuf-eof)))
         ((eq object lispbuf-eof))
       (eval object)))
    (message "Evaluation complete.")))



(defcommand "Editor Compile File" (p)
  "Prompts for file to compile in the editor Lisp.  Does not compare source
   and binary write dates.  Does not check any buffer for that file for
   whether the buffer needs to be saved."
  (declare (ignore p))
  (let ((pn (prompt-for-file :default
			     (buffer-default-pathname (current-buffer))
			     :prompt "File to compile: ")))
    (in-lisp (compile-file (namestring pn) #+cmu :error-file #+cmu nil))))


(defun older-or-non-existent-fasl-p (pathname &optional definitely)
  (let ((obj-pn (probe-file (compile-file-pathname pathname))))
    (or definitely
	(not obj-pn)
	(< (file-write-date obj-pn) (file-write-date pathname)))))


(defcommand "Editor Compile Buffer File" (p)
  "Compile the file in the current buffer in the editor Lisp if its associated
   binary file (of type .fasl) is older than the source or doesn't exist.  When
   the binary file is up to date, the user is asked if the source should be
   compiled anyway.  When the prefix argument is supplied, compile the file
   without checking the binary file.  When \"Compile Buffer File Confirm\" is
   set, this command will ask for confirmation when it otherwise would not."
  "Compile the file in the current buffer in the editor Lisp if the fasl file
   isn't up to date.  When p, always do it."
  (let* ((buf (current-buffer))
	 (pn (buffer-pathname buf)))
    (unless pn (editor-error "Buffer has no associated pathname."))
    (cond ((buffer-modified buf)
	   (when (or (not (value compile-buffer-file-confirm))
		     (prompt-for-y-or-n
		      :default t :default-string "Y"
		      :prompt (list "Save and compile file ~A? "
				    (namestring pn))))
	     (write-buffer-file buf pn)
	     (in-lisp (compile-file (namestring pn) #+cmu :error-file #+cmu nil))))
	  ((older-or-non-existent-fasl-p pn p)
	   (when (or (not (value compile-buffer-file-confirm))
		     (prompt-for-y-or-n
		      :default t :default-string "Y"
		      :prompt (list "Compile file ~A? " (namestring pn))))
	     (in-lisp (compile-file (namestring pn) #+cmu :error-file #+cmu nil))))
	  (t (when (or p
		       (prompt-for-y-or-n
			:default t :default-string "Y"
			:prompt
			"Fasl file up to date, compile source anyway? "))
	       (in-lisp (compile-file (namestring pn) #+cmu :error-file #+cmu nil)))))))







;;;; Lisp documentation stuff.

;;; FUNCTION-TO-DESCRIBE is used in "Editor Describe Function Call" and
;;; "Describe Function Call".
;;;
(defmacro function-to-describe (var error-name)
  `(cond ((not (symbolp ,var))
	  (,error-name "~S is not a symbol." ,var))
         ((special-operator-p ,var) ,var)
	 ((macro-function ,var))
	 ((fboundp ,var))
	 (t
	  (,error-name "~S is not a function." ,var))))

(defcommand "Editor Describe Function Call" (p)
  "Describe the most recently typed function name in the editor Lisp."
  (declare (ignore p))
  (with-mark ((mark1 (current-point))
	      (mark2 (current-point)))
    (pre-command-parse-check mark1)
    (unless (backward-up-list mark1) (editor-error))
    (form-offset (move-mark mark2 (mark-after mark1)) 1)
    (with-input-from-region (s (region mark1 mark2))
      (in-lisp
       (let* ((sym (read s))
	      (fun (function-to-describe sym editor-error)))
	 (with-pop-up-display (*standard-output* :title (format nil "~s" sym))
	   (editor-describe-function fun sym)))))))


(defcommand "Editor Describe Symbol" (p)
  "Describe the previous s-expression if it is a symbol in the editor Lisp."
  (declare (ignore p))
  (with-mark ((mark1 (current-point))
	      (mark2 (current-point)))
    (mark-symbol mark1 mark2)
    (with-input-from-region (s (region mark1 mark2))
      (let ((thing (in-lisp (read s))))
        (if (symbolp thing)
          (with-pop-up-display (*standard-output* :title (format nil "~s" thing))
            (describe thing))
          (if (and (consp thing)
                   (or (eq (car thing) 'quote)
                       (eq (car thing) 'function))
                   (symbolp (cadr thing)))
            (with-pop-up-display (*standard-output* :title (format nil "~s" (cadr thing)))
              (describe (cadr thing)))
            (editor-error "~S is not a symbol, or 'symbol, or #'symbol."
                          thing)))))))

;;; MARK-SYMBOL moves mark1 and mark2 around the previous or current symbol.
;;; However, if the marks are immediately before the first constituent char
;;; of the symbol name, we use the next symbol since the marks probably
;;; correspond to the point, and Hemlock's cursor display makes it look like
;;; the point is within the symbol name.  This also tries to ignore :prefix
;;; characters such as quotes, commas, etc.
;;;
(defun mark-symbol (mark1 mark2)
  (pre-command-parse-check mark1)
  (with-mark ((tmark1 mark1)
	      (tmark2 mark1))
    (cond ((and (form-offset tmark1 1)
		(form-offset (move-mark tmark2 tmark1) -1)
		(or (mark= mark1 tmark2)
		    (and (find-attribute tmark2 :lisp-syntax
					 #'(lambda (x) (not (eq x :prefix))))
			 (mark= mark1 tmark2))))
	   (form-offset mark2 1))
	  (t
	   (form-offset mark1 -1)
	   (find-attribute mark1 :lisp-syntax
			   #'(lambda (x) (not (eq x :prefix))))
	   (form-offset (move-mark mark2 mark1) 1)))))


(defcommand "Editor Describe" (p)
  "Call Describe on a Lisp object.
  Prompt for an expression which is evaluated to yield the object."
  (declare (ignore p))
  (in-lisp
   (let* ((exp (prompt-for-expression
		:prompt "Object: "
		:help "Expression to evaluate to get object to describe."))
	  (obj (eval exp)))
     (with-pop-up-display (*standard-output* :title (format nil "~s" exp))
       (describe obj)))))

(defcommand "Filter Region" (p)
  "Apply a Lisp function to each line of the region.
  An expression is prompted for which should evaluate to a Lisp function
  from a string to a string.  The function must neither modify its argument
  nor modify the return value after it is returned."
  "Call prompt for a function, then call Filter-Region with it and the region."
  (declare (ignore p))
  (let* ((exp (prompt-for-expression
	       :prompt "Function: "
	       :help "Expression to evaluate to get function to use as filter."))
	 (fun (in-lisp (eval exp)))
	 (region (current-region)))
    (let* ((start (copy-mark (region-start region) :left-inserting))
	   (end (copy-mark (region-end region) :left-inserting))
	   (region (region start end))
	   (undo-region (copy-region region)))
      (filter-region fun region)
      (make-region-undo :twiddle "Filter Region" region undo-region))))
