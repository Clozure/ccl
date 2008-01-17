(in-package "ELISP")

(cl:defun mangle-key (key)
  "Turn a CL-elisp key designator to a PHemlock KEY-EVENT"
  (typecase key
;    (string (with-input-from-string (stream key)
;	       (let ((*readtable* elisp-internals:*elisp-readtable*))
;		 (elisp-internals::read-string-char stream :event))))
    (string (map 'vector #'mangle-key key))
    ((or vector array)
     (map 'vector #'mangle-key key))
    (hemlock-ext:key-event key)
    ((or integer character)
     (multiple-value-bind (ismeta ischar) (truncate (if (characterp key)
							(char-code key)
						      key) 
						    128)
		 (cl:let ((charspec (if (cl:= 1 ismeta) (list :meta))))
		   (when (< ischar 32)
		       (push :control charspec)
		       (setq ischar (1- (+ ischar (char-code #\a)))))
		   (push (code-char ischar) charspec)
		   (elisp-internals::emit-character (reverse charspec) :event)
		   )))))

(cl:defun global-set-key (key command)
  (let ((key (mangle-key key)))
    (bind-key (string command) key :global)))

(cl:defun local-set-key (key command)
  (let ((key (mangle-key key)))
    (bind-key (string command) key :mode major-mode)))

(cl:defun use-local-map (keymap)
  (cond ((and (listp keymap)
	      (eq (car keymap) 'keymap))
	 (cl:let ((has-menu-name (stringp (cadr keymap))))
	   (let ((char-table (if has-menu-name
				 (if (vectorp (caddr keymap))
				     (caddr keymap))
			       (if (vectorp (cadr keymap))
				     (cadr keymap))))
		 (the-alist (if has-menu-name
				(if (vectorp (caddr keymap))
				     (cdddr keymap))
			      (if (vectorp (cadr keymap))
				     (cddr keymap)))))
	     ; iterate through the relevant sections
	     )))
	((symbolp keymap)
	 (use-local-map (eval keymap)))))

(cl:defun get-buffer-create (buffer-name)
  (or (getstring buffer-name *buffer-names*)
      (make-buffer buffer-name)))

(cl:defun get-buffer (buffer-name)
   (getstring buffer-name *buffer-names*))

(cl:defun commandp (function-designator)
  (typecase function-designator
    (symbol (hemlock-internals:commandp (getstring (string-downcase (string function-designator)) hemlock-internals:*command-names*)))
    (function nil) ; Bug, but as far as I can tell, we can't portably
                   ; extract the name from the function object
    (string (hemlock-internals:commandp (getstring (string-downcase function-designator) hemlock-internals:*command-names*)))
    (t nil)))

(cl:defun bolp ()
  (= 0 (hemlock-internals:mark-charpos (hemlock-internals:current-point))))

(cl:defun bobp ()
  (and (= 0 (hemlock-internals::line-number (hemlock-internals:mark-line (hemlock-internals:current-point))))
       (bolp)))

(cl:defun abort-recursive-edit ()
  (and (hemlock-internals:in-recursive-edit)
       (hemlock-internals:exit-recursive-edit)))
