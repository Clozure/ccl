;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates

(in-package "GUI")

(defvar *grep-program* "grep")

(defun cocoa-edit-grep-line (file line-num)
  (assume-cocoa-thread)
  (let ((view (find-or-make-hemlock-view file)))
    (hi::handle-hemlock-event view #'(lambda ()
                                       (edit-grep-line-in-buffer line-num)))))

(defun edit-grep-line-in-buffer (line-num)
  (let ((point (hi::current-point-collapsing-selection)))
    (hi::buffer-start point)
    (unless (hi::line-offset point line-num)
      (hi::buffer-end point))))

(defun parse-grep-line (line)
  (let* ((pos1 (position #\: line))
	 (pos2 (and pos1 (position #\: line :start (1+ pos1))))
	 (num (and pos2 (ignore-errors
			 (parse-integer line :start (1+ pos1) :end pos2
					:junk-allowed nil))))
	 (file (and num (subseq line 0 pos1))))
    (when file
      (values file (1- num)))))
  
(defun request-edit-grep-line (line)
  (multiple-value-bind (file line-num) (parse-grep-line line)
    (when file
      (execute-in-gui #'(lambda ()
                          (cocoa-edit-grep-line file line-num))))))

(defun grep-comment-line-p (line)
  (multiple-value-bind (file line-num) (parse-grep-line line)
    #+gz (when (member "archive" (pathname-directory file) :test #'equalp)
	   (return-from grep-comment-line-p t))
    (with-open-file (stream file)
      (loop while (> line-num 0)
	for ch = (read-char stream nil nil)
	when (null ch) do (return nil)
	do (when (member ch '(#\Return #\Linefeed))
	     (decf line-num)
	     (when (and (eql ch #\Return)
			(eql (peek-char nil stream nil nil) #\Linefeed))
	       (read-char stream))))
      (when (eql line-num 0)
	(loop as ch = (read-char stream nil nil)
	  while (and ch (whitespacep ch) (not (member ch '(#\Return #\Linefeed))))
	  finally (return (eql ch #\;)))))))

(defun grep-remove-comment-lines (lines)
  (remove-if #'grep-comment-line-p lines))

(defun split-grep-lines (output)
  (loop with end = (length output)
    for start = 0 then (1+ pos)
    as pos = (or (position #\Newline output :start start :end end) end)
    when (< start pos) collect (subseq output start pos)
    while (< pos end)))

(defvar *grep-ignore-case* t)
(defvar *grep-include-pattern* "*.lisp")
(defvar *grep-exclude-pattern* "*~.lisp")

(defun grep (pattern directory &key (ignore-case *grep-ignore-case*)
		                    (include *grep-include-pattern*)
				    (exclude *grep-exclude-pattern*))
  (with-output-to-string (stream)
    (let* ((proc (run-program *grep-program*
			      (nconc (and include (list "--include" include))
				     (and exclude (list "--exclude" exclude))
				     (and ignore-case (list "--ignore-case"))
				     (list "--recursive"
					   "--with-filename"
					   "--line-number"
                                           "--no-messages"
					   "-e" pattern
					   (ccl::native-untranslated-namestring directory)))
			      :input nil
			      :output stream)))
      (multiple-value-bind (status exit-code) (external-process-status proc)
	(let ((output (get-output-stream-string stream)))
	  (if (and (eq :exited status) (or (= exit-code 0) (= exit-code 2)))
	      (let ((lines (split-grep-lines output)))
		(unless (hi:value hemlock::grep-search-comments)
		  (setq lines (grep-remove-comment-lines lines)))
		(make-instance 'sequence-window-controller
			       :sequence lines
			       :result-callback #'request-edit-grep-line
			       :display #'princ
			       :title (format nil "~a in ~a" pattern directory)))
	      (hi:editor-error "Error in grep status ~s code ~s: ~a" status exit-code output)))))))


(hi:defhvar "Grep Directory"
  "The directory searched by \"Grep\".  NIL means to use the directory of the buffer."
  :value nil)

(hi:defhvar "Grep Search Comments"
  "If true (the default) grep will find results anywhere.  NIL means to ignore results
   within comments.  For now only recognizes as comments lines which start with semi-colon."
  :value t)

(hi:defcommand "Grep" (p)
  "Prompts for a pattern and invokes grep, searching recursively through .lisp
   files in \"Grep Directory\".
   With an argument, prompts for a directory to search, and sets \"Grep Directory\"
   for the next time."
  ""
  (let* ((default (make-pathname :name :unspecific
				 :type :unspecific
				 :defaults (or (hi:value hemlock::grep-directory)
					       (hi:buffer-pathname hi::*current-buffer*)
					       "ccl:")))
	 (directory (if p
			(setf (hi:value hemlock::grep-directory)
			      (hi:prompt-for-file :must-exist t
						  :default default
						  :default-string (namestring default)
						  :prompt "Directory: "))
			default))
	 (pattern (hi:prompt-for-string
		   :prompt "Pattern: "
		   :help "Pattern to search for")))
    (grep pattern directory)))
