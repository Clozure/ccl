;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2007 Clozure Associates

(in-package "GUI")

(defvar *grep-program* "grep")

(defun cocoa-edit-grep-line (file line-num &optional search-string)
  (assume-cocoa-thread)
  (let ((view (find-or-make-hemlock-view file)))
    (hi::handle-hemlock-event view #'(lambda ()
                                       (edit-grep-line-in-buffer line-num search-string)))))

(defun edit-grep-line-in-buffer (line-num search-string)
  (let ((point (hi::current-point-collapsing-selection)))
    (hi::buffer-start point)
    (if (hi::line-offset point line-num)
      (when search-string
        (setf hi::*last-search-string* search-string)
        (hemlock::start-isearch-mode :forward)
        (let ((iss (hi::value hemlock::i-search-state)))
          (hemlock::i-search-repeat iss)))
      (hi::buffer-end point))))

(defvar *grep-ignore-case* t)
(defvar *grep-include-pattern* "*.lisp")
(defvar *grep-exclude-pattern* "*.lisp~")


(defun grep (pattern directory)
  (let ((wc (make-instance 'search-files-window-controller)))
    (#/showWindow: wc +null-ptr+)
    (set-search-files-dir wc (namestring directory))
    (set-search-files-pattern wc pattern)
    (with-slots (expand-results-p expand-results-checkbox
                 case-sensitive-p case-sensitive-checkbox) wc
      (setf case-sensitive-p (not *grep-ignore-case*))
      (#/setState: case-sensitive-checkbox
                   (if (not *grep-ignore-case*)
                       #$NSOnState
                       #$NSOffState))
      (setf expand-results-p t)
      (#/setState: expand-results-checkbox #$NSOnState))
    (#/doSearch: wc +null-ptr+)))

(hi:defhvar "Grep Directory"
  "The directory searched by \"Grep\".  NIL means to use the directory of the buffer."
  :value nil)

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
