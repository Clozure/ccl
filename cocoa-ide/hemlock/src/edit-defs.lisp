;;; -*- Log: hemlock.log; Package: hemlock -*-
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
;;; Editing DEFMACRO and DEFUN definitions.  Also, has directory translation
;;; code for moved and/or different sources.
;;;

(in-package :hemlock)


;;; Definition Editing Commands.



;;; For the "Go to Definition" search pattern, we just use " " as the initial
;;; pattern, so we can make a search pattern.  Invocation of the command alters
;;; the search pattern.

(defvar *go-to-def-pattern*
  (new-search-pattern :string-insensitive :forward " "))

(defvar *last-go-to-def-string* "")
(declaim (simple-string *last-go-to-def-string*))
  
(defun symbol-at-point (buffer)
  "Returns symbol at point, or contents of selection if there is one"
  (let ((point (buffer-point buffer))
        (mark (buffer-mark buffer)))
    (if (and (hi::%buffer-current-region-p buffer)
             (not (mark= mark point)))
      (string-trim '(#\space #\tab)
                   (region-to-string (if (mark< mark point)
                                       (region mark point)
                                       (region point mark))))
      (symbol-at-mark point))))

(defun symbol-at-mark (mark)
  (with-mark ((mark1 mark)
              (mark2 mark))
    ;; This doesn't handle embedded #'s or escaped chars in names.
    ;; So let them report it as a bug...
    (when (test-char (previous-character mark) :lisp-syntax :constituent)
      (or (rev-scan-char mark1 :lisp-syntax (not :constituent))
          (buffer-start mark1))
      (scan-char mark1 :lisp-syntax :constituent))
    (when (test-char (next-character mark) :lisp-syntax :constituent)
      (or (scan-char mark2 :lisp-syntax (not :constituent))
          (buffer-end mark2)))
    (when (mark= mark1 mark2)
      ;; Try to get whole form
      (pre-command-parse-check mark)
      (move-mark mark1 mark)
      (form-offset mark1 -1)
      (move-mark mark2 mark1)
      (form-offset mark2 1))
    (loop until (or (mark= mark1 mark2) (not (eql (previous-character mark2) #\:)))
      do (mark-before mark2))
    (when (and (eql (previous-character mark1) #\#) (eql (next-character mark1) #\<))
      (mark-after mark1))
    (unless (mark= mark1 mark2)
      (region-to-string (region mark1 mark2)))))

(defcommand "Goto Definition" (p)
  "Go to the current function/macro's definition.  With a numarg, prompts for name to go to."
  (if p
      (edit-definition-command nil)
      (let* ((buffer (current-buffer))
	     (fun-name (symbol-at-point buffer)))
	(if fun-name
	    (get-def-info-and-go-to-it fun-name (or
						 (buffer-package buffer)
						 *package*))
	    (beep)))))

(defcommand "Edit Definition" (p)
  "Prompts for function/macro's definition name and goes to it for editing."
  (declare (ignore p))
  (let ((fun-name (prompt-for-string
		   :prompt "Name: "
		   :help "Symbol name of function.")))
    (get-def-info-and-go-to-it fun-name (or
                                         (buffer-package (current-buffer))
                                         *package*))))

(defun get-def-info-and-go-to-it (string package)
  (multiple-value-bind (fun-name error)
      (let* ((*package* (ccl:require-type package 'package)))
        (ignore-errors (values (read-from-string string))))
    (if error
      (editor-error "unreadable name: ~s" string)
      (handler-case (edit-definition fun-name)
        (error (c) (editor-error "~a" c))))))

(defcommand "Edit Command Definition" (p)
  "Prompts for command definition name and goes to it for editing."
  (multiple-value-bind
      (name command)
      (if p
        (multiple-value-bind (key cmd)
                             (prompt-for-key :prompt "Edit command bound to: "
                                             :must-exist t)
          (declare (ignore key))
          (values (command-name cmd) cmd))
        (prompt-for-keyword :tables (list *command-names*)
                            :prompt "Command to edit: "))
    (declare (ignore name))
    (handler-case (edit-definition (command-function command))
      (error (c) (editor-error "~a" c)))))

#|
;;; FUN-DEFINED-FROM-PATHNAME takes a symbol or function object.  It
;;; returns a pathname for the file the function was defined in.  If it was
;;; not defined in some file, then nil is returned.
;;; 
(defun fun-defined-from-pathname (function)
  "Takes a symbol or function and returns the pathname for the file the
   function was defined in.  If it was not defined in some file, nil is
   returned."
  (flet ((true-namestring (path) (namestring (truename path))))
    (typecase function
      (function (fun-defined-from-pathname (ccl:function-name function)))
      (symbol (let* ((info (ccl::%source-files function)))
                (if (atom info)
                  (true-namestring info)
                  (let* ((finfo (assq 'function info)))
                    (when finfo
                      (true-namestring
                       (if (atom finfo)
                         finfo
                         (car finfo)))))))))))

;;; GO-TO-DEFINITION tries to find name in file with a search pattern based
;;; on type (defun or defmacro).  File may be translated to another source
;;; file, and if type is a function that cannot be found, we try to find a
;;; command by an appropriate name.
;;; 
(defun go-to-definition (file type name)
  (let ((pattern (get-definition-pattern type name)))
    (cond
     (file
      (setf file (go-to-definition-file file))
      (let* ((buffer (old-find-file-command nil file))
	     (point (buffer-point buffer))
	     (name-len (length name)))
	(declare (fixnum name-len))
	(with-mark ((def-mark point))
	  (buffer-start def-mark)
	  (unless (find-pattern def-mark pattern)
	    (if (and (or (eq type :function) (eq type :unknown-function))
		     (> name-len 7)
		     (string= name "COMMAND" :start1 (- name-len 7)))
		(let ((prev-search-str *last-go-to-def-string*))
		  (unless (find-pattern def-mark
					(get-definition-pattern :command name))
		    (editor-error "~A is not defined with ~S or ~S, ~
				   but this is the defined-in file."
				  (string-upcase name) prev-search-str
				  *last-go-to-def-string*)))
		(editor-error "~A is not defined with ~S, ~
			       but this is the defined-in file."
			      (string-upcase name) *last-go-to-def-string*)))
	  (if (eq buffer (current-buffer))
	      (push-new-buffer-mark point))
	  (move-mark point def-mark))))
     (t
      (when (or (eq type :unknown-function) (eq type :unknown-macro))
	(with-mark ((m (buffer-start-mark (current-buffer))))
	  (unless (find-pattern m pattern)
	    (editor-error
	     "~A is not compiled and not defined in current buffer with ~S"
	     (string-upcase name) *last-go-to-def-string*))
	  (let ((point (current-point)))
	    (push-new-buffer-mark point)
	    (move-mark point m))))))))
|#

(defparameter *type-defining-operators* ())

(defun define-type-defining-operators (name &rest operators)
  (assert (subtypep name 'ccl::definition-type))
  (let ((a (assoc name *type-defining-operators*)))
    (when (null a)
      (push (setq a (cons name nil)) *type-defining-operators*))
    (loop for op in operators do (pushnew op (cdr a)))
    name))

(defun type-defining-operator-p (def-type operator)
  (loop for (type . ops) in *type-defining-operators*
    thereis (and (typep def-type type) (memq operator ops))))

(define-type-defining-operators 'ccl::class-definition-type 'defclass)
(define-type-defining-operators 'ccl::type-definition-type 'deftype)
(define-type-defining-operators 'ccl::function-definition-type 'defun 'defmacro 'defgeneric #+x8664-target 'ccl::defx86lapfunction #+ppc-target 'ccl::defppclapfunction)
(define-type-defining-operators 'ccl::constant-definition-type 'defconstant)
(define-type-defining-operators 'ccl::variable-definition-type 'defvar 'defparameter 'ccl::defstatic 'ccl::defglobal)
(define-type-defining-operators 'ccl::method-combination-definition-type 'define-method-combination)
(define-type-defining-operators 'ccl::compiler-macro-definition-type 'define-compiler-macro)


(defun match-definition-context-for-method (end-mark package indicator)
  (let* ((specializers (openmcl-mop:method-specializers indicator))
         (qualifiers (openmcl-mop:method-qualifiers indicator)))
    (block win
      (with-mark ((work end-mark))
        (when qualifiers
          (dotimes (i (length qualifiers))
            (unless (and (form-offset end-mark 1)
                         (progn
                           (move-mark work end-mark)
                           (form-offset work -1)))
              (return-from win nil))
            (let* ((qualifier (ignore-errors
                                (let* ((*package* package))
                                  (values
                                   (read-from-string (region-to-string
                                                      (region
                                                       work
                                                       end-mark))))))))
              (unless (member qualifier qualifiers)
                (return-from win nil)))))
        ;; end-mark is now either at end of last qualifier or
        ;; after method name.  Try to read the lambda list and
        ;; match specializers.
        (unless (and (form-offset end-mark 1)
                     (progn
                       (move-mark work end-mark)
                       (form-offset work -1)))
          (return-from win nil))
        (multiple-value-bind (lambda-list error)
            (ignore-errors
              (let* ((*package* package))
                (values
                 (read-from-string (region-to-string
                                    (region
                                     work
                                     end-mark))))))
          (unless (and (null error)
                       (consp lambda-list)
                       (ccl::proper-list-p lambda-list))
            (return-from win nil))
          (flet ((match-specializer (spec)
                   (when lambda-list
                     (let* ((arg (pop lambda-list)))
                       (typecase spec
                         (ccl::eql-specializer
                          (let* ((obj (openmcl-mop:eql-specializer-object spec)))
                            (and (ccl::proper-list-p arg)
                                 (= 2 (length arg))
                                 (symbolp (pop arg))
                                 (ccl::proper-list-p (setq arg (car arg)))
                                 (= (length arg) 2)
                                 (eq (car arg) 'eql)
                                 (eql (cadr arg) obj))))
                         (class
                          (let* ((name (class-name spec)))
                            (or (and (eq name t) (symbolp arg))
                                (and (consp arg)
                                     (symbolp (car arg))
                                     (consp (cdr arg))
                                     (null (cddr arg))
                                     (eq name (cadr arg)))))))))))
            (dolist (spec specializers t)
              (unless (match-specializer spec)
                (return nil)))))))))
                                 
;;; START and END delimit a function name that matches what we're looking for
(defun match-context-for-indicator (start end def-type full-name)
  (with-mark ((op-start start)
              (op-end start))
    (and (form-offset op-start -1)
         (progn
           (move-mark op-end op-start)
           (form-offset op-end 1))
         (let* ((package (or (find-package (variable-value 'current-package :buffer (current-buffer)))
                             *package*))
                (defining-operator
                    (ignore-errors
                      (let* ((*package* package))
                        (values (read-from-string (region-to-string (region op-start op-end))))))))
           (and (type-defining-operator-p def-type defining-operator)
                (or (not (typep full-name 'method))
                    (match-definition-context-for-method end package full-name)))))))

(defun match-definition-context (mark def-type full-name)
  (pre-command-parse-check mark)
  (when (valid-spot mark t)
    (with-mark ((start mark)
                (end mark))
      (and (form-offset end 1)
           (progn
             (move-mark start end)
             (form-offset start -1))
           (let ((package (or (find-package (variable-value 'current-package :buffer (current-buffer)))
                              *package*)))
             (eq (ccl::definition-base-name def-type full-name)
                 (ignore-errors
                  (let* ((*package* package))
                    (values (read-from-string (region-to-string (region start end))))))))
           (match-context-for-indicator start end def-type full-name)))))

(defun find-definition-by-context (def-type full-name)
  (let* ((base-name (ccl::definition-base-name def-type full-name))
	 (string (string base-name))
         (pattern (new-search-pattern :string-insensitive :forward string))
         (found 0))
    (with-mark ((mark (buffer-start-mark (current-buffer))))
      (when (or (loop
                  while (and (find-pattern mark pattern) (incf found))
                  thereis (and (match-definition-context mark def-type full-name)
                               (backward-up-list mark))
                  do (character-offset mark 1))
                ;; if there is only one instance, just go there
                (and (eql found 1) (find-pattern (buffer-start mark) pattern))
                ;; Else should try again, being less strict...
                )
        (move-point-leaving-mark mark)))))

(defun move-point-leaving-mark (target)
  (let ((point (current-point-collapsing-selection)))
    (push-new-buffer-mark point)
    (move-mark point target)
    point))

;;; Adjust for CRLF line termination.  Multibyte character encodings
;;; can also cause discrepancies between physical/logical positions.
;;; Handling that would require making the source location stuff
;;; aware of that newfangled Unicode thing ...
(defun byte-position-to-character-position (pos &optional (buffer (current-buffer)))
  (let* ((line-termination (hi::buffer-line-termination buffer)))
    (if (eq line-termination :crlf)
      (- pos (hi::buffer-lines-before-absolute-position buffer pos))
      pos)))

(defun move-to-source-note (source)
  (let ((start-pos (ccl:source-note-start-pos source)))
    (when start-pos
      (setq start-pos (byte-position-to-character-position start-pos))
      (let ((full-text (ccl:source-note-text source))
            (pattern nil)
            (offset 0))
        (flet ((ssearch (mark string direction)
                 (find-pattern mark
                               (setq pattern (new-search-pattern :string-insensitive
                                                                 direction
                                                                 string
                                                                 pattern)))))
          (declare (inline ssearch))
          (with-mark ((temp-mark (current-point)))
            (unless full-text
              ;; Someday, might only store a snippet for toplevel, so inner notes
              ;; might not have text, but can still find them through the toplevel.
              (let* ((toplevel (ccl::source-note-toplevel-note source))
                     (toplevel-start-pos (and (not (eq toplevel source))
                                              (ccl:source-note-start-pos toplevel)))
                     (text (and toplevel-start-pos (ccl:source-note-text toplevel))))
                (when text
                  (setq toplevel-start-pos (byte-position-to-character-position toplevel-start-pos))
                  (setq offset (- start-pos toplevel-start-pos))
                  (setq start-pos toplevel-start-pos)
                  (setq full-text text)
                  (character-offset temp-mark (- offset)))))
            (unless (move-to-absolute-position temp-mark start-pos)
              (buffer-end temp-mark))

            (when (or (null full-text)
                      (or (ssearch temp-mark full-text :forward)
                          (ssearch temp-mark full-text :backward))
                      ;; Maybe body changed, try at least to match the start of it
                      (let ((snippet (and (> (length full-text) 60) (subseq full-text 0 60))))
                        (and snippet
                             (or (ssearch temp-mark snippet :forward)
                                 (ssearch temp-mark snippet :backward)))))
              (let ((point (move-point-leaving-mark temp-mark)))
                (or (character-offset point offset)
                    (buffer-end point))))))))))

(defun find-definition-in-buffer (def-type full-name source)
  (or (and (ccl:source-note-p source)
           (move-to-source-note source))
      (find-definition-by-context def-type full-name)
      (editor-error "Couldn't find definition for ~s" full-name)))

;; Note this isn't necessarily called from hemlock, e.g. it might be called by cl:ed,
;; from any thread, or it might be called from a sequence dialog, etc.
(defun edit-definition (name)
  (flet ((get-source-alist (name)
           (let ((list (ccl:find-definition-sources name t)))
             ;; filter interactive-only defs
             (loop for (id . sources) in list as source = (find-if-not #'null sources)
               when source collect (cons id source))))
         (defn-name (defn stream)
           (destructuring-bind (dt . full-name) (car defn)
             (format stream "~s ~s" (ccl:definition-type-name dt) (ccl:name-of full-name))))
         (defn-action (defn &optional msg)
           (destructuring-bind ((def-type . full-name) . source) defn
             (hemlock-ext:execute-in-file-view
              (ccl:source-note-filename source)
              (lambda ()
                (when msg (loud-message msg))
                (find-definition-in-buffer def-type full-name source))))))
    (let* ((info (get-source-alist name))
           (msg nil))
      (when (and (null info) (symbolp name))
        (let* ((seen (list name))
               (found ())
               (pname (symbol-name name)))
          (dolist (pkg (list-all-packages))
            (let ((sym (find-symbol pname pkg)))
              (when (and sym (not (member sym seen :test 'eq)))
                (let ((new (get-source-alist sym)))
                  (when new
                    (setq info (nconc new info))
                    (push sym found)))
                (push sym seen))))
          (when found
            (setq msg (format nil "No definitions for ~s, found ~s instead"
                              name (if (cdr found) found (car found)))))))
      (if info
        (if (cdr info)
          (progn
            (when msg (loud-message msg))
            (hemlock-ext:open-sequence-dialog
             :title (format nil "Definitions of ~s" name)
             :sequence info
             :action #'defn-action
             :printer #'defn-name))
          (defn-action (car info) msg))
        (editor-error "No known definitions for ~s" name)))))

