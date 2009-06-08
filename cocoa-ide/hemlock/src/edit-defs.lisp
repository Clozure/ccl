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
  
(defun symbol-at-point (buffer point)
  "Returns symbol at point, or contents of selection if there is one"
  (with-mark ((mark1 point)
	      (mark2 point))
    (if (hi::%buffer-current-region-p buffer)
	(let* ((mark (hi::buffer-%mark buffer)))
	  (if (mark< mark point)
              (move-mark mark1 mark)
              (move-mark mark2 mark)))
	;; This doesn't handle embedded #'s or escaped chars in names.
	;; So let them report it as a bug...
	(progn
	  (when (test-char (previous-character point) :lisp-syntax :constituent)
	    (or (rev-scan-char mark1 :lisp-syntax (not :constituent))
		(buffer-start mark1))
	    (scan-char mark1 :lisp-syntax :constituent))
	  (when (test-char (next-character point) :lisp-syntax :constituent)
	    (or (scan-char mark2 :lisp-syntax (not :constituent))
		(buffer-end mark2)))
	  (when (mark= mark1 mark2)
	    ;; Try to get whole form
	    (pre-command-parse-check point)
	    (when (valid-spot point t)
	      (move-mark mark1 point)
	      (form-offset mark1 -1)
	      (move-mark mark2 mark1)
	      (form-offset mark2 1)))))
    (unless (mark= mark1 mark2)
      (region-to-string (region mark1 mark2)))))

(defcommand "Goto Definition" (p)
  "Go to the current function/macro's definition.  With a numarg, prompts for name to go to."
  "Go to the current function/macro's definition."
  (if p
      (edit-definition-command nil)
      (let* ((point (current-point))
	     (buffer (current-buffer))
	     (fun-name (symbol-at-point buffer point)))
	(if fun-name
	    (get-def-info-and-go-to-it fun-name (or
						 (buffer-package (current-buffer))
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
        (error (c) (editor-error (format nil "~a" c)))))))
      

#|
;;; "Edit Command Definition" is a hack due to creeping evolution in
;;; GO-TO-DEFINITION.  We specify :function type and a name with "-COMMAND"
;;; instead of :command type and the real command name because this causes
;;; the right pattern to be created for searching.  We could either specify
;;; that you always edit command definitions with this command (breaking
;;; "Go to Definition" for commands called as functions), fixing the code,
;;; or we can hack this command so everything works.
;;;
(defcommand "Edit Command Definition" (p)
  "Prompts for command definition name and goes to it for editing."
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
    (go-to-definition (fun-defined-from-pathname (command-function command))
		      :function
		      (concatenate 'simple-string name "-COMMAND"))))

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

(defparameter *source-file-indicator-defining-operators* ())

(defun define-source-file-indicator-defining-operators (name &rest operators)
  (setf (getf *source-file-indicator-defining-operators* name) operators))

(defun get-source-file-indicator-defining-operators (thing)
  (if (typep thing 'method)
    '(defmethod)
    (getf *source-file-indicator-defining-operators* thing)))

(define-source-file-indicator-defining-operators 'class 'defclass)
(define-source-file-indicator-defining-operators 'type 'deftype)
(define-source-file-indicator-defining-operators 'function 'defun 'defmacro 'defgeneric #+x8664-target 'ccl::defx86lapfunction #+ppc-target 'ccl::defppclapfunction)
(define-source-file-indicator-defining-operators 'ccl::constant 'defconstant)
(define-source-file-indicator-defining-operators 'variable 'defvar 'defparameter 'ccl::defstatic 'ccl::defglobal)
(define-source-file-indicator-defining-operators 'method-combination 'define-method-combination)
(define-source-file-indicator-defining-operators 'ccl::method-combination-evaluator 'ccl::define-method-combination-evaluator)
(define-source-file-indicator-defining-operators 'compiler-macro 'define-compiler-macro)
#+ppc32-target
(define-source-file-indicator-defining-operators 'ccl::ppc32-vinsn 'ccl::define-ppc32-vinsn)
#+ppc64-target
(define-source-file-indicator-defining-operators 'ccl::ppc64-vinsn 'ccl::define-ppc64-vinsn)
#+x8664-target
(define-source-file-indicator-defining-operators 'ccl::x8664-vinsn 'ccl::define-x8664-vinsn)


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
                                 
                        
        
;;; START and END delimit a function name that matches what we're looking
;;; for, PACKAGE is the buffer's package (or *PACKAGE*), and INDICATOR
;;; is either a symbol (FUNCTION, MACRO, etc) or a METHOD object.
(defun match-context-for-indicator (start end package indicator)
  (declare (ignorable end))
  (with-mark ((op-start start)
              (op-end start))
    (and (form-offset op-start -1)
         (progn
           (move-mark op-end op-start)
           (form-offset op-end 1))
         (let* ((defining-operator
                    (ignore-errors
                      (let* ((*package* package))
                        (values (read-from-string (region-to-string (region op-start op-end))))))))
           (memq
            defining-operator
            (get-source-file-indicator-defining-operators indicator)))
         (or (not (typep indicator 'method))
             (match-definition-context-for-method end package indicator)))))


(defun match-definition-context (mark name indicator package)
  (pre-command-parse-check mark)
  (when (valid-spot mark t)
    (with-mark ((start mark)
                (end mark))
      (and (form-offset end 1)
           (progn
             (move-mark start end)
             (form-offset start -1))
           (eq name (ignore-errors
                      (let* ((*package* package))
                        (values (read-from-string (region-to-string (region start end)))))))
           (match-context-for-indicator start end package indicator)))))

(defun find-definition-in-buffer (name indicator)
  (let ((buffer (current-buffer)))
    (setf (hi::buffer-region-active buffer) nil)
    (when (symbolp name)
      (let* ((string (string name))
             (len (length string))
             (pattern (get-search-pattern string :forward))
             (mark (copy-mark (buffer-start-mark buffer)))
             (package (or
                       (find-package
                        (variable-value 'current-package :buffer buffer))
                       *package*)))
        (or
         (loop
           (let* ((won (find-pattern mark pattern)))
             (unless won
               (return))
             (when (match-definition-context mark name indicator package)
               (backward-up-list mark)
               (move-mark (buffer-point buffer) mark)
               (return t))
             (unless (character-offset mark len)
               (return))))
         (editor-error "Couldn't find definition for ~s" name))))))
