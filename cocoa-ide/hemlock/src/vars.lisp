;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
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
;;; Written by Rob MacLachlan
;;;
;;; The file contains the routines which define Hemlock variables.
;;;

(in-package :hemlock-internals)

(defstruct (binding
	    (:type vector)
	    (:copier nil)
	    (:constructor make-binding (cons object across symbol)))
  cons		; The cons which holds the value for the property.
  object	; The variable-object for the binding.
  across        ; The next binding in this place.
  symbol)	; The symbol name for the variable bound.



;;; UNDEFINED-VARIABLE-ERROR  --  Internal
;;;
;;;    Complain about an undefined Hemlock variable in a helpful fashion.
;;;
(defun undefined-variable-error (name)
  (if (eq (symbol-package name) (find-package :hemlock))
      (error "Undefined Hemlock variable ~A." name)
      (error "Hemlock variables must be in the \"HEMLOCK\" package, but~%~
	     ~S is in the ~S package."
	     name (package-name (symbol-package name)))))

;;; GET-MODE-OBJECT  --  Internal
;;;
;;;    Get the mode-object corresponding to name or die trying.
;;;
(defun get-mode-object (name)
  (unless (stringp name) (error "Mode name ~S is not a string." name))
  (let ((res (getstring name *mode-names*)))
    (unless res (error "~S is not a defined mode." name))
    res))

;;; FIND-BINDING  --  Internal
;;;
;;;    Return the Binding object corresponding to Name in the collection
;;; of binding Binding, or NIL if none.
;;;
(defun find-binding (name binding)
  (do ((b binding (binding-across b)))
      ((null b) nil)
    (when (eq (binding-symbol b) name) (return b))))

;;; GET-VARIABLE-OBJECT  --  Internal
;;;
;;;    Get the variable-object with the specified symbol-name, kind and where,
;;; or die trying.
;;;
(defun get-variable-object (name kind where)
  (case kind
    (:current
     (let ((obj (get name 'hemlock-variable-value)))
       (if obj obj (undefined-variable-error name))))
    (:buffer
     (check-type where buffer)
     (let ((binding (find-binding name (buffer-var-values where))))
       (unless binding
	 (error "~S is not a defined Hemlock variable in buffer ~S." name where))
       (binding-object binding)))
    (:global
     (do ((obj (get name 'hemlock-variable-value)
	       (variable-object-down obj))
	  (prev nil obj))
	 ((symbolp obj)
	  (unless prev (undefined-variable-error name))
	  (unless (eq obj :global)
	    (error "Hemlock variable ~S is not globally defined." name))
	  prev)))
    (:mode
     (let ((binding (find-binding name (mode-object-var-values
					(get-mode-object where)))))
       (unless binding
	 (error "~S is not a defined Hemlock variable in mode ~S." name where))
       (binding-object binding)))
    (t
     (error "~S is not a defined value for Kind." kind))))

;;; VARIABLE-VALUE  --  Public
;;;
;;;    Get the value of the Hemlock variable "name".
;;;
(defun variable-value (name &optional (kind :current) where)
  "Return the value of the Hemlock variable given."
  (variable-object-value (get-variable-object name kind where)))

;;; %VALUE  --  Internal
;;;
;;;    This function is called by the expansion of Value.
;;;
(defun %value (name)
  (let ((obj (get name 'hemlock-variable-value)))
    (unless obj (undefined-variable-error name))
    (variable-object-value obj)))

;;; %SET-VALUE  --  Internal
;;;
;;;    The setf-inverse of Value, set the current value.
;;;
(defun %set-value (var new-value)
  (let ((obj (get var 'hemlock-variable-value)))
    (unless obj (undefined-variable-error var))
    (invoke-hook (variable-object-hooks obj) var :current nil new-value)
    (setf (variable-object-value obj) new-value)))

;;; %SET-VARIABLE-VALUE  --  Internal
;;;
;;;   Set the Hemlock variable with the symbol name "name".
;;;
(defun %set-variable-value (name kind where new-value)
  (let ((obj (get-variable-object name kind where)))
    (invoke-hook (variable-object-hooks obj) name kind where new-value)
    (setf (variable-object-value obj) new-value)))

;;; VARIABLE-HOOKS  --  Public
;;;
;;;    Return the list of hooks for "name".
;;;
(defun variable-hooks (name &optional (kind :current) where)
  "Return the list of hook functions for the Hemlock variable given."
  (variable-object-hooks (get-variable-object name kind where)))

;;; %SET-VARIABLE-HOOKS --  Internal
;;;
;;;    Set the hook-list for Hemlock variable Name.
;;;
(defun %set-variable-hooks (name kind where new-value)
  (setf (variable-object-hooks (get-variable-object name kind where)) new-value))

;;; VARIABLE-DOCUMENTATION  --  Public
;;;
;;;    Return the documentation for "name".
;;;
(defun variable-documentation (name &optional (kind :current) where)
  "Return the documentation for the Hemlock variable given."
  (variable-object-documentation (get-variable-object name kind where)))

;;; %SET-VARIABLE-DOCUMENTATION  --  Internal
;;;
;;;    Set a variables documentation.
;;;
(defun %set-variable-documentation (name kind where new-value)
  (setf (variable-object-documentation (get-variable-object name kind where))
	new-value))

;;; VARIABLE-NAME  --  Public
;;;
;;;    Return the String Name for a Hemlock variable.
;;;
(defun variable-name (name &optional (kind :current) where)
   "Return the string name of a Hemlock variable."
  (variable-object-name (get-variable-object name kind where)))

;;; HEMLOCK-BOUND-P  --  Public
;;;
(defun hemlock-bound-p (name &optional (kind :current) where)
  "Returns T Name is a Hemlock variable defined in the specifed place, or
  NIL otherwise."
  (case kind
    (:current (not (null (get name 'hemlock-variable-value))))
    (:buffer
     (check-type where buffer)
     (not (null (find-binding name (buffer-var-values where)))))
    (:global
     (do ((obj (get name 'hemlock-variable-value)
	       (variable-object-down obj)))
	 ((symbolp obj) (eq obj :global))))
    (:mode
     (not (null (find-binding name (mode-object-var-values
				    (get-mode-object where))))))))

(declaim (special *global-variable-names*))

;;; DEFHVAR  --  Public
;;;
;;;    Define a Hemlock variable somewhere.
;;;
(defun defhvar (name documentation &key mode buffer (hooks nil hook-p)
		     (value nil value-p))
  (let* ((symbol-name (string-to-variable name))
	 (new-binding (make-variable-object documentation name))
	 (plist (symbol-plist symbol-name))
	 (prop (cdr (or (member 'hemlock-variable-value plist)
			(setf (symbol-plist symbol-name)
			      (list* 'hemlock-variable-value nil plist)))))
	 (kind :global) where string-table)
    (cond
      (mode
       (setq kind :mode  where mode)
       (let* ((obj (get-mode-object where))
	      (vars (mode-object-var-values obj)))
	 (setq string-table (mode-object-variables obj))
	 (unless (find-binding symbol-name vars)
	   (let ((binding (make-binding prop new-binding vars symbol-name)))
	     (cond ((member obj (buffer-mode-objects *current-buffer*))
		    (let ((l (unwind-bindings obj)))
		      (setf (mode-object-var-values obj) binding)
		      (wind-bindings l)))
		   (t
		    (setf (mode-object-var-values obj) binding)))))))
      (buffer
       (check-type buffer buffer)
       (setq kind :buffer  where buffer  string-table (buffer-variables buffer))
       (let ((vars (buffer-var-values buffer)))
	 (unless (find-binding symbol-name vars)
	   (let ((binding (make-binding prop new-binding vars symbol-name)))
	     (setf (buffer-var-values buffer) binding)
	     (when (eq buffer *current-buffer*)
	       (setf (variable-object-down new-binding) (car prop)
		     (car prop) new-binding))))))
      (t
       (setq string-table *global-variable-names*)
       (unless (hemlock-bound-p symbol-name :global)
	 (setf (variable-object-down new-binding) :global)
	 (let ((l (unwind-bindings nil)))
	   (setf (car prop) new-binding)
	   (wind-bindings l)))))
    (setf (getstring name string-table) symbol-name)
    (when hook-p
      (setf (variable-hooks symbol-name kind where) hooks))
    (when value-p
      (setf (variable-value symbol-name kind where) value)))
  name)

;;; DELETE-BINDING  --  Internal
;;;
;;;    Delete a binding from a list of bindings.
;;;
(defun delete-binding (binding bindings)
  (do ((b bindings (binding-across b))
       (prev nil b))
      ((eq b binding)
       (cond (prev
	      (setf (binding-across prev) (binding-across b))
	      bindings)
	     (t
	      (binding-across bindings))))))

;;; DELETE-VARIABLE  --  Public
;;;
;;; Make a Hemlock variable no longer bound, fixing up the saved
;;;binding values as necessary.
;;;
(defun delete-variable (name &optional (kind :global) where)
  "Delete a Hemlock variable somewhere."
  (let* ((obj (get-variable-object name kind where))
	 (sname (variable-object-name obj)))
    (case kind
      (:buffer
       (let* ((values (buffer-var-values where))
	      (binding (find-binding name values)))
	 (invoke-hook hemlock::delete-variable-hook name :buffer where)
	 (delete-string sname (buffer-variables where))
	 (setf (buffer-var-values where) (delete-binding binding values))
	 (when (eq where *current-buffer*)
	   (setf (car (binding-cons binding)) (variable-object-down obj)))))
      (:mode
       (let* ((mode (get-mode-object where))
	      (values (mode-object-var-values mode))
	      (binding (find-binding name values)))
	 (invoke-hook hemlock::delete-variable-hook name :mode where)
	 (delete-string sname (mode-object-variables mode))
	 (if (member mode (buffer-mode-objects *current-buffer*))
	     (let ((l (unwind-bindings mode)))
	       (setf (mode-object-var-values mode)
		     (delete-binding binding values))
	       (wind-bindings l))
	     (setf (mode-object-var-values mode)
		   (delete-binding binding values)))))
      (:global
       (invoke-hook hemlock::delete-variable-hook name :global nil)
       (delete-string sname *global-variable-names*)
       (let ((l (unwind-bindings nil)))
	 (setf (get name 'hemlock-variable-value) nil)
	 (wind-bindings l)))
      (t (error "Invalid variable kind: ~S" kind)))
    nil))
