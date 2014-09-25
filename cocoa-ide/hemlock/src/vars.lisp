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
(defun find-binding (symbol-name bindings)
  (find symbol-name bindings :key #'variable-object-symbol-name :test #'eq))

;;; GET-VARIABLE-OBJECT  --  Internal
;;;
;;;    Get the variable-object with the specified symbol-name, kind and where,
;;; or die trying.
;;;
(defun get-variable-object (name kind &optional where)
  (or (lookup-variable-object name kind where)
      (undefined-variable-error name)))

(defun lookup-variable-object (name kind where)
  (ecase kind
    (:current
     (let ((buffer (current-buffer)))
       (if (null buffer)
         (lookup-variable-object name :global t)
         (or (find-binding name (buffer-var-values buffer))
             (loop for mode in (buffer-minor-mode-objects buffer)
               thereis (find-binding name (mode-object-var-values mode)))
             (find-binding name (mode-object-var-values (buffer-major-mode-object buffer)))
             (get name 'hemlock-variable-value)))))
    (:buffer
     (find-binding name (buffer-var-values (ccl:require-type where 'buffer))))
    (:mode
     (find-binding name (mode-object-var-values (get-mode-object where))))
    (:global
     (get name 'hemlock-variable-value))))

;;; VARIABLE-VALUE  --  Public
;;;
;;;    Get the value of the Hemlock variable "name".
;;;
(defun variable-value (name &optional (kind :current) where)
  "Return the value of the Hemlock variable given."
  (variable-object-value (get-variable-object name kind where)))

;;; %SET-VARIABLE-VALUE  --  Internal
;;;
;;;   Set the Hemlock variable with the symbol name "name".
;;;
(defun %set-variable-value (name kind where new-value)
  (let ((obj (get-variable-object name kind where)))
    (invoke-hook (variable-object-hooks obj) name kind where new-value)
    (setf (variable-object-value obj) new-value)))

;;; %VALUE  --  Internal
;;;
;;;    This function is called by the expansion of Value.
;;;
(defun %value (name)
  (variable-value name :current t))

;;; %SET-VALUE  --  Internal
;;;
;;;    The setf-inverse of Value, set the current value.
;;;
(defun %set-value (name new-value)
  (%set-variable-value name :current t new-value))


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
  (not (null (lookup-variable-object name kind where))))


(declaim (special *global-variable-names*))

(ccl::define-definition-type hemlock-variable ())

(defmethod ccl::definition-base-name ((dt hemlock-variable-definition-type) name)
  (and (stringp name)
       (or (getstring name *global-variable-names*)
           ;; Yeah, should check for different syms in different modes, but in practice there aren't
           ;; any because we always use string-to-variable
           (loop for mode-obj in (string-table-values *mode-names*)
		;; mode-obj is nil at startup, while creating first mode
	        thereis (and mode-obj (getstring name (mode-object-variables mode-obj)))))))

(defmethod ccl::definition-same-p ((dt hemlock-variable-definition-type) name1 name2)
  (equalp name1 name2))


;;; DEFHVAR  --  Public
;;;
;;;    Define a Hemlock variable somewhere.
;;;
(defun defhvar (name documentation &key mode buffer (hooks nil hook-p)
		     (value nil value-p))
  (let* ((symbol-name (string-to-variable name)) var)
    ;; Unfortunately this is used both at toplevel and at runtime.  make a guess.
    (when (and ccl:*loading-file-source-file* (not buffer))
      (ccl:record-source-file name 'hemlock-variable))
    (cond
     (mode
      (let* ((mode-obj (get-mode-object mode)))
        (setf (getstring name (mode-object-variables mode-obj)) symbol-name)
        (unless (setq var (find-binding symbol-name (mode-object-var-values mode-obj)))
          (push (setq var (make-variable-object symbol-name))
                (mode-object-var-values mode-obj)))))
     (buffer
      (check-type buffer buffer)
      (setf (getstring name (buffer-variables buffer)) symbol-name)
      (unless (setq var (find-binding symbol-name (buffer-var-values buffer)))
        (push (setq var (make-variable-object symbol-name))
              (buffer-var-values buffer))))
     (t
      (setf (getstring name *global-variable-names*) symbol-name)
      (unless (setq var (get symbol-name 'hemlock-variable-value))
        (setf (get symbol-name 'hemlock-variable-value)
              (setq var (make-variable-object symbol-name))))))
    (setf (variable-object-name var) name)
    (when (> (length documentation) 0)
      (setf (variable-object-documentation var) documentation))
    (when hook-p
      (setf (variable-object-hooks var) hooks))
    (when value-p
      (setf (variable-object-value var) value)))
  name)

;;; DELETE-VARIABLE  --  Public
;;;
;;; Make a Hemlock variable no longer bound, fixing up the saved
;;;binding values as necessary.
;;;
(defun delete-variable (name &optional (kind :global) where)
  "Delete a Hemlock variable somewhere."
  (let* ((obj (get-variable-object name kind where))
	 (sname (variable-object-name obj)))
    (ecase kind
      (:buffer
       (let* ((values (buffer-var-values where))
	      (binding (find-binding name values)))
	 (invoke-hook hemlock::delete-variable-hook name :buffer where)
         (delete-string sname (buffer-variables where))
         (setf (buffer-var-values where) (delete binding values))))
      (:mode
       (let* ((mode (get-mode-object where))
	      (values (mode-object-var-values mode))
	      (binding (find-binding name values)))
	 (invoke-hook hemlock::delete-variable-hook name :mode where)
	 (delete-string sname (mode-object-variables mode))
         (setf (mode-object-var-values mode) (delete binding values))))
      (:global
       (invoke-hook hemlock::delete-variable-hook name :global nil)
       (delete-string sname *global-variable-names*)
       (setf (get name 'hemlock-variable-value) nil)))
    nil))
