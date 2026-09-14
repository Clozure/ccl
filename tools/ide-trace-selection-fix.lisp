;;;; Live patch: contextual Trace/Source/Inspect for selected forms like (+ 1 2).
;;;;
;;;;   (load "ccl:tools;ide-trace-selection-fix.lisp")
(in-package :gui)

(defun find-symbol-in-buffer-packages (string buffer)
  (let ((package-name (ignore-errors
                        (hi::variable-value 'hemlock::current-package :buffer buffer)))
        (packages nil))
    (unless (find #\: string)
      (let* ((pkg (and package-name (find-package package-name)))
             (preferred (and pkg (cons package-name (package-use-list pkg)))))
        (setf packages (if preferred
                         (append preferred
                                 (set-difference (list-all-packages) preferred))
                         (list-all-packages)))))
    (find-symbol-in-packages string packages)))

(defun selection-function-name (raw)
  (cond ((and (symbolp raw) (not (null raw))) raw)
        ((and (consp raw) (symbolp (car raw))) (car raw))
        (t nil)))

(defun traceable-selection (raw)
  (selection-function-name raw))

(objc:defmethod (#/traceSelection: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (with-string-under-cursor (self symbol-name buffer)
    (let* ((raw (find-symbol-in-buffer-packages symbol-name buffer))
           (sym (selection-function-name raw)))
      (if sym
        (eval-in-listener (format nil "(trace ~S)" sym))
        (#_NSBeep)))))

(objc:defmethod (#/inspectSelection: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (with-string-under-cursor (self symbol-name buffer)
    (let* ((raw (find-symbol-in-buffer-packages symbol-name buffer)))
      (if (or (symbolp raw) (consp raw) (streamp raw) (typep raw 'structure-object))
        (inspect raw)
        (#_NSBeep)))))

(objc:defmethod (#/sourceForSelection: :void) ((self hemlock-text-view) sender)
  (declare (ignore sender))
  (with-string-under-cursor (self symbol-name buffer)
    (let* ((raw (find-symbol-in-buffer-packages symbol-name buffer))
           (sym (selection-function-name raw)))
      (cond
        ((null sym) (#_NSBeep))
        (t
         (handler-case (hemlock:edit-definition sym)
           (error (c)
             (log-debug "Source of ~s failed: ~a" sym c)
             (#_NSBeep))))))))

(format t "~&;; ide-trace-selection-fix loaded (Trace/Source/Inspect)~%")
(force-output)
