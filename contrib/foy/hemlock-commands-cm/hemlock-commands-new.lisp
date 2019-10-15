;;;-*-Mode: LISP; Package: HEMLOCK-COMMANDS-TOOL -*-

;;; ----------------------------------------------------------------------------
;;;
;;;      hemlock-commands-new.lisp
;;;
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      This code implements a two new Hemlock commands.
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History, most recent first:
;;;      9/2/9   Added "Show Callers" command.
;;;      8/31/9  version 0.1b1
;;;              First cut.
;;;
;;; ----------------------------------------------------------------------------

(in-package "HEMLOCK-COMMANDS")

(defparameter *MCL-doc* (merge-pathnames ";MCL-doc.lisp" cl-user::*hemlock-commands-directory*))

;;; Hemlock has some internal code to do this, but it appears to be broken 
;;; and definitely does not work for ObjC methods.
(defun parse-symbol ()
  "Parse and return the symbol at point."
  (let ((point (hi::current-point)))
    (hemlock::pre-command-parse-check point)
    (hi::with-mark ((mark1 point)
                    (mark2 point))
      (hemlock::mark-symbol mark1 mark2)
      ;; For an objc method, mark-symbol removes the prepended #\#
      (let* ((string (hi::region-to-string (hi::region mark1 mark2)))
             (objc-p (when string (char= (elt string 0) #\/)))
             (colons-start-position (when string
                                      (unless objc-p (position #\: string))))
             (colons-end-position (when colons-start-position
                                    (if (char= (elt string (1+ colons-start-position)) #\:)
                                      (1+ colons-start-position)
                                      colons-start-position)))
             (package-prefix (when colons-start-position
                               (string-upcase (subseq string 0 colons-start-position))))
             (sym-string (if colons-end-position
                           (subseq string (incf colons-end-position))
                           string))
             (package (if objc-p
                        (find-package "NEXTSTEP-FUNCTIONS")
                        (when package-prefix (find-package package-prefix))))
             symbol)
        (when (and sym-string objc-p)
          (setq sym-string (subseq sym-string 1))) ;chuck the #\/
        (setq symbol (if package
                       (if objc-p
                         (find-symbol sym-string package)
                         (find-symbol (string-upcase sym-string) package))
                       (find-symbol (string-upcase sym-string) (hemlock::buffer-package hi::*current-buffer*))))
        symbol))))

(hemlock::defcommand "Inspect Symbol" (p)
  "Open the Inspector for the symbol at point."
  (declare (ignore p))
  (let ((symbol (parse-symbol)))
    (cond (symbol 
           (inspect symbol))
          (t
           (hi::editor-error "Could not parse a valid symbol at point.")))))

(hi::bind-key "Inspect Symbol" #k"control-x control-i")

(defun MCL-documentation (symbol)
  "Fetch the MCL documentation for SYMBOL."
  (let ((path *MCL-doc*))
    (when (probe-file path)
      (with-open-file (stream path :direction :input)
        (let (sym args type doc)
          (loop
            (setq sym (read stream nil :eof))
            (setq args (read stream nil :eof))
            (setq type (read stream nil :eof))
            (setq doc (read stream nil :eof))
            (cond ((eq sym :eof)
                   (return-from MCL-documentation))
                  ((eq sym symbol)
                   (return (values args type doc))))))))))

(defun display-ccl-doc (sym text-view)
  "Display the CCL documentation for SYM, if it exists."
  (let (docstring args)
    (dolist (doctype '(compiler-macro function method-combination
                                      setf structure t type variable))
      (when (setq docstring (documentation sym doctype))
        (when (eq doctype 'function) 
          (setq args (arglist sym))
          (when (macro-function sym) (setq doctype 'macro))
          (when (special-form-p sym) (setq doctype 'special-form)))
        (when (eq doctype 'type)
          (when (find-class sym nil)
            (setq doctype 'class)))
        (open-documentation-dialog
         (if args
           (format nil "~A  ~A" (string-upcase sym) 
                   (string-downcase (format nil "~A" args)))
           (string-upcase sym))
         (format nil "[~A]" (string-capitalize (string-downcase (string doctype))))
         docstring :text-view text-view :symbol sym)
        (return t)))))

(defun display-mcl-doc (sym text-view)
  "Display the MCL documentation for SYM, if it exists."
  (multiple-value-bind (args type doc)
                       (MCL-documentation sym)
    (when doc
      (setq doc (substitute #\space #\newline doc))
      (open-documentation-dialog
       (if args
         (format nil "~A  ~A" (string-upcase sym) 
                 (string-downcase (format nil "~A" args)))
         (string-upcase sym)) 
       type 
       (concatenate 'string doc "    (MCL)")
       :text-view text-view :symbol sym) t)))
  
(hi:defcommand "Symbol Documentation" (p)
  "Display the documentation for the symbol at point."
  (declare (ignore p))
  (let* ((sym (parse-symbol))
         (hemlock-view (hi::current-view))
         (pane (when hemlock-view (hi::hemlock-view-pane hemlock-view)))
         (text-view (when pane (gui::text-pane-text-view pane))))
      (cond ((and sym text-view)
             (cond ((eq (symbol-package sym) (find-package :common-lisp))
                    (or (display-ccl-doc sym text-view)
                        (display-mcl-doc sym text-view)
                        (gui::lookup-hyperspec-symbol sym text-view)))
                   (t
                    (or (display-ccl-doc sym text-view)
                        (open-documentation-dialog
                         (format nil "No documentation found for ~S" sym) nil nil)))))
            (t
             (hi::editor-error "Could not parse a valid symbol at point.")))))

(hi::bind-key "Symbol Documentation" #k"control-x control-d")

(hi:defcommand "Show Callers" (p)
  "Display a scrolling list of the callers of the symbol at point.
   Double-click a row to go to the caller's definition."
  (declare (ignore p))
  (let* ((symbol (parse-symbol))
         (callers (ccl::callers symbol)))
    (cond (symbol
           (if callers
             (make-instance 'gui::sequence-window-controller
               :title (format nil "Callers of ~a" symbol)
               :sequence (mapcar #'(lambda (entry)
                                     (if (listp entry)
                                       (car (last entry))
                                       entry))
                                 (ccl::callers symbol))
               :result-callback #'hemlock::edit-definition
               :display #'princ)
             (gui::alert-window :title "Notification"
                                :message (format nil "Could not find any callers for ~S" symbol))))
          (t
           (hi::editor-error "Could not parse a valid symbol at point.")))))

(hi::bind-key "Show Callers" #k"control-meta-c")








