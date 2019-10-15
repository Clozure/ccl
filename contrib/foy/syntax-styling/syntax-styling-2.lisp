;;;-*- Mode: Lisp; Package: (SYNTAX-STYLING (CL CCL HEMLOCK-INTERNALS)) -*-

;;; ****************************************************************************
;;; 
;;;      syntax-styling-2.lisp
;;;      
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      Macros and styling functions.
;;;      
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History, most recent first:
;;;      10/18/9   First cut.
;;;
;;; ****************************************************************************

(in-package "SAX")

;;; ----------------------------------------------------------------------------
;;; Macros used to construct the styling functions below.
;;; ----------------------------------------------------------------------------
;;; 
;;; NOTE: Not bothering to gensym these macros.  They are only used in this file,
;;; and the only variable capture is the intensional variable capture of POS.
;;;
(defMacro sparen (style debug-flag name)
  (declare (ignorable debug-flag name))
  `(when pos 
     #+sax-debug (when (and debug-function-p ,debug-flag) 
                   (debug-out "~%~%~S" ,name)
                   (debug-out "~%pos: ~S" pos))
     (let ((end (mark-next pos)))
       #+sax-debug (when (and debug-function-p ,debug-flag) 
                     (debug-out "~%end: ~S" end))
       (when end 
         (if *inc-p*             
           (when (mark< pos *inc-pos*)
             (let* ((macro-start (next-sexpr-start end))
                    (macro-end (sexpr-end macro-start)))
               (when (or (mark= end *inc-pos*) 
                         (and macro-end macro-start (alpha-char-p (mark-char macro-start))
                              (mark= *inc-pos* macro-end) (mark= macro-start end)))
                 #+sax-debug (when (and debug-function-p ,debug-flag) 
                               (debug-out "~%*inc-pos*: ~S" *inc-pos*)
                               (debug-out "~%macro-end: ~S" macro-end))
                 (let ((start (clone pos)))
                   (setq *superparen-closure*
                         #'(lambda () 
                             #+sax-debug (when (and debug-function-p ,debug-flag)
                                           (debug-out "~%~%closure being called."))
                             (set-style-attributes (attribute-dictionary ,style)
                                                   start end)))))))
           (style-region ,style pos end nil))
         (setq pos (nnext-sexpr-start end))))))

(defMacro superparen ()
  "Super parens surround top-level forms and embedded function definitions."
  `(sparen *superparen-style* *superparen-debug* 'superparen))

(defMacro eval-when-superparen ()
  "Eval-when deserves a distinctive style for its parens."
  `(sparen *eval-when-superparen-style* *eval-when-superparen-debug* 'eval-when-superparen))

(defMacro loop-superparen ()
  "Loop deserves a distinctive style for its parens."
  `(sparen *loop-superparen-style* *loop-superparen-debug* 'loop-superparen))

(defMacro paren ()
  "This does no styling; it just increments POS."
  `(when pos #+sax-debug (when (and debug-function-p *paren-debug*) 
                          (debug-out "~%~%~S" 'paren))
     (setq pos (nnext-sexpr-start (mark-next pos)))))

(defMacro optional-paren ()
  "This does no styling; it just increments POS, if there is a paren."
  `(when pos #+sax-debug (when (and debug-function-p *optional-paren-debug*)
                          (debug-out "~%~%~S" 'optional-paren))
     (let ((pos-char (mark-char pos)))
       (when (or (char= pos-char #\()
                 (char= pos-char #\)))
         (setq pos (nnext-sexpr-start (mark-next pos)))))))

(defMacro objc-symbl (pos)
  "Style an objc symbol, or list containing symbol and return value."
  `(setq ,pos (objc-symbol-styling-function ,pos)))

(defMacro symbl ()
  "Style a symbol-name, taking into account exported symbols."
  `(when pos #+sax-debug (when (and debug-function-p *symbol-debug*)
                          (debug-out "~%~%~S" 'symbl)
                           (debug-out "~%symbol-style: ~S" symbol-style))
     (let ((pos-end (sexpr-end pos)))
       (when pos-end 
         #+sax-debug (when (and debug-function-p *symbol-debug*)
                      (debug-out "~%pos-end: ~S" pos-end))
         (let ((name (string-upcase (region-to-string (region pos pos-end)))))
           (when name
             (multiple-value-bind (symbol kind)
                                  (find-symbol name *current-package*)
               (cond ((and symbol *current-package* (eq kind :external)
                           (not (eq symbol-style *variable-definition-symbol-style*)))
                      (cond ((char= (mark-char pos) #\") 
                             ; a string, don't set caps  
                             (style-region *exported-symbol-style* pos pos-end nil))
                            (t
                             (style-region *exported-symbol-style* pos pos-end))))
                     (t
                      (cond ((char= (mark-char pos) #\")
                             (style-region symbol-style pos pos-end nil))
                            (t
                             (style-region symbol-style pos pos-end))))))))
         (setq pos (next-sexpr-start pos-end))))))

(defMacro struct-sym ()
  "Style the name of a structure."
  `(when pos #+sax-debug (when (and debug-function-p *struct-sym-debug*)
                          (debug-out "~%~%~S" 'struct-sym))
     (setq pos (next-sexpr-start (struct-sym-styling-function pos)))))

(defMacro struct-fields ()
  "Style structure fields."
  `(when pos #+sax-debug (when (and debug-function-p *struct-fields-debug*)
                          (debug-out "~%~%~S" 'struct-fields))
     (do* ((field-start pos (next-sexpr-start field-end))
           (field-end (when field-start (sexpr-end field-start))
                      (when field-start (sexpr-end field-start))))
          ((or (null field-start) (mark> field-start form-end)))
       (cond ((char= (mark-char field-start) #\()
              (let* ((symbol-start (mark-next field-start))
                     (symbol-end (when symbol-start (sexpr-end symbol-start)))
                     (next-start (when symbol-end (next-sexpr-start symbol-end))))
                (style-region *defstruct-field-style* symbol-start symbol-end)
                (when next-start (rd-style-forms :start next-start :end field-end))))
             (t
              (style-region *defstruct-field-style* field-start field-end))))
     (setq pos (mark-prev form-end))))

(defMacro ancestor ()
  "Style a structure's ancestor."
  `(when pos #+sax-debug (when (and debug-function-p *ancestor-debug*)
                          (debug-out "~%~%~S" 'ancestor))
     (let* ((start (next-sexpr-start (mark-next pos)))
            (end (when start (sexpr-end start)))
            (string (when (and start end) (region-to-string (region start end))))
            ancestor-start)
       (when (and string (string-equal string ":include"))
         (style-region *keyword-package-style* start end)
         (when (setq ancestor-start (next-sexpr-start end))
           (style-region *defstruct-ancestor-style* ancestor-start
                         (sexpr-end ancestor-start)))
         (setq pos (next-sexpr-start (sexpr-end pos)))))))

(defMacro macro ()
  "Style the name of the macro."
  `(when pos #+sax-debug (when (and debug-function-p *macro-debug*)
                          (debug-out "~%~%~S" 'macro))
     (let ((pos-end (sexpr-end pos)))
       #+sax-debug (when (and debug-function-p *macro-debug*)
                    (debug-out "~%pos-end: ~S" pos-end))
       (when pos-end
         (style-region macro-style pos pos-end)
         (setq pos (next-sexpr-start pos-end))))))

(defMacro derivation-list ()
  "Style the DEFCLASS derivation list."
  `(when pos #+sax-debug (when (and debug-function-p *derivation-list-debug*)
                          (debug-out "~%~%~S" 'derivation-list))
     (let* ((pos-char (mark-char pos))
            (pos-next (mark-next pos))
            (pos-end (sexpr-end pos))
            (end-prev (when pos-end (mark-prev pos-end))))
       (when (and pos-next end-prev pos-char (char= pos-char #\())
         #+sax-debug (when (and debug-function-p *derivation-list-debug*)
                      (debug-out "~%pos-next: ~S" pos-next)
                      (debug-out "~%end-prev: ~S" end-prev))
         (style-region *defclass-derivation-style* pos-next end-prev))
     (setq pos (next-sexpr-start pos-end)))))

(defMacro slot-list ()
  "Style DEFCLASS slots."
  `(when pos #+sax-debug (when (and debug-function-p *derivation-list-debug*)
                          (debug-out "~%~%~S" 'slot-list))
     (let (slot-positions
           (pos-end (sexpr-end pos)))
       (do ((current-start (sexpr-start (mark-prev pos-end))
                           (sexpr-start (mark-prev current-start))))
           ((mark<= current-start pos))
         (when (or (not *inc-p*)
                   (and *inc-p*
                        (mark>= *inc-pos* current-start)
                        (mark<= *inc-pos* (sexpr-end current-start))))
           (push current-start slot-positions)))
       (dolist (slot-position slot-positions)
         (rd-style-forms :start slot-position :end (sexpr-end slot-position))
         (style-region *defclass-slot-style* (mark-next slot-position)
                       (sexpr-end (mark-next slot-position))))
       (setq pos (next-sexpr-start pos-end)))))

(defMacro qualifier ()
  "Style method qualifiers."
  `(when pos #+sax-debug (when (and debug-function-p *qualifier-debug*)
                          (debug-out "~%~%~S" 'qualifier))
     (let ((pos-end (sexpr-end pos)))
       (when (char= (mark-char pos) #\:)
         (style-region *keyword-package-style* pos pos-end)
         (setq pos (next-sexpr-start pos-end))))))

(defun list-regions (start end  &aux e1-start e1-end e2-start e2-end)
  "List parameter and specializer or optional parameter and defaults."
  (declare (ignorable end))
  #+sax-debug (when (and debug-function-p *list-regions-debug*)
                (debug-out "~%~%~S" 'list-regions)
                (debug-out "~%start: ~S" start)
                (debug-out "~%end: ~S" end))
  (setq e1-end (sexpr-end (mark-next start))
        e1-start (sexpr-start e1-end))
  (setq e2-start (next-sexpr-start (mark-next e1-end))
        e2-end (sexpr-end e2-start))
  (list e1-start e1-end e2-start e2-end))

(defun parameter-regions (list-start)
  "Collect specialized and non-specialized parameter regions. Style the defaults for
  lambda-list-keyword parameters."
  #+sax-debug (when (and debug-function-p *parameter-regions-debug*)
                (debug-out "~%~%~S" 'parameter-regions))
  (let ((list-end (sexpr-end list-start))
        results option-p)
    (do* ((start (next-sexpr-start (mark-next list-start)) 
                 (when (sexpr-end start) (next-sexpr-start (sexpr-end start))))
          (char (when start (mark-char start)) (when start (mark-char start))))
         ((or (null start) (mark>= start list-end)) results)
      #+sax-debug (when (and debug-function-p *parameter-regions-debug*)
                    (debug-out "~%start: ~S" start))
      (cond ((char= char #\()
             (let ((specializer-regions (list-regions start (sexpr-end start))))
               #+sax-debug (when (and debug-function-p *parameter-regions-debug*)
                             (debug-out "~%specializer-regions: ~S" specializer-regions))
               (when (and option-p (third specializer-regions) (fourth specializer-regions))
                 (rd-style-forms :start (third specializer-regions) :end (fourth specializer-regions)))
               (push (subseq specializer-regions 0 (when option-p 2))
                     results)))
            ((char= char #\&) 
             (style-region *keyword-package-style* start (sexpr-end start))
             (setq option-p t))
            (t 
             (push (list start (sexpr-end start)) results))))))

(defMacro parameter-list ()
  "Style the parameter list.  This is called by both functions and methods."
  `(when pos #+sax-debug (when (and debug-function-p *parameter-list-debug*)
                          (debug-out "~%~%~S" 'parameter-list))
     (let ((parameter-regions (parameter-regions pos)))
       #+sax-debug (when (and debug-function-p *parameter-list-debug*)
                     (debug-out "~%parameter-regions: ~S" parameter-regions))
       (dolist (arg parameter-regions)
         (style-region *parameter-style* (first arg) (second arg))
         (when (and (third arg) (fourth arg))
           #+sax-debug (when (and debug-function-p *parameter-list-debug*)
                         (debug-out "~%third: ~S" (third arg))
                         (debug-out "~%fourth: ~S" (fourth arg))
                         (debug-out "~%*specializer-style*: ~S" *specializer-style*))
           (style-region *specializer-style* (third arg) (fourth arg))))
       (setq pos (next-sexpr-start (sexpr-end pos))))))

(defMacro embedded-function-definitions ()
  "Style the functions defined by LABELS and FLET."
  `(when pos #+sax-debug (when (and debug-function-p *embedded-function-definitions-debug*)
                          (debug-out "~%~%~S" 'embedded-function-definitions))
     (let ((pos-end (sexpr-end pos)))
       (do ((position (next-sexpr-start (mark-next pos))
                      (next-sexpr-start (nmark-next (sexpr-end position)))))
           ((or (null position) (mark>= position pos-end)))
         (embedded-function-styling-function (clone position)))
       (setq pos (next-sexpr-start pos-end)))))

(defMacro variable-definitions ()
  "Style the variables and default values defined by LET, DO*, etc."
  `(when pos #+sax-debug (when (and debug-function-p *variable-definitions-debug*)
                          (debug-out "~%~%~S" 'variable-definitions)
                          (debug-out "~%pos: ~S" pos))
     (let ((pos-end (sexpr-end pos)))
       (do ((position (next-sexpr-start (mark-next pos))
                      (next-sexpr-start (nmark-next (sexpr-end position)))))
           ((or (null position) (mark>= position pos-end)))
         #+sax-debug (when (and debug-function-p *variable-definitions-debug*)
                      (debug-out "~%variable-definition position: ~S" position))
         (variable-definition-styling-function (clone position)))
       (setq pos (next-sexpr-start pos-end)))))

(defMacro case-match-forms ()
  "Style the match forms of a case statement"
  `(when pos #+sax-debug (when (and debug-function-p *case-match-forms-debug*)
                          (debug-out "~%~%~S" 'case-match-forms))
     (let ((end (mark-prev form-end)))
       (do ((position (next-sexpr-start pos)
                      (next-sexpr-start (nmark-next (sexpr-end position)))))
           ((or (null position) (mark>= position end)))
         (case-match-styling-function position))
       (setq pos (next-sexpr-start end)))))

(defMacro loop-test ()
  "Style the test form used by an iteration macro."
  `(when pos #+sax-debug (when (and debug-function-p *loop-test-debug*)
                          (debug-out "~%~%~S" 'loop-test))
     (let ((pos-end (sexpr-end pos)))
       (rd-style-forms :start pos :end pos-end)
       (setq pos (next-sexpr-start pos-end)))))

(defMacro variable-form ()
  "Style the initialization form of a variable definition."
  `(when pos #+sax-debug (when (and debug-function-p *variable-form-debug*)
                          (debug-out "~%~%~S" 'variable-form))
     (let ((pos-end (sexpr-end pos)))
       (variable-definition-styling-function pos)
       (setq pos (next-sexpr-start pos-end)))))

(defMacro variable-list ()
  "Style the variable list of multiple-value-setq, multiple-value-bind, etc."
  `(when pos #+sax-debug (when (and debug-function-p *variable-list-debug*)
                          (debug-out "~%~%~S" 'variable-list))
     (let ((pos-end (sexpr-end pos)))
       (do* ((var-start (next-sexpr-start (mark-next pos)) 
                        (next-sexpr-start (nmark-next var-end)))
             (var-end (when var-start (sexpr-end var-start))
                      (when var-start (sexpr-end var-start))))
            ((or (null var-start) (mark> var-start pos-end)))
         (style-region *variable-definition-symbol-style* var-start var-end nil))
       (setq pos (next-sexpr-start pos-end)))))

(defMacro body ()
  "Style the body of a macro."
  `(when pos #+sax-debug (when (and debug-function-p *body-debug*)
                          (debug-out "~%~%~S" 'body)
                           (debug-out "~%pos: ~S" pos)
                           (debug-out "~%form-end: ~S" form-end))
     (rd-style-forms :start pos :end (mark-prev form-end))
     (setq pos (mark-prev form-end))))

(defMacro loop-body ()
  "Style the body of a loop macro."
  `(when pos #+sax-debug (when (and debug-function-p *loop-body-debug*)
                          (debug-out "~%~%~S" 'loop-body))
     (style-elements pos (mark-prev form-end) t)
     (setq pos (mark-prev form-end))))

(defMacro form ()
  "Style a single form."
  `(when pos #+sax-debug (when (and debug-function-p *form-debug*)
                          (debug-out "~%~%~S" 'form)
                          (debug-out "~%pos: ~S" pos))
     (let ((pos-end (sexpr-end pos)))
       #+sax-debug (when (and debug-function-p *form-debug*)
                    (debug-out "~%pos-end: ~S" pos-end))
       (rd-style-forms :start pos :end pos-end)
       (setq pos (if (next-sexpr-start pos-end)
                   (mark-min (or (mark-prev form-end) form-end)
                             (next-sexpr-start pos-end))
                   (mark-prev form-end))))))

(defMacro doc ()
  "Style the doc in DEFUN, DEFMETHOD, DEFMACRO, DEFPARAMETER, etc."
  `(when pos #+sax-debug (when (and debug-function-p *doc-debug*)
                          (debug-out "~%~%~S" 'doc))
     (let ((pos-end (sexpr-end pos)))
       (cond ((mark< pos form-end)
              (cond ((char-equal #\" (mark-char pos))
                     (cond (*inc-p*
                            (style-region *string-style* 
                                          pos (mark-min *inc-pos* (or pos-end pos))
                                          nil))
                           (t
                            (style-region *string-style* pos pos-end nil)))
                     (setq pos (if (next-sexpr-start pos-end)
                                 (if (mark< (mark-prev form-end) 
                                            (next-sexpr-start pos-end))
                                   (mark-prev form-end)
                                   (next-sexpr-start pos-end))
                                 (mark-prev form-end))))
                    (t
                     pos)))
             (t 
              form-end)))))

(defMacro options ()
  "Style DEFCLASS and DEFGENERIC options."
  `(when pos #+sax-debug (when (and debug-function-p *options-debug*)
                          (debug-out "~%~%~S" 'options))
     (do* ((option-start pos (next-sexpr-start (sexpr-end option-start)))
           (symbol-start (when option-start (mark-next option-start))
                         (when option-start (mark-next option-start)))
           (symbol-end (when symbol-start (sexpr-end symbol-start))
                       (when symbol-start (sexpr-end symbol-start))))
          ((or (null symbol-start) (mark>= symbol-start form-end)))
       (when (char-equal #\: (mark-char symbol-start))
         (style-region *keyword-package-style* symbol-start symbol-end nil)
         (cond ((string-equal (region-to-string (region symbol-start symbol-end))
                              ":documentation")
                (when (next-sexpr-start symbol-end)
                  (style-region *string-style* 
                                (next-sexpr-start symbol-end)
                                (sexpr-end (next-sexpr-start symbol-end)) nil)))
               (t 
                (when (next-sexpr-start (sexpr-end symbol-start))
                  (style-elements (next-sexpr-start symbol-end) form-end))))))
     (setq pos (mark-prev form-end))))


;;; These are called by the macros above:
(defun struct-sym-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *defstruct-symbol-style*))
    (optional-paren) (symbl) (ancestor) (body) (optional-paren) pos))

(defun embedded-function-styling-function (pos)
  #+sax-debug (when *embedded-function-styling-function-debug*
                 (debug-out "~%~%~S" 'embedded-function-styling-function))
  (let ((form-end (sexpr-end pos))
        (symbol-style *embedded-function-symbol-style*))
    (superparen) (symbl) (parameter-list) (doc) (body) (superparen) pos))

(defun variable-definition-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *variable-definition-symbol-style*))
    (optional-paren) (symbl) (body) (optional-paren) pos))

(defun case-match-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *case-match-style*))
    (paren) (symbl) (body) (paren) pos))

(defun objc-symbol-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *objc-symbol-style*))
    (optional-paren) (symbl) (body) (optional-paren) pos))


;;; The defstyle styles:
(defun defpackage-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *defpackage-symbol-style*)
        (macro-style *generic-macro-style*))
    (superparen) (macro) (symbl) (body) (superparen)))

(add-style "defpackage" #'defpackage-styling-function)

(defun defparameter-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *defparameter-symbol-style*)
        (macro-style *generic-macro-style*))
    (superparen) (macro) (symbl) (form) (doc) (superparen)))

(add-style "defparameter" #'defparameter-styling-function)

(defun defvar-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *defvar-symbol-style*)
        (macro-style *generic-macro-style*))
    (superparen) (macro) (symbl) (form) (doc) (superparen)))

(add-style "defvar" #'defvar-styling-function)

(defun defconstant-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *defconstant-symbol-style*)
        (macro-style *generic-macro-style*))
    (superparen) (macro) (symbl) (form) (doc) (superparen)))

(add-style "defconstant" #'defconstant-styling-function)

(defun defclass-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *defclass-symbol-style*)
        (macro-style *generic-macro-style*))
    (superparen) (macro) (symbl) (derivation-list) (slot-list) (options) (superparen)))

(add-style "defclass" #'defclass-styling-function)

(defun defun-styling-function (pos)
  #+sax-debug (when *defun-styling-function-debug*
                 (debug-out "~%~%~S" 'defun-styling-function))
  (let ((form-end (sexpr-end pos))
        (symbol-style *defun-symbol-style*)
        (macro-style *defun-macro-style*))
    (superparen) (macro) (symbl) (parameter-list) (doc) (body) (superparen)))

(add-style "defun" #'defun-styling-function)

(defun defmacro-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *defmacro-symbol-style*)
        (macro-style *generic-macro-style*))
    (superparen) (macro) (symbl) (parameter-list) (doc) (body) (superparen)))

(add-style "defmacro" #'defmacro-styling-function)

(defun define-compiler-macro-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *generic-function-symbol-style*)
        (macro-style *defun-macro-style*))
    (superparen) (macro) (symbl) (parameter-list) (doc) (body) (superparen)))

(add-style "define-compiler-macro" #'define-compiler-macro-styling-function)

(defun define-modify-macro-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *generic-function-symbol-style*)
        (macro-style *defun-macro-style*))
    (superparen) (macro) (symbl) (parameter-list) (form) (doc) (superparen)))

(add-style "define-modify-macro" #'define-modify-macro-styling-function)

(defun define-setf-expander-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *generic-function-symbol-style*)
        (macro-style *defun-macro-style*))
    (superparen) (macro) (symbl) (parameter-list) (doc) (body) (superparen)))

(add-style "define-setf-expander" #'define-setf-expander-styling-function)

(defun define-condition-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *generic-function-symbol-style*)
        (macro-style *defun-macro-style*))
    (superparen) (macro) (symbl) (derivation-list) (slot-list) (options) (superparen)))

(add-style "define-condition" #'define-condition-styling-function)

(defun defgeneric-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *defgeneric-symbol-style*)
        (macro-style *generic-macro-style*))
    (superparen) (macro) (symbl) (parameter-list) (options) (superparen)))

(add-style "defgeneric" #'defgeneric-styling-function)

(defun defmethod-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *defmethod-symbol-style*)
        (macro-style *generic-macro-style*))
    (when pos 
    (superparen) (macro) (symbl) (qualifier) (parameter-list) (doc) (body) (superparen))))

(add-style "defmethod" #'defmethod-styling-function)

(defun objc-defmethod-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *objc-macro-style*))
    (superparen) (macro) (objc-symbl pos) (parameter-list) (doc) (body) (superparen)))

(add-style "objc:defmethod" #'objc-defmethod-styling-function)

(defun defcommand-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (symbol-style *defcommand-symbol-style*)
        (macro-style *defcommand-macro-style*))
    (superparen) (macro) (symbl) (parameter-list) (doc) (doc) (body) (superparen)))

(add-style "hemlock::defcommand" #'defcommand-styling-function)

(defun labels-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (embedded-function-definitions) (body) (paren)))

(add-style "labels" #'labels-styling-function)

(defun lambda-styling-function (pos)
  #+sax-debug (when *lambda-styling-function-debug*
                (debug-out "~%~%~S" 'lambda-styling-function))
  (let ((form-end (sexpr-end pos))
        (macro-style *lambda-macro-style*))
    (superparen) (macro) (parameter-list) (doc) (body) (superparen)))

(add-style "lambda" #'lambda-styling-function)

(defun flet-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (embedded-function-definitions) (body) (paren)))

(add-style "flet" #'flet-styling-function)

(defun loop-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *loop-macro-style*))
    (loop-superparen) (macro) (loop-body) (loop-superparen)))

(add-style "loop" #'loop-styling-function)

(defun defstruct-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *generic-macro-style*))
    (superparen) (macro) (struct-sym) (doc) (struct-fields) (superparen)))

(add-style "defstruct" #'defstruct-styling-function)

(defun dotimes-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-form) (body) (paren)))

(add-style "dotimes" #'dotimes-styling-function)

(defun dolist-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-form) (body) (paren)))

(add-style "dolist" #'dolist-styling-function)

(defun multiple-value-bind-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-list) (body) (paren)))

(add-style "multiple-value-bind" #'multiple-value-bind-styling-function)

(defun multiple-value-setq-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-list) (body) (paren)))

(add-style "multiple-value-setq" #'multiple-value-setq-styling-function)

(defun destructuring-bind-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (parameter-list) (body) (paren)))

(add-style "destructuring-bind" #'destructuring-bind-styling-function)

(defun do-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-definitions) (form) (body) (paren)))

(add-style "do" #'do-styling-function)

(defun do*-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-definitions) (form) (body) (paren)))

(add-style "do*" #'do-styling-function)

(defun let-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-definitions) (body) (paren)))

(add-style "let" #'let-styling-function)

(defun let*-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-definitions) (body) (paren)))

(add-style "let*" #'let-styling-function)

(defun prog-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-definitions) (body) (paren)))

(add-style "prog" #'prog-styling-function)

(defun prog*-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-definitions) (body) (paren)))

(add-style "prog*" #'prog*-styling-function)

(defun with-slots-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-definitions) (form) (body) (paren)))

(add-style "with-slots" #'with-slots-styling-function)

(defun with-accessors-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-definitions) (form) (body) (paren)))

(add-style "with-accessors" #'with-accessors-styling-function)

(defun with-open-file-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (variable-form) (body) (paren)))

(add-style "with-open-file" #'with-open-file-styling-function)

(defun macrolet-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (embedded-function-definitions) (body) (paren)))

(add-style "macrolet" #'macrolet-styling-function)

(defun case-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (paren) (macro) (form) (case-match-forms) (paren)))

(add-style "case" #'case-styling-function)
(add-style "ccase" #'case-styling-function)
(add-style "ecase" #'case-styling-function)
(add-style "typecase" #'case-styling-function)
(add-style "etypecase" #'case-styling-function)
(add-style "ctypecase" #'case-styling-function)

(defun eval-when-styling-function (pos)
  (let ((form-end (sexpr-end pos))
        (macro-style *cl-package-style*))
    (eval-when-superparen) (macro) (form) (body) (eval-when-superparen)))

(add-style "eval-when" #'eval-when-styling-function)

;;; history-lists.lisp needs this, for now:
(pushnew :syntax-styling *features*)

































