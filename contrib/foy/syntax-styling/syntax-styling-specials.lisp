;;;-*- Mode: Lisp; Package: SYNTAX-STYLING -*-

;;; ****************************************************************************
;;; 
;;;      syntax-styling-specials.lisp
;;;
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      Special variables, utility functions and macros.
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History, most recent first:
;;;      10/18/9   First cut.
;;;
;;; ****************************************************************************

#-sax-debug
(defPackage syntax-styling (:use :cl :ccl :hemlock-internals) (:nicknames "SAX"))

(in-package "SAX")

(defParameter *style-case-p* nil "To set case, or not to set case.")

;;; ----------------------------------------------------------------------------
;;; Configure your style by hacking the colors and style parameters below:
;;; ----------------------------------------------------------------------------
;;;
(defParameter *black-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.0 0.0 0.0 1.0))
(defParameter *gray-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.92 0.92 0.92 1.0))
(defParameter *medium-gray-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.30 0.30 0.30 1.0))
(defParameter *darker-gray-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.11 0.11 0.11 1.0))
(defParameter *dark-gray-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.01 0.01 0.01 1.0))
(defParameter *blue-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.0 0.1 0.65 1.0))
(defParameter *light-blue-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.0 0.35 0.65 1.0))
(defParameter *green-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.0 0.2 0.0 1.0))
(defParameter *turquoise-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.0 0.3 0.4 1.0))
(defParameter *violet-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.15 0.1 0.7 1.0))
(defParameter *wine-red-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.5 0.1 0.2 1.0))
(defParameter *medium-red-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.8 0.0 0.2 1.0))
(defParameter *magenta-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.75 0.0 0.5 1.0))
(defParameter *dark-magenta-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.35 0.0 0.25 1.0))
(defParameter *brown-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.35 0.2 0.0 1.0))

(defParameter *generic-symbol-color* *blue-color*)
(defParameter *generic-macro-color* *wine-red-color*)

;;; Convert style-spec to an ns-dictionary with the specified attributes.
;;; Temporary text attributes only support color and underlining.
(defun spec-to-dict (font-spec)
  (let* ((dict (make-instance 'ns:ns-mutable-dictionary :with-capacity 2))
         (color (getf font-spec :font-color)) 
         (underline (getf font-spec :font-underline)) ; :single :double :thick
         (underline-int (case underline (:single 1) (:double 2) (:thick 3))))
    (when color (#/setObject:forKey: dict color 
                                     #&NSForegroundColorAttributeName))
    (when (and underline underline-int) 
      (#/setObject:forKey: dict (#/numberWithInt: ns:ns-number underline-int)
                           #&NSUnderlineStyleAttributeName))
    dict))

;;; ----------------------------------------------------------------------------
;;; The Styles:
;;; ----------------------------------------------------------------------------
;;;
;;; The cdr of each dotted-pair is the capitalization spec:
(defParameter *vanilla-styling* (cons (spec-to-dict (list :font-color *black-color*)) :down))
(defParameter *generic-text-style* (cons (spec-to-dict (list :font-color *darker-gray-color*)) :down))
(defParameter *generic-macro-style* (cons (spec-to-dict (list :font-color *generic-macro-color*)) :cap3))
(defParameter *generic-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color*)) :down))
(defParameter *generic-function-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color* :font-underline :single)) :down))  
(defParameter *embedded-function-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color* :font-underline :single)) :down))  
;;; This is also the style for lambda-list keywords:
(defParameter *keyword-package-style* (cons (spec-to-dict (list :font-color *dark-magenta-color*)) :down))
(defParameter *cl-package-style* (cons (spec-to-dict (list :font-color *blue-color*)) :down))
(defParameter *exported-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color* :font-underline :double)) :up))

(defParameter *semi-colon-comment-style* (cons (spec-to-dict (list :font-color *turquoise-color*)) :unchanged))
(defParameter *sharp-comment-style* (cons (spec-to-dict (list :font-color *medium-gray-color*)) :unchanged))
(defParameter *string-style* (cons (spec-to-dict (list :font-color *turquoise-color*)) :unchanged))

(defParameter *superparen-style* (cons (spec-to-dict (list :font-color *magenta-color*)) :unchanged))
(defParameter *eval-when-superparen-style* (cons (spec-to-dict (list :font-color *magenta-color*)) :unchanged))
(defParameter *loop-superparen-style* (cons (spec-to-dict (list :font-color *turquoise-color*)) :unchanged))

(defParameter *variable-definition-symbol-style* (cons (spec-to-dict (list :font-color *light-blue-color*)) :down))
(defParameter *defstruct-field-style* (cons (spec-to-dict (list :font-color *blue-color*)) :down))
(defParameter *defstruct-ancestor-style* (cons (spec-to-dict (list :font-color *blue-color*)) :down))
(defParameter *defclass-derivation-style* (cons (spec-to-dict (list :font-color *blue-color*)) :down))
(defParameter *defclass-slot-style* (cons (spec-to-dict (list :font-color *blue-color*)) :down))
(defParameter *parameter-style* (cons (spec-to-dict (list :font-color *light-blue-color*)) :down))
(defParameter *specializer-style* (cons (spec-to-dict (list :font-color *green-color*)) :unchanged))
(defParameter *case-match-style* (cons (spec-to-dict (list :font-color *light-blue-color*)) :down))

(defParameter *defpackage-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color*)) :down))
(defParameter *defparameter-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color*)) :down))
(defParameter *defvar-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color*)) :down))
(defParameter *defconstant-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color*)) :down))
(defParameter *defclass-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color* :font-underline :single)) :up))
(defParameter *defun-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color* :font-underline :single)) :down))
(defParameter *defmacro-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color* :font-underline :single)) :down))
(defParameter *defgeneric-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color* :font-underline :single)) :down))
(defParameter *defmethod-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color* :font-underline :single)) :down))
(defParameter *objc-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color* :font-underline :single)) :unchanged))
(defParameter *defcommand-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color* :font-underline :single)) :unchanged))
(defParameter *defstruct-symbol-style* (cons (spec-to-dict (list :font-color *generic-symbol-color* :font-underline :single)) :up))

(defParameter *lambda-macro-style* (cons (spec-to-dict (list :font-color *generic-symbol-color* :font-underline :single)) :down))
(defParameter *loop-macro-style* (cons (spec-to-dict (list :font-color *magenta-color*)) :up))
(defParameter *loop-keyword-style* (cons (spec-to-dict (list :font-color *dark-magenta-color*)) :down))
(defParameter *defun-macro-style* (cons (spec-to-dict (list :font-color *generic-macro-color*)) :down))
(defParameter *objc-macro-style* (cons (spec-to-dict (list :font-color *generic-macro-color*)) :cap8))
(defParameter *defcommand-macro-style* (cons (spec-to-dict (list :font-color *generic-macro-color*)) :cap12))

;;; ----------------------------------------------------------------------------
;;; Various:
;;; ----------------------------------------------------------------------------
;;;
(defParameter *styling-p* t "To style or not to style.")
(defParameter *buf* nil "The target buffer.")
(defParameter *layout* nil "The NSLayoutManager of the target text-view.")
(defParameter *current-package* nil "Package used to style exported symbols.")
;;; consolidate these two:
(defParameter *inc-p* nil "Styling incrementally?")
(defParameter *inc-pos* nil "Buffer-point during an incremental parse.")
(defParameter *inside-semi-colon-comment-p* nil)
(defParameter *paste-p* nil "Is a paste in progress?")
(defParameter *paste-start* nil "Starting position of a paste operation.")
(defParameter *paste-end* nil "Ending position of a paste operation.")

;;; test
(defParameter *style-screen-p* t "To style or not to style the screen after a given operation.")
(defParameter *style-top-level-form-p* nil "To style or not to style the top-level form after a given operation.")
(defParameter *segment-list* nil "Comment and string code data structure.")
(defParameter *segment-array* nil "Comment and string code data structure.")

(defParameter *form-style* nil "The style of the atom being processed incrementally.")
(defParameter *form-start* nil "The start position of the atom being processed incrementally.")
(defParameter *form-end* nil "The end position of the atom being processed incrementally.")
(defParameter *superparen-closure* nil "An ugly hack to style superparens.")

;;; key-event constants:
(defParameter %control-y% #k"control-y")
(defParameter %control-meta-q% #k"control-meta-q")
(defParameter %control-d% #k"control-d")
(defParameter %backspace% #k"Backspace")
(defParameter %control-j% #k"control-j")
(defparameter %backward-char-event% (hi::get-key-event* 98 8))

;;; Search patterns:
(defparameter *l-paren-forward-pattern* (new-search-pattern :character :forward #\())
(defparameter *l-paren-backward-pattern* (new-search-pattern :character :backward #\())
(defparameter *sharp-stroke-forward-pattern* (new-search-pattern :string-insensitive :forward "#|"))
(defparameter *stroke-sharp-forward-pattern* (new-search-pattern :string-insensitive :forward "|#"))
(defparameter *semicolon-forward-pattern* (new-search-pattern :character :forward #\;))
(defParameter *sharp-slash-forward-pattern* (new-search-pattern :string-insensitive :forward "#/"))
(defParameter *sharp-backslash-forward-pattern* (new-search-pattern :string-insensitive :forward "#\\"))
(defParameter *sharp-dollar-forward-pattern* (new-search-pattern :string-insensitive :forward "#$"))
(defParameter *sharp-ampersand-forward-pattern* (new-search-pattern :string-insensitive :forward "#&"))
(defParameter *colon-lessthan-forward-pattern* (new-search-pattern :string-insensitive :forward ":<"))

;;; ----------------------------------------------------------------------------
;;; Mark functions and macros.
;;; ----------------------------------------------------------------------------
;;;
;;; Hemlock's BUFFER is a doubly linked list of LINES.  MARKS specify relative positions 
;;; within LINES.  Programming Hemlock involves a lot of MARK manipulation. These are some 
;;; useful macros that operate on MARKS.  Destructive and non-destructive versions
;;; are usually provided, using the prepended "n" convention for destructive functions.

(defmacro clone (mark) `(hi::copy-mark ,mark :temporary))

(defmacro set-storage (storage source)
  `(progn
     (setf (mark-charpos ,storage) (mark-charpos ,source))
     (setf (mark-line ,storage) (mark-line ,source))
     ,storage))

;;; Needs to support nested forms as in: (mark-next (sexpr-end pos)),
;;; only evaluating MARK-OR-FORM once.
;;; No error, if MARK-OR-FORM evaluates to nil, just return nil.
(defmacro mark-next (mark-or-form)
  (let ((param (gensym))
        (new-mark (gensym)))
    `(let ((,param ,mark-or-form))
       (when ,param
         (let ((,new-mark (clone ,param)))
           (setq ,new-mark (mark-after ,new-mark))
           #+sax-debug (when (and *mark-next-debug* (null ,new-mark))
                         (debug-out "~%mark-next returning nil."))
           ,new-mark)))))

(defmacro nmark-next (mark-or-form)
  (let ((param (gensym)))
    `(let ((,param ,mark-or-form))
       (when ,param (mark-after ,param)))))

(defmacro mark-prev (mark-or-form)
  (let ((param (gensym))
        (new-mark (gensym)))
    `(let ((,param ,mark-or-form))
       (when ,param
         (let ((,new-mark (clone ,param)))
           (setq ,new-mark (mark-before ,new-mark))
           #+sax-debug (when (and *mark-prev-debug* (null ,new-mark))
                         (debug-out "~%mark-prev returning nil."))
           ,new-mark)))))

(defmacro nmark-prev (mark-or-form)
  (let ((param (gensym)))
    `(let ((,param ,mark-or-form))
       (when ,param (mark-before ,param)))))

;;; This does not cross lines
(defmacro mark-char (mark &optional offset)
  (if offset
    (let ((line (gensym))
          (line-length (gensym))
          (mark-charpos (gensym))
          (offset-position (gensym)))
      `(when ,mark
         (let* ((,line (mark-line ,mark))
                (,line-length (line-length ,line))
                (,mark-charpos (mark-charpos ,mark))
                (,offset-position (+ ,mark-charpos ,offset)))
           (cond ((and (<= 0 ,offset-position) ; offset can be negative
                       (< ,offset-position ,line-length))
                  (line-character ,line ,offset-position))
                 (t
                  nil)))))
      `(when ,mark
         (next-character ,mark))))

(defmacro mark-move (mark pos)
  (let ((new-mark (gensym)))
    `(when ,mark
       (let ((,new-mark (clone ,mark)))
         (move-to-position ,new-mark ,pos)))))

(defmacro nmark-move (mark pos)
  `(move-to-position ,mark ,pos))

(defmacro mark-line-start (mark)
  (let ((new-mark (gensym)))
    `(when ,mark 
       (let ((,new-mark (clone ,mark)))
         (line-start ,new-mark)))))

(defmacro mark-offset (mark offset)
  (let ((new-mark (gensym)))
    `(when ,mark
       (let ((,new-mark (clone ,mark)))
         (character-offset ,new-mark ,offset)))))

(defmacro nmark-offset (mark offset)
  `(when ,mark
     (character-offset ,mark ,offset)
     ,mark))

(defMacro mark-min (m1 m2) `(if (mark< ,m1 ,m2) ,m1 ,m2))

(defMacro mark-max (m1 m2) `(if (mark> ,m1 ,m2) ,m1 ,m2))

(defmacro buf-end-mark (&optional buffer) 
  `(clone (buffer-end-mark (if ,buffer ,buffer *buf*))))

(defmacro buf-start-mark (&optional buffer) 
  `(clone (buffer-start-mark (if ,buffer ,buffer *buf*))))

;;; ----------------------------------------------------------------------------
;;; Buffer functions and macros.
;;; ----------------------------------------------------------------------------
;;;
(defmacro buffer-empty-p () `(mark= (buffer-start-mark *buf*) (buffer-end-mark *buf*)))

(defun buffer-line-start (buffer &optional storage)
  (let ((line (mark-line (buffer-point buffer))))
    (cond (storage
           (setf (mark-line storage) line)
           (setf (mark-charpos storage) 0)
           storage)
          (
           (mark line 0)))))

(defun buffer-line-end (buffer &optional storage)
  (let ((line (mark-line (buffer-point buffer))))
    (cond (storage
           (setf (mark-line storage) line)
           (setf (mark-charpos storage) (line-length line)))
          (t
           (mark line (line-length line))))))

;;; ----------------------------------------------------------------------------
;;; Lisp syntax functions and macros.
;;; ----------------------------------------------------------------------------
;;;
(defmacro sexpr-end (start)
    (let ((sexpr-start (gensym))
          (sexpr-end (gensym)))
      `(when ,start
         (let* ((,sexpr-start (clone ,start))
                (,sexpr-end (when (hemlock::form-offset ,sexpr-start 1) ,sexpr-start)))
           (if ,sexpr-end
             ,sexpr-end
             #+sax-debug (when *sexpr-end-debug* 
                           (debug-out "~%sexpr-end returning nil - start-mark: ~S" ,start)))))))

(defmacro sexpr-start (pos)
  (let ((sexpr-start (gensym)))
    `(when ,pos
       (let ((,sexpr-start (clone ,pos)))
         (if (hemlock::form-offset ,sexpr-start -1) 
           ,sexpr-start
           #+sax-debug (when *sexpr-start-debug* 
                         (debug-out "~%sexpr-start returning nil - pos-mark: ~S" ,pos)))))))

(defmacro limited-sexpr-end (start limit)
  (let ((sexpr-start (gensym))
        (sexpr-end (gensym))) 
    `(when ,start
       #+sax-debug (when *limited-sexpr-end-debug* 
                     (debug-out "~%~%~S" 'limited-sexpr-end)
                     (debug-out "~%start: ~S" ,start)
                     (debug-out "~%limit: ~S" ,limit))
       (let* ((,sexpr-start (clone ,start))
              (,sexpr-end (when (hemlock::form-offset ,sexpr-start 1) ,sexpr-start)))
         #+sax-debug (when *limited-sexpr-end-debug*
                       (debug-out "~%sexpr-end: ~S" ,sexpr-end))
         (if ,sexpr-end
           (when (mark<= ,sexpr-end ,limit) ,sexpr-end)
           #+sax-debug (when *limited-sexpr-end-debug* 
                         (debug-out "~%limited-sexpr-end returning nil - start-mark: ~S" ,start)))))))

(defmacro next-sexpr-start (mark-or-form)
  (let ((position (gensym))
        (forward (gensym))
        (start (gensym))
        (param (gensym)))
    ;; evaluate mark-or-form once, only:
    `(let ((,param ,mark-or-form)) 
       (when ,param
         #+sax-debug (when *next-sexpr-start-debug*
                      (debug-out "~%next-sexpr-start mark-or-form: ~S" ,mark-or-form)
                      (debug-out "~%next-sexpr-start param: ~S" ,param))
         (do* ((,position (clone ,param))
               (,forward (when (hemlock::form-offset ,position 1) ,position)
                         (when (hemlock::form-offset ,position 1) ,position))
               (,start (when ,forward (when (hemlock::form-offset ,forward -1) ,forward))
                       (when ,forward (when (hemlock::form-offset ,forward -1) ,forward))))
              ((or (null ,start) (mark>= ,start ,param)) 
               #+sax-debug (when (and *next-sexpr-start-debug* (null ,start)) 
                            (debug-out "~%next-sexpr-start returning nil"))
               (if *inc-p*
                 (when (and ,start (mark< ,start *inc-pos*))
                   ,start)
                 ,start))
           #+sax-debug (when *next-sexpr-start-debug* (debug-out "~%start: ~S" ,start))
           (hemlock::form-offset ,position 1)
           #+sax-debug (when *next-sexpr-start-debug* (debug-out "~%(form-offset position 1): ~S" ,position))
           (cond ((null ,position) 
                  #+sax-debug (when *next-sexpr-start-debug* (debug-out "~%next-sexpr-start returning nil"))
                  (return nil))
                 ((mark<= ,position ,param)
                  ;; wretched special case: avoid getting stuck:  ie.  (eq ,errsym #.^#$ o )
                  #+sax-debug (when *next-sexpr-start-debug* (debug-out "~%next-sexpr-start returning (mark-next ,position)"))
                  (set-storage ,position ,param)
                  (return (mark-next ,position)))))))))

(defMacro nnext-sexpr-start (mark-or-form)
  (let ((position (gensym))
        (forward (gensym))
        (start (gensym))
        (param (gensym)))
    `(let ((,param ,mark-or-form))
       (when ,param
         #+sax-debug (when *nnext-sexpr-start-debug*
                      (debug-out "~%nnext-sexpr-start mark-or-form: ~S" ,mark-or-form)
                      (debug-out "~%nnext-sexpr-start param: ~S" ,param))
         (let* ((,position ,param)
                (,forward (when (hemlock::form-offset ,position 1) ,position))
                (,start (when ,forward (when (hemlock::form-offset ,forward -1) ,forward))))
           #+sax-debug (when *nnext-sexpr-start-debug* 
                        (if (null ,start)
                          (debug-out "~%nnext-sexpr-start returning nil")
                          (debug-out "~%nnext-sexpr-start returning: ~S" ,start)))
           (if *inc-p*
             (when (and ,start (mark< ,start *inc-pos*))
               ,start)
             ,start))))))

(defMacro atom-start (start)
  (let ((pos (gensym))
        (char (gensym))
        (buf-start (gensym)))
    `(when ,start
       (let ((,buf-start (buf-start-mark *buf*)))
         (do* ((,pos ,start (mark-before ,pos))
               (,char (when (and ,pos (mark>= ,pos ,buf-start))
                        (mark-char ,pos))
                      (when (and ,pos (mark>= ,pos ,buf-start))
                        (mark-char ,pos))))
              ((or (null ,char) ; ***
                   (whitespacep ,char) (char= ,char #\() 
                   (char= ,char #\)) (char= ,char #\"))
               (if ,pos (mark-after ,pos) ,buf-start)))))))

(defMacro atom-end (s)
  (let ((start (gensym))
        (buffer-end-mark (gensym))
        (pos (gensym))
        (char (gensym)))
    `(when ,s
       (let ((,start (clone ,s))
             (,buffer-end-mark (buffer-end-mark *buf*)))
         (do* ((,pos ,start (mark-after ,pos))
               (,char (when (mark<= ,pos ,buffer-end-mark) (mark-char ,pos))
                      (when (mark<= ,pos ,buffer-end-mark) (mark-char ,pos))))
              ((or (null ,char) ; ***
                   (whitespacep ,char) (char= ,char #\)) (char= ,char #\() 
                   (char= ,char #\") (char= ,char #\;)) 
               ,pos))))))

(defun buffer-top-level-sexpr-start (buffer &optional storage)
  (cond (storage
         (set-storage storage (buffer-point buffer))
         (hemlock::top-level-offset storage -1))
        (t
         (let ((mark (clone (buffer-point buffer))))
           (hemlock::top-level-offset mark -1)))))

(defun buffer-top-level-sexpr-end (buffer &optional storage)
  (cond (storage
         (set-storage storage (buffer-point buffer))
         (hemlock::top-level-offset storage 1))
        (t
         (let ((mark (clone (buffer-point buffer))))
           (hemlock::top-level-offset mark 1)))))


;;; ----------------------------------------------------------------------------
;;; Miscellaneous functions and macros.
;;; ----------------------------------------------------------------------------
;;;
(defun pattern-search (mark pattern &optional end)
  (with-mark ((m mark))
    (if end 
      (when (and (find-pattern m pattern) (mark< m end)) m)
      (when (find-pattern m pattern) m))))

#|
;;; (buffer-writable buffer) is broken
(defun writable-p (thing)
  (declare (ignore thing))
  t)

(defun writable-path-p (path)
  (let* ((file-manager (#/defaultManager ns:ns-file-manager))
         (path (ccl::%make-nsstring path)))
    (#/isWritableFileAtPath: file-manager path)))

(defMethod writable-p ((hemlock-view hi::hemlock-view))
  (let ((buffer (hemlock-view-buffer hemlock-view)))
    (or (not *style-case-p*)
        (format t "~%view-writable-p: ~S" (buffer-writable buffer))
        ;; *** broken
        (buffer-writable buffer))))

(defMethod writable-p ((text-view gui::hemlock-textstorage-text-view))
  (let* ((hemlock-view (gui::hemlock-view text-view))
         (buffer (hemlock-view-buffer hemlock-view)))
    (or (not *style-case-p*)
        (format t "~%writable-p: ~S" (buffer-writable buffer))
        (buffer-writable buffer))))

(defMethod writable-p ((window gui::hemlock-frame))
  (let* ((hemlock-view (gui::hemlock-view window))
         (buffer (hemlock-view-buffer hemlock-view)))
    (or (not *style-case-p*)
        (format t "~%writable-p: ~S" (buffer-writable buffer))
        (buffer-writable buffer))))
|#

(defun active-hemlock-window ()
  "Return the active hemlock-frame."
  (gui::first-window-satisfying-predicate 
   #'(lambda (w)
       (and (typep w 'gui::hemlock-frame)
            (not (typep w 'gui::hemlock-listener-frame))
            (#/isKeyWindow w)))))

(defun window-path (w)
  "Return the window's path."
  (let* ((pane (slot-value w 'gui::pane))
         (hemlock-view (when pane (gui::text-pane-hemlock-view pane)))
         (buffer (when hemlock-view (hi::hemlock-view-buffer hemlock-view))))
    (when buffer (hi::buffer-pathname buffer))))

(defmacro char-eolp (char) 
  `(member ,char '(#\return #\linefeed #\newline ,(code-char #x2028) ,(code-char #x2029))))

(defun ed-beep () (#_NSBeep)) ; *** this beeper doesn't beep

(define-symbol-macro *listener-output* (hemlock-ext::top-listener-output-stream))

(defun listener-msg (string &rest args)
  (apply 'format *listener-output* string args))

(defun selection-marks (text-view)
  (let ((selection (#/selectedRange text-view))
        start end)
    (when selection
      (let ((length (ns:ns-range-length selection))
            (location (ns:ns-range-location selection)))
        (unless (zerop length)
          (setf start (move-to-absolute-position (buf-start-mark) location))
          (setf end (character-offset (clone start) length)))))
    (values start end)))

(defun key-event= (k1 k2)
  (and (= (hi::key-event-keysym k1) (hi::key-event-keysym k2))
       (= (hi::key-event-bits k1) (hi::key-event-bits k2))))

(defmethod hemlock-update ((view hi:hemlock-view) start end &optional count)
  (let* ((buffer (hemlock-view-buffer view))
         (document (hi::buffer-document buffer))
         (text-storage (if document (slot-value document 'gui::textstorage)))
         (location (mark-absolute-position start))
         (length (or count (- (mark-absolute-position end) location))))
;         (count (hemlock::count-characters (region start end))))
    #+sax-debug (when *hemlock-update-debug*
                   (debug-out "~%~%~S" 'hemlock-update)
                   (debug-out "~%start: ~S" start)
                   (debug-out "~%end: ~S" end)
                   (debug-out "~%location: ~S" location)
                   (debug-out "~%length: ~S" length))
    ;;; 0 is the fontnum
    (gui::perform-edit-change-notification 
     text-storage
     (objc:@selector #/noteHemlockAttrChangeAtPosition:length:)
     location length 0)))        

(defmethod hemlock-update ((frame gui::hemlock-frame) start end &optional count)
  (let ((hemlock-view (gui::hemlock-view frame)))
    (hemlock-update hemlock-view start end count)))

(defMacro attribute-dictionary (var) `(car ,var)) 

(defMacro style-case (var) `(cdr ,var))

(defun set-style-attributes (dictionary &optional (start (buf-start-mark))
                                        (end (buf-end-mark)))
  #+sax-debug (when *set-style-attributes-debug* 
                 (debug-out "~%~%~S" 'set-style-attributes)
                 (debug-out "~%dictionary: ~S" dictionary)
                 (debug-out "~%start: ~S" start)
                 (debug-out "~%end: ~S" end))

  (ns:with-ns-range (range)
    (let* ((location (mark-absolute-position start))
           (length (- (mark-absolute-position end) location)))
      (setf (ns:ns-range-location range) location)
      (setf (ns:ns-range-length range) length)
      ;; Remove all temporary attributes from the character range
      (#/removeTemporaryAttribute:forCharacterRange:
       *layout* #&NSForegroundColorAttributeName range)
      (#/removeTemporaryAttribute:forCharacterRange:
       *layout* #&NSUnderlineStyleAttributeName range)
      (#/addTemporaryAttributes:forCharacterRange: *layout* dictionary range))))

(defun set-generic-text-style (text-view &optional (start (buf-start-mark)) (end (buf-end-mark)))
  ;; eliminate paren highlighting:
  (let* ((begin (mark-absolute-position start))
         (count (- (mark-absolute-position end) begin)))
    (when (and begin count)
      (ns:with-ns-range  (char-range begin count)
        (let* ((layout (#/layoutManager text-view)))
          (#/removeTemporaryAttribute:forCharacterRange: 
           layout #&NSBackgroundColorAttributeName 
           char-range)))))
  ;; *** maybe chuck this:
  (set-style-attributes  (attribute-dictionary *generic-text-style*) start end))

(defun downcase-region (start end)
  ;; downcases all nonescaped characters in region
  (filter-region #'string-downcase (region start end)))

(defun upcase-region (start end)
  (filter-region #'string-upcase (region start end)))

(defun capitalize-region (start end)
  (filter-region #'string-capitalize (region start end)))

(defMethod set-style-case ((case (eql :down)) start end)
  (downcase-region start end))

(defMethod set-style-case ((case (eql :up)) start end)
  ;; don't use eupcase region...
  (upcase-region start end))

(defMethod set-style-case ((case (eql :unchanged)) start end)
  (declare (ignore start end)) ())

(defMethod set-style-case ((case (eql :cap)) start end)
  (capitalize-region start end))

(defMethod set-style-case ((case (eql :cap3)) start end)
  (set-style-case :down start end)
  (capitalize-region (mark-offset start 3) (mark-offset start 4)))

(defMethod set-style-case ((case (eql :cap03)) start end)
  (set-style-case :down start end)
  (capitalize-region start end)
  (capitalize-region (mark-offset start 3) (mark-offset start 4)))

(defMethod set-style-case ((case (eql :cap8)) start end)
  (set-style-case :down start end)
  (capitalize-region (mark-offset start 8) (mark-offset start 9)))

(defMethod set-style-case ((case (eql :cap12)) start end)
  (set-style-case :down start end)
  (capitalize-region (mark-offset start 12) (mark-offset start 13)))

(defMacro style-region (style start end  &optional (set-case-p t))
  "This is the basic styling macro that calls SET-STYLE-ATTRIBUTES and SET-STYLE-CASE."
  `(progn
     #+sax-debug (when *style-region-debug* 
                  (debug-out "~%~%~S" 'style-region)
                  (debug-out "~%start: ~S" ,start)
                  (debug-out "~%end: ~S" ,end)
                  (debug-out "~%style: ~S" ,style)
                  (debug-out "~%set-case-p: ~S" ,set-case-p)
                  (debug-out "~%*paste-p*: ~S" *paste-p*)
                  (debug-out "~%*paste-start*: ~S" *paste-start*)
                  (debug-out "~%*paste-end*: ~S" *paste-end*)
                  (debug-out "~%*inc-p*: ~S" *inc-p*)
                  (debug-out "~%*inc-pos*: ~S" *inc-pos*))
     (when (or (and *inc-p* (not *paste-p*)
                    (mark>= *inc-pos* ,start)
                    (mark<= *inc-pos* ,end))
               (not *inc-p*)
               (and *paste-p*
                    (mark>= ,start *paste-start*)
                    (mark<= ,end *paste-end*)))

       (when (and *style-case-p* ,set-case-p (style-case ,style))
         #+sax-debug (when *style-region-debug*
                      (debug-out "~%set-style-case, case: ~S" (style-case ,style))
                      (debug-out "~%set-style-case, region: ~S" (region ,start ,end)))
           (set-style-case (style-case ,style) ,start ,end))

       (cond ((and *inc-p* (not *paste-p*))
              ;; Don't set attributes when doing incremental. We are
              ;; inside #/beginEditing, #/endEditing.  Save the values.
              #+sax-debug (when *style-region-debug* 
                            (debug-out "~%~%*** setting *form-style* for: ~S ***" 
                                       (region-to-string (region ,start ,end))))
              (setq *form-style* ,style
                    *form-start* ,start
                    *form-end* ,end))
             (t
              #+sax-debug (when *style-region-debug*
                             (if (equalp ,style *generic-text-style*)
                               (debug-out "~%*** styling-region-generically: ~S ***"
                                          (region-to-string (region ,start ,end)))
                               (debug-out "~%*** styling-region: ~S ***"
                                          (region-to-string (region ,start ,end))))
                             (debug-out "~%style: ~S" ,style))
              (set-style-attributes (attribute-dictionary ,style) ,start ,end))))))


