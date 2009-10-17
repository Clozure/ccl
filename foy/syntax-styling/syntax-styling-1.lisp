;;;-*- mode: lisp; package: (syntax-styling (cl ccl hemlock-internals)) -*-

;;; ****************************************************************************
;;; 
;;;      syntax-styling-1.lisp
;;;      
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod history, most recent first:
;;;      10/18/9   first cut.
;;;
;;; ****************************************************************************


(in-package "SAX")

;;; *** redefinition ***
(let ((text-view nil)
      (text-view-vscroll -100000))
  (defMethod gui::compute-temporary-attributes ((self gui::hemlock-textstorage-text-view))
    #+sax-debug (when *compute-temporary-attributes-debug* 
                   (debug-out "~%~%~S" 'compute-temporary-attributes)
                   (debug-out "~%*style-screen-p*: ~S" *style-screen-p*)
                   (debug-out "~%*style-top-level-form-p*: ~S" *style-top-level-form-p*)
                   (debug-out "~%*paste-p*: ~S" *paste-p*)
                   (debug-out "~%*paste-start*: ~S" *paste-start*)
                   (debug-out "~%*paste-end*: ~S" *paste-end*))
    (let ((current-vscroll (gui::text-view-vscroll self)))
      (when (or (not (equal self text-view))
                (not (= current-vscroll text-view-vscroll)))
        (when (and *styling-p* *style-screen-p* (not *paste-p*))
         (style-screen self)))
        (setq text-view self)
        (setq text-view-vscroll current-vscroll))
    (cond (*style-top-level-form-p* 
           (style-top-level-form self))
          (*paste-p* 
           (setq *paste-end* (sexpr-end *paste-start*))
           (yank-after (gui::hemlock-view self) *paste-start* *paste-end*)))
    (let* ((container (#/textContainer self))
           (layout (#/layoutManager container)))
      (when (eql #$YES (gui::text-view-paren-highlight-enabled self))
        (let* ((background #&NSBackgroundColorAttributeName)
               (paren-highlight-left (gui::text-view-paren-highlight-left-pos self))
               (paren-highlight-right (gui::text-view-paren-highlight-right-pos self))
               (paren-highlight-color (gui::text-view-paren-highlight-color self))
               (attrs (#/dictionaryWithObject:forKey: ns:ns-dictionary
                                                      paren-highlight-color
                                                      background)))
          (#/addTemporaryAttributes:forCharacterRange:
           layout attrs (ns:make-ns-range paren-highlight-left 1))
          (#/addTemporaryAttributes:forCharacterRange:
           layout attrs (ns:make-ns-range paren-highlight-right 1))))))

  (defun reset-text-view () (setq text-view nil)))

;;; *** Buffer-writable is broken
;;; *** Instead of doing all this stuff need the equivalent of:
;;; *** (setf ccl::*default-editor-class* 'derived-hemlock-frame-class)
#-list-definitions
(let ((writable-p t)
      (lisp-file-p t)
      (hemlock-frame nil))
  (objc:defMethod (#/becomeKeyWindow :void) ((w gui::hemlock-frame))
    (unless (equal w hemlock-frame)
      (let ((path (window-path w))
            (file-manager (#/defaultManager ns:ns-file-manager)))
        (setq writable-p 
              (if path
                (#/isWritableFileAtPath: file-manager (ccl::%make-nsstring path))
                t)) ; new files may not have a path yet.
        (setq lisp-file-p
              (if path
                (string-equal (pathname-type path) "lisp")
                t))) ; we assume a new file is a lisp file.
      (setq hemlock-frame w))
    (let ((become-key-function (find-symbol "BECOME-KEY-WINDOW" (find-package :ldefs))))
      (when become-key-function (funcall become-key-function w)))
    (call-next-method))
  (defun lisp-file-p () lisp-file-p)
  (defun writable-p () writable-p))

#+list-definitions
(let ((writable-p t)
      (lisp-file-p t)
      (hemlock-frame nil))
  (defMethod become-key-window ((w gui::hemlock-frame))
    (unless (equal w hemlock-frame)
      (let ((path (window-path w))
            (file-manager (#/defaultManager ns:ns-file-manager)))
        (setq writable-p 
              (if path
                (#/isWritableFileAtPath: file-manager (ccl::%make-nsstring path))
                t)) ; new files may not have a path yet.
        (setq lisp-file-p
              (if path
                (string-equal (pathname-type path) "lisp")
                t))) ; we assume a new file is a lisp file.
      (setq hemlock-frame w)))
  (defun lisp-file-p () lisp-file-p)
  (defun writable-p () writable-p))

(defun style-screen (text-view &optional generic-start generic-end)
  (when *styling-p*
    #+sax-debug (when *style-screen-debug* 
                  (debug-out "~%~%~S" 'style-screen)
                  (debug-out "~%*paste-start*: ~S" *paste-start*)
                  (debug-out "~%*paste-end*: ~S" *paste-end*))
    (let* ((container (#/textContainer text-view))
           (scrollview (#/enclosingScrollView text-view))
           (contentview (if (%null-ptr-p scrollview) text-view (#/contentView scrollview)))
           (rect (#/bounds contentview))
           (layout (#/layoutManager container))
           (glyph-range (#/glyphRangeForBoundingRect:inTextContainer:
                         layout rect container))
           (char-range (#/characterRangeForGlyphRange:actualGlyphRange:
                        layout glyph-range +null-ptr+))
           (window (#/window scrollview))
           (hemlock-view (gui::hemlock-view text-view))
           (current-buffer (hi::hemlock-view-buffer hemlock-view))
           (*buf* current-buffer)
           (hi::*current-buffer* *buf*)
           (*inc-pos* (clone (buffer-point *buf*)))
           (*layout* (#/layoutManager container))
           top-pos bot-pos start-mark end-mark)
      (unless (typep (#/window scrollview) 'gui::hemlock-listener-frame)
        (setq top-pos (ns:ns-range-location char-range))
        (setq bot-pos (+ top-pos (ns:ns-range-length char-range)))
        (setq start-mark (hemlock::top-level-offset (mark-offset (buf-start-mark current-buffer) top-pos) -1))
        (setq end-mark (hemlock::top-level-offset (mark-offset (buf-start-mark current-buffer)  bot-pos) 1))
        (when (null start-mark) (setq start-mark (buf-start-mark)))
        (when (null end-mark) (setq end-mark (buf-end-mark)))
        (when (and start-mark end-mark)
          (hemlock::parse-over-block (hemlock-internals::mark-line start-mark) 
                                     (hemlock-internals::mark-line end-mark))
          (if (and generic-start generic-end)
            (set-generic-text-style text-view generic-start generic-end)
            (set-generic-text-style text-view start-mark end-mark))
          (style-comments start-mark end-mark)
          (style-forms window :start start-mark :end end-mark :caps-p nil :toplevel-p t))))))

(defParameter *defstyle-hash-table* (make-hash-table :test 'equal))

(defun get-function (name)
  (gethash (string-upcase name) *defstyle-hash-table*))

(defun add-style (name-string func)
  (setf (gethash (string-upcase name-string) *defstyle-hash-table*) func))

(defun style-elements (symbol-start form-end &optional loop-p)
  "Step through the code sexpr by sexpr, styling appropriately."
  #+sax-debug (when *style-elements-debug* 
               (debug-out "~%~%~S" 'style-elements)
               (debug-out "~%element symbol-start: ~S" symbol-start)
               (debug-out "~%element form-end: ~S" form-end))
  (flet ((not-char-constant-p (element-start)
           (or (< (mark-charpos element-start) 2)
               (char/= (mark-char element-start -1) #\\)
               (char/= (mark-char element-start -2) #\#)))
         (check-dynamic-p (element-start element-end)
           (or (not *inc-p*)
               (and *inc-p*
                    (mark>= *inc-pos* element-start)
                    (mark<= *inc-pos* element-end))))
         (loop-keywd-p (string)
           ;; hash table?
           (member string
                   '("above" "across" "always" "and" "append" "appending" "by" "collect" "collecting" "count" 
                     "counting" "do" "doing" "downfrom" "downto" "each" "else" "end" "external-symbol" 
                     "external-symbols" "finally" "for" "from" "hash-key" "hash-keys" "hash-value"
                     "hash-values" "if" "in" "into" "initially" "loop-finish" "maximize maximizing" 
                     "minimize" "minimizing" "named" "nconc" "nconcing" "never" "of" "on" "present-symbol" 
                     "present-symbols" "repeat" "return" "sum" "summing" "symbol" "symbols" "the" "then" 
                     "thereis" "to" "unless" "until" "upfrom" "upto" "using" "when" "while" "with")
                   :test #'string-equal)))
    (do* ((element-start symbol-start
                         (when element-end (next-sexpr-start element-end)))
          (element-end (when element-start (sexpr-end element-start))
                       (when element-start (sexpr-end element-start)))
          (current-char (when element-start (mark-char element-start))
                        (when element-start (mark-char element-start))))
         ((or (null element-start) (null element-end) (mark>= element-start form-end)))
      #+sax-debug (when *style-elements-debug* 
                   (debug-out "~%element-start: ~S" element-start)
                   (debug-out "~%element-end: ~S" element-end))
      (when (or (not *segment-array*)
                (not-embedded-in-segment-p *segment-array* element-start))
        (when (or (char= current-char #\')
                  (char= current-char #\`)
                  (char= current-char #\,))
          (nmark-next element-start)
          (setf current-char (mark-char element-start)))
        (when (char= current-char #\@)
          (nmark-next element-start)
          (setf current-char (mark-char element-start)))
        (when (char= current-char #\')
          (nmark-next element-start)
          (setf current-char (mark-char element-start)))
        (when (char= current-char #\,)
          (nmark-next element-start)
          (setf current-char (mark-char element-start)))
        (cond ((and (char= current-char #\()
                    (not-char-constant-p element-start)
                    (check-dynamic-p element-start element-end))
               (rd-style-forms :start element-start :end element-end))
              ((and (char= current-char #\#)
                    (mark< element-start (mark-offset (buf-end-mark) -2))
                    (char= (mark-char element-start 1) #\')
                    (char= (mark-char element-start 2) #\()
                    (check-dynamic-p element-start element-end))
               (rd-style-forms :start (mark-offset element-start 2) :end element-end))
              ((and (char= current-char #\:)
                    (not-char-constant-p element-start))
               (style-region *keyword-package-style*
                             element-start (sexpr-end element-start)))
              ((and loop-p
                    (alpha-char-p current-char)
                    (loop-keywd-p (region-to-string (region element-start element-end))))
               (style-region *loop-keyword-style* 
                             element-start element-end)))))))

(defun backward-top-level-list (start)
  "Get the previous #\( in charpos 0, that is not embedded in a comment."
  #+sax-debug (when *backward-top-level-list-debug*
               (debug-out "~%~%~S" 'backward-top-level-list)
               (debug-out "~%start: ~S" start))
  (when (null start) (return-from backward-top-level-list nil))
  (do* ((next (pattern-search start *l-paren-backward-pattern*)
              (pattern-search (mark-prev next) *l-paren-backward-pattern*))
        not-embedded)
       ((null next) (return nil))
    #+sax-debug (when *backward-top-level-list-debug* 
                  (debug-out "~%next: ~S" next))
    (if *segment-array*
      (setf not-embedded (not-embedded-in-segment-p *segment-array* next))
      (setf not-embedded t))
    #+sax-debug (when *backward-top-level-list-debug* 
                  (debug-out "~%*segment-array*: ~S" *segment-array*)
                  (debug-out "~%not-embedded: ~S" not-embedded))
    (when (and (= (mark-charpos next) 0) not-embedded)
      (return next))))

(defun forward-top-level-list (start &optional (end (buf-end-mark)))
  "Get the next #\( in charpos 0, that is not embedded in a comment."
  #+sax-debug (when *forward-top-level-list-debug*
               (debug-out "~%~%~S" 'forward-top-level-list)
               (debug-out "~%start: ~S" start)
               (debug-out "~%end: ~S" end))
  (when (or (null start) (null end)) (return-from forward-top-level-list nil))
  (do* ((next (pattern-search start *l-paren-forward-pattern* end)
              (pattern-search (mark-next next) *l-paren-forward-pattern* end))
        not-embedded)
       ((null next) (return nil))
    #+sax-debug (when *forward-top-level-list-debug* 
                  (debug-out "~%next: ~S" next))
    (if *segment-array*
      (setf not-embedded (not-embedded-in-segment-p *segment-array* next))
      (setf not-embedded t))
    #+sax-debug (when *forward-top-level-list-debug* 
                  (debug-out "~%*segment-array*: ~S" *segment-array*)
                  (debug-out "~%not-embedded: ~S" not-embedded))
    (when (and (= (mark-charpos next) 0) not-embedded)
      (return next))))

;;; This will skip incomplete forms and continue with the next toplevel list.
(defun list-top-level-forms (&optional (start (buf-start-mark)) (end (buf-end-mark)))
  "Returns a list of starting marks for all the top-level lists in the range START, END."
   #+sax-debug (when *list-top-level-forms-debug* 
               (debug-out "~%~%~S" 'list-top-level-forms)
               (debug-out "~%start: ~S" start)
               (debug-out "~%end: ~S" end)) 
  (do* ((positions nil)
        (sexpr-start (forward-top-level-list start  end)
                     (when sexpr-end (forward-top-level-list sexpr-end end)))
        (sexpr-end (when sexpr-start (limited-sexpr-end sexpr-start end))
                   (when sexpr-start (limited-sexpr-end sexpr-start end))))
       ((or (null sexpr-start)
            (mark> sexpr-start end))
        (return (nreverse positions)))
    (cond (sexpr-end ; ie a complete list
           (push sexpr-start positions))
          (t ; an incomplete list - skip it
           (setq sexpr-end (mark-next sexpr-start))))))
  
(defun forward-list (start &optional (end (buf-end-mark)))
  "Get the next #\( that is not embedded in a comment and not a character constant."
  #+sax-debug (when *forward-list-debug*
               (debug-out "~%~%~S" 'forward-list)
               (debug-out "~%forward-list start: ~S" start)
               (debug-out "~%forward-list end: ~S" end))
  (when (or (null start) (null end)) (return-from forward-list nil))
  (do* ((next (pattern-search start *l-paren-forward-pattern* end)
              (pattern-search (mark-next next) *l-paren-forward-pattern* end))
        not-embedded)
       ((null next) (return nil))
    #+sax-debug (when *forward-list-debug* 
                 (debug-out "~%next: ~S" next))
    (if *segment-array*
      (setf not-embedded (not-embedded-in-segment-p *segment-array* next))
      (setf not-embedded t))
    #+sax-debug (when *forward-list-debug* 
                  (debug-out "~%*segment-array*: ~S" *segment-array*)
                  (debug-out "~%not-embedded: ~S" not-embedded))
    (cond ((>= (mark-charpos next) 2)
           #+sax-debug (when *forward-list-debug* 
                        (debug-out "~%(>= (mark-charpos next) 2)"))
           (when (and not-embedded
                      (not (and (eq (mark-char next -1) #\\)
                                (eq (mark-char next -2) #\#)))
                      (neq (mark-char next -1) #\#))
             #+sax-debug (when *forward-list-debug* 
                          (debug-out "~%returning: ~S" next))
             (return next)))
          (t 
           #+sax-debug (when *forward-list-debug* 
                        (debug-out "~%(< (mark-charpos next) 2)"))
           (when not-embedded 
             #+sax-debug (when *forward-list-debug* 
                          (debug-out "~%returning: ~S" next))
             (return next))))))

(defun list-forms (&optional (start (buf-start-mark)) (end (buf-end-mark)))
  "Returns a list of starting marks for all the lists in the range START, END."
  #+sax-debug (when *list-forms-debug* 
               (debug-out "~%~%~S" 'list-forms)
               (debug-out "~%start: ~S" start)
               (debug-out "~%end: ~S" end))
  (do* ((positions nil)
        (sexpr-start (forward-list start end)
                    (forward-list sexpr-end end))
        (sexpr-end (when sexpr-start (limited-sexpr-end sexpr-start end))
                   (when sexpr-start (limited-sexpr-end sexpr-start end)))
        (current-char (when sexpr-start (mark-char sexpr-start))
                      (when sexpr-start (mark-char sexpr-start))))
       ((or (null sexpr-end)
            (null sexpr-start)
            (mark> sexpr-start end))
        (return (nreverse positions)))
    #+sax-debug (when *list-forms-debug* 
                 (debug-out "~%sexpr-start: ~S" sexpr-start)
                 (debug-out "~%sexpr-end: ~S" sexpr-end)
                 (debug-out "~%*inc-pos*: ~S" *inc-pos*)
                 (debug-out "~%current-char: ~S" current-char))
    (when (or (char= current-char #\')
              (char= current-char #\`)
              (char= current-char #\,))
      (nmark-next sexpr-start) 
      (setf current-char (mark-char sexpr-start)))
    (when (char= current-char #\@)
      (nmark-next sexpr-start) 
      (setf current-char (mark-char sexpr-start)))
    (when (char= current-char #\')
      (nmark-next sexpr-start) 
      (setf current-char (mark-char sexpr-start)))
    (when (char= current-char #\,)
      (nmark-next sexpr-start) 
      (setf current-char (mark-char sexpr-start)))
    ;; when styling incrementally, only include forms 
    ;; if *inc-pos* is inside the form.
    (cond ((char= current-char #\()
           (when (or (not *inc-p*)
                     (and *inc-p*
                          (mark>= *inc-pos* sexpr-start)
                          (mark<= *inc-pos* sexpr-end)))
             #+sax-debug (when *list-forms-debug* 
                           (debug-out "~%pushing: ~S" (region-to-string (region sexpr-start sexpr-end))))
             (push sexpr-start positions)))
          ((char= current-char #\#)
           (cond ((and (mark< sexpr-start (buf-end-mark))
                       (char= (mark-char sexpr-start 1) #\')
                       (char= (mark-char sexpr-start 2) #\())
                  (when (or (not *inc-p*)
                            (and *inc-p*
                                 (mark>= *inc-pos* sexpr-start)
                                 (mark<= *inc-pos* sexpr-end)))
                    (push (nmark-next (nmark-next sexpr-start)) positions))))))))

(defun defstyle-form-styled-p (position)
  "If there is a defstyle form at POSITION, style it and return T.  If not, return NIL."
  (when position
    #+sax-debug (when *defstyle-form-styled-p-debug* 
                 (debug-out "~%~%~S" 'defstyle-form-styled-p)
                 (debug-out "~%defstyle position: ~S" position))
    (let* ((symbol-start (mark-next position)) ; skip paren
           (symbol-end (sexpr-end symbol-start))
           (string (region-to-string (region symbol-start symbol-end)))
           (styling-function (get-function string)))
      (when styling-function 
        (funcall styling-function position) 
        t))))

(defun package-form-styled-p (position)
  "If there is a :cl function at POSITION, style it and return T.  If not, return NIL."
  (when position
    #+sax-debug (when *package-form-styled-p-debug* 
                 (debug-out "~%~%~S" 'package-form-styled-p)
                 (debug-out "~%package position: ~S" position))
    (let* ((symbol-start (mark-next position))
           (symbol-end (sexpr-end symbol-start)))
      (cond ((char= (mark-char position) #\:)
             (style-region *keyword-package-style* symbol-start symbol-end) t)
            ((find-symbol (string-upcase (region-to-string (region symbol-start symbol-end))) :cl)
             (style-region *cl-package-style* symbol-start symbol-end)
             #+sax-debug (when *package-form-styled-p-debug* (debug-out "~%package styled"))
             t)))))

(defun rd-style-forms (&key (start (buf-start-mark)) (end (buf-end-mark)) top-level-p)
  "Style the buffer using a recursive descent algorithm, given the range START, END."
  #+sax-debug (when *rd-style-forms-debug* 
                 (debug-out "~%~%~S" 'rd-style-forms)
                 (debug-out "~%rd-style-forms start: ~S" start)
                 (debug-out "~%rd-style-forms end: ~S" end))
  (let ((positions (if top-level-p (list-top-level-forms start end) (list-forms start end)))
        form-end)
    #+sax-debug (when *rd-style-forms-debug* 
                 (debug-out "~%rd-style-forms positions: ~S" positions))
    (cond (positions 
           (dolist (position positions)
             #+sax-debug (when *rd-style-forms-debug* 
                           (debug-out "~%all positions: ~S" positions)
                           (debug-out "~%rd position list position: ~S" position))
             (unless (defstyle-form-styled-p position)
               (when (setf form-end (limited-sexpr-end position end))
                 (cond ((package-form-styled-p position)
                        #+sax-debug (when *rd-style-forms-debug* 
                                      (debug-out "~%rd position after package style: ~S" position))
                        (let* ((next (nmark-next position))
                               (end (sexpr-end next))
                               (next-start (next-sexpr-start end)))
                          #+sax-debug (when *rd-style-forms-debug* 
                                       (debug-out "~%next: ~S" next)
                                       (debug-out "~%end: ~S" end)
                                       (debug-out "~%next-start: ~S" next-start))
                          (setf position next-start))
                        #+sax-debug (when *rd-style-forms-debug* 
                                     (debug-out "~%rd position after next-sexpr: ~S" position)))
                       (t
                        (nmark-next position)))
                 (when position (style-elements position form-end))))))
          (t
           #+sax-debug (when *rd-style-forms-debug* 
                        (debug-out "~%No positions in rd positions list -- doing style-elements."))
           (style-elements (nmark-next start) end)))))

(defMethod style-top-level-form ((text-view gui::hemlock-textstorage-text-view))
  #+sax-debug (when *style-top-level-form-debug* 
                (debug-out  (format nil "~%~%~S" 'style-top-level-form)))
  (setq *style-top-level-form-p* nil)
  (let* ((hemlock-view (gui::hemlock-view text-view))
         (*buf* (hemlock-view-buffer hemlock-view))
         (hi::*current-buffer* *buf*)
         (*layout* (#/layoutManager (#/textContainer text-view)))
         (*current-package* (hemlock::buffer-package *buf*))
         (*style-case-p* (if (null *style-case-p*) nil (writable-p))))
    (cond ((not (buffer-empty-p))
           (let* ((start (backward-top-level-list (clone (buffer-point *buf*))))
                  (end (when start (clone start))))
             (when (and end (hemlock::form-offset end 1))
               #+sax-debug (when *style-top-level-form-debug* 
                             (debug-out  (format nil "~%start: ~S" start))
                             (debug-out  (format nil "~%end: ~S" end)))
               (hemlock::parse-over-block (mark-line start) (mark-line end))
               (set-generic-text-style text-view start end)
               (rd-style-forms :start start :end end :top-level-p t))))
          (t
           (ed-beep)))))

(defMethod style-forms ((hemlock-view hi::hemlock-view) &key (caps-p t) start end toplevel-p)
  (let* ((text-view (gui::text-pane-text-view (hi::hemlock-view-pane hemlock-view)))
         (*buf* (hemlock-view-buffer hemlock-view))
         (hi::*current-buffer* *buf*)
         (*layout* (#/layoutManager (#/textContainer text-view)))
         (*current-package* (hemlock::buffer-package *buf*))
         (*style-case-p* (if (null caps-p) nil *style-case-p*)))
    (cond ((not (buffer-empty-p))
           (unless (and start end)
             (multiple-value-setq (start end)
               (selection-marks text-view)))
           (unless (and start end)
             (setf start (buf-start-mark) end (buf-end-mark)))
           (hemlock::parse-over-block (mark-line start) (mark-line end))
           (rd-style-forms :start start :end end :top-level-p toplevel-p))
          (t
           (ed-beep)))))

(defMethod style-forms ((window gui::hemlock-frame) &key (caps-p t) start end toplevel-p)
  (style-forms (gui::hemlock-view window) :start start :end end :caps-p caps-p :toplevel-p toplevel-p))


;;; ----------------------------------------------------------------------------
;;; The batch styling interface:
;;; ----------------------------------------------------------------------------
;;;
(defMethod style-window ((window gui::hemlock-frame))
  (if (writable-p)
    (let* ((hemlock-view (gui::hemlock-view window))
           (text-view (gui::text-pane-text-view (hi::hemlock-view-pane hemlock-view)))
           (*buf* (hemlock-view-buffer hemlock-view))
           (hi::*current-buffer* *buf*)
           (*layout* (#/layoutManager (#/textContainer text-view)))
           (*current-package* (hemlock::buffer-package *buf*))
           ;; If a file is not writable, style with color and underlining, but not caps.
           (*style-case-p* (if (null *style-case-p*) nil (writable-p))))
      (multiple-value-bind (start end) (selection-marks text-view)
        (unless (and start end)
          (setf start (buf-start-mark) end (buf-end-mark)))   
        (hemlock::parse-over-block (mark-line start) (mark-line end))
        (set-generic-text-style text-view start end)
        (style-comments start end)     
        (style-forms window :start start :end end)))
    (listener-msg "~%~S is not writable." (window-path window))))

(defun style-folder-recursively ()
  (let ((dir (gui::cocoa-choose-directory-dialog)))
    (when dir
      (cond ((pathnamep dir)
             (listener-msg "~%~%~a files styled."
                           (style-folder (directory-namestring dir))))
            (t
             (listener-msg "~%~%~a files styled."
                           (style-folder dir)))))))

(defun style-folder (folder)
  (let ((files (directory (merge-pathnames folder "*.lisp") :files t :directories nil))
        (folders (directory (merge-pathnames folder "*") :files nil :directories t))
        (file-count 0))
    (dolist (file files)
      (listener-msg "~%;;; Styling: ~a" file)
      (incf file-count)
      (let* ((view (gui::cocoa-edit file))
             (window (#/window (hi::hemlock-view-pane view)))
             (buffer (hemlock-view-buffer view))
             (document (hi::buffer-document buffer)))
      (cond ((writable-p)
             (style-window window)
             (gui::save-hemlock-document document)
             (#/close window))
            (t
             (listener-msg "~%;;; File is read-only: ~S" file)))))
    (dolist (folder folders)
      (incf file-count (style-folder folder)))
    file-count))

(defun vanilla-style (buffer start end)
  ;; Set the font spec of the text to the default; but leave the capitalization
  ;; of strings, comments and various constants alone.
  (let ((buf-start (buf-start-mark buffer))
        (buf-end (buf-end-mark buffer))
        skip-list case)
    (hemlock::parse-over-block (mark-line start) (mark-line end))
    (set-style-attributes (attribute-dictionary *vanilla-styling*) start end)
    ;; *** this should use start and end
    (setf skip-list (get-combined-segment-list))
    (setf case (style-case *vanilla-styling*))
    ;; (pprint skip-list)
    (cond (skip-list
           (do* ((segment (pop skip-list) (pop skip-list))
                 (seg-start buf-start next-start)
                 (seg-end (first segment) (first segment))
                 (next-start (second segment) (second segment)))
                ((or (mark>= seg-start end)
                     (null seg-start)
                     (null seg-end)))
             (when (and (mark>= seg-start start)
                        (mark<= seg-start end))
               (cond ((eql case :up)
                      (upcase-region seg-start (mark-min seg-end end)))
                     ((eql case :down)
                      (downcase-region seg-start (mark-min seg-end end)))))))
          (t 
           (cond ((eql case :up)
                  (upcase-region buf-start buf-end))
                 ((eql case :down)
                  (downcase-region buf-start buf-end)))))))

(defMethod style-vanilla ((window gui::hemlock-frame))
  (let* ((hemlock-view (gui::hemlock-view window))
         (text-view (gui::text-pane-text-view (hi::hemlock-view-pane hemlock-view)))
         (*layout* (#/layoutManager (#/textContainer text-view)))
         (*buf* (hemlock-view-buffer hemlock-view))
         (hi::*current-buffer* *buf*))
    (cond ((writable-p)
           (multiple-value-bind (start end) (selection-marks text-view)
             (unless (and start end)
               (setf start (buf-start-mark) end (buf-end-mark)))
             (vanilla-style *buf* start end)))
          (t
           (listener-msg "~%;;; File is read-only: ~S" (window-path window))))))

;;; ----------------------------------------------------------------------------
;;; The interface for the incremental algorithm:
;;; ----------------------------------------------------------------------------
;;;
(defConstant %inserted-parens% 37)

(defun dynamically-style-buffer (hemlock-view)
  (let* ((*inc-p* t)
         (*buf* (hemlock-view-buffer hemlock-view))
         (*form-style* nil)
         (*form-start* nil)
         (*form-end* nil)
         (*superparen-closure* nil)
         (*segment-array* nil)
         (hi::*current-buffer* *buf*)
         (text-view (gui::text-pane-text-view (hi::hemlock-view-pane hemlock-view)))
         (*layout* (#/layoutManager (#/textContainer text-view)))
         (*inc-pos* (clone (buffer-point *buf*)))
         (comment-end (or (buffer-top-level-sexpr-end *buf*) (buf-end-mark)))
         (atom-start (atom-start (mark-prev *inc-pos*))) ; *** ?
         (atom-end (or (atom-end *inc-pos*) *inc-pos*))
         (char (mark-char (mark-max (buf-start-mark) (or (mark-prev *inc-pos*) *inc-pos*))))
         (*style-case-p* (if (null *style-case-p*) nil (writable-p)))
         style-end)
    (when char
      #+sax-debug (when *dynamically-style-buffer-debug* 
                     (debug-out "~%~%~S" 'dynamically-style-buffer)
                     (debug-out "~%*inc-pos*: ~S" *inc-pos*)
                     (debug-out "~%char: ~S" char)
                     (debug-out "~%atom-start: ~s" atom-start)
                     (debug-out "~%atom-end: ~s" atom-end))
      (cond ((and (char= char #\#)
                  (char= (mark-char (mark-max (buf-start-mark) (mark-offset *inc-pos* -2))) #\|))
             ;; *** could do better than buf-start-mark
             (style-comments (buf-start-mark) comment-end))
            (t
             (multiple-value-bind (start inside-quotes-p semi-colon-pos)
                                  (calculate-context char)
               #+sax-debug (when *dynamically-style-buffer-debug* 
                              (debug-out "~%~%start: ~S" start)
                              (debug-out "~%inside-quotes-p: ~S" inside-quotes-p)
                              (debug-out "~%semi-colon-pos: ~S" semi-colon-pos))
               (unless start (setq start (buf-start-mark)))
               (dynamically-style-comments start comment-end t t)
               (when (or inside-quotes-p
                         (and (char= char #\") (not inside-quotes-p)))
                 #+sax-debug (when *dynamically-style-buffer-debug* 
                                (debug-out "~%start: ~S" start)
                                (debug-out "~%comment-end: ~S" comment-end))
                 (return-from dynamically-style-buffer (values atom-start atom-end)))
               (cond (semi-colon-pos
                      (let ((line-end (line-end (clone semi-colon-pos))))
                        (when line-end
                          ;; eliminate paren highlighting:
                          (let* ((begin (mark-absolute-position start))
                                 (count (- (mark-absolute-position line-end) begin)))
                            (when (and begin count)
                              (ns:with-ns-range  (char-range begin count)
                                (let* ((layout (#/layoutManager text-view)))
                                  (#/removeTemporaryAttribute:forCharacterRange: 
                                   layout #&NSBackgroundColorAttributeName 
                                   char-range)))))
                          (set-style-attributes (attribute-dictionary *semi-colon-comment-style*) 
                                                semi-colon-pos line-end))))
                     (t
                      (unwind-protect
                          (progn
                            (#/beginEditing (#/textStorage text-view))
                            (insert-string (copy-mark atom-end) " o ))))))))))))))))))))))))))))))))))")
                            (setf style-end (mark-offset (copy-mark atom-end) %inserted-parens%))
                            (hemlock::parse-over-block (hi::mark-line start) (hi::mark-line style-end))
                            (rd-style-forms :start start :end style-end)
                            (unless (or *form-style* *paste-p* (member char '(#\( #\) #\" #\space #\;)))
                              (when atom-start
                                (setq *form-style* *generic-text-style*
                                      *form-start* atom-start
                                      *form-end* atom-end))))
                        (delete-characters atom-end %inserted-parens%)
                        (#/endEditing (#/textStorage text-view))
                        (when *form-style*
                          (set-style-attributes (attribute-dictionary *form-style*) *form-start* *form-end*))
                        (when *superparen-closure* 
                          (funcall *superparen-closure*))
                        ;; Setting attributes for a region leaves point at the end 
                        ;; of the symbol.  Move it back, unless editing there:
                        (let ((point (buffer-point *buf*)))
                          (when (not (mark= point *inc-pos*))
                            (let ((offset (- (mark-charpos point) (mark-charpos *inc-pos*))))
                              (dotimes (count offset)
                                ;; a less moronic way to do this??
                                (hi::handle-hemlock-event hemlock-view %backward-char-event%)))))))))))
      (values atom-start atom-end))))

(defun calculate-context (new-char)
  "Calculate top-level-start-pos inside-quotes-p semi-colon-pos"
  #+sax-debug (when *calculate-context-debug* 
                 (debug-out "~%~%~S" 'calculate-context)
                 (debug-out "~%new-char: ~S" new-char)
                 (debug-out "~%*inc-pos*: ~S" *inc-pos*)
                 (debug-out "~%point: ~S" (buffer-point *buf*))
                 (debug-out "~%(mark-char point): ~S" (mark-char (buffer-point *buf*))))
  (let* ((point (clone (buffer-point *buf*)))
         (right-quote-pos (when (char= new-char #\") (clone point)))
         top-level-start-pos inside-quotes-p semi-colon-pos left-quote-pos)
    (flet ((return-even-quote-values ()
             (when (and right-quote-pos left-quote-pos semi-colon-pos)
               ;; mark< is not trinary
               (when (and (mark< left-quote-pos semi-colon-pos)
                          (mark< semi-colon-pos right-quote-pos))
                 (setq semi-colon-pos nil)))
             (return-from calculate-context
                          (values top-level-start-pos inside-quotes-p semi-colon-pos)))
           (return-odd-quote-values ()
             (when (and semi-colon-pos left-quote-pos)
               (cond ((mark< left-quote-pos semi-colon-pos)
                      (setq semi-colon-pos nil))
                     (t
                      (setq inside-quotes-p nil))))
             (return-from calculate-context
                          (values top-level-start-pos inside-quotes-p semi-colon-pos))))
      (do* ((buf-start (buf-start-mark))
            (pos (or (mark-prev *inc-pos*) buf-start))
            (char (mark-char pos) (mark-char pos))
            (char-1 (mark-char (mark-max (or (mark-prev pos) pos) buf-start))
                    (mark-char (mark-max (or (mark-prev pos) pos) buf-start)))
            (first-char-p t nil)
            (quote-count 0)
            line-start-p)
           ((and char char-1 (char= char #\() (or (char-eolp char-1) (mark= pos buf-start)))
            (setq top-level-start-pos pos)
            #+sax-debug (when *calculate-context-debug* 
                           (debug-out "~%quote-count: ~S" quote-count))
            (cond ((= (mod quote-count 2) 0) ; even quotes
                   (setf inside-quotes-p nil)
                   (return-even-quote-values))
                  (t
                   (setf inside-quotes-p t)
                   (return-odd-quote-values))))
        (cond ((null char)
               (setq semi-colon-pos nil))
              ((and (char-eolp char) (not first-char-p))
               (setq line-start-p t))
              ((and (char= char #\;) (not line-start-p) (not (char= char-1 #\\)))
               (setq semi-colon-pos pos))
              ((and (char= char #\") (not (char= char-1 #\\)))
               (incf quote-count)
               (unless right-quote-pos (setq right-quote-pos pos))
               (setq left-quote-pos pos)))
        (setq pos (mark-prev pos))
        (when (null pos)
          (setq top-level-start-pos nil)
          (cond ((= (mod quote-count 2) 0) 
                 (setq inside-quotes-p nil)
                 #+sax-debug (when *calculate-context-debug* 
                                (debug-out "~%inside-quotes-p is nil"))
                 (return-even-quote-values))
                (t
                 (setq inside-quotes-p t)
                 #+sax-debug (when *calculate-context-debug* 
                                (debug-out "~%inside-quotes-p: t"))
                 (return-odd-quote-values))))))))

;;; *** This need work:
(defun char-printable-p (char event)
  "Is the char printable?"
  (let ((code (char-code char)))
    #+sax-debug (when *char-printable-p-debug* 
                 (debug-out "~%~%~S" 'char-printable-p)
                 (debug-out "~%char: ~s" char)
                 ;; (hi::print-pretty-key-event (hi::char-key-event char) t t)
                 (debug-out "~%code: ~s" code))
    (let ((control-key-p (hi::key-event-bit-p event "Control"))
          (option-key-p (hi::key-event-bit-p event "Meta")))
      #+sax-debug
      (when *automated-testing-p*
        (setq control-key-p nil
              option-key-p nil))
      #+sax-debug (when *char-printable-p-debug* (debug-out "~%control-key-p: ~s" control-key-p))
      #+sax-debug (when *char-printable-p-debug* (debug-out "~%option-key-p: ~s" option-key-p))
      (cond ((not (or control-key-p option-key-p))
             (when (or (and (>= code 32) (<= code 127)) ; this is the primary case
                       ;; *** define constants
                       (= code 13) ; #\newline
                       (= code 8)  ; #\delete, #\backspace
                       (= code 10) ; $\linefeed
                       (= code 127)) ; #\del
               #+sax-debug (when *char-printable-p-debug* (debug-out "~%printable1"))
               t))
            #+elvis
            ((and control-key-p option-key-p) 
             #+sax-debug (when *char-printable-p-debug* (debug-out "~%printable2"))
             (when (or (= code 8)) ; control-meta-h & control-meta-delete ****
               t))
            (control-key-p
             (when (or (= code 100) ; control-d
                       ;; (= code 4) ; *** ?
                       (= code 11) ; control-k
                       (= code 23)) ; control-w
               #+sax-debug (when *char-printable-p-debug* (debug-out "~%printable3"))
               t))
            (option-key-p
             (when (or (= code 182) ; meta-d
                       (= code 202) ; meta-space ?? ***
                       (= code 199)) ; meta-\ ?? ***
               #+sax-debug (when *char-printable-p-debug* (debug-out "~%printable4"))
               t))
            (t nil)))))

(defun restyle-comment (view)
  #+sax-debug (when *handle-hemlock-event-debug* (debug-out "~%restyle-comment-p"))
  (let* ((line-start (buffer-line-start *buf*))
         (line-end (buffer-line-end *buf*))
         (hi::*current-buffer* *buf*)
         (*current-package* (hemlock::buffer-package *buf*))
         (text-view (gui::text-pane-text-view (hi::hemlock-view-pane view)))
         (*layout* (#/layoutManager (#/textContainer text-view))))
    (when (and line-start line-end)
      (style-region *generic-text-style* line-start line-end nil)
      (style-comments line-start line-end)
      (style-forms view :start line-start :end line-end))))

;;; *** redefinition ***
(defMethod hi::handle-hemlock-event :around ((view hi:hemlock-view) event)
  (let* ((*buf* (hemlock-view-buffer view))
         (hi::*current-buffer* *buf*)
         (*inc-pos* nil)
         (*paste-p* nil)
         (*paste-start* nil)
         (*paste-end* nil)
         (keysym (when (typep event 'hi::key-event)
                   (hi::key-event-keysym event)))
         (keysym-code (when keysym (hi::code-for-keysym keysym))))
    (cond ((and keysym *styling-p* (not (keywordp  keysym)) ; char can be :end, :home, etc
                (not (hi::buffer-minor-mode *buf* "I-Search")))
           (let ((char (code-char keysym-code)))
           #+sax-debug (when *handle-hemlock-event-debug* 
                        (debug-out "~%~%~S" 'handle-hemlock-event)
                        (debug-out "~%char: ~S" char))
             (when (key-event= event %control-y%)
               #+sax-debug (when *handle-hemlock-event-debug* 
                            (debug-out "~%*paste-start*: ~S" (clone (buffer-point *buf*))))
               (setq *paste-p* t)
               (setq *paste-start* (clone (buffer-point *buf*))))
             (when (key-event= event %control-j%)
               (setq *style-top-level-form-p* t))
             (if (and char (not (typep (#/window (hi::hemlock-view-pane view)) 'gui::hemlock-listener-frame))
                      (char-printable-p char event) (lisp-file-p))
               (let* ((point (buffer-point *buf*))
                      (point-char (mark-char point))
                      (char-1 (mark-char (mark-max (or (mark-prev point) point) (buffer-start-mark *buf*))))
                      (*style-screen-p* nil)
                      ;; If a file is not writable, style with color and underlining, but not caps.
                      (*style-case-p* (if (null *style-case-p*) nil (writable-p)))
                      restyle-comment-p)
                 #+sax-debug (when *handle-hemlock-event-debug* 
                              (debug-out "~%point: ~S" point)
                              (debug-out "~%point-char: ~S" point-char)
                              (debug-out "~%char-1: ~S" char-1))
                 (cond ((and (key-event= event %backspace%) ; backspace & delete
                             char-1
                             (char= char-1 #\;))
                        (setf restyle-comment-p t))
                       ((and point-char (char= point-char #\;)
                             ;; (or (key-event= event %del%) ; #\del
                             (key-event= event %control-d%)) ; control-d                             
                        (setf restyle-comment-p t)))

                 ;; insert the char:
                 #+sax-debug (when *handle-hemlock-event-debug* 
                                (debug-out "~%~%inserting char: ~S" char))
                 (ccl::with-autorelease-pool
                     (call-next-method view event))
                 #+sax-debug (when *handle-hemlock-event-debug* 
                                (debug-out "~%~%char inserted"))

                 (cond (restyle-comment-p
                        (restyle-comment view))
                       (t 
                        (dynamically-style-buffer view))))
               (ccl::with-autorelease-pool
                 #+sax-debug (when *handle-hemlock-event-debug* 
                              (debug-out "~%~%not styled -- calling next method."))
                 (call-next-method view event)
                 (cond ((key-event= event %control-y%)
                        #+sax-debug (when *handle-hemlock-event-debug* 
                                      (debug-out "~%setting *paste-end*: ~S" (clone (buffer-point *buf*))))
                        (setq *paste-end* (clone (buffer-point *buf*)))))))))
                       ; (yank-after view *paste-start* *paste-end*)))))))
                       ; ((key-event= event %control-meta-q%)
                        ; (indentation-after view)))))))
          (t
           (ccl::with-autorelease-pool
               (call-next-method view event))))))

;;; Neither of these two are right.  See the note below.
(objc:defMethod (#/paste: :void) ((text-view gui::hemlock-text-view) (sender :id))
  (reset-text-view)
  (call-next-method sender))
  
(defMethod yank-after ((view  hi::hemlock-view) generic-start generic-end)
  (let ((text-view (gui::text-pane-text-view (hi::hemlock-view-pane view))))
    (hi::handle-hemlock-event view #'(lambda () (style-screen text-view generic-start generic-end)))))

#|
;;; This is the right way to do paste and yank, but the text
;;; is being set back to plain by some disagreeable and as yet
;;; unidentified function. (Cocoa??)
(defMethod yank-after ((view  hi::hemlock-view) generic-start generic-end)
  (when (and *styling-p* (lisp-file-p)
             (not (typep (#/window (hi::hemlock-view-pane view)) 'gui::hemlock-listener-frame)))
    (let* ((text-view (gui::text-pane-text-view (hi::hemlock-view-pane view)))
           (*buf* (hi::hemlock-view-buffer view))
           (hi::*current-buffer* *buf*)
           (*layout* (#/layoutManager (#/textContainer text-view)))
           (*current-package* (hemlock::buffer-package *buf*))
           (start (buffer-top-level-sexpr-start *buf*))
           (end (buffer-point *buf*))
           (*style-screen-p* nil)
           ;; If a file is not writable, style with color and underlining, but not caps.
           (*style-case-p* (if (null *style-case-p*) nil (writable-p))))
      #+sax-debug (when *yank-after-debug* 
                    (debug-out "~%~%~S" 'yank-or-paste-after)
                    (debug-out "~%start: ~S" start)
                    (debug-out "~%end: ~S" end)
                    (debug-out "~%*inc-pos*: ~S" *inc-pos*))
      ;; *paste-p*, *paste-start* and *paste-end* are set above.
      (when (and start end)
        (hemlock::parse-over-block (hemlock-internals::mark-line start) 
                                   (hemlock-internals::mark-line end))
        (set-generic-text-style text-view generic-start generic-end) 
        (dynamically-style-comments start end t t)
        (dynamically-style-buffer view))
      (setq *paste-p* nil *paste-start* nil *paste-end* nil))))
      ;; (gui::update-paren-highlight text-view))))
|#

;;; ----------------------------------------------------------------------------
;;; styling menu items
;;; ----------------------------------------------------------------------------
;;;
(defParameter *edit-menu* 
  (#/submenu (#/itemWithTitle: (#/mainMenu (ccl::application-ui-object ccl::*application*)) #@"Edit")))

(objc:defMethod (#/toggleStylingAction: :void) ((item ns:ns-menu-item) (sender :id))
  (cond (*styling-p* 
         (setq *styling-p* nil)
         (#/setState: sender #$NSOffState))
        (t
         (setq *styling-p* t)
         (#/setState: sender #$NSOnState))))

(let ((styling-item (#/itemWithTitle: *edit-menu* #@"Styling"))
      item)
  (unless (%null-ptr-p styling-item) (#/removeItem: *edit-menu* styling-item))
  (when (%null-ptr-p styling-item)
    (#/addItem: *edit-menu* (#/separatorItem ns:ns-menu-item))
    (setf item (#/initWithTitle:action:keyEquivalent: (#/alloc ns:ns-menu-item)
                                                      #@"Syntax Styling"
                                                      (ccl::@selector "toggleStylingAction:")
                                                      #@""))
    (#/setTarget: item item)
    (#/setState: item #$NSOnState)
    (#/addItem: *edit-menu* item)))



(when *style-case-p*

(defParameter *style-file-item* nil)
(defParameter *style-file-vanilla-item* nil)

(defClass STYLING-MENU-ITEM (ns:ns-menu-item)
  ()
  (:metaclass ns:+ns-object))

(objc:defMethod (#/styleFileAction: :void) ((item styling-menu-item) (sender :id))
  (declare (ignore sender))
  (let ((window (active-hemlock-window)))
    (when window
      (style-window window))))

(objc:defMethod (#/styleFileVanillaAction: :void) ((item styling-menu-item) (sender :id))
  (declare (ignore sender))
  (let ((window (active-hemlock-window)))
    (when window
      (style-vanilla window)))) 

(objc:defMethod (#/styleFolderAction: :void) ((item styling-menu-item) (sender :id))
  (declare (ignore sender))
  (style-folder-recursively))

(objc:defMethod (#/validateMenuItem: :<BOOL>) ((item styling-menu-item) item)
  *styling-p*)

(let ((style-file-item (#/itemWithTitle: *edit-menu* #@"Style File"))
      item)
  (when (%null-ptr-p style-file-item)
    (setf item (#/initWithTitle:action:keyEquivalent: (#/alloc styling-menu-item)
                                                      #@"Style File"
                                                      (ccl::@selector "styleFileAction:")
                                                      #@"u"))
    (#/setTarget: item item)
    (setq *style-file-item* item)
    (#/addItem: *edit-menu* item)))

(let ((style-file-vanilla-item (#/itemWithTitle: *edit-menu* #@"Style File Vanilla"))
      item)
  (when (%null-ptr-p style-file-vanilla-item)
    (setf item (#/initWithTitle:action:keyEquivalent: (#/alloc styling-menu-item)
                                                      #@"Style File Vanilla"
                                                      (ccl::@selector "styleFileVanillaAction:")
                                                      #@"U"))
    (#/setTarget: item item)
    (setq *style-file-vanilla-item* item)
    (#/addItem: *edit-menu* item)))

(let ((style-folder-item (#/itemWithTitle: *edit-menu* #@"Style Folder ..."))
      item)
  (when (%null-ptr-p style-folder-item)
    (setf item (#/initWithTitle:action:keyEquivalent: (#/alloc styling-menu-item)
                                                      #@"Style Folder ..."
                                                      (ccl::@selector "styleFolderAction:")
                                                      #@""))
    (#/setTarget: item item)
    (#/addItem: *edit-menu* item)))

) ; closing paren for when



