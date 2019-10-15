;;;-*-Mode: LISP; Package: LIST-DEFINITIONS -*-

;;; ----------------------------------------------------------------------------
;;;
;;;      list-definitions.lisp
;;;
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      This code adds a dynamic contextual popup menu to Hemlock.
;;;
;;;      Right-Click produces an alphabetized listing of the file's definitions.  
;;;      Command-Right-Click produces a positional listing.
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History, most recent first:
;;;      9/19/9  Added parse-over-block to list-definitions.
;;;      8/17/9  Added position history list and file history list.
;;;      8/12/9  Numerous interface suggestions, Alexander Repenning.
;;;      8/10/9  First cut.
;;;
;;; ----------------------------------------------------------------------------


(defpackage "LIST-DEFINITIONS" (:nicknames "LDEFS") (:use :cl :ccl))
(in-package "LIST-DEFINITIONS")

(defParameter *objc-defmethod-search-pattern* (hi::new-search-pattern :string-insensitive :forward "(objc:defmethod"))
(defParameter *def-search-pattern* (hi::new-search-pattern :string-insensitive :forward "(def"))
(defParameter *left-paren-search-pattern* (hi::new-search-pattern :character :forward #\())
(defParameter *colon-search-pattern* (hi::new-search-pattern :character :forward #\:))
(defParameter *slash-search-pattern* (hi::new-search-pattern :character :forward #\/))

(defVar *position-history-list* nil "The position-history-list instance.")
(defVar *file-history-list* nil "The file-history-list instance.")

(defmacro clone (mark) `(hi::copy-mark ,mark :temporary))

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

;;; ----------------------------------------------------------------------------
;;; 
(defclass list-definitions-menu (ns:ns-menu)
  ((text-view :initarg :menu-text-view :reader menu-text-view)
   (path :initarg :menu-path :reader menu-path))
  (:documentation "The definitions popup menu.")
  (:metaclass ns:+ns-object))

(objc:defmethod (#/listDefinitionsAction: :void) ((m list-definitions-menu) (sender :id))
  (display-position (menu-text-view m) (item-mark sender))
  (maybe-add-history-entry *position-history-list* (item-info sender) (menu-path m)))

(defun display-position (text-view mark)
  "Display the position of MARK in TEXT-VIEW."
  (let* ((def-pos (hi::mark-absolute-position mark))
         (def-end-pos (let ((temp-mark (clone mark)))
                        (when (hemlock::form-offset temp-mark 1)
                          (hi::mark-absolute-position temp-mark)))))
    (unless def-end-pos (when def-pos (setq def-end-pos (1+ def-pos))))
    (when (and def-pos def-end-pos)
      (ns:with-ns-range (range def-pos (- def-end-pos def-pos))
        (#/scrollRangeToVisible: text-view range))
      (hi::move-mark (hi::buffer-point (gui::hemlock-buffer text-view)) mark)
      (gui::update-paren-highlight text-view))))

;;; ----------------------------------------------------------------------------
;;; 
(defclass list-definitions-menu-item (ns:ns-menu-item)
  ((mark :accessor item-mark)
   (path :accessor item-path)
   (info :accessor item-info))
  (:documentation "Support for the definitions list menu.")
  (:metaclass ns:+ns-object))

(defparameter *dark-blue-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.2 0.2 0.5 1.0))
(defparameter *dark-green-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.0 0.3 0.1 1.0))
(defparameter *dark-gray-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.1 0.1 0.1 1.0))
(defparameter *dark-brown-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.3 0.05 0.0 1.0))
(defparameter *dark-turquoise-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.0 0.2 0.3 1.0))
(defparameter *wine-red-color* (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color 0.4 0.1 0.2 1.0))

(defparameter *generic-dictionary* (make-instance 'ns:ns-mutable-dictionary :with-capacity 2))
(#/setObject:forKey: *generic-dictionary* (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)) #&NSFontAttributeName)
(#/setObject:forKey: *generic-dictionary* *dark-gray-color* #&NSForegroundColorAttributeName)

(defparameter *file-history-dictionary* (make-instance 'ns:ns-mutable-dictionary :with-capacity 2))
(#/setObject:forKey: *file-history-dictionary* (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)) #&NSFontAttributeName)
(#/setObject:forKey: *file-history-dictionary* *dark-blue-color* #&NSForegroundColorAttributeName)

(defparameter *defclass-dictionary* (make-instance 'ns:ns-mutable-dictionary :with-capacity 3))
(#/setObject:forKey: *defclass-dictionary* (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)) #&NSFontAttributeName)
(#/setObject:forKey: *defclass-dictionary* *wine-red-color* #&NSForegroundColorAttributeName)
(#/setObject:forKey: *defclass-dictionary* (#/numberWithInt: ns:ns-number 1) #&NSUnderlineStyleAttributeName)

(defparameter *defstruct-dictionary* (make-instance 'ns:ns-mutable-dictionary :with-capacity 3))
(#/setObject:forKey: *defstruct-dictionary* (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)) #&NSFontAttributeName)
(#/setObject:forKey: *defstruct-dictionary* *dark-turquoise-color* #&NSForegroundColorAttributeName)
(#/setObject:forKey: *defstruct-dictionary* (#/numberWithInt: ns:ns-number 1) #&NSUnderlineStyleAttributeName)

(defparameter *defmethod-dictionary* (make-instance 'ns:ns-mutable-dictionary :with-capacity 2))
(#/setObject:forKey: *defmethod-dictionary* (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)) #&NSFontAttributeName)
(#/setObject:forKey: *defmethod-dictionary* *dark-blue-color* #&NSForegroundColorAttributeName)

(defparameter *defun-dictionary* (make-instance 'ns:ns-mutable-dictionary :with-capacity 2))
(#/setObject:forKey: *defun-dictionary* (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)) #&NSFontAttributeName)
(#/setObject:forKey: *defun-dictionary* *dark-green-color* #&NSForegroundColorAttributeName)

(defparameter *defmacro-dictionary* (make-instance 'ns:ns-mutable-dictionary :with-capacity 2))
(#/setObject:forKey: *defmacro-dictionary* (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)) #&NSFontAttributeName)
(#/setObject:forKey: *defmacro-dictionary* *dark-brown-color* #&NSForegroundColorAttributeName)

(defparameter *objc-dictionary* (make-instance 'ns:ns-mutable-dictionary :with-capacity 2))
(#/setObject:forKey: *objc-dictionary* (#/systemFontOfSize: ns:ns-font (#/smallSystemFontSize ns:ns-font)) #&NSFontAttributeName)
(#/setObject:forKey: *objc-dictionary* *dark-blue-color* #&NSForegroundColorAttributeName)

;;; This is not retained -- assumming autorelease.
(defun list-definitions-context-menu (text-view &optional alpha-p)
  "Construct the list-definitions popup menu."
  (let* ((menu (make-instance 'list-definitions-menu 
                 :menu-text-view text-view 
                 :menu-path (window-path (#/window text-view))))
         (window (active-hemlock-window))
         (alist (when window (list-definitions window alpha-p)))
         (class-icon (#/iconForFileType: (#/sharedWorkspace ns:ns-workspace) (ccl::%make-nsstring "lisp")))
         current-class menu-item)
    (ns:with-ns-size (icon-size 16 16)
      (#/setSize: class-icon icon-size))
    (dolist (entry alist)
      (let* ((def-info (car entry))
             (def-type (first def-info))
             (name (second def-info))
             (signature (third def-info))
             (specializer (fourth def-info))
             (dictionary (case def-type
                           (:defclass *defclass-dictionary*)
                           (:defstruct *defstruct-dictionary*)
                           (:defmethod *defmethod-dictionary*)
                           (:defun *defun-dictionary*)
                           (:defmacro *defmacro-dictionary*)
                           (:objc *objc-dictionary*)
                           (t *generic-dictionary*)))
             (attributed-string (#/initWithString:attributes:
                                 (#/alloc ns:ns-attributed-string) 
                                 ;; indent methods if directly under specializing class or struct:
                                 (if (or (eq def-type :defmethod)
                                         (eq def-type :objc))
                                   (if (and (not alpha-p)
                                            current-class specializer
                                            (string-equal specializer current-class))
                                     (ccl::%make-nsstring (format nil "      ~A" signature))
                                     (ccl::%make-nsstring (format nil "~A" signature)))
                                   (ccl::%make-nsstring name))
                                 dictionary)))
        (when (or (eq def-type :defclass) (eq def-type :defstruct)) (setq current-class name))
        (setq menu-item (make-instance 'list-definitions-menu-item))
        (setf (item-mark menu-item) (cdr entry))
        (setf (item-info menu-item) def-info)
        (#/setAttributedTitle: menu-item attributed-string)
        ;; Prepend CCL icon to class names:
        (when (eq def-type :defclass) (#/setImage: menu-item class-icon))
        (#/setAction: menu-item (ccl::@selector "listDefinitionsAction:"))
        (#/setTarget: menu-item  menu)
        (#/addItem: menu menu-item)))
    menu))

(objc:defmethod #/menuForEvent: ((view gui::hemlock-text-view) (event :id))
  (let ((view-window (#/window view)))
    (#/makeKeyAndOrderFront: view-window nil)
    (if (logtest #$NSAlternateKeyMask (#/modifierFlags event))
      (if (logtest #$NSCommandKeyMask (#/modifierFlags event))
        (files-context-menu)
        (positions-context-menu))
      (if (logtest #$NSCommandKeyMask (#/modifierFlags event))
        (list-definitions-context-menu view nil)
        (list-definitions-context-menu view t)))))

;;; This includes definitions in sharp-stroke comments.  We'll claim it's a feature.
(defun list-definitions (hemlock &optional alpha-p)
  "Create a list of all the top-level definitions in the file."
  (labels ((get-name (entry)
             (let ((def-info (car entry)))
               (second def-info)))
           (get-defs (mark pattern &optional objc-p)
             (do ((def-found-p (hi::find-pattern mark pattern)
                               (hi::find-pattern mark pattern))
                  alist)
                 ((not def-found-p) (when alist
                                      (if alpha-p 
                                        (sort alist #'string-lessp :key #'get-name) 
                                        (nreverse alist))))
               (when (zerop (hi::mark-charpos mark)) 
                 (let ((def-info (definition-info (clone mark) objc-p)))
                   (when def-info
                     (push (cons def-info (hi::line-start (clone mark))) alist))))
               (hi::line-end mark))))
    (let* ((pane (slot-value hemlock 'gui::pane))
           (text-view (gui::text-pane-text-view pane))
           (buffer (gui::hemlock-buffer text-view))
           (hi::*current-buffer* buffer))
      (hemlock::parse-over-block (hi::mark-line (hi::buffer-start-mark buffer))
                                 (hi::mark-line (hi::buffer-end-mark buffer)))
      (let* ((def-mark (clone (hi::buffer-start-mark buffer)))
             (objc-mark (clone (hi::buffer-start-mark buffer)))
             (def-alist (get-defs def-mark *def-search-pattern*))
             (objc-alist (get-defs objc-mark *objc-defmethod-search-pattern* t)))
        (when objc-alist
          (setq def-alist
                (if alpha-p
                  (merge 'list def-alist objc-alist #'string-lessp :key #'get-name)
                  (merge 'list def-alist objc-alist #'hi::mark< :key #'cdr))))
        def-alist))))

(defun definition-info (mark &optional objc-p)
  "Returns (type name) or (type name signature specializer) for methods."
  (flet ((substring-equal (string len)
           (string-equal string 
                         (hi::region-to-string 
                          (hi::region mark (hi::character-offset (clone mark) len))))))
    (let* ((def-type (cond (objc-p :objc)
                           ((substring-equal "(defmethod" 10) :defmethod)
                           ((substring-equal "(defun" 6) :defun)
                           ((substring-equal "(defmacro" 9) :defmacro)
                           ((substring-equal "(defclass" 9) :defclass)
                           ((substring-equal "(defstruct" 10) :defstruct)
                           (t :other)))
           (end (let ((temp-mark (clone mark)))
                  (when (hemlock::form-offset (hi::mark-after temp-mark) 2)
                    temp-mark)))
           (start (when end
                    (let ((temp-mark (clone end)))
                      (when (hemlock::form-offset temp-mark -1)
                        temp-mark)))))
      (when (and start end)
        (let ((name (hi::region-to-string (hi::region start end)))
              param-string specializer)
          (when (and (stringp name) (string-not-equal name ""))
            (case def-type
              (:defmethod
                  (let ((qualifier-start-mark (clone end))
                        (left-paren-mark (clone end))
                        right-paren-mark qualifier-end-mark qualifier-string)
                    (when (hi::find-pattern left-paren-mark *left-paren-search-pattern*)
                      (setq right-paren-mark (clone left-paren-mark))
                      (when (hemlock::form-offset right-paren-mark 1)
                        (multiple-value-setq (param-string specializer)
                          (parse-parameters (clone left-paren-mark) right-paren-mark))))
                    (when (hi::find-pattern qualifier-start-mark *colon-search-pattern* left-paren-mark)
                      (setq qualifier-end-mark (clone qualifier-start-mark))
                      (when (hemlock::form-offset qualifier-end-mark 1)
                        (setq qualifier-string
                              (hi::region-to-string (hi::region qualifier-start-mark qualifier-end-mark)))))
                    (if qualifier-string
                      ;; name is used to simplify the alpha sort:
                      (list def-type name (format nil "(~A ~A ~A)" name qualifier-string param-string) specializer)
                      (list def-type name (format nil "(~A ~A)" name param-string) specializer))))
              (:objc
               (let* ((name-start-mark (let ((temp-mark (clone start)))
                                         (when (hi::find-pattern temp-mark *slash-search-pattern*)
                                           (hi::mark-after temp-mark))))
                      (name-end-mark (when name-start-mark
                                       (let ((temp-mark (clone name-start-mark)))
                                         (when (hemlock::form-offset temp-mark 1)
                                           temp-mark))))
                      (objc-name (when (and name-start-mark name-end-mark) 
                                   (hi::region-to-string (hi::region name-start-mark name-end-mark))))
                      (left-paren-mark (let ((temp-mark (clone end)))
                                         (when (hi::find-pattern temp-mark *left-paren-search-pattern*)
                                           temp-mark)))
                      (right-paren-mark (when left-paren-mark 
                                          (let ((temp-mark (clone left-paren-mark)))
                                            (when (hi::form-offset temp-mark 1)
                                              temp-mark)))))
                 (when (and left-paren-mark right-paren-mark)
                   (multiple-value-setq (param-string specializer)
                     (parse-parameters left-paren-mark right-paren-mark t))
                   ;; Using curly braces to distinguish objc methods from Lisp methods:
                   (list def-type objc-name (format nil "{~A ~A}" objc-name param-string) specializer))))
              (:defstruct
                  (cond ((char= (hi::next-character start) #\()
                         (let* ((space-position (position #\space name :test #'char=))
                                (new-name (when space-position (subseq name 1 space-position))))
                           (if new-name
                             (list def-type new-name)
                             (list def-type name))))
                        (t
                         (list def-type name))))
              (t
               (list def-type name)))))))))

(defun parse-parameters (start-mark end-mark &optional objc-p)
  "Construct the method's parameter string."
  (let (specializers-processed-p specializer)
    (flet ((get-param (start end)
             (let ((next-character (hi::next-character start)))
               (when (char= next-character #\&) (setq specializers-processed-p t))
               (cond ((and (char= next-character #\() (not specializers-processed-p))
                      (let* ((specializer-end (when (hemlock::form-offset (hi::mark-after start) 2) start))
                             (specializer-start (when specializer-end (clone specializer-end))))
                        (when (and specializer-end specializer-start
                                   (hemlock::form-offset specializer-start -1)
                                   (hi::mark< specializer-end end))
                          (when objc-p (setq specializers-processed-p t))
                          (hi::region-to-string (hi::region specializer-start specializer-end)))))
                     (t 
                      (unless (char= next-character #\&)
                        (format nil "t")))))))
      (do* ((sexp-end (let ((temp-mark (hi::mark-after (clone start-mark))))
                        (when (hemlock::form-offset temp-mark 1) temp-mark))
                      (when (hemlock::form-offset (hi::mark-after sexp-end) 1) sexp-end))
            (sexp-start (when sexp-end
                          (let ((temp-mark (clone sexp-end)))
                            (when (hemlock::form-offset temp-mark -1) temp-mark)))
                        (when sexp-end
                          (let ((temp-mark (clone sexp-end)))
                            (when (hemlock::form-offset temp-mark -1) temp-mark))))
            (param-string (when (and sexp-start sexp-end) (get-param (clone sexp-start) 
                                                                     (clone sexp-end)))
                          (when (and sexp-start sexp-end) (get-param (clone sexp-start)
                                                                     (clone sexp-end))))
            (first-param-p t)
            parameters)
           ((or (null sexp-start) (null sexp-end) 
                (hi::mark> sexp-start end-mark)
                ;; Empty body case:
                (hi::mark< sexp-start start-mark))
            (values (concatenate 'string parameters ")") specializer))
        (when param-string
          (cond (first-param-p
                 (setq parameters (concatenate 'string "(" param-string))
                 (setq specializer param-string)
                 (setq first-param-p nil))
                (t
                 (setq parameters (concatenate 'string parameters " " param-string)))))))))



