;;;-*- Mode: Lisp; Package: LIST-DEFINITIONS -*-

;;; ----------------------------------------------------------------------------
;;;
;;;      history-lists.lisp
;;;
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      This code supports file and position history lists.
;;;
;;;      Alt-Right-Click produces a most-recently-visited list of definition 
;;;      positions.  Alt-Command-Right-Click produces a most-recently-visited
;;;      list of files.  Both lists are persistent and are stored here:
;;;
;;;      ~/Library/Preferences/org.clairvaux/list-definitions/file-history
;;;      ~/Library/Preferences/org.clairvaux/list-definitions/position-history
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History, most recent first:
;;;      8/31/9  version 0.2b2
;;;              Modified to work with Context-Menu mechanism.
;;;      8/17/9  version 0.2b1
;;;              This file added.
;;;
;;; ----------------------------------------------------------------------------

(in-package "LIST-DEFINITIONS")

(defParameter *position-history-list-length* 25)
(defParameter *file-history-list-length* 25)

(defun maybe-open-file (path)
  "If a window with PATH is open, return it.  Otherwise open a new window."
  (let ((w (cmenu:window-with-path path)))
    (if w 
      w
      (let ((hemlock-view (gui::cocoa-edit path)))
        (when hemlock-view (#/window (hi::hemlock-view-pane hemlock-view)))))))

(defun construct-history-path (filename)
  "Construct the path to the history file."
    (merge-pathnames (concatenate 'string 
                                  ";Library;Preferences;org.clairvaux;list-definitions;" 
                                  filename)
                     (hemlock::user-homedir-pathname)))


;;; ----------------------------------------------------------------------------
;;;
(defClass HISTORY-LIST-ENTRY ()
  ((name :initarg :name :reader hle-name)
   (path :initarg :path :reader hle-path))
  (:documentation "Support for the history lists."))  

;;; ----------------------------------------------------------------------------
;;;
(defClass POSITION-LIST-ENTRY (history-list-entry) 
  ((info :initarg :info :reader hle-info))
  (:documentation "Support for the position history list."))

(defMethod show-entry ((entry position-list-entry))
  "Display the file and scroll to position."
  (let* ((name (hle-name entry))
         (path (hle-path entry))
         (window (cmenu:window-with-path path))
         mark def-list text-view hemlock-view)
    (unless (probe-file path)
      (cmenu:notify (format nil "~a does not exist.  It will be deleted from the history lists."
                      path))
      (purge-file-references *position-history-list* path)
      (remove-path *file-history-list* path)
      (return-from show-entry nil))
    (cond (window 
           (setq hemlock-view (gui::hemlock-view window))
           (setq text-view (gui::text-pane-text-view (hi::hemlock-view-pane hemlock-view))))
          (t
           (setq hemlock-view (gui::cocoa-edit path))
           (when hemlock-view
             (setq window (#/window (hi::hemlock-view-pane hemlock-view)))
             (setq text-view (gui::text-pane-text-view (hi::hemlock-view-pane hemlock-view))))))
    (when window
      (#/makeKeyAndOrderFront: window nil)
      (setq def-list (list-definitions window))
      (setq mark (cdr (assoc name def-list 
                             :test #'string-equal
                             :key #'(lambda (def-info)
                                      (let ((def-type (first def-info)))
                                        (if (or (eq def-type :defmethod)
                                                (eq def-type :objc))
                                          (third def-info)
                                          (second def-info)))))))
      (cond (mark
             (display-position text-view mark)
             (move-entry-to-front *file-history-list* path) t)
            (t 
             (cmenu:notify (format nil "Cannot find ~S.  It will be deleted from the position history list." 
                             name))
             (remove-entry *position-history-list* name) nil)))))

;;; ----------------------------------------------------------------------------
;;;
(defClass FILE-LIST-ENTRY (history-list-entry) 
  ((short-path :initarg :short-path :accessor hle-short-path))
  (:documentation "Support for the file history list."))

(defMethod show-entry ((entry file-list-entry))
  (let ((path (hle-path entry)))
    (unless (probe-file path)
      (cmenu:notify (format nil "~S does not exist.  It will be deleted from the history lists." path))
      (purge-file-references *position-history-list* path)
      (remove-path *file-history-list* path)
      (return-from show-entry nil))
    (let ((window (cmenu:window-with-path path))) 
      (unless window 
        (let ((hemlock-view (gui::cocoa-edit path)))
          (when hemlock-view 
            (setq window (#/window (hi::hemlock-view-pane hemlock-view))))))
      (when window
        (#/makeKeyAndOrderFront: window nil) t))))

;;; ----------------------------------------------------------------------------
;;;
(defClass HISTORY-LIST ()
  ((capacity :initarg :capacity :reader hl-capacity)
   (path :initarg :path :reader hl-path)
   (list :initform nil :accessor hl-list))
  (:documentation "Super class of position-history-list and file-history-list."))

;;; ----------------------------------------------------------------------------
;;;
(defClass POSITION-HISTORY-LIST (history-list) 
  ()
  (:documentation "A persistent history list of most-recently-visited definition positions."))

(setq *position-history-list* (make-instance 'position-history-list 
                                :path (construct-history-path "position-history")
                                :capacity *position-history-list-length*))

(defMethod find-entry ((hl position-history-list) name)
  (find-if  #'(lambda (entry) (string-equal name (hle-name entry)))
            (hl-list hl)))

(defMethod move-entry-to-front ((hl position-history-list) name)
  (let ((entry (find-entry hl name)))
    (when entry
      (setf (hl-list hl) 
            (cons entry (delete name (hl-list hl) :test #'string-equal :key #'hle-name)))
      entry)))

(defMethod purge-file-references ((hl position-history-list) path)
  (setf (hl-list hl) (delete-if #'(lambda (entry)
                                    (equal (hle-path entry) path))
                                (hl-list hl))))

(defMethod remove-entry ((hl position-history-list) name)
  (setf (hl-list hl) (delete name (hl-list hl) :test #'string-equal :key #'hle-name)))

(defMethod add-history-entry ((hl position-history-list) def-info path)
  (let* ((def-type (first def-info))
         (name (second def-info))
         (signature (third def-info))
         (entry (make-instance 'position-list-entry 
                  :name (if (or (eq def-type :defmethod)
                                (eq def-type :objc))
                          signature
                          name)
                  :info def-info :path path)))
    (setf (hl-list hl) (cons entry (hl-list hl)))
    entry))

(defMethod maybe-add-history-entry ((hl position-history-list) def-info path)
  (let* ((def-type (first def-info))
         (name (if (or (eq def-type :defmethod)
                       (eq def-type :objc))
                 (third def-info)
                 (second def-info))))
    (cond ((member name (hl-list hl) :test #'string-equal :key #'hle-name)
           ;; it's there; move it to the front:
           (move-entry-to-front hl name))
          (t
           (when (>= (length (hl-list hl)) (hl-capacity hl))
             ;; bump the last entry, then add:
             (setf (hl-list hl) (butlast (hl-list hl))))
           (add-history-entry hl def-info path)))))

(defun clear-position-history-list()
  "Remove all the entries from the position history list."
  (setf (hl-list *position-history-list*) nil))

;;; ----------------------------------------------------------------------------
;;;
(defClass FILE-HISTORY-LIST (history-list) 
  ()
  (:documentation "A persistent history list of most-recently-visited files."))

(setf *file-history-list* (make-instance 'file-history-list
                            :path (construct-history-path "file-history")
                            :capacity *file-history-list-length*))

(defMethod find-entry ((hl file-history-list) path)
  (find-if  #'(lambda (entry) (string-equal path (hle-path entry)))
            (hl-list hl)))

(defMethod move-entry-to-front ((hl file-history-list) path)
  (let ((entry (find-entry hl path))) 
    (when entry
      (setf (hl-list hl) 
            (cons entry (delete path (hl-list hl) :test #'string-equal :key #'hle-path)))
      entry)))

(defmethod remove-path ((hl file-history-list) path)
  (setf (hl-list hl) (delete path (hl-list hl) 
                             :test #'string-equal :key #'hle-path)))

(defMethod add-history-entry ((hl file-history-list) name path)
  (let* ((name-position (position #\/ path :test #'char= :from-end t))
         (short-path (when name-position (subseq path 0 (incf name-position))))
         (entry (when short-path (make-instance 'file-list-entry :name name 
                                   :short-path short-path :path path))))
    (when entry
      (setf (hl-list hl) (cons entry (hl-list hl)))
      entry)))

(defMethod maybe-add-history-entry ((hl file-history-list) name path)
  (cond ((member path (hl-list hl) :test #'string-equal :key #'hle-path)
         (move-entry-to-front hl path))
        (t 
         (cond ((< (length (hl-list hl)) (hl-capacity hl))
                (add-history-entry hl name path))
               (t 
                (setf (hl-list hl) (butlast (hl-list hl)))
                (add-history-entry hl name path))))))

(defun clear-file-history-list ()
  "Remove all the entries from the file history list."
  (setf (hl-list *file-history-list*) nil))

;;; ----------------------------------------------------------------------------
;;;
(defclass POSITION-MENU-ITEM (ns:ns-menu-item)
   ((path :accessor position-path)
    (name :accessor position-name))
  (:documentation "Support for the positions popup menu.")
  (:metaclass ns:+ns-object))

;;; ----------------------------------------------------------------------------
;;;
(defclass POSITIONS-MENU (ns:ns-menu)
  ((tool-menu :initform nil :accessor tool-menu)
   (sub-title :initform "position history" :reader sub-title))
  (:documentation "A popup menu of most-recently-visited definition positions.")
  (:metaclass ns:+ns-object))

;;; Pressing the shift key when selecting an entry will delete the entry:
(objc:defmethod (#/positionHistoryAction: :void) ((m positions-menu) (sender :id))
  (let ((entry (find-entry *position-history-list* (position-name sender))))
    (when entry
      (cond ((gui::current-event-modifier-p #$NSShiftKeyMask)
             (remove-entry *position-history-list* (position-name sender)))
            (t
             (show-entry entry)
             (move-entry-to-front *position-history-list* (position-name sender)))))))

(objc:defmethod (#/clearPositionHistoryAction: :void) ((m positions-menu) (sender :id))
  (declare (ignore sender))
  (clear-position-history-list))

(objc:defmethod (#/update :void) ((self positions-menu))
  (cmenu:update-tool-menu self (tool-menu self) :sub-title (sub-title self))
  (call-next-method))

(defun positions-context-menu ()
  "Create the positions context menu."
  (let* ((menu (make-instance 'positions-menu))
         (class-icon (#/iconForFileType: (#/sharedWorkspace ns:ns-workspace) (ccl::%make-nsstring "lisp")))
          menu-item)
    (ns:with-ns-size (icon-size 16 16)
      (#/setSize: class-icon icon-size))
    (setf (tool-menu menu) (cmenu:add-default-tool-menu menu))
    (dolist (entry (hl-list *position-history-list*))
      (let* ((def-info (hle-info entry))
             (def-type (first def-info))
             (name (second def-info))
             (signature (third def-info))
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
                                 (if (or (eq def-type :defmethod)
                                         (eq def-type :objc))
                                   (ccl::%make-nsstring signature)
                                   (ccl::%make-nsstring name))
                                 dictionary)))
        (setq menu-item (make-instance 'position-menu-item))
        (setf (position-path menu-item) (hle-path entry))
        (if (or (eq def-type :defmethod) (eq def-type :objc))
          (setf (position-name menu-item) signature)
          (setf (position-name menu-item) name))
        (#/setAttributedTitle: menu-item attributed-string)
        ;; Classes have a prepended CCL icon:
        (when (eq def-type :defclass) (#/setImage: menu-item class-icon))
        (#/setAction: menu-item (ccl::@selector "positionHistoryAction:"))
        (#/setTarget: menu-item  menu)
        (#/addItem: menu menu-item)))
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (let ((attributed-string (#/initWithString:attributes:
                              (#/alloc ns:ns-attributed-string)
                              (ccl::%make-nsstring "Clear List")
                              *generic-dictionary*)))
      (setq menu-item (make-instance 'ns:ns-menu-item))
      (#/setAttributedTitle: menu-item attributed-string)
      (#/setTarget: menu-item menu)
      (#/setAction: menu-item (ccl::@selector "clearPositionHistoryAction:"))
      (#/addItem: menu menu-item))
    menu))

;;; ----------------------------------------------------------------------------
;;; 
(defclass FILE-MENU-ITEM (ns:ns-menu-item)
   ((path :accessor file-path)
    (name :accessor file-name))
  (:documentation "Support for the files popup menu.")
  (:metaclass ns:+ns-object))

;;; ----------------------------------------------------------------------------
;;;
(defclass FILE-MENU (ns:ns-menu)
  ((tool-menu :initform nil :accessor tool-menu)
   (sub-title :initform "file history" :reader sub-title))
  (:documentation "A popup menu of most-recently-visited files.")
  (:metaclass ns:+ns-object))

;;; Pressing the shift key when selecting an entry will delete the entry:
(objc:defmethod (#/fileHistoryAction: :void) ((m file-menu) (sender :id))
  (let ((entry (find-entry *file-history-list* (file-path sender))))
    (when entry
      (cond ((gui::current-event-modifier-p #$NSShiftKeyMask)
             (remove-path *file-history-list* (file-path sender)))
            (t
             (show-entry entry)
             (move-entry-to-front *file-history-list* (file-path sender)))))))

(objc:defmethod (#/update :void) ((self file-menu))
  (cmenu:update-tool-menu self (tool-menu self) :sub-title (sub-title self))
  (call-next-method))

(objc:defmethod (#/clearFileHistoryAction: :void) ((m file-menu) (sender :id))
  (declare (ignore sender))
  (clear-file-history-list))

(defun files-context-menu ()
  "Create the files context menu."
  (let* ((menu (make-instance 'file-menu))
          menu-item)
    (setf (tool-menu menu) (cmenu:add-default-tool-menu menu))
    (dolist (entry (hl-list *file-history-list*))
      (let ((attributed-string (#/initWithString:attributes:
                                (#/alloc ns:ns-attributed-string) 
                                (ccl::%make-nsstring 
                                 (format nil "~A  ~A" 
                                         (hle-name entry)
                                         (hle-short-path entry)))
                                *file-history-dictionary*)))
        (setq menu-item (make-instance 'file-menu-item))
        (setf (file-name menu-item) (hle-name entry))
        (setf (file-path menu-item) (hle-path entry))
        (#/setAttributedTitle: menu-item attributed-string)
        (#/setAction: menu-item (ccl::@selector "fileHistoryAction:"))
        (#/setTarget: menu-item  menu)
        (#/addItem: menu menu-item)))
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (let ((attributed-string (#/initWithString:attributes:
                              (#/alloc ns:ns-attributed-string)
                              (ccl::%make-nsstring "Clear List")
                              *generic-dictionary*)))
      (setq menu-item (make-instance 'ns:ns-menu-item))
      (#/setAttributedTitle: menu-item attributed-string)
      (#/setTarget: menu-item menu)
      (#/setAction: menu-item (ccl::@selector "clearFileHistoryAction:"))
      (#/addItem: menu menu-item))
    menu))

;;; ----------------------------------------------------------------------------
;;; File I/O
;;;
(defun read-history-files ()
  "Read the position and file history lists."
  (let ((path (hl-path *file-history-list*)))
    (when (probe-file path)
      (with-open-file (stream path :direction :input)
        (read-history-list *file-history-list* stream))))
  (let ((path (hl-path *position-history-list*)))
    (when (probe-file path)
      (with-open-file (stream path :direction :input)
        (read-history-list *position-history-list* stream t)))))

(defMethod read-history-list ((hl history-list) stream &optional position-p)
  (flet ((oops ()
           (cmenu:notify (format nil "There is a problem with ~S. Setting the history to NIL." (hl-path hl)))
           (setf (hl-list hl) nil)
           ;;; delete the file?
           (return-from read-history-list)))
    (setf (hl-list hl) nil)
    ;; For the position-history-list, ufo is the def-info list.
    ;; For the file-history-list, ufo is the filename string.
    (let (length ufo path input)
      (setf input (read stream nil :eof))
      (unless (numberp input) (oops))
      (setf length input)
      (dotimes (count length t)
        (setf input (read stream nil :eof))
        (when (or (eql input :eof)
                  (if position-p
                    (not (listp input))
                    (not (stringp input))))
          (oops))
        (setf ufo input)
        (setf input (read stream nil :eof))
        (when (or (eql input :eof)
                  (not (stringp input)))
          (oops))
        (setf path input)
        (when (null (add-history-entry hl ufo path))
          (oops))))))

(defMethod write-history-list ((hl position-history-list) stream)
  (format stream "~s~%" (length (hl-list hl)))
  (dolist (entry (nreverse (hl-list hl)))
    (format stream "~s~%" (hle-info entry))
    (format stream "~s~%" (hle-path entry))))

(defMethod write-history-list ((hl file-history-list) stream)
  (format stream "~s~%" (length (hl-list hl)))
  (dolist (entry (nreverse (hl-list hl)))
    (format stream "~s~%" (hle-name entry))
    (format stream "~s~%" (hle-path entry))))

(defun write-history-files ()
  "Write the history list entries to the path."
  (let ((path (hl-path *position-history-list*)))
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-history-list *position-history-list* stream)))
  (let ((path (hl-path *file-history-list*)))
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-history-list *file-history-list* stream))))

(defun write-history-files-on-shutdown (&rest args)
  "Writing function pushed into *lisp-cleanup-functions*."
  (declare (ignore args))
  (write-history-files))

(defun read-history-files-on-startup (&rest args)
  "Reading function (eventually) pushed into *lisp-startup-functions*."
  (declare (ignore args))
  (read-history-files))

(pushnew 'write-history-files-on-shutdown ccl::*lisp-cleanup-functions*)

;;; To Do:
;;; Heap issues involved in saving an image with the utility loaded.
;;; (pushnew 'read-history-files-on-startup ccl::*lisp-startup-functions*)

;;; ----------------------------------------------------------------------------
;;; File History Interface:
;;; 
(objc:defmethod (#/becomeKeyWindow :void) ((w gui::hemlock-frame))
  (let* ((path (cmenu:window-path w))
         (name (when (and path (string-equal (pathname-type path) "lisp"))
                 (concatenate 'string (pathname-name path) ".lisp"))))
    (when (and name path)
      (maybe-add-history-entry *file-history-list* name path))
    (call-next-method)))

;;; ----------------------------------------------------------------------------
;;; Position History Interface:
;;; 
(hemlock::defcommand "Add Definition Position" (p)
  "Add the position of the definition containing point to *position-history-list*."
  (declare (ignore p))
  (let* ((buffer (hemlock::current-buffer))
         (mark (hi::copy-mark (hemlock::buffer-point buffer) :temporary))
         (path (hi::buffer-pathname buffer))
         (start-mark (hi::top-level-offset mark -1))
         (def-info (when start-mark (definition-info start-mark))))
    (when (and def-info path)
      (maybe-add-history-entry *position-history-list* def-info path))))

(hemlock::bind-key "Add Definition Position" #k"control-shift-space")

(defun add-top-level-position (&optional buffer)
  "Maybe add the top-level definition position to the position history list."
  (let* ((buf (or buffer (hi::current-buffer)))
         (mark (hi::copy-mark (hemlock::buffer-point buf) :temporary))
         (path (hi::buffer-pathname buf))
         start-mark def-info)
    (if (and (= (hi::mark-charpos mark) 0)
             (char= (hi::next-character mark) #\())
      (setq start-mark mark)
      (setq start-mark (hemlock::top-level-offset mark -1)))
    (when start-mark
      (setq def-info (definition-info start-mark))
      (when (and def-info path)
        (maybe-add-history-entry *position-history-list* def-info path)))))

;;; *** These three redefinitions are not a great way of doing this ***
;;; *** Where's CLOS when you need it ...
(hemlock::defcommand "Editor Evaluate Defun" (p)
  "Evaluates the current or next top-level form in the editor Lisp.
   If the current region is active, this evaluates the region."
  "Evaluates the current or next top-level form in the editor Lisp."
  (declare (ignore p))
  (if (hemlock::region-active-p)
    (hemlock::editor-evaluate-region-command nil)
    (hemlock::eval-region (hemlock::defun-region (hi::current-point))))
  (add-top-level-position))

(hemlock::defcommand "Editor Compile Defun" (p)
  "Compiles the current or next top-level form in the editor Lisp.
   First the form is evaluated, then the result of this evaluation
   is passed to compile.  If the current region is active, this
   compiles the region."
  "Evaluates the current or next top-level form in the editor Lisp."
  (declare (ignore p))
  (if (hemlock::region-active-p)
      (hemlock::editor-compile-region (hemlock::current-region))
      (hemlock::editor-compile-region (hemlock::defun-region (hi::current-point)) t))
  (add-top-level-position))

;;; gui::cocoa-edit-single-definition didn't last long.
;;; This one's days are numbered:
(defun hemlock::move-point-leaving-mark (target)
  (let ((point (hi::current-point-collapsing-selection)))
    (hemlock::push-new-buffer-mark point)
    (hi::move-mark point target)
    (add-top-level-position (hi::current-buffer))
    point))


(read-history-files)

;;; Hemlock-Commands needs this, for now:
(pushnew :list-definitions *features*)
