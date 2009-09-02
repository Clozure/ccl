;;;-*-Mode: LISP; Package: HEMLOCK-COMMANDS -*-

;;; ----------------------------------------------------------------------------
;;;
;;;      hemlock-commands-1.lisp
;;;
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      This code adds a Hemlock Commands documentation tool to the Context-Menu 
;;;      mechanism.  Right-Click displays a listing of essential Hemlock Commands
;;;      for the new users.  Selecting an entry executes the command.
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History, most recent first:
;;;      8/31/9  version 0.1b1
;;;              First cut.
;;;
;;; ----------------------------------------------------------------------------

(defpackage "HEMLOCK-COMMANDS" (:nicknames "HCOM") (:use :cl :ccl))
(in-package "HEMLOCK-COMMANDS")

(require :context-menu-cm)
(cmenu:check-hyperspec-availability "Hemlock-Commands-CM")

(defparameter *hemlock-commands-menu* nil "The hemlock-commands-menu instance.")
(defparameter *hemlock-commands-keyword-menu* nil "The hemlock-commands-keyword-menu instance.")

;;; ----------------------------------------------------------------------------
;;;
(defclass HEMLOCK-COMMAND-MENU-ITEM (ns:ns-menu-item)
  ((key-event :initform nil :accessor key-event))
  (:documentation "Support for the hemlock-commands-menu.")
  (:metaclass ns:+ns-object))


;;; ----------------------------------------------------------------------------
;;;
(defclass HEMLOCK-COMMANDS-MENU (ns:ns-menu)
  ((tool-menu :initform nil :accessor tool-menu)
   (sub-title :initform "basic commands" :reader sub-title)
   (doc-path :initform (merge-pathnames ";ReadMe.rtf" cl-user::*hemlock-commands-directory*) :reader doc-path)
   (text-view :initform nil :accessor text-view))
  (:documentation "A popup menu listing a useful subset of Hemlock commands: Hemlock's Greatest Hits, for new users.")
  (:metaclass ns:+ns-object))

(objc:defmethod (#/hemlockCommandAction: :void) ((m hemlock-commands-menu) (sender :id))
  (let ((key-event (key-event sender))) ; can be a vector of events
    (cond ((typep key-event 'hi::key-event)
           (hi::handle-hemlock-event (gui::hemlock-view (text-view m)) key-event))
          ((typep (key-event sender) 'simple-vector)
           (hi::handle-hemlock-event (gui::hemlock-view (text-view m)) (aref key-event 0))
           (hi::handle-hemlock-event (gui::hemlock-view (text-view m)) (aref key-event 1))))))

(defmethod initialize-instance :after ((menu hemlock-commands-menu) &key)
  (flet ((create-menu-item (name key-event)
           (let ((menu-item (make-instance 'hemlock-command-menu-item))
                 (attributed-string (#/initWithString:attributes:
                                     (#/alloc ns:ns-attributed-string) 
                                     (ccl::%make-nsstring name)
                                     cmenu:*hemlock-menu-dictionary*)))
             (#/setAttributedTitle: menu-item attributed-string)
             (#/setAction: menu-item (ccl::@selector "hemlockCommandAction:"))
             (#/setTarget: menu-item  menu)
             (setf (key-event menu-item) key-event)
             (#/addItem: menu menu-item))))
    (setf (tool-menu menu) (cmenu:add-default-tool-menu menu :doc-file (doc-path menu)))
    
    ;;; Hemlock's Greatest Hits:
    (create-menu-item "Inspect Symbol  (control-x, control-i)" 
                      #k"control-x control-i")
    (create-menu-item "Symbol Documentation  (control-x, control-d)" 
                      #k"control-x control-d")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (create-menu-item "Current Function Arglist  (control-x, control-a)" 
                      #k"control-x control-a")
    #|
    (create-menu-item "Show Callers  (control-meta-c)" 
                      #k"control-meta-c")
    |#
    (create-menu-item "Goto Definition  (meta-.)"
                      #k"meta-.")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (create-menu-item "Macroexpand-1 Expression  (control-m)"
                      #k"control-m")
    (create-menu-item "Macroexpand Expression  (control-x, control-m)" 
                      #k"control-x control-m")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (create-menu-item "Editor Evaluate Defun  (control-x, control-e)" 
                      #k"control-x control-e")
    (create-menu-item "Editor Compile Defun  (control-x, control-c)" 
                      #k"control-x control-c")
    (create-menu-item "Editor Evaluate Region  (Enter)"
                      #k"enter")
    #|
    (create-menu-item "Editor Compile Region  (unbound)" 
                      #k"enter")
    (create-menu-item "Editor Evaluate Buffer  (unbound)"
                      #k"enter")
    (create-menu-item "Editor Compile Buffer File  (unbound)"
                      #k"enter")
    |#
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (create-menu-item "Incremental Search  (control-s)"
                      #k"control-s")
    (create-menu-item "I-Search Repeat Forward  (control-s)"
                      #k"control-s")
    (create-menu-item "I-Search Repeat Backward  (control-r)"
                      #k"control-r")
    (create-menu-item "I-Search Abort  (control-g)"
                      #k"control-g")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (create-menu-item "Kill Line  (control-k)"
                      #k"control-k")
    (create-menu-item "Un-Kill  (control-y)"
                      #k"control-y")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (create-menu-item "Forward Character  (control-f)"
                      #k"control-f")
    (create-menu-item "Backward Character  (control-b)"
                      #k"control-b")
    (create-menu-item "Beginning of Line  (control-a)"
                      #k"control-a")
    (create-menu-item "End of Line  (control-e)"
                      #k"control-e")
    (create-menu-item "Previous Line  (control-p)"
                      #k"control-p")
    (create-menu-item "Next Line  (control-n)"
                      #k"control-n")
    (create-menu-item "Beginning of Buffer  (meta-<)"
                      #k"meta-\<")
    (create-menu-item "End of Buffer  (meta->)"
                      #k"meta-\>")
    (create-menu-item "Scroll Window Down  (control-v)"
                      #k"control-v")
    (create-menu-item "Scroll Window Up  (meta-v)"
                      #k"meta-v")))

(objc:defmethod (#/update :void) ((self hemlock-commands-menu))
  (cmenu:update-tool-menu self (tool-menu self) :sub-title (sub-title self))
  (call-next-method))

(setq *hemlock-commands-menu* (make-instance 'hemlock-commands-menu))

(defun get-hemlock-commands-menu (view event)
  "Return the appropriate Hemlock Commands menu based on modifier keys."
  (cond ((logtest #$NSCommandKeyMask (#/modifierFlags event))
         (setf (text-view *hemlock-commands-menu*) view)           
         *hemlock-commands-menu*)
        (t
         *hemlock-commands-keyword-menu*)))

(cmenu:register-tool "Hemlock-Commands-CM" #'get-hemlock-commands-menu)


