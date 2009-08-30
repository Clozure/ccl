;;;-*-Mode: LISP; Package: HEMLOCK-COMMANDS -*-

;;; ----------------------------------------------------------------------------
;;;
;;;      hemlock-commands-2.lisp
;;;
;;;      copyright © 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      This code adds a Hemlock Commands documentation tool to the Context-Menu 
;;;      mechanism.  Right-Click displays a list of submenus.  The submenus are keywords.
;;;      Popping the submenu displays entries for all Hemlock Commands filtered by that 
;;;      keyword.  Selecting an entry opens a documentation dialog.
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History, most recent first:
;;;      8/31/9  version 0.1b1
;;;              First cut.
;;;
;;; ----------------------------------------------------------------------------

(in-package "HEMLOCK-COMMANDS")

;;; ----------------------------------------------------------------------------
;;;
(defclass HEMLOCK-COMMAND-KEYWORD-MENU-ITEM (ns:ns-menu-item)
  ((command :initform nil :accessor item-command))
  (:documentation "Support for the hemlock command keyword menu.")
  (:metaclass ns:+ns-object))

(defun display-doc (command)
  "Open the documentation dialog for COMMAND."
  (let ((keystroke-string
         (do* ((bindings (hi::command-%bindings command) (rest bindings))
               (bindings-length (length bindings))
               (binding (car bindings) (car bindings))
               (event-array (when binding (car binding))
                            (when binding (car binding)))
               (num-events (when event-array (array-dimension event-array 0))
                           (when event-array (array-dimension event-array 0)))
               (keystrokes "" (if binding 
                                (concatenate 'string keystrokes ",   ")
                                keystrokes)))
              ((or (null bindings) (> bindings-length 4))
                   (if (> bindings-length 4)
                     "Too many bindings ..."
                     keystrokes))
           (when event-array
             (cond ((= num-events 1)
                    (setq keystrokes 
                          (concatenate 'string
                                       keystrokes
                                       (hi::pretty-key-string (aref event-array 0) t))))
                   (t
                    (setq keystrokes
                          (concatenate 'string 
                                       keystrokes
                                       (format nil "~A  ~A" 
                                               (hi::pretty-key-string (aref event-array 0) t)
                                               (hi::pretty-key-string (aref event-array 1) t))))))))))
    (open-documentation-dialog (hi::command-%name command)
                             (if (string= keystroke-string "") "no binding" keystroke-string)
                             (hi::command-documentation command) :hemlock-p t)))

(defun populate-submenu (menu command-list)
  "Make menu-items for all commands in COMMAND-LIST, and add them to MENU"
  (dolist (command-cons (reverse command-list))
    (let* ((command-name (car command-cons))
           (command (cdr command-cons))
           (menu-item (make-instance 'hemlock-command-keyword-menu-item))
           (attributed-string (#/initWithString:attributes:
                               (#/alloc ns:ns-attributed-string) 
                               (ccl::%make-nsstring command-name)
                               cmenu:*hemlock-menu-dictionary*)))
      (#/setAttributedTitle: menu-item attributed-string)
      (#/setAction: menu-item (ccl::@selector "hemlockCommandDocAction:"))
      (#/setTarget: menu-item  *hemlock-commands-keyword-menu*)
      ;; (#/setImage: menu-item class-icon)
      (setf (item-command menu-item) command)
      (#/addItem: menu menu-item))))

(defun make-submenu-item (title command-list)
  "Create a menu-item with a submenu, and populate the submenu with the commands in COMMAND-LIST."
  (let ((menu-item (make-instance ns:ns-menu-item))
        (attributed-string (#/initWithString:attributes:
                            (#/alloc ns:ns-attributed-string) 
                            (ccl::%make-nsstring title)
                            cmenu:*hemlock-menu-dictionary*))
        (submenu (make-instance ns:ns-menu)))
    (#/setAttributedTitle: menu-item attributed-string)
    (#/setSubmenu: menu-item submenu)
    (populate-submenu submenu command-list)
    menu-item))

(defparameter *hemlock-command-keywords*
  '("auto" "backward" "beginning" "buffer" "character" "command" "comment" "compile" "completion" "count" "defun" "delete" "describe"
    "down" "echo" "editor" "end" "evaluate" "expression" "file" "form" "forward" "function" "goto" "help" "i-search"
    "indent" "insert" "interactive" "kill" "line" "list" "macroexpand" "mark" "mode" "next" "paragraph" "parse"
    "point" "pop" "previous" "query" "region" "register" "save" "search" "select" "sentence" "set" "show" "space" 
    "transpose" "up" "what" "word" "write"))

;;; ----------------------------------------------------------------------------
;;;
(defclass HEMLOCK-COMMANDS-KEYWORD-MENU (ns:ns-menu)
  ((tool-menu :initform nil :accessor tool-menu)
   (doc-path :initform (merge-pathnames ";ReadMe.rtf" cl-user::*hemlock-commands-directory*) :reader doc-path))
  (:documentation "A popup menu with keyword submenus for filtering Hemlock commands.")
  (:metaclass ns:+ns-object))

(objc:defmethod (#/hemlockCommandDocAction: :void) ((m hemlock-commands-keyword-menu) (sender :id))
  (display-doc (item-command sender)))

(defmethod initialize-instance :after ((menu hemlock-commands-keyword-menu) &key)
  (setf (tool-menu menu) (cmenu:add-default-tool-menu menu :doc-file (doc-path menu))))

(defmethod add-submenus ((menu hemlock-commands-keyword-menu))
  (let ((keyword-array (make-array  (length *hemlock-command-keywords*) :initial-element nil))
        miscellaneous)
    (dotimes (index (hi::string-table-num-nodes hi::*command-names*))
      (let* ((idx 0)
             (command (hi::value-node-value (aref (hi::string-table-value-nodes hi::*command-names*) index)))
             (command-name (hi::command-%name command))
             (entry-found-p nil))
        (dolist (keyword *hemlock-command-keywords*)
          ;; commands will generally have multiple entries
          (when (search keyword command-name :test #'string-equal)
            (setq entry-found-p t)
            (push (cons command-name command) (aref keyword-array idx)))
          (incf idx))
      (unless entry-found-p (push (cons command-name command) miscellaneous))))
    (let ((idx 0))
      (dolist (keyword *hemlock-command-keywords*)
        (let ((submenu-item (make-submenu-item keyword (coerce (aref keyword-array idx) 'list))))
          (#/addItem: menu submenu-item))
        (incf idx)))
    (when miscellaneous
      (#/addItem: menu (#/separatorItem ns:ns-menu-item))    
      (let ((submenu-item (make-submenu-item "Commands Without Keywords:" miscellaneous)))
        (#/addItem: menu submenu-item)))))


(objc:defmethod (#/update :void) ((self hemlock-commands-keyword-menu))
  (cmenu:update-tool-menu self (tool-menu self))
  (call-next-method))


(setq *hemlock-commands-keyword-menu* (make-instance 'hemlock-commands-keyword-menu))

(add-submenus *hemlock-commands-keyword-menu*)




