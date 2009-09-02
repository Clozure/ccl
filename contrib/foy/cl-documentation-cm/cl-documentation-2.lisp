;;;-*-Mode: LISP; Package: HEMLOCK-COMMANDS -*-

;;; ----------------------------------------------------------------------------
;;;
;;;      cl-documentation-2.lisp
;;;
;;;      copyright (c) 2009 Glen Foy
;;;      (Permission is granted to Clozure Associates to distribute this file.)
;;;
;;;      This code adds an alphabetical index of :CL commands to the Context-Menu 
;;;      mechanism.  Command-Right-Click displays a list of letter submenus.
;;;      Popping the submenu displays entries for all Hemlock Commands starting with
;;;      that letter.  Selecting an entry opens a documentation dialog.
;;;
;;;      This software is offered "as is", without warranty of any kind.
;;;
;;;      Mod History, most recent first:
;;;      9/2/9  version 0.1b1
;;;              First cut.
;;;
;;; ----------------------------------------------------------------------------

(in-package "CL-DOCUMENTATION") 


;;; ----------------------------------------------------------------------------
;;;
(defclass CL-ALPHABETICAL-MENU-ITEM (ns:ns-menu-item)
  ((symbol :initarg :symbol :accessor item-symbol))
  (:documentation "Support for the CL alphabetical menu.")
  (:metaclass ns:+ns-object))

(defun populate-submenu (menu symbol-list)
  "Make menu-items for all symbols in SYMBOL-LIST, and add them to MENU"
  (dolist (symbol (reverse symbol-list))
    (let* ((menu-item (make-instance 'cl-alphabetical-menu-item :symbol symbol))
           (attributed-string (#/initWithString:attributes:
                               (#/alloc ns:ns-attributed-string) 
                               (ccl::%make-nsstring (string-downcase (string symbol)))
                               cmenu:*hemlock-menu-dictionary*)))
;      (setf (item-symbol menu-item) symbol)
      (#/setAttributedTitle: menu-item attributed-string)
      (#/setAction: menu-item (ccl::@selector "clAlphabeticalDocAction:"))
      (#/setTarget: menu-item  *cl-alphabetical-menu*)
      (#/addItem: menu menu-item))))

(defun make-submenu-item (title symbol-list)
  "Create a menu-item with a submenu, and populate the submenu with the symbols in SYMBOL-LIST."
  (let ((menu-item (make-instance ns:ns-menu-item))
        (attributed-string (#/initWithString:attributes:
                            (#/alloc ns:ns-attributed-string) 
                            (ccl::%make-nsstring title)
                            cmenu:*hemlock-menu-dictionary*))
        (submenu (make-instance ns:ns-menu)))
    (#/setAttributedTitle: menu-item attributed-string)
    (#/setSubmenu: menu-item submenu)
    (populate-submenu submenu symbol-list)
    menu-item))

(defparameter *ABCs* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

;;; ----------------------------------------------------------------------------
;;;
(defclass CL-ALPHABETICAL-MENU (ns:ns-menu)
  ((tool-menu :initform nil :accessor tool-menu)
   (text-view :initform nil :accessor text-view)
   (sub-title :initform "alphabetical" :reader sub-title))
  (:documentation "A popup menu with alphabetically ordered letter submenus.")
  (:metaclass ns:+ns-object))

(objc:defmethod (#/clAlphabeticalDocAction: :void) ((m cl-alphabetical-menu) (sender :id))
  (display-cl-doc (item-symbol sender) (text-view m)))

(defmethod initialize-instance :after ((menu cl-alphabetical-menu) &key)
  (setf (tool-menu menu) (cmenu:add-default-tool-menu menu)))

(defmethod add-submenus ((menu cl-alphabetical-menu))
  (let* ((letter-array-length (length *ABCs*))
         (letter-array (make-array letter-array-length :initial-element nil))
         miscellaneous first-letter index)
    (dolist (sym (apply #'append *cl-symbol-lists*))
      (setq first-letter (elt (string sym) 0))
      (setq index (position first-letter *ABCs* :test #'char-equal))
      (if index
        (push sym (aref letter-array index))
        (push sym miscellaneous)))
    (dotimes (idx letter-array-length)
      (let ((submenu-item (make-submenu-item (elt *ABCs* idx) 
                                             (sort (coerce (aref letter-array idx) 'list)
                                                   #'string> :key #'string))))
        (#/addItem: menu submenu-item)))
    (when miscellaneous
      (#/addItem: menu (#/separatorItem ns:ns-menu-item))    
      (let ((submenu-item (make-submenu-item "Other:" miscellaneous)))
        (#/addItem: menu submenu-item)))))

(objc:defmethod (#/update :void) ((self cl-alphabetical-menu))
  (cmenu:update-tool-menu self (tool-menu self) :sub-title (sub-title self))
  (call-next-method))

(setq *cl-alphabetical-menu* (make-instance 'cl-alphabetical-menu))

(add-submenus *cl-alphabetical-menu*)




