;; menu-uitls.lisp

#|
The MIT license.

Copyright (c) 2010 Paul L. Krueger

Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
and associated documentation files (the "Software"), to deal in the Software without restriction, 
including without limitation the rights to use, copy, modify, merge, publish, distribute, 
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT 
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :selector-utils))

(defpackage :interface-utilities
  (:nicknames :iu)
  (:export make-and-install-menu make-and-install-menuitems-after 
           test-menu test2-menu))

(in-package :iu)

;; Utility functions that allow lisp programs to add menus to the existing menubar

#|
(defun make-menu-item (title-str action-str &key (key-equiv ""))
  (let* ((ns-title (ccl::%make-nsstring name-str))
         (action-selector (get-selector action-str))
         (ns-key (ccl::%make-nsstring key-equiv))
         (mi (#/initWithTitle:action:keyEquivalent: 
              (#/alloc ns:ns-menu-item) 
              ns-title 
              action-selector
              ns-key)))
    (#/autorelease ns-title)
    (#/autorelease ns-key)
    (#/autorelease mi)
    mi))

|#

;; For the following function menu-item-specs should be a list where each
;; element of the list is itself a list of the form:
;;  (menu-item-name menu-item-action menu-item-key-equivalent menu-item-target)
;; where each of these except the menu-item-target must be acceptable as an 
;; argument to the function "string". Menu-item-target must be an Ojbective-C object,
;; but can be nil to signify no target (the default)
;; Used "menumadness" sample code from Apple Developer website to know what to do ...
;;    NSMenu *newMenu;
;;    NSMenuItem *newItem;
;;
;;    // Add the submenu
;;    newItem = [[NSMenuItem allocWithZone:[NSMenu menuZone]] initWithTitle:@"Flashy" action:NULL keyEquivalent:@""];
;;    newMenu = [[NSMenu allocWithZone:[NSMenu menuZone]] initWithTitle:@"Flashy"];
;;    [newItem setSubmenu:newMenu];
;;    [newMenu release];
;;    [[NSApp mainMenu] addItem:newItem];
;;    [newItem release];
;;  Basically you need to create both a menuitem and a menu for any menu displayed in the menubar
;;  and link them together. Then add the menuitem to the main menu (i.e. menubar)

(defun make-and-install-menu (menu-name &rest menu-item-specs)
  (let* ((ns-menu-name (ccl::%make-nsstring menu-name))
         (menuitem (#/initWithTitle:action:keyEquivalent: 
                    (#/allocWithZone: ns:ns-menu-item 
                                      (#/menuZone ns:ns-menu))
                    ns-menu-name
                    (%null-ptr)
                    #@""))
         (menu (#/initWithTitle: (#/allocWithZone: 
                                  ns:ns-menu (#/menuZone ns:ns-menu))
                                 ns-menu-name))
         (main-menu (#/mainMenu #&NSApp)))
    (dolist (mi menu-item-specs)
      (destructuring-bind (mi-title mi-selector &optional (mi-key "") mi-target) mi
        (let* ((ns-title (ccl::%make-nsstring (string mi-title)))
               (action-selector (get-selector (string mi-selector)))
               (ns-key (ccl::%make-nsstring (string mi-key)))
               (men-item (#/addItemWithTitle:action:keyEquivalent: menu 
                                                                   ns-title 
                                                                   action-selector
                                                                   ns-key)))
          (when mi-target
            (#/setTarget: men-item mi-target))
          (#/release ns-title)
          (#/release ns-key))))
    ;; Link up the new menuitem and new menu
    (#/setSubmenu: menuitem menu)
    (#/release menu)
    ;; Now tell the main menu to make this a sub-menu
    (#/addItem: main-menu menuitem)
    (#/release ns-menu-name)
    (#/release menuitem)
    menu))

;; The following function inserts one or more new menu-items immediately 
;; following a specified menu-item.
;; If the menu-item-name is "" the insertion will be after the first divider
;; (which has a blank name). If the menu-item-name does not exist in the menu,
;;  the items will be placed at the beginning of the menu.
;; menu-item-specs are as defined above for the make-and-install-menu function.
;; menu-name and menu-item-name arguments must be acceptable as argument to the 
;; "string" function.
(defun make-and-install-menuitems-after (menu-name menu-item-name &rest menu-item-specs)
  (let* ((ns-menu-name (ccl::%make-nsstring menu-name))
         (main-menu (#/mainMenu #&NSApp))
         (menuitem (or (#/itemWithTitle: main-menu ns-menu-name) 
                       (error "~s is not a valid menu title" menu-name)))
         (sub-menu (#/submenu menuitem))
         (ns-menu-item-name (ccl::%make-nsstring menu-item-name))
         (insert-index (#/indexOfItemWithTitle: sub-menu ns-menu-item-name)))
    (dolist (mi menu-item-specs)
      (destructuring-bind (mi-title mi-selector &optional (mi-key "") mi-target) mi
        (let* ((ns-title (ccl::%make-nsstring (string mi-title)))
               (action-selector (get-selector (string mi-selector)))
               (ns-key (ccl::%make-nsstring (string mi-key)))
               (men-item (#/insertItemWithTitle:action:keyEquivalent:atIndex: 
                          sub-menu 
                          ns-title 
                          action-selector
                          ns-key
                          (incf insert-index))))
          (when mi-target
            (#/setTarget: men-item mi-target))
          (#/release ns-title)
          (#/release ns-key))))
    (#/release ns-menu-item-name)
    (#/release ns-menu-name)))

(defun menu-item-action-selector (menu-name menu-item-name)
  (let* ((ns-menu-name (ccl::%make-nsstring menu-name))
         (main-menu (#/mainMenu #&NSApp))
         (menuitem (or (#/itemWithTitle: main-menu ns-menu-name) 
                       (error "~s is not a valid menu title" menu-name)))
         (sub-menu (#/submenu menuitem))
         (ns-menu-item-name (ccl::%make-nsstring menu-item-name))
         (target-mi (#/itemWithTitle: sub-menu ns-menu-item-name)))
    (#/release ns-menu-item-name)
    (#/release ns-menu-name)
    (#/action target-mi)))


(defun test-menu ()
  (make-and-install-menu "New App Menu" 
                         '("Menu Item1" "doFirstThing")
                         '("Menu Item2" "doSecondThing")))

(defun test2-menu ()
  (make-and-install-menuitems-after "File" "New"
                                    '("New myDoc" "newMyDoc")))
                                         

(provide :menu-utils)