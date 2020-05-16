;;;
;;; Copyright 2014 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "GUI")

(defun @application-menu ()
  (let ((menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"App"))
        (item nil))
    (setq item (#/addItemWithTitle:action:keyEquivalent: menu #@"About Clozure CL" (objc:@selector #/orderFrontStandardAboutPanel:) #@""))
    (#/setTarget: item *nsapp*)
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Preferences..." (objc:@selector #/showPreferences:) #@";")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (setq item (#/addItemWithTitle:action:keyEquivalent: menu #@"Services" +null-ptr+ #@""))
    (let ((services-menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"Services")))
      (#/setSubmenu:forItem: menu services-menu item)
      (#/setServicesMenu: *nsapp* services-menu)
      (#/release services-menu))
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Hide Clozure CL" (objc:@selector #/hide:) #@"h")
    (setq item (#/addItemWithTitle:action:keyEquivalent: menu #@"Hide Others" (objc:@selector #/hideOtherApplications:) #@"h"))
    (#/setKeyEquivalentModifierMask: item (logior #$NSCommandKeyMask #$NSAlternateKeyMask))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Show All" (objc:@selector #/unhideAllApplications:) #@"")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Quit Clozure CL" (objc:@selector #/terminate:) #@"q")
    menu))

(defun @file-menu ()
  (let ((menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"File")))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"New" (objc:@selector #/newDocument:) #@"n")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"New Listener" (objc:@selector #/newListener:) #@"N")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Open..." (objc:@selector #/openDocument:) #@"o")
    ;; The "Open Recent" menu will be added here automatically.
    ;; NSDocumentController looks for a menu item with an action of
    ;; #/openDocument: and inserts the Open Recent menu immediately after
    ;; that item.
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Open Selection" (objc:@selector #/openSelection:) #@"d")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Close" (objc:@selector #/performClose:) #@"w")
    (let ((closesimilar
           (#/addItemWithTitle:action:keyEquivalent: menu #@"Close Similar" (objc:@selector #/performClose:) #@"w")))
      (#/setKeyEquivalentModifierMask: closesimilar (logior #$NSCommandKeyMask #$NSAlternateKeyMask)))
    
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Save" (objc:@selector #/saveDocument:) #@"s")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Save As..." (objc:@selector #/saveDocumentAs:) #@"S")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Save To..." (objc:@selector #/saveDocumentTo:) #@"")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Revert" (objc:@selector #/revertDocumentToSaved:) #@"")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Load File..." (objc:@selector #/loadFile:) #@"")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Compile File..." (objc:@selector #/compileFile:) #@"")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Page Setup..." (objc:@selector #/runPageLayout:) #@"P")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Print" (objc:@selector #/print:) #@"p")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Quit Clozure CL" (objc:@selector #/terminate:) #@"q")
    menu))

(defun @edit-menu ()
  (let ((menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"Edit"))
        (item nil))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Undo" (objc:@selector #/undo:) #@"z")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Redo" (objc:@selector #/redo:) #@"Z")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Cut" (objc:@selector #/cut:) #@"x")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Copy" (objc:@selector #/copy:) #@"c")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Paste" (objc:@selector #/paste:) #@"v")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Clear" (objc:@selector #/clear:) #@"")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Select All" (objc:@selector #/selectAll:) #@"a")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (setq item (#/addItemWithTitle:action:keyEquivalent: menu #@"Find" +null-ptr+ #@""))
    (let ((find-menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"Find"))
          (find-item nil))
      (setq find-item (#/addItemWithTitle:action:keyEquivalent: find-menu #@"Find..." (objc:@selector #/performFindPanelAction:) #@"f"))
      (#/setTag: find-item #$NSFindPanelActionShowFindPanel)
      (setq find-item (#/addItemWithTitle:action:keyEquivalent: find-menu #@"Find Next" (objc:@selector #/performFindPanelAction:) #@"g"))
      (#/setTag: find-item #$NSFindPanelActionNext)
      (setq find-item (#/addItemWithTitle:action:keyEquivalent: find-menu #@"Find Previous" (objc:@selector #/performFindPanelAction:) #@"G"))
      (#/setTag: find-item #$NSFindPanelActionPrevious)
      (#/setSubmenu:forItem: menu find-menu item)
      (#/release find-menu))
    menu))

(defun @lisp-menu ()
  (let ((menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"Lisp")))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Execute Selection" (objc:@selector #/evalSelection:) #@"e")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Execute All" (objc:@selector #/evalAll:) #@"E")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Compile Buffer" (objc:@selector #/compileBuffer:) #@"")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Compile and Load Buffer" (objc:@selector #/compileAndLoadBuffer:) #@"")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Interrupt" (objc:@selector #/interrupt:) #@",")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Continue" (objc:@selector #/continue:) #@"/")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Exit Break Loop" (objc:@selector #/exitBreak:) #@".")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Restarts..." (objc:@selector #/restarts:) #@"\\")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Backtrace..." (objc:@selector #/backtrace:) #@"B")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Inspect" (objc:@selector #/inspect:) #@"i")
    menu))

(defun @tools-menu ()
  (let ((menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"Tools")))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Apropos..." (objc:@selector #/showAproposWindow:) #@"")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Processes..." (objc:@selector #/showProcessesWindow:) #@"")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Search Files..." (objc:@selector #/showSearchFiles:) #@"F")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Definitions..." (objc:@selector #/showListDefinitions:) #@"")
    menu))

#-mac-app-store
(defun @experiments-menu ()
  (let ((menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"Experiments")))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"xapropos" (objc:@selector #/showXaproposWindow:) #@"")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"xinspector" (objc:@selector #/showNewInspector:) #@"")
    menu))

(defun @window-menu ()
  (let ((menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"Window")))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Minimize" (objc:@selector #/performMiniaturize:) #@"m")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Listener" (objc:@selector #/showListener:) #@"l")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Show System Console" (objc:@selector #/toggleConsole:) #@"")
    (let ((menu-item
           (#/addItemWithTitle:action:keyEquivalent: menu #@"Wrap Lines to Window" (objc:@selector #/toggleWindowLineWrapping:) #@"")))
      (if *wrap-lines-to-window*
          (#/setState: menu-item #$NSOnState)
          (#/setState: menu-item #$NSOffState)))
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Bring All to Front" (objc:@selector #/arrangeInFront:) #@"")
    menu))

(defun @help-menu ()
  (let ((menu (#/initWithTitle: (#/alloc ns:ns-menu) #@"Help")))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Clozure CL Help" (objc:@selector #/showHelp:) #@"?")
    (#/addItem: menu (#/separatorItem ns:ns-menu-item))
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Clozure CL Manual" (objc:@selector #/showManual:) #@"")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"HyperSpec Lookup" (objc:@selector #/hyperSpecLookUp:) #@"y")
    (#/addItemWithTitle:action:keyEquivalent: menu #@"Hemlock Commands" (objc:@selector #/showHemlockCommands:) #@"")
    menu))

(defun initialize-menus ()
  (let ((main-menu (#/mainMenu ccl::*nsapp*))
	(pool (#/init (#/alloc ns:ns-autorelease-pool))))
    (flet ((add-menu (name menu)
	     (let ((parent-item (#/addItemWithTitle:action:keyEquivalent:
				 main-menu name +null-ptr+ #@"")))
	       (#/setSubmenu:forItem: main-menu menu parent-item))))
      (add-menu #@"App" (#/autorelease (@application-menu)))
      (add-menu #@"File" (#/autorelease (@file-menu)))
      (add-menu #@"Edit" (#/autorelease (@edit-menu)))
      (add-menu #@"Lisp" (#/autorelease (@lisp-menu)))
      (add-menu #@"Tools" (#/autorelease (@tools-menu)))
      #-mac-app-store
      (add-menu #@"Experiments" (#/autorelease (@experiments-menu)))
      (let ((window-menu (@window-menu)))
	(add-menu #@"Window" window-menu)
	(#/setWindowsMenu: ccl::*nsapp* window-menu)
	(#/release window-menu))
      (add-menu #@"Help" (#/autorelease (@help-menu))))
    (#/release pool)))
