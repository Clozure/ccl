;; -*- Log: hemlock.log; Package: Hemlock -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+CMU (ext:file-comment
       "$Header$")
;;;
;;; **********************************************************************
;;;
;;; Some bindings:
;;;

(in-package :hemlock)



;;;; Default key translations:

;;; This page defines prefix characters that set specified modifier bits on
;;; the next character typed.
;;;
(setf (key-translation #k"escape") '(:bits :meta))
(setf (key-translation #k"control-[") '(:bits :meta))
(setf (key-translation #k"control-z") '(:bits :control :meta))
(setf (key-translation #k"control-Z") '(:bits :control :meta))
(setf (key-translation #k"control-^") '(:bits :control))
(setf (key-translation #k"control-c") '(:bits :hyper))
(setf (key-translation #k"control-C") '(:bits :hyper))



;;;; Most every binding.

;;; Self insert letters:
;;;
(do-alpha-key-events (key-event :both)
  (bind-key "Self Insert" key-event))

(bind-key "Beginning of Line" #k"control-a")
(bind-key "Select to Beginning of Line" #k"control-A")
(bind-key "Delete Next Character" #k"control-d")
(bind-key "Delete Next Character" #k"del")
(bind-key "End of Line" #k"control-e")
(bind-key "Select to End of Line" #k"control-E")
(bind-key "Forward Character" #k"control-f")
(bind-key "Forward Character" #k"rightarrow")
(bind-key "Select Forward Character" #k"control-F")
(bind-key "Select Forward Character" #k"shift-rightarrow")
(bind-key "Backward Character" #k"control-b")
(bind-key "Backward Character" #k"leftarrow")
(bind-key "Select Backward Character" #k"control-B")
(bind-key "Select Backward Character" #k"shift-leftarrow")
(bind-key "Kill Line" #k"control-k")
(bind-key "Refresh Screen" #k"control-l")
(bind-key "Next Line" #k"control-n")
(bind-key "Next Line" #k"downarrow")
(bind-key "Select Next Line" #k"control-N")
(bind-key "Select Next Line" #k"shift-downarrow")
(bind-key "Previous Line" #k"control-p")
(bind-key "Previous Line" #k"uparrow")
(bind-key "Select Previous Line" #k"control-P")
(bind-key "Select Previous Line" #k"shift-uparrow")
(bind-key "Query Replace" #k"meta-%")
(bind-key "Reverse Incremental Search" #k"control-r")
(bind-key "Incremental Search" #k"control-s")
(bind-key "Forward Search" #k"meta-s")
(bind-key "Reverse Search" #k"meta-r")
(bind-key "Transpose Characters" #k"control-t")
(bind-key "Universal Argument" #k"control-u")
(bind-key "Scroll Window Down" #k"control-v")
(bind-key "Page Down" #k"pagedown")
(bind-key "Scroll Window Up" #k"meta-v")
(bind-key "Page Up" #k"pageup")
;(bind-key "Scroll Next Window Down" #k"control-meta-v")
;(bind-key "Scroll Next Window Up" #k"control-meta-V")

(bind-key "Do Nothing" #k"leftdown")
;(bind-key "Do Nothing" #k"leftup")

(bind-key "Abort Command" #k"control-g")
(bind-key "Abort Command" #k"control-G")
(bind-key "Abort Command" #k"control-x control-g")
(bind-key "Abort Command" #k"control-x control-G")


(bind-key "Process File Options" #k"control-x m" :global)
(bind-key "Ensure File Options Line" #k"control-meta-M" :global)
(bind-key "Beginning of Buffer" #k"home")
(bind-key "End of Buffer" #k"end")
(bind-key "Undo" #k"control-_")
(bind-key "Undo" #k"control-\/")
(bind-key "Describe Key" #k"meta-?")
(bind-key "What Cursor Position" #k"control-x =")


#||
(bind-key "Here to Top of Window" #k"leftdown")
(bind-key "Do Nothing" #k"leftup")
(bind-key "Top Line to Here" #k"rightdown")
(bind-key "Do Nothing" #k"rightup")
(bind-key "Point to Here" #k"middledown")
(bind-key "Point to Here" #k"super-leftdown")
(bind-key "Generic Pointer Up" #k"middleup")
(bind-key "Generic Pointer Up" #k"super-leftup")
(bind-key "Do Nothing" #k"super-rightup")
(bind-key "Insert Kill Buffer" #k"super-rightdown")
||#

(bind-key "Insert File" #k"control-x control-r")
(bind-key "Save File" #k"control-x control-s")
(bind-key "Write File" #k"control-x control-w")
;(bind-key "Visit File" #k"control-x control-v")
(bind-key "Find File" #k"control-x control-v")
(bind-key "Find File" #k"control-x control-f")
(bind-key "Backup File" #k"control-x meta-b")
;(bind-key "Save All Files" #k"control-x control-m")
;(bind-key "Save All Files" #k"control-x return")
;(bind-key "Save All Files and Exit" #k"control-x meta-z")

;(bind-key "List Buffers" #k"control-x control-b")
(bind-key "Buffer Not Modified" #k"meta-~")
;(bind-key "Check Buffer Modified" #k"control-x ~")
;(bind-key "Select Buffer" #k"control-x b")
;(bind-key "Select Previous Buffer" #k"control-meta-l")
;(bind-key "Circulate Buffers" #k"control-meta-L")
;(bind-key "Create Buffer" #k"control-x meta-b")
;(bind-key "Kill Buffer" #k"control-x k")
;(bind-key "Select Random Typeout Buffer" #k"hyper-t")

;(bind-key "Next Window" #k"control-x n")
;(bind-key "Next Window" #k"control-x o")
;(bind-key "Previous Window" #k"control-x p")
;(bind-key "Split Window" #k"control-x 2")
;(bind-key "New Window" #k"control-x control-n")
;(bind-key "Delete Window" #k"control-x d")
;(bind-key "Delete Next Window" #k"control-x 1")
;(bind-key "Line to Top of Window" #k"meta-!")
;(bind-key "Line to Center of Window" #k"meta-#")
;(bind-key "Top of Window" #k"meta-,")
;(bind-key "Bottom of Window" #k"meta-.")

(bind-key "Delete Previous Character" #k"delete")
(bind-key "Delete Previous Character" #k"backspace")
(bind-key "Kill Next Word" #k"meta-d")
(bind-key "Kill Previous Word" #k"meta-delete")
(bind-key "Kill Previous Word" #k"meta-backspace")
(bind-key "Exchange Point and Mark" #k"control-x control-x")
(bind-key "Mark Whole Buffer" #k"control-x h")
(bind-key "Set/Pop Mark" #k"control-@")
(bind-key "Set/Pop Mark" #k"control-space")
(bind-key "Pop and Goto Mark" #k"meta-@")
(bind-key "Pop Mark" #k"control-meta-space") ;#k"control-meta-@" = "Mark Form".
(bind-key "Kill Region" #k"control-w")
(bind-key "Save Region" #k"meta-w")
(bind-key "Un-Kill" #k"control-y")
(bind-key "Rotate Kill Ring" #k"meta-y")

(bind-key "Forward Word" #k"meta-f")
(bind-key "Forward Word" #k"meta-rightarrow")
(bind-key "Select Forward Word" #k"meta-F")
(bind-key "Select Forward Word" #k"meta-shift-rightarrow")
(bind-key "Backward Word" #k"meta-b")
(bind-key "Backward Word" #k"meta-leftarrow")
(bind-key "Select Backward Word" #k"meta-B")
(bind-key "Select Backward Word" #k"meta-shift-leftarrow")

(bind-key "Forward Paragraph" #k"meta-]")
(bind-key "Forward Sentence" #k"meta-e")
(bind-key "Backward Paragraph" #k"meta-[")
(bind-key "Backward Sentence" #k"meta-a")

(bind-key "Mark Paragraph" #k"meta-h")

(bind-key "Forward Kill Sentence" #k"meta-k")
(bind-key "Backward Kill Sentence" #k"control-x delete")
(bind-key "Backward Kill Sentence" #k"control-x backspace")

(bind-key "Beginning of Buffer" #k"meta-\<")
(bind-key "End of Buffer" #k"meta-\>")
(bind-key "Mark to Beginning of Buffer" #k"control-\<")
(bind-key "Mark to End of Buffer" #k"control-\>")

(bind-key "Extended Command" #k"meta-x")

(bind-key "Uppercase Word" #k"meta-u")
(bind-key "Lowercase Word" #k"meta-l")
(bind-key "Capitalize Word" #k"meta-c")

;(bind-key "Previous Page" #k"control-x [")
;(bind-key "Next Page" #k"control-x ]")
;(bind-key "Mark Page" #k"control-x control-p")
;(bind-key "Count Lines Page" #k"control-x l")

(bind-key "Expand Dynamic Abbreviation" #k"meta-/") ;; Aquamacs and LW binding
(bind-key "Expand Dynamic Abbreviation" #k"meta-`") ;; MCL binding

(bind-key "Help" #k"control-h")

;;;; Argument Digit and Negative Argument.

(bind-key "Argument Digit" #k"meta-\-")
(bind-key "Argument Digit" #k"meta-0")
(bind-key "Argument Digit" #k"meta-1")
(bind-key "Argument Digit" #k"meta-2")
(bind-key "Argument Digit" #k"meta-3")
(bind-key "Argument Digit" #k"meta-4")
(bind-key "Argument Digit" #k"meta-5")
(bind-key "Argument Digit" #k"meta-6")
(bind-key "Argument Digit" #k"meta-7")
(bind-key "Argument Digit" #k"meta-8")
(bind-key "Argument Digit" #k"meta-9")
(bind-key "Argument Digit" #k"control-\-")
(bind-key "Argument Digit" #k"control-0")
(bind-key "Argument Digit" #k"control-1")
(bind-key "Argument Digit" #k"control-2")
(bind-key "Argument Digit" #k"control-3")
(bind-key "Argument Digit" #k"control-4")
(bind-key "Argument Digit" #k"control-5")
(bind-key "Argument Digit" #k"control-6")
(bind-key "Argument Digit" #k"control-7")
(bind-key "Argument Digit" #k"control-8")
(bind-key "Argument Digit" #k"control-9")
(bind-key "Argument Digit" #k"control-meta-\-")
(bind-key "Argument Digit" #k"control-meta-0")
(bind-key "Argument Digit" #k"control-meta-1")
(bind-key "Argument Digit" #k"control-meta-2")
(bind-key "Argument Digit" #k"control-meta-3")
(bind-key "Argument Digit" #k"control-meta-4")
(bind-key "Argument Digit" #k"control-meta-5")
(bind-key "Argument Digit" #k"control-meta-6")
(bind-key "Argument Digit" #k"control-meta-7")
(bind-key "Argument Digit" #k"control-meta-8")
(bind-key "Argument Digit" #k"control-meta-9")

(bind-key "Digit" #k"\-")
(bind-key "Digit" #k"0")
(bind-key "Digit" #k"1")
(bind-key "Digit" #k"2")
(bind-key "Digit" #k"3")
(bind-key "Digit" #k"4")
(bind-key "Digit" #k"5")
(bind-key "Digit" #k"6")
(bind-key "Digit" #k"7")
(bind-key "Digit" #k"8")
(bind-key "Digit" #k"9")


;;;; Self Insert and Quoted Insert.

(bind-key "Quoted Insert" #k"control-q")
(bind-key "Native Quoted Insert" #k"meta-!")

(bind-key "Self Insert" #k"space")
(bind-key "Self Insert" #k"!")
(bind-key "Self Insert" #k"@")
(bind-key "Self Insert" #k"#")
(bind-key "Self Insert" #k"$")
(bind-key "Self Insert" #k"%")
(bind-key "Self Insert" #k"^")
(bind-key "Self Insert" #k"&")
(bind-key "Self Insert" #k"*")
(bind-key "Self Insert" #k"(")
(bind-key "Self Insert" #k")")
(bind-key "Self Insert" #k"_")
(bind-key "Self Insert" #k"+")
(bind-key "Self Insert" #k"~")
(bind-key "Self Insert" #k"[")
(bind-key "Self Insert" #k"]")
(bind-key "Self Insert" #k"\\")
(bind-key "Self Insert" #k"|")
(bind-key "Self Insert" #k":")
(bind-key "Self Insert" #k";")
(bind-key "Self Insert" #k"\"")
(bind-key "Self Insert" #k"'")
(bind-key "Self Insert" #k"=")
(bind-key "Self Insert" #k"`")
(bind-key "Self Insert" #k"\<")
(bind-key "Self Insert" #k"\>")
(bind-key "Self Insert" #k",")
(bind-key "Self Insert" #k".")
(bind-key "Self Insert" #k"?")
(bind-key "Self Insert" #k"/")
(bind-key "Self Insert" #k"{")
(bind-key "Self Insert" #k"}")



;;;; Echo Area.

;;; Basic echo-area commands.
;;; 
(bind-key "Help on Parse" #k"home" :mode "Echo Area")
(bind-key "Help on Parse" #k"control-_" :mode "Echo Area")

(bind-key "Complete Keyword" #k"escape" :mode "Echo Area")
(bind-key "Complete Field" #k"space" :mode "Echo Area")
(bind-key "Confirm Parse" #k"return" :mode "Echo Area")

;;; Rebind some standard commands to behave better.
;;; 
;;(bind-key "Kill Parse" #k"control-u" :mode "Echo Area")
(bind-key "Insert Parse Default" #k"control-i" :mode "Echo Area")
(bind-key "Insert Parse Default" #k"tab" :mode "Echo Area")
(bind-key "Echo Area Delete Previous Character" #k"delete" :mode "Echo Area")
(bind-key "Echo Area Delete Previous Character" #k"backspace" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"meta-h" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"meta-delete" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"meta-backspace" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"control-w" :mode "Echo Area")
(bind-key "Beginning of Parse" #k"control-a" :mode "Echo Area")
(bind-key "Beginning of Parse" #k"meta-\<" :mode "Echo Area")
(bind-key "Echo Area Backward Character" #k"control-b" :mode "Echo Area")
(bind-key "Echo Area Backward Word" #k"meta-b" :mode "Echo Area")
(bind-key "Next Parse" #k"control-n" :mode "Echo Area")
(bind-key "Previous Parse" #k"control-p" :mode "Echo Area")

;;; Remove some dangerous standard bindings.
;;; 
(bind-key "Illegal" #k"control-x" :mode "Echo Area")
(bind-key "Illegal" #k"control-meta-c" :mode "Echo Area")
(bind-key "Illegal" #k"control-meta-s" :mode "Echo Area")
(bind-key "Illegal" #k"control-meta-l" :mode "Echo Area")
(bind-key "Illegal" #k"meta-x" :mode "Echo Area")
(bind-key "Illegal" #k"control-s" :mode "Echo Area")
(bind-key "Illegal" #k"control-r" :mode "Echo Area")
(bind-key "Illegal" #k"hyper-t" :mode "Echo Area")



;;;; Listener and Editor Modes.
(bind-key "Confirm Listener Input" #k"return" :mode "Listener")
(bind-key "Confirm Listener Input" #k"shift-return" :mode "Listener")
(bind-key "Previous Interactive Input" #k"meta-p" :mode "Listener")
(bind-key "Search Previous Interactive Input" #k"meta-P" :mode "Listener")
(bind-key "Next Interactive Input" #k"meta-n" :mode "Listener")
(bind-key "Kill Interactive Input" #k"meta-i" :mode "Listener")
;(bind-key "Abort Eval Input" #k"control-meta-i" :mode "Listener")
(bind-key "Interactive Beginning of Line" #k"control-a" :mode "Listener")
(bind-key "POP Or Delete Forward" #k"control-d" :mode "Listener")
(bind-key "Reenter Interactive Input" #k"control-return" :mode "Listener")

;;; Make the user use C-x C-w to save the file, and take care
;;; not to associate the Listener document with any particular
;;; file or type.
(bind-key "Illegal" #k"control-x control-s" :mode "Listener")
(bind-key "Save To File" #k"control-x control-w" :mode "Listener")

(bind-key "Editor Evaluate Expression" #k"control-meta-escape")
(bind-key "Editor Evaluate Expression" #k"meta-escape"  :mode "Editor")

(bind-key "Editor Execute Expression" #k"enter" :mode "Editor")
(bind-key "Editor Execute Expression" #k"control-x control-e" :mode "Editor")
(bind-key "Editor Execute Defun" #k"control-meta-x" :mode "Editor")
(bind-key "Editor Execute Defun" #k"control-x control-c" :mode "Editor")
(bind-key "Editor Execute Defun" #k"control-x control-C" :mode "Editor")

(bind-key "Editor Macroexpand-1 Expression" #k"control-m" :mode "Editor")
(bind-key "Editor Macroexpand Expression" #k"control-x control-m" :mode "Editor")
(bind-key "Editor Describe Function Call" #k"control-meta-A" :mode "Editor")
(bind-key "Editor Describe Symbol" #k"control-meta-S" :mode "Editor")


;;;; Lisp (some).

(bind-key "Indent Form" #k"control-meta-q")
(bind-key "Fill Lisp Comment Paragraph" #k"meta-q" :mode "Lisp")
(bind-key "Current Function Arglist" #k"control-x control-a" :mode "Lisp")
(bind-key "Arglist On Space" #k"Space" :mode "Lisp")
(bind-key "Defindent" #k"control-meta-#")
(bind-key "Beginning of Defun" #k"control-meta-a")
(bind-key "Select to Beginning of Defun" #k"control-meta-A")
(bind-key "End of Defun" #k"control-meta-e")
(bind-key "Select to End of Defun" #k"control-meta-E")
(bind-key "Forward Form" #k"control-meta-f")
(bind-key "Forward Form" #k"control-rightarrow")
(bind-key "Select Forward Form" #k"control-meta-F")
(bind-key "Select Forward Form" #k"control-shift-rightarrow")
(bind-key "Backward Form" #k"control-meta-b")
(bind-key "Backward Form" #k"control-leftarrow")
(bind-key "Select Backward Form" #k"control-meta-B")
(bind-key "Select Backward Form" #k"control-shift-leftarrow")
(bind-key "Forward List" #k"control-meta-n")
(bind-key "Select Forward List" #k"control-meta-N")
(bind-key "Backward List" #k"control-meta-p")
(bind-key "Select Backward List" #k"control-meta-P")
(bind-key "Transpose Forms" #k"control-meta-t")
(bind-key "Forward Kill Form" #k"control-meta-k")
(bind-key "Backward Kill Form" #k"control-meta-backspace")
(bind-key "Backward Kill Form" #k"control-meta-delete")
(bind-key "Mark Form" #k"control-meta-@")
(bind-key "Mark Defun" #k"control-meta-h")
(bind-key "Insert ()" #k"meta-(")
(bind-key "Move over )" #k"meta-)")
(bind-key "Backward Up List" #k"control-meta-(")
(bind-key "Backward Up List" #k"control-meta-u")
(bind-key "Forward Up List" #k"control-meta-)")
(bind-key "Down List" #k"control-meta-d")
(bind-key "Extract List" #k"control-meta-l")
;;(bind-key "Lisp Insert )" #k")" :mode "Lisp")
(bind-key "Delete Previous Character Expanding Tabs" #k"backspace" :mode "Lisp")
(bind-key "Delete Previous Character Expanding Tabs" #k"delete" :mode "Lisp")
(bind-key "Goto Absolute Line" #k"meta-g")
;;;(bind-key "Set Package Name" #k"control-x p" :mode "Lisp")

#+listener-bindings
(progn
(bind-key "Evaluate Expression" #k"meta-escape")
(bind-key "Evaluate Defun" #k"control-x control-e")
(bind-key "Compile Defun" #k"control-x control-c")
(bind-key "Compile Buffer File" #k"control-x c")

(bind-key "Describe Function Call" #k"control-meta-A")
(bind-key "Describe Symbol" #k"control-meta-S")
)

(bind-key "Goto Definition" #k"meta-.")

#+debugger-bindings
(progn
(bind-key "Debug Up" #k"control-meta-hyper-u")
(bind-key "Debug Down" #k"control-meta-hyper-d")
(bind-key "Debug Top" #k"control-meta-hyper-t")
(bind-key "Debug Bottom" #k"control-meta-hyper-b")
(bind-key "Debug Frame" #k"control-meta-hyper-f")
(bind-key "Debug Quit" #k"control-meta-hyper-q")
(bind-key "Debug Go" #k"control-meta-hyper-g")
(bind-key "Debug Abort" #k"control-meta-hyper-a")
(bind-key "Debug Restart" #k"control-meta-hyper-r")
(bind-key "Debug Help" #k"control-meta-hyper-h")
(bind-key "Debug Error" #k"control-meta-hyper-e")
(bind-key "Debug Backtrace" #k"control-meta-hyper-B")
(bind-key "Debug Print" #k"control-meta-hyper-p")
(bind-key "Debug Verbose Print" #k"control-meta-hyper-P")
(bind-key "Debug List Locals" #k"control-meta-hyper-l")
(bind-key "Debug Source" #k"control-meta-hyper-s")
(bind-key "Debug Edit Source" #k"control-meta-hyper-S")
(bind-key "Debug Flush Errors" #k"control-meta-hyper-F")
)


;;;; More Miscellaneous bindings.

(bind-key "Open Line" #k"Control-o")
(bind-key "New Line" #k"return")
(bind-key "New Line" #k"shift-return")

(bind-key "Transpose Words" #k"meta-t")
(bind-key "Transpose Lines" #k"control-x control-t")
(bind-key "Transpose Regions" #k"control-x t")

;(bind-key "Uppercase Region" #k"control-x control-u")
;(bind-key "Lowercase Region" #k"control-x control-l")
;(bind-key "Capitalize Region" #k"control-x control-c")

(bind-key "Delete Indentation" #k"meta-^")
(bind-key "Delete Indentation" #k"control-meta-^")
(bind-key "Delete Horizontal Space" #k"meta-\\")
(bind-key "Delete Blank Lines" #k"control-x control-o" :global)
(bind-key "Just One Space" #k"meta-space")
(bind-key "Back to Indentation" #k"meta-m")
(bind-key "Back to Indentation" #k"control-meta-m")
(bind-key "Indent Rigidly" #k"control-x tab")
(bind-key "Indent Rigidly" #k"control-x control-i")

(bind-key "Indent New Line" #k"linefeed")
(bind-key "Indent New Line" #k"control-return")
(bind-key "Indent" #k"tab")
(bind-key "Indent" #k"control-i")
(bind-key "Indent Region" #k"control-meta-\\")
(bind-key "Quote Tab" #k"meta-tab")

#||
(bind-key "Directory" #k"control-x control-\d")
(bind-key "Verbose Directory" #k"control-x control-D")
||#

(bind-key "Activate Region" #k"control-x control-@")
(bind-key "Activate Region" #k"control-x control-space")

(bind-key "Save Position" #k"control-x s")
(bind-key "Jump to Saved Position" #k"control-x j")
(bind-key "Put Register" #k"control-x x")
(bind-key "Get Register" #k"control-x g")

#+pascal-mode
(progn
(bind-key "Delete Previous Character Expanding Tabs" #k"backspace"
          :mode "Pascal")
(bind-key "Delete Previous Character Expanding Tabs" #k"delete" :mode "Pascal")
(bind-key "Scribe Insert Bracket" #k")" :mode "Pascal")
(bind-key "Scribe Insert Bracket" #k"]" :mode "Pascal")
(bind-key "Scribe Insert Bracket" #k"}" :mode "Pascal")
)


;;;; Auto Fill Mode.

(bind-key "Fill Paragraph" #k"meta-q")
(bind-key "Fill Region" #k"meta-g")
(bind-key "Set Fill Prefix" #k"control-x .")
(bind-key "Set Fill Column" #k"control-x f")
(bind-key "Auto Fill Return" #k"return" :mode "Fill")
(bind-key "Auto Fill Space" #k"space" :mode "Fill")
(bind-key "Auto Fill Linefeed" #k"linefeed" :mode "Fill")



#|
;;;; Keyboard macro bindings.

(bind-key "Define Keyboard Macro" #k"control-x (")
(bind-key "Define Keyboard Macro Key" #k"control-x meta-(")
(bind-key "End Keyboard Macro" #k"control-x )")
(bind-key "End Keyboard Macro" #k"control-x hyper-)")
(bind-key "Last Keyboard Macro" #k"control-x e")
(bind-key "Keyboard Macro Query" #k"control-x q")
|#


;;;; Spell bindings.
#||
(progn
  (bind-key "Check Word Spelling" #k"meta-$")
  (bind-key "Add Word to Spelling Dictionary" #k"control-x $")

  (dolist (info (command-bindings (getstring "Self Insert" *command-names*)))
    (let* ((key (car info))
           (key-event (svref key 0))
           (character (key-event-char key-event)))
      (unless (or (alpha-char-p character) (eq key-event #k"'"))
        (bind-key "Auto Check Word Spelling" key :mode "Spell"))))
  (bind-key "Auto Check Word Spelling" #k"return" :mode "Spell")
  (bind-key "Auto Check Word Spelling" #k"tab" :mode "Spell")
  (bind-key "Auto Check Word Spelling" #k"linefeed" :mode "Spell")
  (bind-key "Correct Last Misspelled Word" #k"meta-:")
  (bind-key "Undo Last Spelling Correction" #k"control-x a")
  )

;;;; Overwrite Mode.
||#

#||
(bind-key "Overwrite Delete Previous Character" #k"delete" :mode "Overwrite")
(bind-key "Overwrite Delete Previous Character" #k"backspace" :mode "Overwrite")

;;; Do up the printing characters ...
(do ((i 33 (1+ i)))
    ((= i 126))
  (let ((key-event (hi:char-key-event (code-char i))))
    (bind-key "Self Overwrite" key-event :mode "Overwrite")))

(bind-key "Self Overwrite" #k"space" :mode "Overwrite")
||#


;;;; Comment bindings.

(bind-key "Indent for Comment" #k"meta-;")
(bind-key "Set Comment Column" #k"control-x ;")
(bind-key "Kill Comment" #k"control-meta-;")
(bind-key "Down Comment Line" #k"meta-n")
(bind-key "Up Comment Line" #k"meta-p")
(bind-key "Indent New Comment Line" #k"meta-j")
(bind-key "Indent New Comment Line" #k"meta-linefeed")


#||
;;;; Word Abbrev Mode.

(bind-key "Add Mode Word Abbrev" #k"control-x control-a")
(bind-key "Add Global Word Abbrev" #k"control-x +")
(bind-key "Inverse Add Mode Word Abbrev" #k"control-x control-h")
(bind-key "Inverse Add Global Word Abbrev" #k"control-x \-")
;; Removed in lieu of "Pop and Goto Mark".
;;(bind-key "Abbrev Expand Only" #k"meta-space")
(bind-key "Word Abbrev Prefix Mark" #k"meta-\"")
(bind-key "Unexpand Last Word" #k"control-x u")

(dolist (key (list #k"!" #k"~" #k"@" #k"#" #k";" #k"$" #k"%" #k"^" #k"&" #k"*"
                   #k"\-" #k"_" #k"=" #k"+" #k"[" #k"]" #k"(" #k")" #k"/" #k"|"
                   #k":" #k"'" #k"\"" #k"{" #k"}" #k"," #k"\<" #k"." #k"\>"
                   #k"`" #k"\\" #k"?" #k"return" #k"newline" #k"tab" #k"space"))
  (bind-key "Abbrev Expand Only" key :mode "Abbrev"))

||#


;;;; Scribe Mode.

#+scribe-mode
(progn
(dolist (key (list #k"]" #k")" #k"}" #k"\>"))
  (bind-key "Scribe Insert Bracket" key :mode "Scribe"))

;;GB (bind-key "Scribe Buffer File" #k"control-x c" :mode "Scribe")
(bind-key "Select Scribe Warnings" #k"control-meta-C" :mode "Scribe")

(bind-key "Insert Scribe Directive" #k"hyper-i" :mode "Scribe")
)


;;;; Mailer commands.
#+mail-mode
(progn
;;; Clear everything user might hit to avoid getting the internal error
;;; message about modifying read-only buffers.
;;;
(do-alpha-key-events (key-event :both)
  (bind-key "Illegal" key-event :mode "Headers")
  (bind-key "Illegal" key-event :mode "Message"))

;;; Global.
;;;
(bind-key "Incorporate and Read New Mail" #k"control-x i")
(bind-key "Send Message" #k"control-x m")
(bind-key "Message Headers" #k"control-x r")

;;; Both Headers and Message modes.
;;;
;;; The bindings in these two blocks should be the same, one for "Message" mode
;;; and one for "Headers" mode.
;;;
(bind-key "Next Message" #k"meta-n" :mode "Message")
(bind-key "Previous Message" #k"meta-p" :mode "Message")
(bind-key "Next Undeleted Message" #k"n" :mode "Message")
(bind-key "Previous Undeleted Message" #k"p" :mode "Message")
(bind-key "Send Message" #k"s" :mode "Message")
(bind-key "Send Message" #k"m" :mode "Message")
(bind-key "Forward Message" #k"f" :mode "Message")
(bind-key "Headers Delete Message" #k"k" :mode "Message")
(bind-key "Headers Undelete Message" #k"u" :mode "Message")
(bind-key "Headers Refile Message" #k"o" :mode "Message")
(bind-key "List Mail Buffers" #k"l" :mode "Message")
(bind-key "Quit Headers" #k"q" :mode "Message")
(bind-key "Incorporate and Read New Mail" #k"i" :mode "Message")
(bind-key "Beginning of Buffer" #k"\<" :mode "Message")
(bind-key "End of Buffer" #k"\>" :mode "Message")
;;;
(bind-key "Next Message" #k"meta-n" :mode "Headers")
(bind-key "Previous Message" #k"meta-p" :mode "Headers")
(bind-key "Next Undeleted Message" #k"n" :mode "Headers")
(bind-key "Previous Undeleted Message" #k"p" :mode "Headers")
(bind-key "Send Message" #k"s" :mode "Headers")
(bind-key "Send Message" #k"m" :mode "Headers")
(bind-key "Forward Message" #k"f" :mode "Headers")
(bind-key "Headers Delete Message" #k"k" :mode "Headers")
(bind-key "Headers Undelete Message" #k"u" :mode "Headers")
(bind-key "Headers Refile Message" #k"o" :mode "Headers")
(bind-key "List Mail Buffers" #k"l" :mode "Headers")
(bind-key "Quit Headers" #k"q" :mode "Headers")
(bind-key "Incorporate and Read New Mail" #k"i" :mode "Headers")
(bind-key "Beginning of Buffer" #k"\<" :mode "Headers")
(bind-key "End of Buffer" #k"\>" :mode "Headers")


;;; Headers mode.
;;;
(bind-key "Delete Message and Down Line" #k"d" :mode "Headers")
(bind-key "Pick Headers" #k"h" :mode "Headers")
(bind-key "Show Message" #k"space" :mode "Headers")
(bind-key "Show Message" #k"." :mode "Headers")
(bind-key "Reply to Message" #k"r" :mode "Headers")
(bind-key "Expunge Messages" #k"!" :mode "Headers")
(bind-key "Headers Help" #k"?" :mode "Headers")


;;; Message mode.
;;;
(bind-key "Delete Message and Show Next" #k"d" :mode "Message")
(bind-key "Goto Headers Buffer" #k"^" :mode "Message")
(bind-key "Scroll Message" #k"space" :mode "Message")
(bind-key "Scroll Message" #k"control-v" :mode "Message")
(bind-key "Scroll Window Up" #k"backspace" :mode "Message")
(bind-key "Scroll Window Up" #k"delete" :mode "Message")
(bind-key "Reply to Message in Other Window" #k"r" :mode "Message")
(bind-key "Edit Message Buffer" #k"e" :mode "Message")
(bind-key "Insert Message Region" #k"hyper-y" :mode "Message")
(bind-key "Message Help" #k"?" :mode "Message")


;;; Draft mode.
;;;
(bind-key "Goto Headers Buffer" #k"hyper-^" :mode "Draft")
(bind-key "Goto Message Buffer" #k"hyper-m" :mode "Draft")
(bind-key "Deliver Message" #k"hyper-s" :mode "Draft")
(bind-key "Deliver Message" #k"hyper-c" :mode "Draft")
(bind-key "Insert Message Buffer" #k"hyper-y" :mode "Draft")
(bind-key "Delete Draft and Buffer" #k"hyper-q" :mode "Draft")
(bind-key "List Mail Buffers" #k"hyper-l" :mode "Draft")
(bind-key "Draft Help" #k"hyper-?" :mode "Draft")
);#+mail-mode


;;;; Netnews.

#+netnews-mode
(progn
;;; Clear everything user might hit to avoid getting the internal error
;;; message about modifying read-only buffers.
;;;
(do-alpha-key-events (key-event :both)
  (bind-key "Illegal" key-event :mode "News-Headers")
  (bind-key "Illegal" key-event :mode "News-Message"))


;;; Global Netnews bindings
;;;
(bind-key "Netnews Post Message" #k"C-x P")


;;; Both News-Headers and News-Message modes.
;;;
;;; The bindings in these two blocks should be the same, one for "News-Message"
;;; mode and one for "News-Headers" mode.
;;;
(bind-key "List All Groups" #k"l" :mode "News-Headers")
(bind-key "Netnews Append to File" #k"a" :mode "News-Headers")
(bind-key "Netnews Forward Message" #k"f" :mode "News-Headers")
(bind-key "Netnews Go to Next Group" #k"g" :mode "News-Headers")
(bind-key "Netnews Next Article" #k"n" :mode "News-Headers")
(bind-key "Netnews Previous Article" #k"p" :mode "News-Headers")
(bind-key "Netnews Quit Starting Here" #k"." :mode "News-Headers")
(bind-key "Netnews Group Punt Messages" #k"G" :mode "News-Headers")
(bind-key "Netnews Show Whole Header" #k"w" :mode "News-Headers")
(bind-key "Netnews Reply to Sender in Other Window" #k"r" :mode "News-Headers")
(bind-key "Netnews Reply to Group in Other Window" #k"R" :mode "News-Headers")
;;;
(bind-key "List All Groups" #k"l" :mode "News-Message")
(bind-key "Netnews Append to File" #k"a" :mode "News-Message")
(bind-key "Netnews Forward Message" #k"f" :mode "News-Message")
(bind-key "Netnews Go to Next Group" #k"g" :mode "News-Message")
(bind-key "Netnews Next Article" #k"n" :mode "News-Message")
(bind-key "Netnews Previous Article" #k"p" :mode "News-Message")
(bind-key "Netnews Quit Starting Here" #k"." :mode "News-Message")
(bind-key "Netnews Group Punt Messages" #k"G" :mode "News-Message")
(bind-key "Netnews Show Whole Header" #k"w" :mode "News-Message")
(bind-key "Netnews Reply to Sender in Other Window" #k"r" :mode "News-Message")
(bind-key "Netnews Reply to Group in Other Window" #k"R" :mode "News-Message")


;;; News-Headers.
;;;
(bind-key "Netnews Exit" #k"q" :mode "News-Headers")
(bind-key "Netnews Headers File Message" #k"o" :mode "News-Headers")
(bind-key "Netnews Headers Scroll Window Down" #k"c-v" :mode "News-Headers")
(bind-key "Netnews Headers Scroll Window Up" #k"m-v" :mode "News-Headers")
(bind-key "Netnews Next Line" #k"C-n" :mode "News-Headers")
(bind-key "Netnews Next Line" #k"Downarrow" :mode "News-Headers")
(bind-key "Netnews Previous Line" #k"C-p" :mode "News-Headers")
(bind-key "Netnews Previous Line" #k"Uparrow" :mode "News-Headers")
(bind-key "Netnews Select Message Buffer" #k"hyper-m" :mode "News-Headers")
(bind-key "Netnews Show Article" #k"space" :mode "News-Headers")


;;; News-Message.
;;;
(bind-key "Insert Message Region" #k"Hyper-y" :mode "News-Message")
(bind-key "Netnews Message File Message" #k"o" :mode "News-Message")
(bind-key "Netnews Message Keep Buffer" #k"k" :mode "News-Message")
(bind-key "Netnews Message Quit" #k"q" :mode "News-Message")
(bind-key "Netnews Message Scroll Down"  #k"space" :mode "News-Message")
(bind-key "Netnews Goto Draft Buffer" #k"hyper-d" :mode "News-Message")
(bind-key "Netnews Goto Headers Buffer" #k"^" :mode "News-Message")
(bind-key "Netnews Goto Headers Buffer" #k"hyper-h" :mode "News-Message")
(bind-key "Netnews Goto Post Buffer" #k"hyper-p" :mode "News-Message")
(bind-key "Scroll Window Up" #k"backspace" :mode "News-Message")


;;; Post.
;;;
(bind-key "Netnews Select Message Buffer" #k"hyper-m" :mode "Post")
(bind-key "Netnews Deliver Post" #k"hyper-s" :mode "Post")
(bind-key "Netnews Abort Post" #k"hyper-q" :mode "Post")
(bind-key "Insert Message Buffer" #k"Hyper-y" :mode "Post")


;;; News-Browse.

(bind-key "Netnews Quit Browse" #k"q" :mode "News-Browse")
(bind-key "Netnews Browse Add Group To File" #k"a" :mode "News-Browse")
(bind-key "Netnews Browse Read Group" #k"space" :mode "News-Browse")
(bind-key "Next Line" #k"n" :mode "News-Browse")
(bind-key "Previous Line" #k"p" :mode "News-Browse")
)

;;;; Process (Shell).

#+shell-mode
(progn
(bind-key "Shell" #k"control-meta-s")
(bind-key "Confirm Process Input" #k"return" :mode "Process")
(bind-key "Shell Complete Filename" #k"M-escape" :mode "Process")
(bind-key "Interrupt Buffer Subprocess" #k"hyper-c" :mode "Process")
(bind-key "Stop Buffer Subprocess" #k"hyper-z" :mode "Process")
(bind-key "Quit Buffer Subprocess" #k"hyper-\\")
(bind-key "Send EOF to Process" #k"hyper-d")

(bind-key "Previous Interactive Input" #k"meta-p" :mode "Process")
(bind-key "Search Previous Interactive Input" #k"meta-P" :mode "Process")
(bind-key "Interactive Beginning of Line" #k"control-a" :mode "Process")
(bind-key "Kill Interactive Input" #k"meta-i" :mode "Process")
(bind-key "Next Interactive Input" #k"meta-n" :mode "Process")
(bind-key "Reenter Interactive Input" #k"control-return" :mode "Process")
)

;;;; Bufed.

#||
(bind-key "Bufed" #k"control-x control-meta-b")
(bind-key "Bufed Delete" #k"d" :mode "Bufed")
(bind-key "Bufed Delete" #k"control-d" :mode "Bufed")
(bind-key "Bufed Undelete" #k"u" :mode "Bufed")
(bind-key "Bufed Expunge" #k"!" :mode "Bufed")
(bind-key "Bufed Quit" #k"q" :mode "Bufed")
(bind-key "Bufed Goto" #k"space" :mode "Bufed")
(bind-key "Bufed Goto and Quit" #k"super-leftdown" :mode "Bufed")
(bind-key "Bufed Save File" #k"s" :mode "Bufed")
(bind-key "Next Line" #k"n" :mode "Bufed")
(bind-key "Previous Line" #k"p" :mode "Bufed")


(bind-key "Bufed Help" #k"?" :mode "Bufed")
|#


;;;; Dired.
#||
(progn
(bind-key "Dired" #k"control-x control-meta-d")

(bind-key "Dired Delete File and Down Line" #k"d" :mode "Dired")
(bind-key "Dired Delete File with Pattern" #k"D" :mode "Dired")
(bind-key "Dired Delete File" #k"control-d" :mode "Dired")
(bind-key "Dired Delete File" #k"k" :mode "Dired")

(bind-key "Dired Undelete File and Down Line" #k"u" :mode "Dired")
(bind-key "Dired Undelete File with Pattern" #k"U" :mode "Dired")
(bind-key "Dired Undelete File" #k"control-u" :mode "Dired")

(bind-key "Dired Expunge Files" #k"!" :mode "Dired")
(bind-key "Dired Update Buffer" #k"hyper-u" :mode "Dired")
(bind-key "Dired View File" #k"space" :mode "Dired")
(bind-key "Dired Edit File" #k"e" :mode "Dired")
(bind-key "Dired Up Directory" #k"^" :mode "Dired")
(bind-key "Dired Quit" #k"q" :mode "Dired")
(bind-key "Dired Help" #k"?" :mode "Dired")

(bind-key "Dired Copy File" #k"c" :mode "Dired")
(bind-key "Dired Copy with Wildcard" #k"C" :mode "Dired")
(bind-key "Dired Rename File" #k"r" :mode "Dired")
(bind-key "Dired Rename with Wildcard" #k"R" :mode "Dired")

(bind-key "Next Line" #k"n" :mode "Dired")
(bind-key "Previous Line" #k"p" :mode "Dired")
)
||#

;;;; View Mode.
#||
(progn
(bind-key "View Scroll Down" #k"space" :mode "View")
(bind-key "Scroll Window Up" #k"b" :mode "View")
(bind-key "Scroll Window Up" #k"backspace" :mode "View")
(bind-key "Scroll Window Up" #k"delete" :mode "View")
(bind-key "View Return" #k"^" :mode "View")
(bind-key "View Quit" #k"q" :mode "View")
(bind-key "View Edit File" #k"e" :mode "View")
(bind-key "View Help" #k"?" :mode "View")
(bind-key "Beginning of Buffer" #k"\<" :mode "View")
(bind-key "End of Buffer" #k"\>" :mode "View")
)
||#

;;;; Lisp Library.

#||
(bind-key "Describe Pointer Library Entry" #k"leftdown" :mode "Lisp-Lib")
(bind-key "Load Pointer Library Entry" #k"rightdown" :mode "Lisp-Lib")
(bind-key "Describe Library Entry" #k"space" :mode "Lisp-Lib")
(bind-key "Load Library Entry" #k"l" :mode "Lisp-Lib")
(bind-key "Exit Lisp Library" #k"q" :mode "Lisp-Lib")
(bind-key "Lisp Library Help" #k"?" :mode "Lisp-Lib")
||#


;;;; Completion mode.

(dolist (c (command-bindings (getstring "Self Insert" *command-names*)))
  (bind-key "Completion Self Insert" (car c) :mode "Completion"))

(bind-key "Completion Self Insert" #k"space" :mode "Completion")
(bind-key "Completion Self Insert" #k"tab" :mode "Completion")
(bind-key "Completion Self Insert" #k"return" :mode "Completion")
(bind-key "Completion Self Insert" #k"linefeed" :mode "Completion")

(bind-key "Completion Complete Word" #k"end" :mode "Completion")
(bind-key "Completion Rotate Completions" #k"meta-end" :mode "Completion")

;;;; Caps-Lock mode.

(do-alpha-key-events (key-event :lower)
  (bind-key "Self Insert Caps Lock" key-event :mode "CAPS-LOCK"))


;;;; I-Search mode.
;;;;
;;;; Anything that's not explicitly bound here will exit i-search.

(dotimes (n hi::hemlock-char-code-limit)
  (when (standard-char-p (code-char n))
    (let ((key (make-key-event n)))
      (bind-key "I-Search Self Insert" key :mode "I-Search"))))

(bind-key "I-Search Repeat Forward" #k"control-s" :mode "I-Search")
(bind-key "I-Search Repeat Backward" #k"control-r" :mode "I-Search")
(bind-key "I-Search Backup" #k"backspace" :mode "I-Search")
(bind-key "I-Search Backup" #k"delete" :mode "I-Search")
(bind-key "I-Search Abort" #k"control-g" :mode "I-Search")
(bind-key "I-Search Abort" #k"control-G" :mode "I-Search")
(bind-key "I-Search Exit or Search" #k"escape" :mode "I-Search")
(bind-key "I-Search Yank Word" #k"control-w" :mode "I-Search")
(bind-key "Quoted Insert" #k"control-q" :mode "I-Search")


;;;; Query/Replace mode.
;;;;
;;;; Anything that's not explicitly bound here will exit i-search.

(bind-key "Query/Replace This" #k"y" :mode "Query/Replace")
(bind-key "Query/Replace This" #k"space" :mode "Query/Replace")
(bind-key "Query/Replace Skip" #k"n" :mode "Query/Replace")
(bind-key "Query/Replace Skip" #k"backspace" :mode "Query/Replace")
(bind-key "Query/Replace Skip" #k"delete" :mode "Query/Replace")
(bind-key "Query/Replace All" #k"!" :mode "Query/Replace")
(bind-key "Query/Replace Last" #k"." :mode "Query/Replace")
(bind-key "Query/Replace Exit" #k"q" :mode "Query/Replace")
(bind-key "Query/Replace Exit" #k"escape" :mode "Query/Replace")
(bind-key "Query/Replace Abort" #k"control-g" :mode "Query/Replace")
(bind-key "Query/Replace Abort" #k"control-G" :mode "Query/Replace")
(bind-key "Query/Replace Help" #k"h" :mode "Query/Replace")
(bind-key "Query/Replace Help" #k"?" :mode "Query/Replace")
(bind-key "Query/Replace Help" #k"home" :mode "Query/Replace")
(bind-key "Query/Replace Help" #k"control-_" :mode "Query/Replace")

;;;; Logical characters.
 
(setf (logical-key-event-p #k"control-g" :abort) t)
(setf (logical-key-event-p #k"y" :yes) t)
(setf (logical-key-event-p #k"space" :yes) t)
(setf (logical-key-event-p #k"n" :no) t)
(setf (logical-key-event-p #k"backspace" :no) t)
(setf (logical-key-event-p #k"delete" :no) t)
(setf (logical-key-event-p #k"home" :help) t)
(setf (logical-key-event-p #k"h" :help) t)
(setf (logical-key-event-p #k"?" :help) t)
(setf (logical-key-event-p #k"control-_" :help) t)
(setf (logical-key-event-p #k"return" :confirm) t)
(setf (logical-key-event-p #k"control-q" :quote) t)
(setf (logical-key-event-p #k"k" :keep) t)
(setf (logical-key-event-p #k"y" :y) t)
(setf (logical-key-event-p #k"Y" :y) t)
(setf (logical-key-event-p #k"n" :n) t)
(setf (logical-key-event-p #k"N" :n) t)

