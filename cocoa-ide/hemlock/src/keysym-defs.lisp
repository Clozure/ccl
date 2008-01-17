;;; -*- Log: hemlock.log; Mode: Lisp; Package: Hemlock-Internals -*-
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
;;; This file defines all the definitions of keysyms (see key-event.lisp).
;;; These keysyms match those for X11.
;;;
;;; Written by Bill Chiles
;;; Modified by Blaine Burks.
;;;

(in-package :hemlock-internals)




;;; Function keys for the RT.
;;;

;;; This isn't the RT.
(eval-when (:compile-toplevel :execute)
  (ccl::use-interface-dir :cocoa))

(hemlock-ext:define-keysym #$NSF1FunctionKey "F1")
(hemlock-ext:define-keysym #$NSF2FunctionKey "F2")
(hemlock-ext:define-keysym #$NSF3FunctionKey "F3")
(hemlock-ext:define-keysym #$NSF4FunctionKey "F4")
(hemlock-ext:define-keysym #$NSF5FunctionKey "F5")
(hemlock-ext:define-keysym #$NSF6FunctionKey "F6")
(hemlock-ext:define-keysym #$NSF7FunctionKey "F7")
(hemlock-ext:define-keysym #$NSF8FunctionKey "F8")
(hemlock-ext:define-keysym #$NSF9FunctionKey "F9")
(hemlock-ext:define-keysym #$NSF10FunctionKey "F10")
(hemlock-ext:define-keysym #$NSF11FunctionKey "F11")
(hemlock-ext:define-keysym #$NSF12FunctionKey "F12")
(hemlock-ext:define-keysym #$NSF13FunctionKey "F13")
(hemlock-ext:define-keysym #$NSF14FunctionKey "F14")
(hemlock-ext:define-keysym #$NSF15FunctionKey "F15")
(hemlock-ext:define-keysym #$NSF16FunctionKey "F16")
(hemlock-ext:define-keysym #$NSF17FunctionKey "F17")
(hemlock-ext:define-keysym #$NSF18FunctionKey "F18")
(hemlock-ext:define-keysym #$NSF19FunctionKey "F19")
(hemlock-ext:define-keysym #$NSF20FunctionKey "F20")
(hemlock-ext:define-keysym #$NSF21FunctionKey "F21")
(hemlock-ext:define-keysym #$NSF22FunctionKey "F22")
(hemlock-ext:define-keysym #$NSF23FunctionKey "F23")
(hemlock-ext:define-keysym #$NSF24FunctionKey "F24")
(hemlock-ext:define-keysym #$NSF25FunctionKey "F25")
(hemlock-ext:define-keysym #$NSF26FunctionKey "F26")
(hemlock-ext:define-keysym #$NSF27FunctionKey "F27")
(hemlock-ext:define-keysym #$NSF28FunctionKey "F28")
(hemlock-ext:define-keysym #$NSF29FunctionKey "F29")
(hemlock-ext:define-keysym #$NSF30FunctionKey "F30")
(hemlock-ext:define-keysym #$NSF31FunctionKey "F31")
(hemlock-ext:define-keysym #$NSF32FunctionKey "F32")
(hemlock-ext:define-keysym #$NSF33FunctionKey "F33")
(hemlock-ext:define-keysym #$NSF34FunctionKey "F34")
(hemlock-ext:define-keysym #$NSF35FunctionKey "F35")


;;; Upper right key bank.
;;;
(hemlock-ext:define-keysym #$NSPrintScreenFunctionKey "Printscreen")
;; Couldn't type scroll lock.
(hemlock-ext:define-keysym #$NSPauseFunctionKey "Pause")

;;; Middle right key bank.
;;;
(hemlock-ext:define-keysym #$NSInsertFunctionKey "Insert")
(hemlock-ext:define-keysym #$NSDeleteFunctionKey "Del" "Rubout" (string (code-char 127)))
(hemlock-ext:define-keysym #$NSHomeFunctionKey "Home")
(hemlock-ext:define-keysym #$NSPageUpFunctionKey "Pageup")
(hemlock-ext:define-keysym #$NSEndFunctionKey "End")
(hemlock-ext:define-keysym #$NSPageDownFunctionKey "Pagedown")

;;; Arrows.
;;;
(hemlock-ext:define-keysym #$NSLeftArrowFunctionKey "Leftarrow")
(hemlock-ext:define-keysym #$NSUpArrowFunctionKey "Uparrow")
(hemlock-ext:define-keysym #$NSDownArrowFunctionKey "Downarrow")
(hemlock-ext:define-keysym #$NSRightArrowFunctionKey "Rightarrow")


;;; "Named" keys.
;;;
(hemlock-ext:define-keysym 9 "Tab")
(hemlock-ext:define-keysym 27 "Escape" "Altmode" "Alt")		;escape
(hemlock-ext:define-keysym 127 "Delete" "Backspace")				;backspace
(hemlock-ext:define-keysym 13 "Return" "Newline")
(hemlock-ext:define-keysym 10 "LineFeed")
(hemlock-ext:define-keysym 3 "Enter")
(hemlock-ext:define-keysym 32 "Space" " ")

;;; Letters.
;;;
(hemlock-ext:define-keysym 97 "a") (hemlock-ext:define-keysym 65 "A")
(hemlock-ext:define-keysym 98 "b") (hemlock-ext:define-keysym 66 "B")
(hemlock-ext:define-keysym 99 "c") (hemlock-ext:define-keysym 67 "C")
(hemlock-ext:define-keysym 100 "d") (hemlock-ext:define-keysym 68 "D")
(hemlock-ext:define-keysym 101 "e") (hemlock-ext:define-keysym 69 "E")
(hemlock-ext:define-keysym 102 "f") (hemlock-ext:define-keysym 70 "F")
(hemlock-ext:define-keysym 103 "g") (hemlock-ext:define-keysym 71 "G")
(hemlock-ext:define-keysym 104 "h") (hemlock-ext:define-keysym 72 "H")
(hemlock-ext:define-keysym 105 "i") (hemlock-ext:define-keysym 73 "I")
(hemlock-ext:define-keysym 106 "j") (hemlock-ext:define-keysym 74 "J")
(hemlock-ext:define-keysym 107 "k") (hemlock-ext:define-keysym 75 "K")
(hemlock-ext:define-keysym 108 "l") (hemlock-ext:define-keysym 76 "L")
(hemlock-ext:define-keysym 109 "m") (hemlock-ext:define-keysym 77 "M")
(hemlock-ext:define-keysym 110 "n") (hemlock-ext:define-keysym 78 "N")
(hemlock-ext:define-keysym 111 "o") (hemlock-ext:define-keysym 79 "O")
(hemlock-ext:define-keysym 112 "p") (hemlock-ext:define-keysym 80 "P")
(hemlock-ext:define-keysym 113 "q") (hemlock-ext:define-keysym 81 "Q")
(hemlock-ext:define-keysym 114 "r") (hemlock-ext:define-keysym 82 "R")
(hemlock-ext:define-keysym 115 "s") (hemlock-ext:define-keysym 83 "S")
(hemlock-ext:define-keysym 116 "t") (hemlock-ext:define-keysym 84 "T")
(hemlock-ext:define-keysym 117 "u") (hemlock-ext:define-keysym 85 "U")
(hemlock-ext:define-keysym 118 "v") (hemlock-ext:define-keysym 86 "V")
(hemlock-ext:define-keysym 119 "w") (hemlock-ext:define-keysym 87 "W")
(hemlock-ext:define-keysym 120 "x") (hemlock-ext:define-keysym 88 "X")
(hemlock-ext:define-keysym 121 "y") (hemlock-ext:define-keysym 89 "Y")
(hemlock-ext:define-keysym 122 "z") (hemlock-ext:define-keysym 90 "Z")

;;; Standard number keys.
;;;
(hemlock-ext:define-keysym 49 "1") (hemlock-ext:define-keysym 33 "!")
(hemlock-ext:define-keysym 50 "2") (hemlock-ext:define-keysym 64 "@")
(hemlock-ext:define-keysym 51 "3") (hemlock-ext:define-keysym 35 "#")
(hemlock-ext:define-keysym 52 "4") (hemlock-ext:define-keysym 36 "$")
(hemlock-ext:define-keysym 53 "5") (hemlock-ext:define-keysym 37 "%")
(hemlock-ext:define-keysym 54 "6") (hemlock-ext:define-keysym 94 "^")
(hemlock-ext:define-keysym 55 "7") (hemlock-ext:define-keysym 38 "&")
(hemlock-ext:define-keysym 56 "8") (hemlock-ext:define-keysym 42 "*")
(hemlock-ext:define-keysym 57 "9") (hemlock-ext:define-keysym 40 "(")
(hemlock-ext:define-keysym 48 "0") (hemlock-ext:define-keysym 41 ")")

;;; "Standard" symbol keys.
;;;
(hemlock-ext:define-keysym 96 "`") (hemlock-ext:define-keysym 126 "~")
(hemlock-ext:define-keysym 45 "-") (hemlock-ext:define-keysym 95 "_")
(hemlock-ext:define-keysym 61 "=") (hemlock-ext:define-keysym 43 "+")
(hemlock-ext:define-keysym 91 "[") (hemlock-ext:define-keysym 123 "{")
(hemlock-ext:define-keysym 93 "]") (hemlock-ext:define-keysym 125 "}")
(hemlock-ext:define-keysym 92 "\\") (hemlock-ext:define-keysym 124 "|")
(hemlock-ext:define-keysym 59 ";") (hemlock-ext:define-keysym 58 ":")
(hemlock-ext:define-keysym 39 "'") (hemlock-ext:define-keysym 34 "\"")
(hemlock-ext:define-keysym 44 ",") (hemlock-ext:define-keysym 60 "<")
(hemlock-ext:define-keysym 46 ".") (hemlock-ext:define-keysym 62 ">")
(hemlock-ext:define-keysym 47 "/") (hemlock-ext:define-keysym 63 "?")


(hemlock-ext::define-mouse-keysym 1 #xe000 "Leftdown" "Super" :button-press)

;;;

;(hemlock-ext:define-keysym 65290 "linefeed")



;;;; SETFs of KEY-EVENT-CHAR and CHAR-KEY-EVENT.

;;; Converting ASCII control characters to Common Lisp control characters:
;;; ASCII control character codes are separated from the codes of the
;;; "non-controlified" characters by the code of atsign.  The ASCII control
;;; character codes range from ^@ (0) through ^_ (one less than the code of
;;; space).  We iterate over this range adding the ASCII code of atsign to
;;; get the "non-controlified" character code.  With each of these, we turn
;;; the code into a Common Lisp character and set its :control bit.  Certain
;;; ASCII control characters have to be translated to special Common Lisp
;;; characters outside of the loop.
;;;    With the advent of Hemlock running under X, and all the key bindings
;;; changing, we also downcase each Common Lisp character (where normally
;;; control characters come in upcased) in an effort to obtain normal command
;;; bindings.  Commands bound to uppercase modified characters will not be
;;; accessible to terminal interaction.
;;; 
(let ((@-code (char-code #\@)))
  (dotimes (i (char-code #\space))
    (setf (hemlock-ext:char-key-event (code-char i))
	  (hemlock-ext::make-key-event (string (char-downcase (code-char (+ i @-code))))
			       (hemlock-ext:key-event-modifier-mask "control")))))
(setf (hemlock-ext:char-key-event (code-char 9)) (hemlock-ext::make-key-event #k"Tab"))
(setf (hemlock-ext:char-key-event (code-char 10)) (hemlock-ext::make-key-event #k"Linefeed"))
(setf (hemlock-ext:char-key-event (code-char 13)) (hemlock-ext::make-key-event #k"Return"))
(setf (hemlock-ext:char-key-event (code-char 27)) (hemlock-ext::make-key-event #k"Alt"))
(setf (hemlock-ext:char-key-event (code-char 8)) (hemlock-ext::make-key-event #k"Backspace"))
;;;
;;; Other ASCII codes are exactly the same as the Common Lisp codes.
;;; 
(do ((i (char-code #\space) (1+ i)))
    ((= i 128))
  (setf (hemlock-ext:char-key-event (code-char i))
	(hemlock-ext::make-key-event (string (code-char i)))))

;;; This makes KEY-EVENT-CHAR the inverse of CHAR-KEY-EVENT from the start.
;;; It need not be this way, but it is.
;;;
(dotimes (i 128)
  (let ((character (code-char i)))
    (setf (hemlock-ext::key-event-char (hemlock-ext:char-key-event character)) character)))

;;; Since we treated these characters specially above when setting
;;; HEMLOCK-EXT:CHAR-KEY-EVENT above, we must set these HEMLOCK-EXT:KEY-EVENT-CHAR's specially
;;; to make quoting characters into Hemlock buffers more obvious for users.
;;;
(setf (hemlock-ext:key-event-char #k"C-h") #\backspace)
(setf (hemlock-ext:key-event-char #k"C-i") #\tab)
(setf (hemlock-ext:key-event-char #k"C-j") #\linefeed)
(setf (hemlock-ext:key-event-char #k"C-m") #\return)
