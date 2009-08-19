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
;;;
;;; Written by Bill Chiles
;;; Modified by Blaine Burks.
;;;
;;; This file defines all the "portable" keysyms.

(in-package :hemlock-internals)

;;; "Named" keys.
;;;
(define-keysym 9 "Tab")
(define-keysym 27 "Escape" "Altmode" "Alt")		;escape
(define-keysym #+cocotron 8 #-cocotron 127 "Delete" "Backspace")  ;backspace
(define-keysym 13 "Return" "Newline")
(define-keysym 10 "LineFeed")
(define-keysym 3 "Enter")
(define-keysym 32 "Space" " ")

;;; Letters.
;;;
(define-keysym 97 "a") (define-keysym 65 "A")
(define-keysym 98 "b") (define-keysym 66 "B")
(define-keysym 99 "c") (define-keysym 67 "C")
(define-keysym 100 "d") (define-keysym 68 "D")
(define-keysym 101 "e") (define-keysym 69 "E")
(define-keysym 102 "f") (define-keysym 70 "F")
(define-keysym 103 "g") (define-keysym 71 "G")
(define-keysym 104 "h") (define-keysym 72 "H")
(define-keysym 105 "i") (define-keysym 73 "I")
(define-keysym 106 "j") (define-keysym 74 "J")
(define-keysym 107 "k") (define-keysym 75 "K")
(define-keysym 108 "l") (define-keysym 76 "L")
(define-keysym 109 "m") (define-keysym 77 "M")
(define-keysym 110 "n") (define-keysym 78 "N")
(define-keysym 111 "o") (define-keysym 79 "O")
(define-keysym 112 "p") (define-keysym 80 "P")
(define-keysym 113 "q") (define-keysym 81 "Q")
(define-keysym 114 "r") (define-keysym 82 "R")
(define-keysym 115 "s") (define-keysym 83 "S")
(define-keysym 116 "t") (define-keysym 84 "T")
(define-keysym 117 "u") (define-keysym 85 "U")
(define-keysym 118 "v") (define-keysym 86 "V")
(define-keysym 119 "w") (define-keysym 87 "W")
(define-keysym 120 "x") (define-keysym 88 "X")
(define-keysym 121 "y") (define-keysym 89 "Y")
(define-keysym 122 "z") (define-keysym 90 "Z")

;;; Standard number keys.
;;;
(define-keysym 49 "1") (define-keysym 33 "!")
(define-keysym 50 "2") (define-keysym 64 "@")
(define-keysym 51 "3") (define-keysym 35 "#")
(define-keysym 52 "4") (define-keysym 36 "$")
(define-keysym 53 "5") (define-keysym 37 "%")
(define-keysym 54 "6") (define-keysym 94 "^")
(define-keysym 55 "7") (define-keysym 38 "&")
(define-keysym 56 "8") (define-keysym 42 "*")
(define-keysym 57 "9") (define-keysym 40 "(")
(define-keysym 48 "0") (define-keysym 41 ")")

;;; "Standard" symbol keys.
;;;
(define-keysym 96 "`") (define-keysym 126 "~")
(define-keysym 45 "-") (define-keysym 95 "_")
(define-keysym 61 "=") (define-keysym 43 "+")
(define-keysym 91 "[") (define-keysym 123 "{")
(define-keysym 93 "]") (define-keysym 125 "}")
(define-keysym 92 "\\") (define-keysym 124 "|")
(define-keysym 59 ";") (define-keysym 58 ":")
(define-keysym 39 "'") (define-keysym 34 "\"")
(define-keysym 44 ",") (define-keysym 60 "<")
(define-keysym 46 ".") (define-keysym 62 ">")
(define-keysym 47 "/") (define-keysym 63 "?")


(define-keysym :F1 "F1")
(define-keysym :F2 "F2")
(define-keysym :F3 "F3")
(define-keysym :F4 "F4")
(define-keysym :F5 "F5")
(define-keysym :F6 "F6")
(define-keysym :F7 "F7")
(define-keysym :F8 "F8")
(define-keysym :F9 "F9")
(define-keysym :F10 "F10")
(define-keysym :F11 "F11")
(define-keysym :F12 "F12")
(define-keysym :F13 "F13")
(define-keysym :F14 "F14")
(define-keysym :F15 "F15")
(define-keysym :F16 "F16")
(define-keysym :F17 "F17")
(define-keysym :F18 "F18")
(define-keysym :F19 "F19")
(define-keysym :F20 "F20")
(define-keysym :F21 "F21")
(define-keysym :F22 "F22")
(define-keysym :F23 "F23")
(define-keysym :F24 "F24")
(define-keysym :F25 "F25")
(define-keysym :F26 "F26")
(define-keysym :F27 "F27")
(define-keysym :F28 "F28")
(define-keysym :F29 "F29")
(define-keysym :F30 "F30")
(define-keysym :F31 "F31")
(define-keysym :F32 "F32")
(define-keysym :F33 "F33")
(define-keysym :F34 "F34")
(define-keysym :F35 "F35")

;;; Upper right key bank.
;;;
(define-keysym :printscreen "Printscreen")
;; Couldn't type scroll lock.
(define-keysym :pause "Pause")

;;; Middle right key bank.
;;;
(define-keysym :insert "Insert")
(define-keysym :del "Del" "Rubout" (string (code-char 127)))
(define-keysym :home "Home")
(define-keysym :pageup "Pageup")
(define-keysym :end "End")
(define-keysym :pagedown "Pagedown")

;;; Arrows.
;;;
(define-keysym :leftarrow "Leftarrow")
(define-keysym :uparrow "Uparrow")
(define-keysym :downarrow "Downarrow")
(define-keysym :rightarrow "Rightarrow")


(define-mouse-keysym 1 #xffff "Leftdown" "Super" :button-press)


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
    (setf (char-key-event (code-char i))
	  (make-key-event (string (char-downcase (code-char (+ i @-code))))
			  (key-event-modifier-mask "control")))))
(setf (char-key-event (code-char 9)) (make-key-event #k"Tab"))
(setf (char-key-event (code-char 10)) (make-key-event #k"Linefeed"))
(setf (char-key-event (code-char 13)) (make-key-event #k"Return"))
(setf (char-key-event (code-char 27)) (make-key-event #k"Alt"))
(setf (char-key-event (code-char 8)) (make-key-event #k"Backspace"))
;;;
;;; Other ASCII codes are exactly the same as the Common Lisp codes.
;;; 
(do ((i (char-code #\space) (1+ i)))
    ((= i 128))
  (setf (char-key-event (code-char i))
	(make-key-event (string (code-char i)))))

;;; This makes KEY-EVENT-CHAR the inverse of CHAR-KEY-EVENT from the start.
;;; It need not be this way, but it is.
;;;
(dotimes (i 128)
  (let ((character (code-char i)))
    (setf (key-event-char (char-key-event character)) character)))

;;; Since we treated these characters specially above when setting
;;; CHAR-KEY-EVENT above, we must set these KEY-EVENT-CHAR's specially
;;; to make quoting characters into Hemlock buffers more obvious for users.
;;;
(setf (key-event-char #k"C-h") #\backspace)
(setf (key-event-char #k"C-i") #\tab)
(setf (key-event-char #k"C-j") #\linefeed)
(setf (key-event-char #k"C-m") #\return)
