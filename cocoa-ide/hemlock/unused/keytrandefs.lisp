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
;;; This file initializes character translation that would otherwise be done
;;; in Rompsite.Slisp, but there are no good hacks for mapping X11 keysyms
;;; to CMU Common Lisp character codes.
;;;
;;; Written by Bill Chiles.
;;; 

;;; The IBM RT keyboard has X11 keysyms defined for the following modifier
;;; keys, but we leave them mapped to nil indicating that they are non-events
;;; to be ignored:
;;;    ctrl		65507
;;;    meta (left)	65513
;;;    meta (right)	65514
;;;    shift (left)	65505
;;;    shift (right)	65506
;;;    lock		65509

(in-package "HEMLOCK-INTERNALS")


;;; Function keys for the RT.
;;;
(define-keysym 65470 #\f1 #\s-f1)
(define-keysym 65471 #\f2 #\s-f2)
(define-keysym 65472 #\f3 #\s-f3)
(define-keysym 65473 #\f4 #\s-f4)
(define-keysym 65474 #\f5 #\s-f5)
(define-keysym 65475 #\f6 #\s-f6)
(define-keysym 65476 #\f7 #\s-f7)
(define-keysym 65477 #\f8 #\s-f8)
(define-keysym 65478 #\f9 #\s-f9)
(define-keysym 65479 #\f10 #\s-f10)
(define-keysym 65480 #\f11 #\s-f11)
(define-keysym 65481 #\f12 #\s-f12)

;;; Function keys for the Sun (and other keyboards) -- L1-L10 and R1-R15.
;;;
(define-keysym 65482 #\f13 #\s-f13)
(define-keysym 65483 #\f14 #\s-f14)
(define-keysym 65484 #\f15 #\s-f15)
(define-keysym 65485 #\f16 #\s-f16)
(define-keysym 65486 #\f17 #\s-f17)
(define-keysym 65487 #\f18 #\s-f18)
(define-keysym 65488 #\f19 #\s-f19)
(define-keysym 65489 #\f20 #\s-f20)
(define-keysym 65490 #\f21 #\s-f21)
(define-keysym 65491 #\f22 #\s-f22)
(define-keysym 65492 #\f23 #\s-f23)
(define-keysym 65493 #\f24 #\s-f24)
(define-keysym 65494 #\f25 #\s-f25)
(define-keysym 65495 #\f26 #\s-f26)
(define-keysym 65496 #\f27 #\s-f27)
(define-keysym 65497 #\f28 #\s-f28)
(define-keysym 65498 #\f29 #\s-f29)
(define-keysym 65499 #\f30 #\s-f30)
(define-keysym 65500 #\f31 #\s-f31)
(define-keysym 65501 #\f32 #\s-f32)
(define-keysym 65502 #\f33 #\s-f33)
(define-keysym 65503 #\f34 #\s-f34)
(define-keysym 65504 #\f35 #\s-f35)

;;; Upper right key bank.
;;;
(define-keysym 65377 #\printscreen #\s-printscreen)
;; Couldn't type scroll lock.
(define-keysym 65299 #\pause #\s-pause)

;;; Middle right key bank.
;;;
(define-keysym 65379 #\insert #\s-insert)
(define-keysym 65535 #\delete #\delete)
(define-keysym 65360 #\home #\s-home)
(define-keysym 65365 #\pageup #\s-pageup)
(define-keysym 65367 #\end #\s-end)
(define-keysym 65366 #\pagedown #\s-pagedown)

;;; Arrows.
;;;
(define-keysym 65361 #\leftarrow #\s-leftarrow)
(define-keysym 65362 #\uparrow #\s-uparrow)
(define-keysym 65364 #\downarrow #\s-downarrow)
(define-keysym 65363 #\rightarrow #\s-rightarrow)

;;; Number pad.
;;;
(define-keysym 65407 #\numlock #\s-numlock)
(define-keysym 65421 #\s-return #\s-return)			;num-pad-enter
(define-keysym 65455 #\s-/ #\s-/)				;num-pad-/
(define-keysym 65450 #\s-* #\s-*)				;num-pad-*
(define-keysym 65453 #\s-- #\s--)				;num-pad--
(define-keysym 65451 #\s-+ #\s-+)				;num-pad-+
(define-keysym 65456 #\s-0 #\s-0)				;num-pad-0
(define-keysym 65457 #\s-1 #\s-1)				;num-pad-1
(define-keysym 65458 #\s-2 #\s-2)				;num-pad-2
(define-keysym 65459 #\s-3 #\s-3)				;num-pad-3
(define-keysym 65460 #\s-4 #\s-4)				;num-pad-4
(define-keysym 65461 #\s-5 #\s-5)				;num-pad-5
(define-keysym 65462 #\s-6 #\s-6)				;num-pad-6
(define-keysym 65463 #\s-7 #\s-7)				;num-pad-7
(define-keysym 65464 #\s-8 #\s-8)				;num-pad-8
(define-keysym 65465 #\s-9 #\s-9)				;num-pad-9
(define-keysym 65454 #\s-. #\s-.)				;num-pad-.

;;; "Named" keys.
;;;
(define-keysym 65289 #\tab #\tab)
(define-keysym 65307 #\escape #\escape)				;esc
(define-keysym 65288 #\backspace #\backspace)
(define-keysym 65293 #\return #\return)				;enter
(define-keysym 65512 #\linefeed #\linefeed)			;action
(define-keysym 32 #\space #\space)

;;; Letters.
;;;
(define-keysym 97 #\a) (define-keysym 65 #\A)
(define-keysym 98 #\b) (define-keysym 66 #\B)
(define-keysym 99 #\c) (define-keysym 67 #\C)
(define-keysym 100 #\d) (define-keysym 68 #\D)
(define-keysym 101 #\e) (define-keysym 69 #\E)
(define-keysym 102 #\f) (define-keysym 70 #\F)
(define-keysym 103 #\g) (define-keysym 71 #\G)
(define-keysym 104 #\h) (define-keysym 72 #\H)
(define-keysym 105 #\i) (define-keysym 73 #\I)
(define-keysym 106 #\j) (define-keysym 74 #\J)
(define-keysym 107 #\k) (define-keysym 75 #\K)
(define-keysym 108 #\l) (define-keysym 76 #\L)
(define-keysym 109 #\m) (define-keysym 77 #\M)
(define-keysym 110 #\n) (define-keysym 78 #\N)
(define-keysym 111 #\o) (define-keysym 79 #\O)
(define-keysym 112 #\p) (define-keysym 80 #\P)
(define-keysym 113 #\q) (define-keysym 81 #\Q)
(define-keysym 114 #\r) (define-keysym 82 #\R)
(define-keysym 115 #\s) (define-keysym 83 #\S)
(define-keysym 116 #\t) (define-keysym 84 #\T)
(define-keysym 117 #\u) (define-keysym 85 #\U)
(define-keysym 118 #\v) (define-keysym 86 #\V)
(define-keysym 119 #\w) (define-keysym 87 #\W)
(define-keysym 120 #\x) (define-keysym 88 #\X)
(define-keysym 121 #\y) (define-keysym 89 #\Y)
(define-keysym 122 #\z) (define-keysym 90 #\Z)

;;; Standard number keys.
;;;
(define-keysym 49 #\1) (define-keysym 33 #\!)
(define-keysym 50 #\2) (define-keysym 64 #\@)
(define-keysym 51 #\3) (define-keysym 35 #\#)
(define-keysym 52 #\4) (define-keysym 36 #\$)
(define-keysym 53 #\5) (define-keysym 37 #\%)
(define-keysym 54 #\6) (define-keysym 94 #\^)
(define-keysym 55 #\7) (define-keysym 38 #\&)
(define-keysym 56 #\8) (define-keysym 42 #\*)
(define-keysym 57 #\9) (define-keysym 40 #\()
(define-keysym 48 #\0) (define-keysym 41 #\))

;;; "Standard" symbol keys.
;;;
(define-keysym 96 #\`) (define-keysym 126 #\~)
(define-keysym 45 #\-) (define-keysym 95 #\_)
(define-keysym 61 #\=) (define-keysym 43 #\+)
(define-keysym 91 #\[) (define-keysym 123 #\{)
(define-keysym 93 #\]) (define-keysym 125 #\})
(define-keysym 92 #\\) (define-keysym 124 #\|)
(define-keysym 59 #\;) (define-keysym 58 #\:)
(define-keysym 39 #\') (define-keysym 34 #\")
(define-keysym 44 #\,) (define-keysym 60 #\<)
(define-keysym 46 #\.) (define-keysym 62 #\>)
(define-keysym 47 #\/) (define-keysym 63 #\?)


;;; Sun keyboard.
;;;
(define-keysym 65387 #\break #\s-break)				;alternate (Sun).
(define-keysym 65290 #\linefeed #\s-linefeed)
