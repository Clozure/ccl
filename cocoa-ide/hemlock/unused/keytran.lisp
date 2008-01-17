;;; -*- Log: hemlock.log; Package: extensions -*-
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
;;; This file contains a default character translation mechanism for X11
;;; scan codes, keysyms, button codes, and modifier bits.
;;;
;;; Written by Bill Chiles.
;;;

(in-package "EXTENSIONS")

(export '(define-keysym define-mouse-code define-keyboard-modifier
	  translate-character translate-mouse-character))



;;;; Keysym to character translation.

;;; Hemlock uses its own keysym to character translation since this is easier
;;; and more versatile than the CLX design.  Also, using CLX's mechanism is no
;;; more portable than writing our own translation based on the X11 protocol
;;; keysym specification.
;;;
;;; In the first table, nil indicates a non-event which is pertinent to
;;; ignoring modifier keys being pressed prior to pressing a key to be
;;; modified.  In the second table, nil simply indicates that there is no
;;; special shift translation for the keysym, and that the CLX shifted keysym
;;; should be looked up as normal (see TRANSLATE-CHARACTER).
;;;
;;; This mapping is initialized with DEFINE-KEYSYM in Keytrandefs.Lisp
;;;
(defvar *keysym-translations* (make-hash-table))
(defvar *shifted-keysym-translations* (make-hash-table))

(defun define-keysym (keysym char &optional shifted-char)
  "Defines a keysym for Hemlock's translation.  If shifted-char is supplied,
   it is a character to use when the :shift modifier is on for an incoming
   keysym.  If shifted-char is not supplied, and the :shift modifier is set,
   then XLIB:KEYCODE->KEYSYM is called with an index of 1 instead of 0.  If
   a :lock modifier is set, it is treated as a caps-lock.  See
   DEFINE-KEYBOARD-MODIFIER."
  (check-type char character)
  (setf (gethash keysym *keysym-translations*) char)
  (when shifted-char
    (check-type shifted-char character)
    (setf (gethash keysym *shifted-keysym-translations*) shifted-char))
  t)


;;; X modifier bits translation
;;;
(defvar *modifier-translations* ())

(defun define-keyboard-modifier (clx-mask modifier-name)
  "Causes clx-mask to be interpreted as modifier-name which must be one of
   :control, :meta, :super, :hyper, :shift, or :lock."
  (let ((map (assoc clx-mask *modifier-translations*)))
    (if map
	(rplacd map modifier-name)
	(push (cons clx-mask modifier-name) *modifier-translations*))))

(define-keyboard-modifier (xlib:make-state-mask :control) :control)
(define-keyboard-modifier (xlib:make-state-mask :mod-1) :meta)
(define-keyboard-modifier (xlib:make-state-mask :shift) :shift)
(define-keyboard-modifier (xlib:make-state-mask :lock) :lock)


(defun translate-character (display scan-code bits)
  "Translates scan-code and modifier bits to a Lisp character.  The scan code
   is first mapped to a keysym with index 0 for unshifted and index 1 for
   shifted.  If this keysym does not map to a character, and it is not a
   modifier key (shift, ctrl, etc.), then an error is signaled.  If the keysym
   is a modifier key, then nil is returned.  If we do have a character, and the
   shift bit is off, and the lock bit is on, and the character is alphabetic,
   then we get a new keysym with index 1, mapping it to a character.  If this
   does not result in a character, an error is signaled.  If we have a
   character, and the shift bit is on, then we look for a special shift mapping
   for the original keysym.  This allows for distinct characters for scan
   codes that map to the same keysym, shifted or unshifted, (e.g., number pad
   or arrow keys)."
  (let ((dummy #\?)
	shiftp lockp)
    (dolist (ele *modifier-translations*)
      (unless (zerop (logand (car ele) bits))
	(case (cdr ele)
	  (:shift (setf shiftp t))
	  (:lock (setf lockp t))
	  (t (setf dummy (set-char-bit dummy (cdr ele) t))))))
    (let* ((keysym (xlib:keycode->keysym display scan-code (if shiftp 1 0)))
	   (temp-char (gethash keysym *keysym-translations*)))
      (cond ((not temp-char)
	     (if (<= 65505 keysym 65518) ;modifier keys.
		 nil
		 (error "Undefined keysym ~S, describe EXT:DEFINE-KEYSYM."
			keysym)))
	    ((and (not shiftp) lockp (alpha-char-p temp-char))
	     (let* ((keysym (xlib:keycode->keysym display scan-code 1))
		    (char (gethash keysym *keysym-translations*)))
	       (unless char
		 (error "Undefined keysym ~S, describe EXT:DEFINE-KEYSYM."
			keysym))
	       (make-char char (logior (char-bits char) (char-bits dummy)))))
	    (shiftp
	     (let ((char (gethash keysym *shifted-keysym-translations*)))
	       (if char
		   (make-char char (logior (char-bits char) (char-bits dummy)))
		   (make-char temp-char (logior (char-bits temp-char)
						(char-bits dummy))))))
	    (t (make-char temp-char (logior (char-bits temp-char)
					    (char-bits dummy))))))))
		   
		   

;;;; Mouse to character translations.
		   
;;; Mouse codes come from the server numbered one through five.  This table is
;;; indexed by the code to retrieve a list.  The CAR is a cons of the char and
;;; shifted char associated with a :button-press event.  The CDR is a cons of
;;; the char and shifted char associated with a :button-release event.  Each
;;; of these is potentially nil (not a cons at all).
;;;
(defvar *mouse-translations* (make-array 6 :initial-element nil))
;;;
(defmacro mouse-press-chars (ele) `(car ,ele))
(defmacro mouse-release-chars (ele) `(cadr ,ele))

(defun define-mouse-code (button char shifted-char event-key)
  "Causes X button code to be interpreted as char.  Shift and Lock modifiers
   associated with button map to shifted-char.  For the same button code,
   event-key may be :button-press or :button-release."
  (check-type char character)
  (check-type shifted-char character)
  (check-type event-key (member :button-press :button-release))
  (let ((stuff (svref *mouse-translations* button))
	(trans (cons char shifted-char)))
    (if stuff
	(case event-key
	  (:button-press (setf (mouse-press-chars stuff) trans))
	  (:button-release (setf (mouse-release-chars stuff) trans)))
	(case event-key
	  (:button-press
	   (setf (svref *mouse-translations* button) (list trans nil)))
	  (:button-release
	   (setf (svref *mouse-translations* button) (list nil trans))))))
  t)

(define-mouse-code 1 #\leftdown #\super-leftdown :button-press)
(define-mouse-code 1 #\leftup #\super-leftup :button-release)

(define-mouse-code 2 #\middledown #\super-middledown :button-press)
(define-mouse-code 2 #\middleup #\super-middleup :button-release)

(define-mouse-code 3 #\rightdown #\super-rightdown :button-press)
(define-mouse-code 3 #\rightup #\super-rightup :button-release)

(defun translate-mouse-character (scan-code bits event-key)
  "Translates X button code, scan-code, and modifier bits, bits, for event-key
   (either :button-press or :button-release) to a Lisp character."
  (let ((temp (svref *mouse-translations* scan-code)))
    (unless temp (error "Unknown mouse button -- ~S." scan-code))
    (let ((trans (ecase event-key
		   (:button-press (mouse-press-chars temp))
		   (:button-release (mouse-release-chars temp)))))
      (unless trans (error "Undefined ~S characters for mouse button ~S."
			   event-key scan-code))
      (let ((dummy #\?)
	    shiftp)
	(dolist (ele *modifier-translations*)
	  (unless (zerop (logand (car ele) bits))
	    (let ((bit (cdr ele)))
	      (if (or (eq bit :shift) (eq bit :lock))
		  (setf shiftp t)
		  (setf dummy (set-char-bit dummy bit t))))))
	(let ((char (if shiftp (cdr trans) (car trans))))
	  (make-char char (logior (char-bits char) (char-bits dummy))))))))
