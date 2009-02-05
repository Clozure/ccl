;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
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
;;; This file implements key-events for representing editor input.
;;;
;;; Written by Blaine Burks and Bill Chiles.
;;;

(in-package :hemlock-internals)


;;; Objects involved in key events:
;;; (1) a KEY-EVENT describes a combination of a KEYSYM and MODIFIERS.  KEY-EVENTS
;;;   are interned, so there is a unique key-event for each combination of keysym and
;;;   modifiers.
;;; (2) A KEYSYM is an object representing a key.  It must be declared to be so via
;;;  define-keysym.  A KEYSYM must be defined before a key-event based on it can be
;;;  defined.
;;; (3) A CODE is a system-dependent fixnum value for a KEYSYM.  It must be defined
;;; before any events actually occur, but it doesn't need to be defined in order to
;;; create key-events.
;;;
;;; The Keysym can be the same as a code, but separating them deals with a bootstrapping
;;; problem: keysyms cannot be defined before hemlock is loaded, but hemlock wants to
;;; define key events while it's loading.  So we define key events using keysyms, and let
;;; their codes be defined later


;;;; Keysym <==> Name translation.

;;; Keysyms are named by case-insensitive names.  However, if the name
;;; consists of a single character, the name is case-sensitive.
;;;

;;; This table maps a keysym to a list of names.  The first name is the
;;; preferred printing name.
;;;
(defvar *keysyms-to-names*)
 
;;; This table maps all keysym names to the appropriate keysym.
;;;
(defvar *names-to-keysyms*)

(declaim (inline name-keysym keysym-names keysym-preferred-name))

(defun name-keysym (name)
  "This returns the keysym named name.  If name is unknown, this returns nil."
  (gethash (get-name-case-right name) *names-to-keysyms*))

(defun keysym-names (keysym)
  "This returns the list of all names for keysym.  If keysym is undefined,
   this returns nil."
  (or (gethash keysym *keysyms-to-names*)
      (let* ((name (char-name (code-char keysym))))
        (when name (setf (gethash keysym *keysyms-to-names*)
                         (list name))))))

(defun keysym-preferred-name (keysym)
  "This returns the preferred name for keysym, how it is typically printed.
   If keysym is undefined, this returns nil."
  (car (keysym-names keysym)))



;;;; Character key-event stuff.

;;; GET-NAME-CASE-RIGHT -- Internal.
;;;
;;; This returns the canonical string for a keysym name for use with
;;; hash tables.
;;;
(defun get-name-case-right (string)
  (if (= (length string) 1) string (string-downcase string)))

;;; DEFINE-KEYSYM -- Public
;;;
(defun define-keysym (keysym preferred-name &rest other-names)
  "This establishes a mapping from preferred-name to keysym for purposes of
   specifying key-events in #k syntax.  Other-names also map to keysym, but the
   system uses preferred-name when printing key-events.  The names are
   case-insensitive simple-strings.  Redefining a keysym or re-using names has
   undefined effects."
  (setf (gethash keysym *keysyms-to-names*) (cons preferred-name other-names))
  (dolist (name (cons preferred-name other-names))
    (setf (gethash (get-name-case-right name) *names-to-keysyms*) keysym)))

;;; This is an a-list mapping native modifier bit masks to defined key-event
;;; modifier names.
;;; 
(defvar *modifier-translations*)

;;; This is an ordered a-list mapping defined key-event modifier names to the
;;; appropriate mask for the modifier.  Modifier names have a short and a long
;;; version.  For each pair of names for the same mask, the names are
;;; contiguous in this list, and the short name appears first.
;;; PRINT-PRETTY-KEY-EVENT and KEY-EVENT-BITS-MODIFIERS rely on this.
;;;
(defvar *modifiers-to-internal-masks*)




(defvar *mouse-translation-info*)

;;; MOUSE-TRANSLATION-INFO -- Internal.
;;;
;;; This returns the requested information, :keysym or :shifted-modifier-name,
;;; for the button cross event-key.  If the information is undefined, this
;;; signals an error.
;;;
#+unused
(defun mouse-translation-info (button event-key info)
  (let ((event-dispatch (svref *mouse-translation-info* button)))
    (unless event-dispatch
      (error "No defined mouse translation information for button ~S." button))
    (let ((data (ecase event-key
		  (:button-press (button-press-info event-dispatch))
		  (:button-release (button-release-info event-dispatch)))))
      (unless data
	(error
	 "No defined mouse translation information for button ~S and event ~S."
	 button event-key))
      (ecase info
	(:keysym (button-keysym data))
	(:shifted-modifier-name (button-shifted-modifier-name data))))))


(eval-when (:compile-toplevel :execute)
  (defmacro button-press-info (event-dispatch) `(car ,event-dispatch))
  (defmacro button-release-info (event-dispatch) `(cdr ,event-dispatch))
  (defmacro button-keysym (info) `(car ,info))
  (defmacro button-shifted-modifier-name (info) `(cdr ,info))
)

;;; MOUSE-TRANSLATION-INFO -- Internal.
;;;
;;; This returns the requested information, :keysym or :shifted-modifier-name,
;;; for the button cross event-key.  If the information is undefined, this
;;; signals an error.
;;;
(defun mouse-translation-info (button event-key info)
  (let ((event-dispatch (svref *mouse-translation-info* button)))
    (unless event-dispatch
      (error "No defined mouse translation information for button ~S." button))
    (let ((data (ecase event-key
		  (:button-press (button-press-info event-dispatch))
		  (:button-release (button-release-info event-dispatch)))))
      (unless data
	(error
	 "No defined mouse translation information for button ~S and event ~S."
	 button event-key))
      (ecase info
	(:keysym (button-keysym data))
	(:shifted-modifier-name (button-shifted-modifier-name data))))))

;;; (setf MOUSE-TRANSLATION-INFO) -- Internal.
;;;
;;; This walks into *mouse-translation-info* the same way MOUSE-TRANSLATION-INFO
;;; does, filling in the data structure on an as-needed basis, and stores
;;; the value for the indicated info.
;;;
(defun (setf mouse-translation-info) (value button event-key info)
  (let ((event-dispatch (svref *mouse-translation-info* button)))
    (unless event-dispatch
      (setf event-dispatch
	    (setf (svref *mouse-translation-info* button) (cons nil nil))))
    (let ((data (ecase event-key
		  (:button-press (button-press-info event-dispatch))
		  (:button-release (button-release-info event-dispatch)))))
      (unless data
	(setf data
	      (ecase event-key
		(:button-press
		 (setf (button-press-info event-dispatch) (cons nil nil)))
		(:button-release
		 (setf (button-release-info event-dispatch) (cons nil nil))))))
      (ecase info
	(:keysym
	 (setf (button-keysym data) value))
	(:shifted-modifier-name
	 (setf (button-shifted-modifier-name data) value))))))



;;; DEFINE-MOUSE-KEYSYM -- Public.
;;;
(defun define-mouse-keysym (button keysym name shifted-bit event-key)
  "This defines keysym named name for the X button cross the X event-key."
  (unless (<= 1 button 5)
    (error "Buttons are number 1-5, not ~D." button))
  (setf (gethash keysym *keysyms-to-names*) (list name))
  (setf (gethash  (get-name-case-right name) *names-to-keysyms*) keysym)
  (setf (mouse-translation-info button event-key :keysym) keysym)
  (setf (mouse-translation-info button event-key :shifted-modifier-name)
	shifted-bit))



;;;; Stuff for parsing #k syntax.



(defstruct (key-event (:print-function %print-key-event)
		      (:constructor %make-key-event (keysym bits)))
  (bits nil :type fixnum)
  (keysym nil))

(defun %print-key-event (object stream ignore)
  (declare (ignore ignore))
  (write-string "#<Key-Event " stream)
  (print-pretty-key object stream)
  (write-char #\> stream))

;;; This maps Common Lisp CHAR-CODE's to character classes for parsing #k
;;; syntax.
;;;
(defvar *key-character-classes* (make-array hemlock-char-code-limit
					    :initial-element :other))

;;; These characters are special:
;;;    #\<  ..........  :ISO-start - Signals start of an ISO character.
;;;    #\>  ..........  :ISO-end - Signals end of an ISO character.
;;;    #\-  ..........  :modifier-terminator - Indicates last *id-namestring*
;;;                                            was a modifier.
;;;    #\"  ..........  :EOF - Means we have come to the end of the character.
;;;    #\{a-z, A-Z} ..  :letter - Means the char is a letter.
;;;    #\space .......  :event-terminator- Indicates the last *id-namestring*
;;;                                        was a character name.
;;;
;;; Every other character has class :other.
;;;
(hi::do-alpha-chars (char :both)
  (setf (svref *key-character-classes* (char-code char)) :letter))
(setf (svref *key-character-classes* (char-code #\<)) :ISO-start)
(setf (svref *key-character-classes* (char-code #\>)) :ISO-end)
(setf (svref *key-character-classes* (char-code #\-)) :modifier-terminator)
(setf (svref *key-character-classes* (char-code #\space)) :event-terminator)
(setf (svref *key-character-classes* (char-code #\")) :EOF)
  
;;; This holds the characters built up while lexing a potential keysym or
;;; modifier identifier.
;;;
(defvar *id-namestring*
  (make-array 30 :adjustable t :fill-pointer 0 :element-type 'base-char))

;;; PARSE-KEY-FUN -- Internal.
;;;
;;; This is the #k dispatch macro character reader.  It is a FSM that parses
;;; key specifications.  It returns either a VECTOR form or a MAKE-KEY-EVENT
;;; form.  Since key-events are unique at runtime, we cannot create them at
;;; readtime, returning the constant object from READ.  Wherever a #k appears,
;;; there's a form that at loadtime or runtime will return the unique key-event
;;; or vector of unique key-events.
;;;
(defun parse-key-fun (stream sub-char count)
  (declare (ignore sub-char count))
  (setf (fill-pointer *id-namestring*) 0)
  (prog ((bits 0)
	 (key-event-list ())
	 char class)
	(unless (char= (read-char stream) #\")
	  (error "Keys must be delimited by ~S." #\"))
	;; Skip any leading spaces in the string.
	(peek-char t stream)
	(multiple-value-setq (char class) (get-key-char stream))
	(ecase class
	  ((:letter :other :escaped) (go ID))
	  (:ISO-start (go ISOCHAR))
	  (:ISO-end (error "Angle brackets must be escaped."))
	  (:modifier-terminator (error "Dash must be escaped."))
	  (:EOF (error "No key to read.")))
	ID
	(vector-push-extend char *id-namestring*)
	(multiple-value-setq (char class) (get-key-char stream))
	(ecase class
	  ((:letter :other :escaped) (go ID))
	  (:event-terminator (go GOT-CHAR))
	  (:modifier-terminator (go GOT-MODIFIER))
	  ((:ISO-start :ISO-end) (error "Angle brackets must be escaped."))
	  (:EOF (go GET-LAST-CHAR)))
	GOT-CHAR
	(push `(make-key-event ,(copy-seq *id-namestring*) ,bits)
	      key-event-list)
	(setf (fill-pointer *id-namestring*) 0)
	(setf bits 0)
	;; Skip any whitespace between characters.
	(peek-char t stream)
	(multiple-value-setq (char class) (get-key-char stream))
	(ecase class
	  ((:letter :other :escaped) (go ID))
	  (:ISO-start (go ISOCHAR))
	  (:ISO-end (error "Angle brackets must be escaped."))
	  (:modifier-terminator (error "Dash must be escaped."))
	  (:EOF (go FINAL)))
	GOT-MODIFIER
	(let ((modifier-name (car (assoc *id-namestring*
					 *modifiers-to-internal-masks*
					 :test #'string-equal))))
	  (unless modifier-name
	    (error "~S is not a defined modifier." *id-namestring*))
	  (setf (fill-pointer *id-namestring*) 0)
	  (setf bits (logior bits (key-event-modifier-mask modifier-name))))
	(multiple-value-setq (char class) (get-key-char stream))
	(ecase class
	  ((:letter :other :escaped) (go ID))
	  (:ISO-start (go ISOCHAR))
	  (:ISO-end (error "Angle brackets must be escaped."))
	  (:modifier-terminator (error "Dash must be escaped."))
	  (:EOF (error "Expected something naming a key-event, got EOF.")))
	ISOCHAR
	(multiple-value-setq (char class) (get-key-char stream))
	(ecase class
	  ((:letter :event-terminator :other :escaped)
	   (vector-push-extend char *id-namestring*)
	   (go ISOCHAR))
	  (:ISO-start (error "Open Angle must be escaped."))
	  (:modifier-terminator (error "Dash must be escaped."))
	  (:EOF (error "Bad syntax in key specification, hit eof."))
	  (:ISO-end (go GOT-CHAR)))
	GET-LAST-CHAR
	(push `(make-key-event ,(copy-seq *id-namestring*) ,bits)
	      key-event-list)
	FINAL
	(return (if (cdr key-event-list)
		    `(vector ,@(nreverse key-event-list))
		    `,(car key-event-list)))))

(set-dispatch-macro-character #\# #\k #'parse-key-fun)

(defconstant key-event-escape-char #\\
  "The escape character that #k uses.")

;;; GET-KEY-CHAR -- Internal.
;;;
;;; This is used by PARSE-KEY-FUN.
;;;
(defun get-key-char (stream)
  (let ((char (read-char stream t nil t)))
    (cond ((char= char key-event-escape-char)
	   (let ((char (read-char stream t nil t)))
	     (values char :escaped)))
	  (t (values char (svref *key-character-classes* (char-code char)))))))



;;;; Code to deal with modifiers.

(defvar *modifier-count* 0
  "The number of modifiers that is currently defined.")

(eval-when (:compile-toplevel :execute :load-toplevel)

(defconstant modifier-count-limit 6
  "The maximum number of modifiers supported.")

); eval-when

;;; This is purely a list for users.
;;;
(defvar *all-modifier-names* ()
  "A list of all the names of defined modifiers.")

;;; Note that short-name is pushed into *modifiers-to-internal-masks* after
;;; long-name.  PRINT-PRETTY-KEY-EVENT and KEY-EVENT-BITS-MODIFIERS rely on
;;; this feature.
;;;
(defun define-key-event-modifier (long-name short-name)
  "This establishes long-name and short-name as modifier names for purposes
   of specifying key-events in #k syntax.  The names are case-insensitive and
   must be strings.  If either name is already defined, this signals an error."
  (when (= *modifier-count* modifier-count-limit)
    (error "Maximum of ~D modifiers allowed." modifier-count-limit))
  (let ((long-name (string-capitalize long-name))
	(short-name (string-capitalize short-name)))
    (flet ((frob (name)
	     (when (assoc name *modifiers-to-internal-masks*
			  :test #'string-equal)
	       (restart-case
		   (error "Modifier name has already been defined -- ~S" name)
		 (blow-it-off ()
		  :report "Go on without defining this modifier."
		  (return-from define-key-event-modifier nil))))))
      (frob long-name)
      (frob short-name))
    (unwind-protect
	(let ((new-bits (ash 1 *modifier-count*)))
	  (push (cons long-name new-bits) *modifiers-to-internal-masks*)
	  (push (cons short-name new-bits) *modifiers-to-internal-masks*)
	  (pushnew long-name *all-modifier-names* :test #'string-equal)
	  ;; Sometimes the long-name is the same as the short-name.
	  (pushnew short-name *all-modifier-names* :test #'string-equal))
      (incf *modifier-count*))))

;;;
;;; RE-INITIALIZE-KEY-EVENTS at the end of this file defines the system
;;; default key-event modifiers.
;;; 

;;; DEFINE-MODIFIER-BIT -- Public.
;;;
(defun define-modifier-bit (bit-mask modifier-name)
  "This establishes a mapping from bit-mask to a define key-event modifier-name."
  (let ((map (assoc modifier-name *modifiers-to-internal-masks*
		    :test #'string-equal)))
    (unless map (error "~S an undefined modifier name." modifier-name))
    (push (cons bit-mask (car map)) *modifier-translations*)))

;;;
;;; RE-INITIALIZE-KEY-EVENTS at the end of this file defines the system
;;; default modifiers, mapping them to some system default key-event
;;; modifiers.
;;; 

(defun make-key-event-bits (&rest modifier-names)
  "This returns bits suitable for MAKE-KEY-EVENT from the supplied modifier
   names.  If any name is undefined, this signals an error."
  (let ((mask 0))
    (dolist (mod modifier-names mask)
      (let ((this-mask (cdr (assoc mod *modifiers-to-internal-masks*
				   :test #'string-equal))))
	(unless this-mask (error "~S is an undefined modifier name." mod))
	(setf mask (logior mask this-mask))))))

;;; KEY-EVENT-BITS-MODIFIERS -- Public.
;;;
(defun key-event-bits-modifiers (bits)
  "This returns a list of key-event modifier names, one for each modifier
   set in bits."
  (let ((res nil))
    (do ((map (cdr *modifiers-to-internal-masks*) (cddr map)))
	((null map) res)
      (when (logtest bits (cdar map))
	(push (caar map) res)))))

;;; KEY-EVENT-MODIFIER-MASK -- Public.
;;;
(defun key-event-modifier-mask (modifier-name)
  "This function returns a mask for modifier-name.  This mask is suitable
   for use with KEY-EVENT-BITS.  If modifier-name is undefined, this signals
   an error."
  (let ((res (cdr (assoc modifier-name *modifiers-to-internal-masks*
			 :test #'string-equal))))
    (unless res (error "Undefined key-event modifier -- ~S." modifier-name))
    res))



;;;; Key event lookup -- GET-KEY-EVENT and MAKE-KEY-EVENT.

(defvar *key-events*)

;;; GET-KEY-EVENT* -- Internal.
;;;
;;; This finds the key-event specified by keysym and bits.  If the key-event
;;; does not already exist, this creates it.  This assumes keysym is defined,
;;; and if it isn't, this will make a key-event anyway that will cause an
;;; error when the system tries to print it.
;;;
(defun get-key-event* (keysym bits)
  (let* ((char (and (fixnump keysym) (code-char keysym))))
    (when (and char (standard-char-p char))
      (let* ((mask (key-event-modifier-mask "Shift")))
        (when (logtest bits mask)
          (setq bits (logandc2 bits mask)
                keysym (char-code (char-upcase char)))))))
  (let* ((data (cons keysym bits)))
    (or (gethash data *key-events*)
	(setf (gethash data *key-events*) (%make-key-event keysym bits)))))

;;;
(defvar *keysym-to-code*)
(defvar *code-to-keysym*)

(defmacro define-keysym-code (keysym code)
  `(progn
     (setf (gethash ,keysym *keysym-to-code*) ,code)
     (setf (gethash ,code *code-to-keysym*) ,keysym)))

(defun keysym-for-code (code)
  (or (gethash code *code-to-keysym*) code))

(defun code-for-keysym (keysym)
  (or (gethash keysym *keysym-to-code*) (and (fixnump keysym) keysym)))

;;;
(defun make-key-event (object &optional (bits 0))
  "This returns a key-event described by object with bits.  Object is one of
   keysym, string, or key-event.  When object is a key-event, this uses
   KEY-EVENT-KEYSYM.  You can form bits with MAKE-KEY-EVENT-BITS or
   KEY-EVENT-MODIFIER-MASK."
  (etypecase object
    (integer
     (let ((keysym (keysym-for-code object)))
       (unless (keysym-names keysym)
	 (error "~S is an undefined code." object))
       (get-key-event* keysym bits)))
    #|(character
     (let* ((name (char-name object))
	    (keysym (name-keysym (or name (string object)))))
       (unless keysym
	 (error "~S is an undefined keysym." object))
       (get-key-event* keysym bits)))|#
    (string
     (let ((keysym (name-keysym object)))
       (unless keysym
	 (error "~S is an undefined keysym." object))
       (get-key-event* keysym bits)))
    (key-event
     (get-key-event* (key-event-keysym object) bits))))

;;; KEY-EVENT-BIT-P -- Public.
;;;
(defun key-event-bit-p (key-event bit-name)
  "This returns whether key-event has the bit set named by bit-name.  This
   signals an error if bit-name is undefined."
  (let ((mask (cdr (assoc bit-name *modifiers-to-internal-masks*
			  :test #'string-equal))))
    (unless mask
      (error "~S is not a defined modifier." bit-name))
    (not (zerop (logand (key-event-bits key-event) mask)))))



;;;; KEY-EVENT-CHAR and CHAR-KEY-EVENT.

;;; This maps key-events to characters.  Users modify this by SETF'ing
;;; KEY-EVENT-CHAR.
;;;
(defvar *key-event-characters*)

(defun key-event-char (key-event)
  "Returns the character associated with key-event. This is SETF'able."
  (check-type key-event key-event)
  (or (gethash key-event *key-event-characters*)
      (code-char (code-for-keysym (key-event-keysym key-event)))))

(defun %set-key-event-char (key-event character)
  (check-type character character)
  (check-type key-event key-event)
  (setf (gethash key-event *key-event-characters*) character))
;;;
(defsetf key-event-char %set-key-event-char)


;;; This maps characters to key-events.  Users modify this by SETF'ing
;;; CHAR-KEY-EVENT.
;;;
(defvar *character-key-events*)

(defun char-key-event (char)
  "Returns the key-event associated with char.  This is SETF'able."
  (check-type char character)
  (svref *character-key-events* (char-code char)))

(defun %set-char-key-event (char key-event)
  (check-type char character)
  (check-type key-event key-event)
  (setf (svref *character-key-events* (char-code char)) key-event))
;;;
(defsetf char-key-event %set-char-key-event)



;;;; DO-ALPHA-KEY-EVENTS.

(defmacro alpha-key-events-loop (var start-keysym end-keysym result body)
  (let ((n (gensym)))
    `(do ((,n ,start-keysym (1+ ,n)))
	 ((> ,n ,end-keysym) ,result)
       (let ((,var (make-key-event ,n 0)))
	 (when (alpha-char-p (key-event-char ,var))
	   ,@body)))))

(defmacro do-alpha-key-events ((var kind &optional result) &rest forms)
  "(DO-ALPHA-KEY-EVENTS (var kind [result]) {form}*)
   This macro evaluates each form with var bound to a key-event representing an
   alphabetic character.  Kind is one of :lower, :upper, or :both, and this
   binds var to each key-event in order as specified in the X11 protocol
   specification.  When :both is specified, this processes lowercase letters
   first."
  (case kind
    (:both
     `(progn (alpha-key-events-loop ,var 97 122 nil ,forms)
	     (alpha-key-events-loop ,var 65 90 ,result ,forms)))
    (:lower
     `(alpha-key-events-loop ,var 97 122 ,result ,forms))
    (:upper
     `(alpha-key-events-loop ,var 65 90 ,result ,forms))
    (t (error "Kind argument not one of :lower, :upper, or :both -- ~S."
	      kind))))



;;;; PRINT-PRETTY-KEY and PRINT-PRETTY-KEY-EVENT.

;;; PRINT-PRETTY-KEY -- Internal
;;;
(defun print-pretty-key (key &optional (stream *standard-output*) long-names-p)
  "This prints key, a key-event or vector of key-events, to stream in a
   user-expected fashion.  Long-names-p indicates whether modifiers should
   print with their long or short name."
  (etypecase key
    (key-event (print-pretty-key-event key stream long-names-p))
    (vector
     (let ((length-1 (1- (length key))))
       (dotimes (i (length key))
	 (let ((key-event (aref key i)))
	   (print-pretty-key-event key-event stream long-names-p)
	   (unless (= i length-1) (write-char #\space stream))))))))

;;; PRINT-PRETTY-KEY-EVENT -- Internal
;;;
;;; Note, this makes use of the ordering in the a-list
;;; *modifiers-to-internal-masks* by CDDR'ing down it by starting on a short
;;; name or a long name.
;;;
(defun print-pretty-key-event (key-event &optional (stream *standard-output*)
					 long-names-p)
  "This prints key-event to stream.  Long-names-p indicates whether modifier
   names should appear using the long name or short name."
  (do ((map (if long-names-p
		(cdr *modifiers-to-internal-masks*)
		*modifiers-to-internal-masks*)
	    (cddr map)))
      ((null map))
    (when (not (zerop (logand (cdar map) (key-event-bits key-event))))
      (write-string (caar map) stream)
      (write-char #\- stream)))
  (let* ((name (keysym-preferred-name (key-event-keysym key-event)))
	 (spacep (position #\space (the simple-string name))))
    (when spacep (write-char #\< stream))
    (write-string name stream)
    (when spacep (write-char #\> stream))))

;;; PRETTY-KEY-STRING - Public.
;;;
(defun pretty-key-string (key &optional long-names-p)
  (with-output-to-string (s)
    (print-pretty-key key s long-names-p)))

;;;; Re-initialization.

;;; RE-INITIALIZE-KEY-EVENTS -- Internal.
;;;
(defun re-initialize-key-events ()
  "This blows away all data associated with keysyms, modifiers, mouse
   translations, and key-event/characters mapping.  Then it re-establishes
   the system defined key-event modifiers and the system defined
   modifier mappings to some of those key-event modifiers.

   When recompiling this file, you should load it and call this function
   before using any part of the key-event interface, especially before
   defining all your keysyms and using #k syntax."
  (setf *keysyms-to-names* (make-hash-table :test #'eql))
  (setf *names-to-keysyms* (make-hash-table :test #'equal))
  (setf *keysym-to-code* (make-hash-table :test #'eql))
  (setf *code-to-keysym* (make-hash-table :test #'eql))
  (setf *modifier-translations* ())
  (setf *modifiers-to-internal-masks* ())
  (setf *mouse-translation-info* (make-array 6 :initial-element nil))
  (setf *modifier-count* 0)
  (setf *all-modifier-names* ())
  (setf *key-events* (make-hash-table :test #'equal))
  (setf *key-event-characters* (make-hash-table))
  (setf *character-key-events*
	(make-array hemlock-char-code-limit :initial-element nil))
  
  (define-key-event-modifier "Hyper" "H")
  (define-key-event-modifier "Super" "S")
  (define-key-event-modifier "Meta" "M")
  (define-key-event-modifier "Control" "C")
  (define-key-event-modifier "Shift" "Shift")
  (define-key-event-modifier "Lock" "Lock")

)

;;; Initialize stuff if not already initialized.
;;;
(unless (boundp '*keysyms-to-names*)
  (re-initialize-key-events))
