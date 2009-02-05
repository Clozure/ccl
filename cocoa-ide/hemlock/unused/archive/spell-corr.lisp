;;; -*- Log: hemlock.log; Package: Spell -*-
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
;;;    Written by Bill Chiles
;;;    Designed by Bill Chiles and Rob Maclachlan
;;;

;;;      This is the file that deals with checking and correcting words
;;; using a dictionary read in from a binary file.  It has been written
;;; from the basic ideas used in Ispell (on DEC-20's) which originated as
;;; Spell on the ITS machines at MIT.  There are flags which have proper
;;; uses defined for them that indicate permissible suffixes to entries.
;;; This allows for about three times as many known words than are actually
;;; stored.  When checking the spelling of a word, first it is looked up;
;;; if this fails, then possible roots are looked up, and if any has the
;;; appropriate suffix flag, then the word is considered to be correctly
;;; spelled.  For an unknown word, the following rules define "close" words
;;; which are possible corrections:
;;;    1] two adjacent letters are transposed to form a correct spelling;
;;;    2] one letter is changed to form a correct spelling;
;;;    3] one letter is added to form a correct spelling; and/or
;;;    4] one letter is removed to form a correct spelling. 
;;; There are two restrictions on the length of a word in regards to its
;;; worthiness of recognition: it must be at least more than two letters
;;; long, and if it has a suffix, then it must be at least four letters
;;; long.  More will be said about this when the flags are discussed.
;;;      This is implemented in as tense a fashion as possible, and it uses
;;; implementation dependent code from Spell-RT.Lisp to accomplish this.
;;; In general the file I/O and structure accesses encompass the system
;;; dependencies.

;;;      This next section will discuss the storage of the dictionary
;;; information.  There are three data structures that "are" the
;;; dictionary: a hash table, descriptors table, and a string table.  The
;;; hash table is a vector of type '(unsigned-byte 16), whose elements
;;; point into the descriptors table.  This is a cyclic hash table to
;;; facilitate dumping it to a file.  The descriptors table (also of type
;;; '(unsigned-byte 16)) dedicates three elements to each entry in the
;;; dictionary.  Each group of three elements has the following organization
;;; imposed on them:
;;;    ----------------------------------------------
;;;    |  15..5  hash code  |      4..0 length      |
;;;    ----------------------------------------------
;;;    |           15..0 character index            |
;;;    ----------------------------------------------
;;;    |  15..14 character index  |  13..0 flags    |
;;;    ----------------------------------------------
;;; "Length" is the number of characters in the entry; "hash code" is some
;;; eleven bits from the hash code to allow for quicker lookup, "flags"
;;; indicate possible suffixes for the basic entry, and "character index"
;;; is the index of the start of the entry in the string table.
;;;      This was originally adopted due to the Perq's word size (can you guess?
;;; 16 bits, that's right).  Note the constraint that is placed on the number
;;; of the entries, 21845, because the hash table could not point to more
;;; descriptor units (16 bits of pointer divided by three).  Since a value of
;;; zero as a hash table element indicates an empty location, the zeroth element
;;; of the descriptors table must be unused (it cannot be pointed to).


;;;      The following is a short discussion with examples of the correct
;;; use of the suffix flags.  Let # and @ be symbols that can stand for any
;;; single letter.  Upper case letters are constants.  "..." stands for any
;;; string of zero or more letters,  but note that no word may exist in the
;;; dictionary which is not at least 2 letters long, so, for example, FLY
;;; may not be produced by placing the "Y" flag on "F".  Also, no flag is
;;; effective unless the word that it creates is at least 4 letters long,
;;; so, for example, WED may not be produced by placing the "D" flag on
;;; "WE".  These flags and examples are from the Ispell documentation with
;;; only slight modifications.  Here are the correct uses of the flags:
;;; 
;;; "V" flag:
;;;         ...E => ...IVE  as in  create => creative
;;;         if # .ne. E, then  ...# => ...#IVE  as in  prevent => preventive
;;; 
;;; "N" flag:
;;;         ...E => ...ION  as in create => creation
;;;         ...Y => ...ICATION  as in  multiply => multiplication
;;;         if # .ne. E or Y, then  ...# => ...#EN  as in  fall => fallen
;;; 
;;; "X" flag:
;;;         ...E => ...IONS  as in  create => creations
;;;         ...Y => ...ICATIONS  as in  multiply => multiplications
;;;         if # .ne. E or Y, ...# => ...#ENS  as in  weak => weakens
;;; 
;;; "H" flag:
;;;         ...Y => ...IETH  as in  twenty => twentieth
;;;         if # .ne. Y, then  ...# => ...#TH  as in  hundred => hundredth
;;; 
;;; "Y" FLAG:
;;;         ... => ...LY  as in  quick => quickly
;;; 
;;; "G" FLAG:
;;;         ...E => ...ING  as in  file => filing
;;;         if # .ne. E, then  ...# => ...#ING  as in  cross => crossing
;;; 
;;; "J" FLAG"
;;;         ...E => ...INGS  as in  file => filings
;;;         if # .ne. E, then  ...# => ...#INGS  as in  cross => crossings
;;; 
;;; "D" FLAG:
;;;         ...E => ...ED  as in  create => created
;;;         if @ .ne. A, E, I, O, or U,
;;;            then  ...@Y => ...@IED  as in  imply => implied
;;;         if # = Y, and @ = A, E, I, O, or U,
;;;            then  ...@# => ...@#ED  as in  convey => conveyed
;;;         if # .ne. E or Y, then  ...# => ...#ED  as in  cross => crossed
;;; 
;;; "T" FLAG:
;;;         ...E => ...EST  as in  late => latest
;;;         if @ .ne. A, E, I, O, or U,
;;;            then  ...@Y => ...@IEST  as in  dirty => dirtiest
;;;         if # = Y, and @ = A, E, I, O, or U,
;;;            then  ...@# => ...@#EST  as in  gray => grayest
;;;         if # .ne. E or Y, then  ...# => ...#EST  as in  small => smallest
;;; 
;;; "R" FLAG:
;;;         ...E => ...ER  as in  skate => skater
;;;         if @ .ne. A, E, I, O, or U,
;;;            then  ...@Y => ...@IER  as in  multiply => multiplier
;;;         if # = Y, and @ = A, E, I, O, or U,
;;;            then ...@# => ...@#ER  as in  convey => conveyer
;;;         if # .ne. E or Y, then  ...# => ...#ER  as in  build => builder
;;; 

;;; "Z FLAG:
;;;         ...E => ...ERS  as in  skate => skaters
;;;         if @ .ne. A, E, I, O, or U,
;;;            then  ...@Y => ...@IERS  as in  multiply => multipliers
;;;         if # = Y, and @ = A, E, I, O, or U,
;;;            then  ...@# => ...@#ERS  as in  slay => slayers
;;;         if # .ne. E or Y, then  ...@# => ...@#ERS  as in  build => builders
;;; 
;;; "S" FLAG:
;;;         if @ .ne. A, E, I, O, or U,
;;;            then  ...@Y => ...@IES  as in  imply => implies
;;;         if # .eq. S, X, Z, or H,
;;;            then  ...# => ...#ES  as in  fix => fixes
;;;         if # .ne. S, X, Z, H, or Y,
;;;            then  ...# => ...#S  as in  bat => bats
;;;         if # = Y, and @ = A, E, I, O, or U,
;;;            then  ...@# => ...@#S  as in  convey => conveys
;;; 
;;; "P" FLAG:
;;;         if # .ne. Y, or @ = A, E, I, O, or U,
;;;            then  ...@# => ...@#NESS  as in  late => lateness and
;;;                                             gray => grayness
;;;         if @ .ne. A, E, I, O, or U,
;;;            then  ...@Y => ...@INESS  as in  cloudy => cloudiness
;;; 
;;; "M" FLAG:
;;;         ... => ...'S  as in DOG => DOG'S

(in-package "SPELL")


;;;; Some Constants

(eval-when (:compile-toplevel :execute :load-toplevel)

(defconstant spell-deleted-entry #xFFFF)

;;; The next number (using 6 bits) is 63, and that's pretty silly because
;;; "supercalafragalistic" is less than 31 characters long.
;;;
(defconstant max-entry-length 31
  "This the maximum number of characters an entry may have.")

;;; These are the flags (described above), and an entry is allowed a
;;; certain suffix if the appropriate bit is on in the third element of
;;; its descriptor unit (described above).
;;;
(defconstant V-mask (ash 1 13))
(defconstant N-mask (ash 1 12))
(defconstant X-mask (ash 1 11))
(defconstant H-mask (ash 1 10))
(defconstant Y-mask (ash 1 9))
(defconstant G-mask (ash 1 8))
(defconstant J-mask (ash 1 7))
(defconstant D-mask (ash 1 6))
(defconstant T-mask (ash 1 5))
(defconstant R-mask (ash 1 4))
(defconstant Z-mask (ash 1 3))
(defconstant S-mask (ash 1 2))
(defconstant P-mask (ash 1 1))
(defconstant M-mask 1)


;;; These are the eleven bits of a computed hash that are stored as part of
;;; an entries descriptor unit.  The shifting constant is how much the
;;; eleven bits need to be shifted to the right, so they take up the upper
;;; eleven bits of one 16-bit element in a descriptor unit.
;;;
(defconstant new-hash-byte (byte 11 13))
(defconstant stored-hash-byte (byte 11 5))


;;; The next two constants are used to extract information from an entry's
;;; descriptor unit.  The first is the two most significant bits of 18
;;; bits that hold an index into the string table where the entry is
;;; located.  If this is confusing, regard the diagram of the descriptor
;;; units above.
;;;
(defconstant whole-index-high-byte (byte 2 16))
(defconstant stored-index-high-byte (byte 2 14))
(defconstant stored-length-byte (byte 5 0))


); eval-when (:compile-toplevel :execute :load-toplevel)


;;;; Some Specials and Accesses

;;; *spell-aeiou* will have bits on that represent the capital letters
;;; A, E, I, O, and U to be used to determine if some word roots are legal
;;; for looking up.
;;;
(defvar *aeiou*
  (make-array 128 :element-type 'bit :initial-element 0))

(setf (aref *aeiou* (char-code #\A)) 1)
(setf (aref *aeiou* (char-code #\E)) 1)
(setf (aref *aeiou* (char-code #\I)) 1)
(setf (aref *aeiou* (char-code #\O)) 1)
(setf (aref *aeiou* (char-code #\U)) 1)


;;; *sxzh* will have bits on that represent the capital letters
;;; S, X, Z, and H to be used to determine if some word roots are legal for
;;; looking up.
;;;
(defvar *sxzh*
  (make-array 128 :element-type 'bit :initial-element 0))

(setf (aref *sxzh* (char-code #\S)) 1)
(setf (aref *sxzh* (char-code #\X)) 1)
(setf (aref *sxzh* (char-code #\Z)) 1)
(setf (aref *sxzh* (char-code #\H)) 1)


;;; SET-MEMBER-P will be used with *aeiou* and *sxzh* to determine if a
;;; character is in the specified set.
;;;
(eval-when (:compile-toplevel :execute)
(defmacro set-member-p (char set)
  `(not (zerop (the fixnum (aref (the simple-bit-vector ,set)
				 (char-code ,char))))))
) ;eval-when


(defvar *dictionary*)
(defvar *dictionary-size*)
(defvar *descriptors*)
(defvar *descriptors-size*)
(defvar *string-table*)
(defvar *string-table-size*)


(eval-when (:compile-toplevel :execute)

;;; DICTIONARY-REF and DESCRIPTOR-REF are references to implementation
;;; dependent structures.  *dictionary* and *descriptors* are "system
;;; area pointers" as a result of the way the binary file is opened for
;;; fast access.
;;;
(defmacro dictionary-ref (idx)
  `(sapref *dictionary* ,idx))

(defmacro descriptor-ref (idx)
  `(sapref *descriptors* ,idx))


;;; DESCRIPTOR-STRING-START access an entry's (indicated by idx)
;;; descriptor unit (described at the beginning of the file) and returns
;;; the start index of the entry in the string table.  The second of three
;;; words in the descriptor holds the 16 least significant bits of 18, and
;;; the top two bits of the third word are the 2 most significant bits.
;;; These 18 bits are the index into the string table.
;;;
(defmacro descriptor-string-start (idx)
  `(dpb (the fixnum (ldb stored-index-high-byte
			 (the fixnum (descriptor-ref (+ 2 ,idx)))))
	whole-index-high-byte
	(the fixnum (descriptor-ref (1+ ,idx)))))

) ;eval-when



;;;; Top level Checking/Correcting

;;; CORRECT-SPELLING can be called from top level to check/correct a words
;;; spelling.  It is not used for any other purpose.
;;; 
(defun correct-spelling (word)
  "Check/correct the spelling of word.  Output is done to *standard-output*."
  (setf word (coerce word 'simple-string))
  (let ((word (string-upcase (the simple-string word)))
	(word-len (length (the simple-string word))))
    (declare (simple-string word) (fixnum word-len))
    (maybe-read-spell-dictionary)
    (when (= word-len 1)
      (error "Single character words are not in the dictionary."))
    (when (> word-len max-entry-length)
      (error "~A is too long for the dictionary." word))
    (multiple-value-bind (idx used-flag-p)
			 (spell-try-word word word-len)
      (if idx
	  (format t "Found it~:[~; because of ~A~]." used-flag-p
		  (spell-root-word idx))
	  (let ((close-words (spell-collect-close-words word)))
	    (if close-words
		(format *standard-output*
			"The possible correct spelling~[~; is~:;s are~]:~
			~:*~[~; ~{~A~}~;~{ ~A~^ and~}~:;~
			~{~#[~; and~] ~A~^,~}~]."
			(length close-words)
			close-words)
		(format *standard-output* "Word not found.")))))))


(defvar *dictionary-read-p* nil)

;;; MAYBE-READ-SPELL-DICTIONARY  --  Public
;;;
(defun maybe-read-spell-dictionary ()
  "Read the spelling dictionary if it has not be read already."
  (unless *dictionary-read-p* (read-dictionary)))


(defun spell-root-word (index)
  "Return the root word corresponding to a dictionary entry at index."
  (let* ((start (descriptor-string-start index))
	 (len (the fixnum (ldb stored-length-byte
			       (the fixnum (descriptor-ref index)))))
	 (result (make-string len)))
    (declare (fixnum start len)
	     (simple-string result))
    (sap-replace result (the system-area-pointer *string-table*)
		 start 0 len)
    result))


(eval-when (:compile-toplevel :execute)
(defmacro check-closeness (word word-len closeness-list)
  `(if (spell-try-word ,word ,word-len)
       (pushnew (subseq ,word 0 ,word-len) ,closeness-list :test #'string=)))
) ;eval-when

(defconstant spell-alphabet
  (list #\A #\B #\C #\D #\E #\F #\G #\H
	#\I #\J #\K #\L #\M #\N #\O #\P
	#\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

;;; SPELL-COLLECT-CLOSE-WORDS Returns a list of all "close" correctly spelled
;;; words.  The definition of "close" is at the beginning of the file, and
;;; there are four sections to this function which collect each of the four
;;; different kinds of close words.
;;; 
(defun spell-collect-close-words (word)
  "Returns a list of all \"close\" correctly spelled words.  This has the
   same contraints as SPELL-TRY-WORD, which you have probably already called
   if you are calling this."
  (declare (simple-string word))
  (let* ((word-len (length word))
	 (word-len--1 (1- word-len))
	 (word-len-+1 (1+ word-len))
	 (result ())
	 (correcting-buffer (make-string max-entry-length)))
    (declare (simple-string correcting-buffer)
	     (fixnum word-len word-len--1 word-len-+1))
    (replace correcting-buffer word :end1 word-len :end2 word-len)

    ;; Misspelled because one letter is different.
    (dotimes (i word-len)
      (do ((save-char (schar correcting-buffer i))
	   (alphabet spell-alphabet (cdr alphabet)))
	  ((null alphabet)
	   (setf (schar correcting-buffer i) save-char))
	(setf (schar correcting-buffer i) (car alphabet))
	(check-closeness correcting-buffer word-len result)))

    ;; Misspelled because two adjacent letters are transposed.
    (dotimes (i word-len--1)
      (rotatef (schar correcting-buffer i) (schar correcting-buffer (1+ i)))
      (check-closeness correcting-buffer word-len result)
      (rotatef (schar correcting-buffer i) (schar correcting-buffer (1+ i))))

    ;; Misspelled because of extraneous letter.
    (replace correcting-buffer word
	     :start2 1 :end1 word-len--1 :end2 word-len)
    (check-closeness correcting-buffer word-len--1 result)
    (dotimes (i word-len--1)
      (setf (schar correcting-buffer i) (schar word i))
      (replace correcting-buffer word
	       :start1 (1+ i) :start2 (+ i 2) :end1 word-len--1 :end2 word-len)
      (check-closeness correcting-buffer word-len--1 result))

    ;; Misspelled because a letter is missing.
    (replace correcting-buffer word
	     :start1 1 :end1 word-len-+1 :end2 word-len)
    (dotimes (i word-len-+1)
      (do ((alphabet spell-alphabet (cdr alphabet)))
	  ((null alphabet)
	   (rotatef (schar correcting-buffer i)
		    (schar correcting-buffer (1+ i))))
	(setf (schar correcting-buffer i) (car alphabet))
	(check-closeness correcting-buffer word-len-+1 result)))
    result))

;;; SPELL-TRY-WORD The literal 4 is not a constant defined somewhere since it
;;; is part of the definition of the function of looking up words.
;;; TRY-WORD-ENDINGS relies on the guarantee that word-len is at least 4.
;;; 
(defun spell-try-word (word word-len)
  "See if the word or an appropriate root is in the spelling dicitionary.
   Word-len must be inclusively in the range 2..max-entry-length."
  (or (lookup-entry word word-len)
      (if (>= (the fixnum word-len) 4)
	  (try-word-endings word word-len))))



;;;; Divining Correct Spelling

(eval-when (:compile-toplevel :execute)

(defmacro setup-root-buffer (word buffer root-len)
  `(replace ,buffer ,word :end1 ,root-len :end2 ,root-len))

(defmacro try-root (word root-len flag-mask)
  (let ((result (gensym)))
    `(let ((,result (lookup-entry ,word ,root-len)))
       (if (and ,result (descriptor-flag ,result ,flag-mask))
	   (return (values ,result ,flag-mask))))))

;;; TRY-MODIFIED-ROOT is used for root words that become truncated
;;; when suffixes are added (e.g., skate => skating).  Char-idx is the last
;;; character in the root that has to typically be changed from a #\I to a
;;; #\Y or #\E.
;;;
(defmacro try-modified-root (word buffer root-len flag-mask char-idx new-char)
  (let ((root-word (gensym)))
    `(let ((,root-word (setup-root-buffer ,word ,buffer ,root-len)))
       (setf (schar ,root-word ,char-idx) ,new-char)
       (try-root ,root-word ,root-len ,flag-mask))))

) ;eval-when


(defvar *rooting-buffer* (make-string max-entry-length))

;;; TRY-WORD-ENDINGS takes a word that is at least of length 4 and
;;; returns multiple values on success (the index where the word's root's
;;; descriptor starts and :used-flag), otherwise nil.  It looks at
;;; characters from the end to the beginning of the word to determine if it
;;; has any known suffixes.  This is a VERY simple finite state machine
;;; where all of the suffixes are narrowed down to one possible one in at
;;; most two state changes.  This is a PROG form for speed, and in some sense,
;;; readability.  The states of the machine are the flag names that denote
;;; suffixes.  The two points of branching to labels are the very beginning
;;; of the PROG and the S state.  This is a fairly straight forward
;;; implementation of the flag rules presented at the beginning of this
;;; file, with char-idx checks, so we do not index the string below zero.

(defun try-word-endings (word word-len)
  (declare (simple-string word)
	   (fixnum word-len))
  (prog* ((char-idx (1- word-len))
	  (char (schar word char-idx))
	  (rooting-buffer *rooting-buffer*)
	  flag-mask)
         (declare (simple-string rooting-buffer)
		  (fixnum char-idx))
         (case char
	   (#\S (go S))        ;This covers over half of the possible endings
	                       ;by branching off the second to last character
	                       ;to other flag states that have plural endings.
	   (#\R (setf flag-mask R-mask)		   ;"er" and "ier"
		(go D-R-Z-FLAG))
	   (#\T (go T-FLAG))			   ;"est" and "iest"
	   (#\D (setf flag-mask D-mask)		   ;"ed" and "ied"
	        (go D-R-Z-FLAG))
	   (#\H (go H-FLAG))			   ;"th" and "ieth"
	   (#\N (setf flag-mask N-mask)		   ;"ion", "ication", and "en"
		(go N-X-FLAG))
	   (#\G (setf flag-mask G-mask)		   ;"ing"
		(go G-J-FLAG))
	   (#\Y (go Y-FLAG))			   ;"ly"
	   (#\E (go V-FLAG)))			   ;"ive"
         (return nil)

    S
         (setf char-idx (1- char-idx))
         (setf char (schar word char-idx))
         (if (char= char #\Y)
	     (if (set-member-p (schar word (1- char-idx)) *aeiou*)
		 (try-root word (1+ char-idx) S-mask)
		 (return nil))
	     (if (not (set-member-p char *sxzh*))
		 (try-root word (1+ char-idx) S-mask)))
         (case char
	   (#\E (go S-FLAG))                    ;"es" and "ies"
	   (#\R (setf flag-mask Z-mask)		;"ers" and "iers"
		(go D-R-Z-FLAG))
	   (#\G (setf flag-mask J-mask)		;"ings"
		(go G-J-FLAG))
	   (#\S (go P-FLAG))			;"ness" and "iness"
	   (#\N (setf flag-mask X-mask)		;"ions", "ications", and "ens"
		(go N-X-FLAG))
	   (#\' (try-root word char-idx M-mask)))
         (return nil)

    S-FLAG
         (setf char-idx (1- char-idx))
         (setf char (schar word char-idx))
	 (if (set-member-p char *sxzh*)
	     (try-root word (1+ char-idx) S-mask))
         (if (and (char= char #\I)
		  (not (set-member-p (schar word (1- char-idx)) *aeiou*)))
	     (try-modified-root word rooting-buffer (1+ char-idx)
				S-mask char-idx #\Y))
         (return nil)

    D-R-Z-FLAG
         (if (char/= (schar word (1- char-idx)) #\E) (return nil))
         (try-root word char-idx flag-mask)
         (if (<= (setf char-idx (- char-idx 2)) 0) (return nil))
         (setf char (schar word char-idx))
         (if (char= char #\Y)
	     (if (set-member-p (schar word (1- char-idx)) *aeiou*) 
		 (try-root word (1+ char-idx) flag-mask)
		 (return nil))
	     (if (char/= (schar word char-idx) #\E)
		 (try-root word (1+ char-idx) flag-mask)))
         (if (and (char= char #\I)
		  (not (set-member-p (schar word (1- char-idx)) *aeiou*)))
	     (try-modified-root word rooting-buffer (1+ char-idx)
				flag-mask char-idx #\Y))
         (return nil)

    P-FLAG
         (if (or (char/= (schar word (1- char-idx)) #\E)
		 (char/= (schar word (- char-idx 2)) #\N))
	     (return nil))
         (if (<= (setf char-idx (- char-idx 3)) 0) (return nil))
         (setf char (schar word char-idx))
         (if (char= char #\Y)
	     (if (set-member-p (schar word (1- char-idx)) *aeiou*) 
		 (try-root word (1+ char-idx) P-mask)
		 (return nil)))
         (try-root word (1+ char-idx) P-mask)
         (if (and (char= char #\I)
		  (not (set-member-p (schar word (1- char-idx)) *aeiou*)))
	     (try-modified-root word rooting-buffer (1+ char-idx)
				P-mask char-idx #\Y))
         (return nil)

    G-J-FLAG
         (if (< char-idx 3) (return nil))
         (setf char-idx (- char-idx 2))
         (setf char (schar word char-idx))
         (if (or (char/= char #\I) (char/= (schar word (1+ char-idx)) #\N))
	     (return nil))
         (if (char/= (schar word (1- char-idx)) #\E)
	     (try-root word char-idx flag-mask))
         (try-modified-root word rooting-buffer (1+ char-idx)
			    flag-mask char-idx #\E)
         (return nil)

    N-X-FLAG
         (setf char-idx (1- char-idx))
         (setf char (schar word char-idx))
         (cond ((char= char #\E)
		(setf char (schar word (1- char-idx)))
		(if (and (char/= char #\Y) (char/= char #\E))
		    (try-root word char-idx flag-mask))
		(return nil))
	       ((char= char #\O)
		(if (char= (schar word (1- char-idx)) #\I)
		    (try-modified-root word rooting-buffer char-idx
				       flag-mask (1- char-idx) #\E)
		    (return nil))
		(if (< char-idx 5) (return nil))
		(if (or (char/= (schar word (- char-idx 2)) #\T)
			(char/= (schar word (- char-idx 3)) #\A)
			(char/= (schar word (- char-idx 4)) #\C)
			(char/= (schar word (- char-idx 5)) #\I))
		    (return nil)
		    (setf char-idx (- char-idx 4)))
		(try-modified-root word rooting-buffer char-idx
				   flag-mask (1- char-idx) #\Y))
	       (t (return nil)))

    T-FLAG
         (if (or (char/= (schar word (1- char-idx)) #\S)
		 (char/= (schar word (- char-idx 2)) #\E))
	     (return nil)
	     (setf char-idx (1- char-idx)))
         (try-root word char-idx T-mask)
         (if (<= (setf char-idx (- char-idx 2)) 0) (return nil))
         (setf char (schar word char-idx))
         (if (char= char #\Y)
	     (if (set-member-p (schar word (1- char-idx)) *aeiou*) 
		 (try-root word (1+ char-idx) T-mask)
		 (return nil))
	     (if (char/= (schar word char-idx) #\E)
		 (try-root word (1+ char-idx) T-mask)))
         (if (and (char= char #\I)
		  (not (set-member-p (schar word (1- char-idx)) *aeiou*)))
	     (try-modified-root word rooting-buffer (1+ char-idx)
				T-mask char-idx #\Y))
         (return nil)

    H-FLAG
         (setf char-idx (1- char-idx))
         (setf char (schar word char-idx))
         (if (char/= char #\T) (return nil))
         (if (char/= (schar word (1- char-idx)) #\Y)
	     (try-root word char-idx H-mask))
         (if (and (char= (schar word (1- char-idx)) #\E)
		  (char= (schar word (- char-idx 2)) #\I))
	     (try-modified-root word rooting-buffer (1- char-idx)
				H-mask (- char-idx 2) #\Y))
         (return nil)

    Y-FLAG
         (setf char-idx (1- char-idx))
         (setf char (schar word char-idx))
         (if (char= char #\L)
	     (try-root word char-idx Y-mask))
         (return nil)

    V-FLAG
         (setf char-idx (- char-idx 2))
         (setf char (schar word char-idx))
         (if (or (char/= char #\I) (char/= (schar word (1+ char-idx)) #\V))
	     (return nil))
         (if (char/= (schar word (1- char-idx)) #\E)
	     (try-root word char-idx V-mask))
         (try-modified-root word rooting-buffer (1+ char-idx)
			    V-mask char-idx #\E)
         (return nil)))



;;; DESCRIPTOR-FLAG returns t or nil based on whether the flag is on.
;;; From the diagram at the beginning of the file, we see that the flags
;;; are stored two words off of the first word in the descriptor unit for
;;; an entry.
;;;
(defun descriptor-flag (descriptor-start flag-mask)
  (not (zerop
	(the fixnum
	     (logand
	      (the fixnum (descriptor-ref (+ 2 (the fixnum descriptor-start))))
	      (the fixnum flag-mask))))))


;;;; Looking up Trials

(eval-when (:compile-toplevel :execute)

;;; SPELL-STRING= determines if string1 and string2 are the same.  Before
;;; it is called it is known that they are both of (- end1 0) length, and
;;; string2 is in system space.  This is used in FOUND-ENTRY-P.
;;;
(defmacro spell-string= (string1 string2 end1 start2)
  (let ((idx1 (gensym))
	(idx2 (gensym)))
    `(do ((,idx1 0 (1+ ,idx1))
	  (,idx2 ,start2 (1+ ,idx2)))
	 ((= ,idx1 ,end1) t)
       (declare (fixnum ,idx1 ,idx2))
       (unless (= (the fixnum (char-code (schar ,string1 ,idx1)))
		  (the fixnum (string-sapref ,string2 ,idx2)))
	 (return nil)))))

;;; FOUND-ENTRY-P determines if entry is what is described at idx.
;;; Hash-and-length is 16 bits that look just like the first word of any
;;; entry's descriptor unit (see diagram at the beginning of the file).  If
;;; the word stored at idx and entry have the same hash bits and length,
;;; then we compare characters to see if they are the same.
;;;
(defmacro found-entry-p (idx entry entry-len hash-and-length)
  `(if (= (the fixnum (descriptor-ref ,idx))
	  (the fixnum ,hash-and-length))
      (spell-string= ,entry *string-table* ,entry-len
		     (descriptor-string-start ,idx))))

(defmacro hash2-increment (hash)
  `(- (the fixnum *dictionary-size*)
      2
      (the fixnum (rem ,hash (- (the fixnum *dictionary-size*) 2)))))

(defmacro hash2-loop ((location-var contents-var)
		       loc hash zero-contents-form
		       &optional body-form (for-insertion-p nil))
  (let ((incr (gensym)))
    `(let* ((,incr (hash2-increment ,hash))
	    (,location-var ,loc)
	    (,contents-var 0))
	(declare (fixnum ,location-var ,contents-var ,incr))
       (loop (setf ,location-var
		   (rem (+ ,location-var ,incr) (the fixnum *dictionary-size*)))
	     (setf ,contents-var (dictionary-ref ,location-var))
	     (if (zerop ,contents-var) (return ,zero-contents-form))
	     ,@(if for-insertion-p
		   `((if (= ,contents-var spell-deleted-entry)
			 (return ,zero-contents-form))))
	     (if (= ,location-var ,loc) (return nil))
	     ,@(if body-form `(,body-form))))))

) ;eval-when


;;; LOOKUP-ENTRY returns the index of the first element of entry's
;;; descriptor unit on success, otherwise nil.  
;;;
(defun lookup-entry (entry &optional len)
  (declare (simple-string entry))
  (let* ((entry-len (or len (length entry)))
	 (hash (string-hash entry entry-len))
	 (hash-and-len (dpb (the fixnum (ldb new-hash-byte hash))
			    stored-hash-byte
			    (the fixnum entry-len)))
	 (loc (rem hash (the fixnum *dictionary-size*)))
	 (loc-contents (dictionary-ref loc)))
    (declare (fixnum entry-len hash hash-and-len loc))
    (cond ((zerop loc-contents) nil)
	  ((found-entry-p loc-contents entry entry-len hash-and-len)
	   loc-contents)
	  (t
	   (hash2-loop (loop-loc loc-contents) loc hash
	     nil
	     (if (found-entry-p loc-contents entry entry-len hash-and-len)
		 (return loc-contents)))))))

;;;; Binary File Reading

(defparameter default-binary-dictionary
  "library:spell-dictionary.bin")

;;; This is the first thing in a spell binary dictionary file to serve as a
;;; quick check of its proposed contents.  This particular number is
;;; "BILLS" on a calculator held upside-down.
;;;
(defconstant magic-file-id 57718)

;;; These constants are derived from the order things are written to the
;;; binary dictionary in Spell-Build.Lisp.
;;;
(defconstant magic-file-id-loc 0)
(defconstant dictionary-size-loc 1)
(defconstant descriptors-size-loc 2)
(defconstant string-table-size-low-byte-loc 3)
(defconstant string-table-size-high-byte-loc 4)
(defconstant file-header-bytes 10)

;;; Initially, there are no free descriptor elements and string table bytes,
;;; but when these structures are grown, they are grown by more than that
;;; which is necessary.
;;;
(defvar *free-descriptor-elements* 0)
(defvar *free-string-table-bytes* 0)

;;; READ-DICTIONARY opens the dictionary and sets up the global structures
;;; manifesting the spelling dictionary.  When computing the start addresses
;;; of these structures, we multiply by two since their sizes are in 16bit
;;; lengths while the RT is 8bit-byte addressable.
;;;
(defun read-dictionary (&optional (f default-binary-dictionary))
  (when *dictionary-read-p*
    (setf *dictionary-read-p* nil)
    (deallocate-bytes (system-address *dictionary*)
		      (* 2 (the fixnum *dictionary-size*)))
    (deallocate-bytes (system-address *descriptors*)
		      (* 2 (the fixnum
				(+ (the fixnum *descriptors-size*)
				   (the fixnum *free-descriptor-elements*)))))
    (deallocate-bytes (system-address *string-table*)
		      (+ (the fixnum *string-table-size*)
			 (the fixnum *free-string-table-bytes*))))
  (setf *free-descriptor-elements* 0)
  (setf *free-string-table-bytes* 0)
  (let* ((fd (open-dictionary f))
	 (header-info (read-dictionary-structure fd file-header-bytes)))
    (unless (= (sapref header-info magic-file-id-loc) magic-file-id)
      (deallocate-bytes (system-address header-info) file-header-bytes)
      (error "File is not a dictionary: ~S." f))
    (setf *dictionary-size* (sapref header-info dictionary-size-loc))
    (setf *descriptors-size* (sapref header-info descriptors-size-loc))
    (setf *string-table-size* (sapref header-info string-table-size-low-byte-loc))
    (setf (ldb (byte 12 16) (the fixnum *string-table-size*))
	  (the fixnum (sapref header-info string-table-size-high-byte-loc)))
    (deallocate-bytes (system-address header-info) file-header-bytes)
    (setf *dictionary*
	  (read-dictionary-structure fd (* 2 (the fixnum *dictionary-size*))))
    (setf *descriptors*
	  (read-dictionary-structure fd (* 2 (the fixnum *descriptors-size*))))
    (setf *string-table* (read-dictionary-structure fd *string-table-size*))
    (setf *dictionary-read-p* t)
    (close-dictionary fd)))
