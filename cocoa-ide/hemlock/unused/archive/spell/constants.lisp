(in-package "SPELL")

(defconstant +spell-deleted-entry+ #xFFFF)

;;; The next number (using 6 bits) is 63, and that's pretty silly because
;;; "supercalafragalistic" is less than 31 characters long.
;;;
(defconstant +max-entry-length+ 31
  "This the maximum number of characters an entry may have.")



;;; These are the eleven bits of a computed hash that are stored as part of
;;; an entries descriptor unit.  The shifting constant is how much the
;;; eleven bits need to be shifted to the right, so they take up the upper
;;; eleven bits of one 16-bit element in a descriptor unit.
;;;
(defconstant +new-hash-byte+ (byte 11 13))
(defconstant +stored-hash-byte+ (byte 11 5))


;;; The next two constants are used to extract information from an entry's
;;; descriptor unit.  The first is the two most significant bits of 18
;;; bits that hold an index into the string table where the entry is
;;; located.  If this is confusing, regard the diagram of the descriptor
;;; units above.
;;;
;;; This is used to break up an 18 bit string table index into two parts
;;; for storage in a word descriptor unit.  See the documentation at the
;;; top of Spell-Correct.Lisp.
;;;
(defconstant +whole-index-low-byte+ (byte 16 0))
(defconstant +whole-index-high-byte+ (byte 2 16))

(defconstant +stored-index-high-byte+ (byte 2 14))
(defconstant +stored-length-byte+ (byte 5 0))

(defconstant +spell-alphabet+
  (list #\A #\B #\C #\D #\E #\F #\G #\H
	#\I #\J #\K #\L #\M #\N #\O #\P
	#\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

;;; This is the first thing in a spell binary dictionary file to serve as a
;;; quick check of its proposed contents.  This particular number is
;;; "BILLS" on a calculator held upside-down.
;;;
(defconstant +magic-file-id+ 57718)

;;; These constants are derived from the order things are written to the
;;; binary dictionary in Spell-Build.Lisp.
;;;
(defconstant +magic-file-id-loc+ 0)
(defconstant +dictionary-size-loc+ 1)
(defconstant +descriptors-size-loc+ 2)
(defconstant +string-table-size-low-byte-loc+ 3)
(defconstant +string-table-size-high-byte-loc+ 4)
(defconstant +file-header-bytes+ 10)

;;; bump this up a bit, but do not lower it.  TRY-WORD-ENDINGS depends on
;;; this value being at least 4.
(defconstant +minimum-try-word-endings-length+ 4)