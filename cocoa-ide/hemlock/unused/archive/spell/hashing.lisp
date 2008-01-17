(in-package "SPELL")

;;; FIXME: the original code included the below comment; obviously, it
;;; utilized implementation-specific primitives to speed up hashing.  is
;;; this reasonable to do?
;;;
;;; STRING-HASH employs the instruction SXHASH-SIMPLE-SUBSTRING which takes
;;; an end argument, so we do not have to use SXHASH.  SXHASH would mean
;;; doing a SUBSEQ of entry.
(declaim (inline string-hash))
(defun string-hash (string length)
  (if (= length (length string))
      (sxhash string)
      (sxhash (subseq string 0 length))))
