;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jun 30 21:43:23 2003
;;;; Contains: Notes concerning various parts of the ANSI spec.

(in-package :cl-test)

(defnote :allow-nil-arrays
  "Allow specialized arrays of type (array nil).")

(defnote :allow-nonzero-nil-vectors
  "Allow specialized vectors of type (vector nil) of nonzero size.")

(defnote :nil-vectors-are-strings
  "Assume that (VECTOR NIL) objects are strings.")

(defnote :standardized-package-nicknames
  "The standardized package nicknames specified in section 11 of ANSI CL are exclusive (disputed).")

(defnote :type-of/strict-builtins
  "Interpret requirement 1.a on the TYPE-OF page to apply to all built-in types that
contain the object, not just to some builtin type that contains the object.")

(defnote :assume-no-gray-streams
  "Disable the test if gray streams are present.")

(defnote :assume-no-simple-streams
  "Disable the test if simple streams are present.")

(defnote :open-if-exists-new-version-no-error
  "Assume that OPEN, when called with :if-exists :new-version, does not fail.")

#+sbcl (rt::disable-note :open-if-exists-new-version-no-error)

(defnote :make-condition-with-compound-name
  "The spec says MAKE-CONDITION should work on any subtype of CONDITION, but this causes all sorts of problems.  They probably meant only non-compound names.")

(defnote :ansi-spec-problem
  "A catch-all for tests that illustrate problems in the ANSI spec.")

(defnote :negative-zero-is-similar-to-positive-zero
  "The definition of similarity implies that -0.0 and 0.0 are similar (for each float type.)
If negative zeros are distinct this is probably not good, since it makes (defconstant x 0.0) be nonportable.")

(defnote :result-type-element-type-by-subtype
  "Assume that (for sequence functions MAP, etc.) the element type of a vector result type
   is defined to be the type X such that result-type is a subtype of (vector X).")

;;; Haible disagrees with :result-type-element-type-by-subtype
#+clisp (rt::disable-note :result-type-element-type-by-subtype)
#+(or openmcl gcl ecl) (rt::disable-note :nil-vectors-are-strings)
#+gcl (rt::disable-note :allow-nil-arrays)
