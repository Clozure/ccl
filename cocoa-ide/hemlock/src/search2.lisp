;;; -*- Log: Hemlock.Log; Package: Hemlock-Internals -*-
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
;;;     More searching function for Hemlock.  This file contains the stuff
;;; to implement the various kinds of character searches.
;;;
;;;    Written by Rob MacLachlan
;;;

(in-package :hemlock-internals)

;;;; Character and Not-Character search kinds:

(eval-when (:compile-toplevel :execute)
(defmacro forward-character-search-macro (string start length char test)
  `(position ,char ,string  :start ,start  :end ,length  :test ,test))

(defmacro backward-character-search-macro (string start char test)
  `(position ,char ,string  :end (1+ ,start)  :test ,test  :from-end t))

(defmacro define-character-search-method (name search macro test)
  `(defun ,name (pattern line start)
     (let ((char (search-pattern-pattern pattern)))
       (when (,search line start ,macro char ,test)
	 (values line start 1)))))
); eval-when (:compile-toplevel :execute)

(define-character-search-method find-character-once-forward-method
  search-once-forward-macro forward-character-search-macro #'char=)
(define-character-search-method find-not-character-once-forward-method
  search-once-forward-macro forward-character-search-macro #'char/=)
(define-character-search-method find-character-once-backward-method
  search-once-backward-macro backward-character-search-macro #'char=)
(define-character-search-method find-not-character-once-backward-method
  search-once-backward-macro backward-character-search-macro #'char/=)


(define-search-kind :character (direction pattern old)
  ":character - Pattern is a character to search for."
  (unless old (setq old (internal-make-search-pattern)))
  (setf (search-pattern-kind old) :character
	(search-pattern-direction old) direction
	(search-pattern-pattern old) pattern
	(search-pattern-reclaim-function old) #'identity
	(search-pattern-search-function old)
	(if (eq direction :forward)
	    #'find-character-once-forward-method
	    #'find-character-once-backward-method))
  old)

(define-search-kind :not-character (direction pattern old)
  ":not-character - Find the first character which is not Char= to Pattern."
  (unless old (setq old (internal-make-search-pattern)))
  (setf (search-pattern-kind old) :not-character
	(search-pattern-direction old) direction
	(search-pattern-pattern old) pattern
	(search-pattern-reclaim-function old) #'identity
	(search-pattern-search-function old)
	(if (eq direction :forward)
	    #'find-not-character-once-forward-method
	    #'find-not-character-once-backward-method))
  old)

;;;; Character set searching.
;;;
;;;    These functions implement the :test, :test-not, :any and :not-any
;;; search-kinds.

;;; The Character-Set abstraction is used to hide somewhat the fact that
;;; we are using %Sp-Find-Character-With-Attribute to implement the
;;; character set searches.

(defvar *free-character-sets* ()
  "A list of unused character-set objects for use by the Hemlock searching
  primitives.")

;;; Create-Character-Set  --  Internal
;;;
;;;    Create-Character-Set returns a character-set which will search
;;; for no character.
;;;
(defun create-character-set ()
  (let ((set (or (pop *free-character-sets*)
		 (make-array 256 :element-type '(mod 256)))))
    (declare (type (simple-array (mod 256)) set))
    (dotimes (i search-char-code-limit)
      (setf (aref set i) 0))
    set))

;;; Add-Character-To-Set  --  Internal
;;;
;;;    Modify the character-set Set to succeed for Character.
;;;
(declaim (inline add-character-to-set))
(defun add-character-to-set (character set)
  (setf (aref (the (simple-array (mod 256)) set)
	      (search-char-code character))
	1))

;;; Release-Character-Set  --  Internal
;;;
;;;    Release the storage for the character set Set.
;;;
(defun release-character-set (set)
  (push set *free-character-sets*))

(eval-when (:compile-toplevel :execute)
;;; Forward-Set-Search-Macro  --  Internal
;;;
;;;    Do a search for some character in Set in String starting at Start
;;; and ending at End.
;;;
(defmacro forward-set-search-macro (string start last set)
  `(%sp-find-character-with-attribute ,string ,start ,last ,set 1))

;;; Backward-Set-Search-Macro  --  Internal
;;;
;;;    Like forward-set-search-macro, only :from-end, and start is
;;; implicitly 0.
;;;
(defmacro backward-set-search-macro (string last set)
  `(%sp-reverse-find-character-with-attribute ,string 0 (1+ ,last) ,set 1))
); eval-when (:compile-toplevel :execute)

(defstruct (set-search-pattern
	    (:include search-pattern)
	    (:print-function %print-search-pattern))
  set)

(eval-when (:compile-toplevel :execute)
(defmacro define-set-search-method (name search macro)
  `(defun ,name (pattern line start)
     (let ((set (set-search-pattern-set pattern)))
       (when (,search line start ,macro set)
	 (values line start 1)))))
); eval-when (:compile-toplevel :execute)

(define-set-search-method find-set-once-forward-method
  search-once-forward-macro forward-set-search-macro)

(define-set-search-method find-set-once-backward-method
  search-once-backward-macro backward-set-search-macro)

(defun frob-character-set (pattern direction old kind)
  (unless old (setq old (make-set-search-pattern)))
  (setf (search-pattern-kind old) kind
	(search-pattern-direction old) direction
	(search-pattern-pattern old) pattern
	(search-pattern-search-function old)
	(if (eq direction :forward)
	    #'find-set-once-forward-method
	    #'find-set-once-backward-method)
	(search-pattern-reclaim-function old)
	#'(lambda (x) (release-character-set (set-search-pattern-set x))))
  old)


(define-search-kind :test (direction pattern old)
  ":test - Find the first character which satisfies the test function Pattern.
  Pattern must be a function of its argument only."
  (setq old (frob-character-set pattern direction old :test))
  (let ((set (create-character-set)))
    (dotimes (i search-char-code-limit)
      (when (funcall pattern (code-char i))
	(add-character-to-set (code-char i) set)))
    (setf (set-search-pattern-set old) set))
  old)


(define-search-kind :test-not (direction pattern old)
  ":test-not - Find the first character which does not satisfy the
  test function Pattern.  Pattern must be a function of its argument only."
  (setq old (frob-character-set pattern direction old :test-not))
  (let ((set (create-character-set)))
    (dotimes (i search-char-code-limit)
      (unless (funcall pattern (code-char i))
	(add-character-to-set (code-char i) set)))
    (setf (set-search-pattern-set old) set))
  old)

(define-search-kind :any (direction pattern old)
  ":any - Find the first character which is the string Pattern."
  (declare (string pattern))
  (setq old (frob-character-set pattern direction old :any))
  (let ((set (create-character-set)))
    (dotimes (i (length pattern))
      (add-character-to-set (char pattern i) set))
    (setf (set-search-pattern-set old) set))
  old)

(define-search-kind :not-any (direction pattern old)
  ":not-any - Find the first character which is not in the string Pattern."
  (declare (string pattern))
  (setq old (frob-character-set pattern direction old :not-any))
  (let ((set (create-character-set)))
    (dotimes (i search-char-code-limit)
      (unless (find (code-char i) pattern)
	(add-character-to-set (code-char i) set)))
    (setf (set-search-pattern-set old) set))
  old)
