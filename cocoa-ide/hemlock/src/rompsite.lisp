;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+CMU
(ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;; "Site dependent" stuff for the editor while on the IBM RT PC machine.
;;;

(in-package :hi)

;;; SITE-INIT  --  Internal
;;;
;;;    This function is called at init time to set up any site stuff.
;;;
(defun site-init ()
  (defhvar "Beep Border Width"
    "Width in pixels of the border area inverted by beep."
    :value 20)
  (defhvar "Default Window Width"
    "This is used to make a window when prompting the user.  The value is in
     characters."
    :value 80)
  (defhvar "Default Window Height"
    "This is used to make a window when prompting the user.  The value is in
     characters."
    :value 24)
  (defhvar "Default Initial Window Width"
    "This is used when Hemlock first starts up to make its first window.
     The value is in characters."
    :value 80)
  (defhvar "Default Initial Window Height"
    "This is used when Hemlock first starts up to make its first window.
     The value is in characters."
    :value 24)
  (defhvar "Default Initial Window X"
    "This is used when Hemlock first starts up to make its first window.
     The value is in pixels."
    :value nil)
  (defhvar "Default Initial Window Y"
    "This is used when Hemlock first starts up to make its first window.
     The value is in pixels."
    :value nil)
  (defhvar "Bell Style"
    "This controls what beeps do in Hemlock.  Acceptable values are :border-flash
     (which is the default), :feep, :border-flash-and-feep, :flash,
     :flash-and-feep, and NIL (do nothing)."
    :value :border-flash)
  (defhvar "Reverse Video"
    "Paints white on black in window bodies, black on white in modelines."
    :value nil)
  (defhvar "Default Font"
    "The string name of the font to be used for Hemlock -- buffer text,
     modelines, random typeout, etc.  The font is loaded when initializing
     Hemlock."
    :value "*-courier-medium-r-normal--*-120-*")
  (defhvar "Active Region Highlighting Font"
    "The string name of the font to be used for highlighting active regions.
     The font is loaded when initializing Hemlock."
    :value "*-courier-medium-o-normal--*-120-*")
  (defhvar "Open Paren Highlighting Font"
    "The string name of the font to be used for highlighting open parens.
     The font is loaded when initializing Hemlock."
    :value "*-courier-bold-r-normal--*-120-*")
  (defhvar "Thumb Bar Meter"
    "When non-nil (the default), windows will be created to be displayed with
     a ruler in the bottom border of the window."
    :value t)
  nil)


;;;; Some generally useful file-system functions.

;;; MERGE-RELATIVE-PATHNAMES takes a pathname that is either absolute or
;;; relative to default-dir, merging it as appropriate and returning a definite
;;; directory pathname.
;;;
;;; This function isn't really needed anymore now that merge-pathnames does
;;; this, but the semantics are slightly different.  So it's easier to just
;;; keep this around instead of changing all the uses of it.
;;; 
(defun merge-relative-pathnames (pathname default-directory)
  "Merges pathname with default-directory.  If pathname is not absolute, it
   is assumed to be relative to default-directory.  The result is always a
   directory."
  (let ((pathname (merge-pathnames pathname default-directory)))
    (if (ccl:directory-pathname-p pathname)
	pathname
	(pathname (concatenate 'simple-string
			       (namestring pathname)
			       "/")))))


;;;; I/O specials and initialization

;;; File descriptor for the terminal.
;;; 
(defvar *editor-file-descriptor*)

(declaim (special *editor-input* *real-editor-input*))

(declaim (declaration values))

;;; font-map-size should be defined in font.lisp, but SETUP-FONT-FAMILY would
;;; assume it to be special, issuing a nasty warning.
;;;
(defconstant font-map-size 32)


;;;; HEMLOCK-BEEP.

(defvar *beep-function* #'(lambda () (print "BEEP!")))

(defun beep (&optional (stream *terminal-io*))
  (funcall *beep-function* stream))


;;;; Line Wrap Char.

(defvar *line-wrap-char* #\!
  "The character to be displayed to indicate wrapped lines.")


(defvar *editor-describe-stream*
  #+CMU (system:make-indenting-stream *standard-output*)
  #-CMU *standard-output*)

;;; EDITOR-DESCRIBE-FUNCTION has to mess around to get indenting streams to
;;; work.  These apparently work fine for DESCRIBE, for which they were defined,
;;; but not in general.  It seems they don't indent initial text, only that
;;; following a newline, so inside our use of INDENTING-FURTHER, we need some
;;; form before the WRITE-STRING.  To get this to work, I had to remove the ~%
;;; from the FORMAT string, and use FRESH-LINE; simply using FRESH-LINE with
;;; the ~% caused an extra blank line.  Possibly I should not have glommed onto
;;; this hack whose interface comes from three different packages, but it did
;;; the right thing ....
;;;
;;; Also, we have set INDENTING-STREAM-STREAM to make sure the indenting stream
;;; is based on whatever *standard-output* is when we are called.
;;;
(defun editor-describe-function (fun sym)
  "Calls DESCRIBE on fun.  If fun is compiled, and its original name is not sym,
   then this also outputs any 'function documentation for sym to
   *standard-output*."
  (declare (ignorable sym))
  (describe fun)
  (let ((doc (documentation sym 'function)))
    (when doc
      (format *standard-output* "~%Function documentation for ~S:~&~%" sym)
      	  (write-string doc *standard-output*))))

