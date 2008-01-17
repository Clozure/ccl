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

;;;; SITE-INIT.

;;; *key-event-history* is defined in input.lisp, but it needs to be set in
;;; SITE-INIT, since MAKE-RING doesn't exist at load time for this file.
;;;
(declaim (special *key-event-history*))

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
    :value nil
    #+clx
    :hooks #+clx '(reverse-video-hook-fun))
  (defhvar "Enter Window Hook"
    "When the mouse enters an editor window, this hook is invoked.  These
     functions take the Hemlock Window as an argument."
    :value nil)
  (defhvar "Exit Window Hook"
    "When the mouse exits an editor window, this hook is invoked.  These
     functions take the Hemlock Window as an argument."
    :value nil)
  (defhvar "Set Window Autoraise"
    "When non-nil, setting the current window will automatically raise that
     window via a function on \"Set Window Hook\".  If the value is :echo-only
     (the default), then only the echo area window will be raised
     automatically upon becoming current."
    :value :echo-only)
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

  (setf *key-event-history* (make-ring 60))
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
    (if (directoryp pathname)
	pathname
	(pathname (concatenate 'simple-string
			       (namestring pathname)
			       "/")))))

(defun directoryp (pathname)
  "Returns whether pathname names a directory, that is whether it has no
   name and no type components."
  (not (or (pathname-name pathname) (pathname-type pathname))))



;;;; I/O specials and initialization

;;; File descriptor for the terminal.
;;; 
(defvar *editor-file-descriptor*)

(declaim (special *editor-input* *real-editor-input*))

(declaim (declaration values))
(declaim (special *default-font-family*))

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


;;;; Current terminal character translation.

(defvar termcap-file "/etc/termcap")



;;;; Event scheduling.

;;; The time queue provides a ROUGH mechanism for scheduling events to
;;; occur after a given amount of time has passed, optionally repeating
;;; using the given time as an interval for rescheduling.  When the input
;;; loop goes around, it will check the current time and process all events
;;; that should have happened before or at this time.  The function gets
;;; called on the number of seconds that have elapsed since it was last
;;; called.
;;;
;;; NEXT-SCHEDULED-EVENT-WAIT and INVOKE-SCHEDULED-EVENTS are used in the
;;; editor stream in methods.
;;;
;;; SCHEDULE-EVENT and REMOVE-SCHEDULED-EVENT are exported interfaces.

(defstruct (tq-event (:print-function print-tq-event)
		     (:constructor make-tq-event
				   (time last-time interval function)))
  time		; When the event should happen.
  last-time	; When the event was scheduled.
  interval	; When non-nil, how often the event should happen.
  function)	; What to do.

(defun print-tq-event (obj stream n)
  (declare (ignore n))
  (format stream "#<Tq-Event ~S>" (tq-event-function obj)))

(defvar *time-queue* nil
  "This is the time priority queue used in Hemlock input streams for event
   scheduling.")

;;; QUEUE-TIME-EVENT inserts event into the time priority queue *time-queue*.
;;; Event is inserted before the first element that it is less than (which
;;; means that it gets inserted after elements that are the same).
;;; *time-queue* is returned.
;;; 
(defun queue-time-event (event)
  (let ((time (tq-event-time event)))
    (if *time-queue*
	(if (< time (tq-event-time (car *time-queue*)))
	    (push event *time-queue*)
	    (do ((prev *time-queue* rest)
		 (rest (cdr *time-queue*) (cdr rest)))
		((or (null rest)
		     (< time (tq-event-time (car rest))))
		 (push event (cdr prev))
		 *time-queue*)))
	(push event *time-queue*))))

;;; NEXT-SCHEDULED-EVENT-WAIT returns nil or the number of seconds to wait for
;;; the next event to happen.
;;; 
(defun next-scheduled-event-wait ()
  (if *time-queue*
      (let ((wait (round (- (tq-event-time (car *time-queue*))
			    (get-internal-real-time))
			 internal-time-units-per-second)))
	(if (plusp wait) wait 0))))

;;; INVOKE-SCHEDULED-EVENTS invokes all the functions in *time-queue* whose
;;; time has come.  If we run out of events, or there are none, then we get
;;; out.  If we popped an event whose time hasn't come, we push it back on the
;;; queue.  Each function is called on how many seconds, roughly, went by since
;;; the last time it was called (or scheduled).  If it has an interval, we
;;; re-queue it.  While invoking the function, bind *time-queue* to nothing in
;;; case the event function tries to read off *editor-input*.
;;;
(defun invoke-scheduled-events ()
  (let ((time (get-internal-real-time)))
    (loop
      (unless *time-queue* (return))
      (let* ((event (car *time-queue*))
	     (event-time (tq-event-time event)))
	(cond ((>= time event-time)
	       (let ((*time-queue* nil))
		 (funcall (tq-event-function event)
			  (round (- time (tq-event-last-time event))
				 internal-time-units-per-second)))
	       (hemlock-ext:without-interrupts
		(let ((interval (tq-event-interval event)))
		  (when interval
		    (setf (tq-event-time event) (+ time interval))
		    (setf (tq-event-last-time event) time)
		    (pop *time-queue*)
		    (queue-time-event event)))))
	      (t (return)))))))

(defun schedule-event (time function &optional (repeat t))
  "This causes function to be called after time seconds have passed,
   optionally repeating every time seconds.  This is a rough mechanism
   since commands can take an arbitrary amount of time to run; the function
   will be called at the first possible moment after time has elapsed.
   Function takes the time that has elapsed since the last time it was
   called (or since it was scheduled for the first invocation)."
  (let ((now (get-internal-real-time))
	(itime (* internal-time-units-per-second time)))
    (queue-time-event (make-tq-event (+ itime now) now (if repeat itime)
				     function))))

(defun remove-scheduled-event (function)
  "Removes function queued with SCHEDULE-EVENT."
  (setf *time-queue* (delete function *time-queue* :key #'tq-event-function)))



;;;; Editor sleeping.

(defun editor-sleep (time)
  "Sleep for approximately Time seconds."
  (unless (or (zerop time) (listen-editor-input *editor-input*))
    ;(internal-redisplay)
    (sleep-for-time time)
    nil))

(defun sleep-for-time (time)
  (timed-wait-for-key-event *editor-input* time))


;;;; Function description and defined-from.

;;; FUN-DEFINED-FROM-PATHNAME takes a symbol or function object.  It
;;; returns a pathname for the file the function was defined in.  If it was
;;; not defined in some file, then nil is returned.
;;; 
(defun fun-defined-from-pathname (function)
  "Takes a symbol or function and returns the pathname for the file the
   function was defined in.  If it was not defined in some file, nil is
   returned."
  #+CMU
  (flet ((frob (code)
	   (let ((info (kernel:%code-debug-info code)))
	     (when info
	       (let ((sources (c::debug-info-source info)))
		 (when sources
		   (let ((source (car sources)))
		     (when (eq (c::debug-source-from source) :file)
		       (c::debug-source-name source)))))))))
    (typecase function
      (symbol (fun-defined-from-pathname (fdefinition function)))
      (kernel:byte-closure
       (fun-defined-from-pathname (kernel:byte-closure-function function)))
      (kernel:byte-function
       (frob (c::byte-function-component function)))
      (function
       (frob (kernel:function-code-header (kernel:%function-self function))))
      (t nil)))
    #+openmcl
    (flet ((true-namestring (path) (namestring (truename path))))
      (typecase function
        (function (fun-defined-from-pathname (ccl::function-name function)))
        (symbol (let* ((info (ccl::%source-files function)))
                  (if (atom info)
                    (true-namestring info)
                    (let* ((finfo (assq 'function info)))
                      (when finfo
                        (true-namestring
                         (if (atom finfo)
                           finfo
                           (car finfo)))))))))))


(defvar *editor-describe-stream*
  (#+CMU system:make-indenting-stream #-CMU progn *standard-output*))

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

