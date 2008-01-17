;;; -*- Log: hemlock.log; Package: Hemlock -*-
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
;;; This file contains code to peruse the CMU Common Lisp library of hacks.
;;;
;;; Written by Blaine Burks.
;;;

(in-package :hemlock)


(defmode "Lisp-Lib" :major-p t)

;;; The library should be in *lisp-library-directory*

(defvar *lisp-library-directory*  "/afs/cs.cmu.edu/project/clisp/library/")

(defvar *selected-library-buffer* nil)


;;;; Commands.

(defcommand "Lisp Library" (p)
  "Goto buffer in 'Lisp-Lib' mode, creating one if necessary."
  "Goto buffer in 'Lisp-Lib' mode, creating one if necessary."
  (declare (ignore p))
  (when (not (and *selected-library-buffer*
		  (member *selected-library-buffer* *buffer-list*)))
    (when (getstring "Lisp Library" *buffer-names*)
      (editor-error "There is already a buffer named \"Lisp Library\"."))
    (setf *selected-library-buffer*
	  (make-buffer "Lisp Library" :modes '("Lisp-Lib")))
    (message "Groveling library ...")
    (let ((lib-directory (directory *lisp-library-directory*))
	  (lib-entries ()))
      (with-output-to-mark (s (buffer-point *selected-library-buffer*))
	(dolist (lib-spec lib-directory)
	  (let* ((path-parts (pathname-directory lib-spec))
		 (last (elt path-parts (1- (length path-parts))))
		 (raw-pathname (merge-pathnames last lib-spec)))
	    (when (and (directoryp lib-spec)
		       (probe-file (merge-pathnames
				    (make-pathname :type "catalog")
				    raw-pathname)))
	      (push raw-pathname lib-entries)
	      (format s "~d~%" last)))))
      (defhvar "Library Entries"
	"Holds a list of library entries for the 'Lisp Library' buffer"
	:buffer *selected-library-buffer*
	:value (coerce (nreverse lib-entries) 'simple-vector))))
  (setf (buffer-writable *selected-library-buffer*) nil)
  (setf (buffer-modified *selected-library-buffer*) nil)
  (change-to-buffer *selected-library-buffer*)
  (buffer-start (current-point)))

(defcommand "Describe Pointer Library Entry" (p)
  "Finds the file that describes the lisp library entry indicated by the
   pointer."
  "Finds the file that describes the lisp library entry indicated by the
   pointer."
  (declare (ignore p))
  (unless (hemlock-bound-p 'library-entries :buffer (current-buffer))
    (editor-error "Not in a Lisp Library buffer."))
  (describe-library-entry (array-element-from-pointer-pos
			   (value library-entries) "No entry on current line")))

(defcommand "Describe Library Entry" (p)
  "Find the file that describes the lisp library entry on the current line."
  "Find the file that describes the lisp library entry on the current line."
  (declare (ignore p))
  (unless (hemlock-bound-p 'library-entries :buffer (current-buffer))
    (editor-error "Not in a Lisp Library buffer."))
  (describe-library-entry (array-element-from-mark (current-point)
			   (value library-entries) "No entry on current line")))

(defun describe-library-entry (pathname)
  (let ((lisp-buf (current-buffer))
	(buffer (view-file-command
		 nil
		 (merge-pathnames (make-pathname :type "catalog") pathname))))
    (push #'(lambda (buffer)
	      (declare (ignore buffer))
	      (setf lisp-buf nil))
	  (buffer-delete-hook lisp-buf))
    (setf (variable-value 'view-return-function :buffer buffer)
	  #'(lambda () (if lisp-buf
			   (change-to-buffer lisp-buf)
			   (lisp-library-command nil))))))

(defcommand "Load Library Entry" (p)
  "Loads the current library entry into the current slave."
  "Loads the current library entry into the current slave."
  (declare (ignore p))
  (unless (hemlock-bound-p 'library-entries :buffer (current-buffer))
    (editor-error "Not in a Lisp Library buffer."))
  (string-eval (format nil "(load ~S)"
		       (namestring (library-entry-load-file nil)))))

(defcommand "Load Pointer Library Entry" (p)
  "Loads the library entry indicated by the mouse into the current slave."
  "Loads the library entry indicated by the mouse into the current slave."
  (declare (ignore p))
  (unless (hemlock-bound-p 'library-entries :buffer (current-buffer))
    (editor-error "Not in a Lisp Library buffer."))
  (string-eval (format nil "(load ~S)"
		       (namestring (library-entry-load-file t)))))

(defcommand "Editor Load Library Entry" (p)
  "Loads the current library entry into the editor Lisp."
  "Loads the current library entry into the editor Lisp."
  (declare (ignore p))
  (unless (hemlock-bound-p 'library-entries :buffer (current-buffer))
    (editor-error "Not in a Lisp Library buffer."))
  (in-lisp (load (library-entry-load-file nil))))

(defcommand "Editor Load Pointer Library Entry" (p)
  "Loads the library entry indicated by the mouse into the editor Lisp."
  "Loads the library entry indicated by the mouse into the editor Lisp."
  (declare (ignore p))
  (unless (hemlock-bound-p 'library-entries :buffer (current-buffer))
    (editor-error "Not in a Lisp Library buffer."))
  (in-lisp (load (library-entry-load-file t))))

;;; LIBRARY-ENTRY-LOAD-FILE uses the mouse's position or the current point,
;;; depending on pointerp, to return a file that will load that library entry.
;;;
(defun library-entry-load-file (pointerp)
  (let* ((lib-entries (value library-entries))
	 (error-msg "No entry on current-line")
	 (base-name (if pointerp
			(array-element-from-pointer-pos lib-entries error-msg)
			(array-element-from-mark (current-point) lib-entries
						 error-msg)))
	 (parts (pathname-directory base-name))
	 (load-name (concatenate 'simple-string
				 "load-" (elt parts (1- (length parts)))))
	 (load-pathname (merge-pathnames load-name base-name))
	 (file-to-load
	  (or
	   (probe-file (compile-file-pathname load-pathname))
	   (probe-file (merge-pathnames (make-pathname :type "fasl")
					load-pathname))
	   (probe-file (merge-pathnames (make-pathname :type "lisp")
					load-pathname))
	   (probe-file (compile-file-pathname base-name))
	   (probe-file (merge-pathnames (make-pathname :type "fasl")
					base-name))
	   (probe-file (merge-pathnames (make-pathname :type "lisp")
					base-name)))))
    (unless file-to-load (editor-error "You'll have to load it yourself."))
    file-to-load))

(defcommand "Exit Lisp Library" (p)
  "Exit Lisp-Lib Mode, deleting the buffer when possible."
  "Exit Lisp-Lib Mode, deleting the buffer when possible."
  (declare (ignore p))
  (unless (hemlock-bound-p 'library-entries :buffer (current-buffer))
    (editor-error "Not in a Lisp Library buffer."))
  (delete-buffer-if-possible (getstring "Lisp Library" *buffer-names*)))

(defcommand "Lisp Library Help" (p)
  "Show this help."
  "Show this help."
  (declare (ignore p))
  (describe-mode-command nil "Lisp-Lib"))

