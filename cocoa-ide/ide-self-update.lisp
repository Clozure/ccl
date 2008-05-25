;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          svn-self.lisp
;;;; Version:       0.1
;;;; Project:       Cocoa IDE
;;;; Purpose:       Cocoa UI for updating the Cocoa IDE from the source repo
;;;;
;;;; ***********************************************************************

(in-package :ccl)

;;; IDE: decide how and whether to handle cvs self-updates.
;;; see the cvs/svn code in update-ccl in compile-ccl.lisp

;;; use GUI::FIND-CCL-DIRECTORY to find the effective CCL directory
;;; (it gracefully handles the case where we are running from an
;;; app bundle, as well as the case where we are not)

;;; How to self-update the IDE from the svn or cvs repo
;;; 1. Find the ccl directory that corresponds to the running image
;;; 2. determine whether this is an svn or cvs checkout
;;; 3. SVN:
;;;   a. find the .svn directory
;;;   b. run svn info to get the svn URL
;;;   c. determine from the URL whether we need to authenticate
;;;   d. get auth tokens if we need them
;;;   d. record the svn revision before we start (so we can roll back
;;;      if things go horribly wrong)
;;;   e. run svn status to check for potential merge conflicts before starting 
;;;      the update
;;;   f. construct the svn command:
;;;       i. cd to the proper CCL directory
;;;      ii. run svn update
;;;   g. run the svn command with external-process-status.
;;;   h.  check the status of the external command for success or failure:
;;;      i. if okay, queue a full-rebuild, and a rebuild of the IDE
;;;         (need to make some infrastructure for queuing these activities
;;;         and running them on next launch, or immediately in an external process)
;;;     ii. if not okay, warn the user, and offer to roll back to the 
;;;         previously-recorded version (need to make some infrastructure for
;;;         running a rollback)
;;; TODO: make a cvs version if needed

;;; VALIDATE-SVN-DATA-PATHNAME p
;;; ---------------------
;;; returns TRUE if P is really an existing directory that appears to
;;; contain valid Subversion metadata; FALSE otherwise

(defun validate-svn-data-pathname (p)
  (and (probe-file p)
       (directoryp p)
       (string= ".svn" (first (last (pathname-directory p))))
       ;; if we reached this point, it's an existing directory
       ;; named ".svn". now, does it have Subversion metadata files
       ;; in it?
       (let ((subversion-metafiles '("dir-prop-base" "entries" "format"
                                     "prop-base/" "props/" "text-base/")))
         (every (lambda (f) (probe-file (merge-pathnames f p))) 
                subversion-metafiles))))

;;; given a valid-looking .svn directory, we should be able to use
;;; the svn executable to get the repository URL. we call:
;;;  svn info
;;; and get a big block of info text. one line of the output
;;; is of the form:
;;;  URL: yatta-yatta
;;; where yatta-yatta is the repository URL of the checked out directory
;;; Another piece of information we want shows Up here, too: the
;;; current revision, on a line of the form:
;;; Revision: foobar

(defun split-svn-info-line (line)
  (let* ((split-sequence ": ")
         (split-index (find-matching-subsequence split-sequence line :test #'char=))
         (prefix (subseq line 0 split-index))
         (suffix (subseq line (if split-index
                                  (+ split-index (length split-sequence))
                                  (length line)))))
    (list prefix suffix)))

(defun parse-svn-info (info-string)
  (let ((info-lines (split-lines info-string)))
    (mapcar #'split-svn-info-line info-lines)))

(defun get-svn-info (p)
  (parse-svn-info
   (with-output-to-string (out)
     (run-program "svn" `("info" ,(namestring p)) :output out))))

;;; we infer from the information in the URL field of the svn info
;;; whether we need to authenticate. The assumed criteria in this
;;; implementation are that we don't need to authenticate if the
;;; URL is an http:: URL; if it's an svn+ssh URL, then we do need
;;; to authenticate

(defclass authentication-window-controller (ns:ns-window-controller)
    ((authentication-window :foreign-type :id :reader authentication-window)
     (username-field :foreign-type :id :reader authentication-window-username-field)
     (password-field :foreign-type :id :reader authentication-window-password-field))
  (:metaclass ns:+ns-object))

(objc:defmethod #/windowNibName ((self authentication-window-controller))
  #@"Authenticate")

(defparameter *authentication-window-controller* nil)

(defun pose-authentication-window ()
  (unless *authentication-window-controller*
    (setf *authentication-window-controller* 
          (make-instance 'authentication-window-controller))
    (#/initWithWindowNibName: *authentication-window-controller* #@"Authenticate"))
  (unless (#/isWindowLoaded *authentication-window-controller*)
    (#/loadWindow *authentication-window-controller*))
  (let ((window (authentication-window *authentication-window-controller*)))
    ;;(#/runModalForWindow: ccl::*nsapp* window)
    window))

