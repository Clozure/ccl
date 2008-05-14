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

;;; How to self-update the IDE from the svn repo
;;; 1. Find the ccl directory that corresponds to the running image
;;; 2. find the .svn directory
;;; 3. read the .svn/entries file to determine whether we need to
;;;    authenticate
;;; 4. make sure we have auth tokens if we need them
;;; 5. record the svn version before we start (so we can roll back
;;;    if things go horribly wrong)
;;; 6. run svn status to check for potential merge conflicts before starting 
;;;    the update
;;; 7. construct the svn command:
;;;    a. cd to the proper CCL directory
;;;    b. run svn update
;;; 8. check the outcome of the update:
;;;    a. if okay, run queue a full-rebuild, and a rebuild of the IDE
;;;       (need to make some infrastructure for queuing these activities
;;;        and running them on next launch)
;;;    b. if not okay, warn the user, and offer to roll back to the 
;;;       previously-recorded version (need to make some infrastructure for
;;;       running a rollback)

;;; LISP-COMMAND-PATHNAME
;;; ---------------------
;;; returns the pathname of the running Lisp kernel binary
;;; (pathname-directory (lisp-command-pathname)) should give the CCL
;;; directory for that lisp

(defun lisp-command-pathname () 
  (pathname (car ccl::*command-line-argument-list*)))

(defun effective-ccl-directory ()
    (make-pathname :directory (pathname-directory (lisp-command-pathname))))

;;; LISP-SUBVERSION-DATA-PATHNAME
;;; ---------------------
;;; returns the pathname of the subversion metadata for the running
;;; Lisp. This function infers where the Subversion data should be; it
;;; does not check to see whether it's really there

;;; TODO: there is a good chance this path will be of a form like this:
;;; #P"/usr/local/ccl/trunk/darwinx8664/ccl/Clozure CL.app/Contents/MacOS/dx86cl64"
;;; need a way to be sure we find the proper CCL directory for the app bundle

(defun lisp-subversion-data-pathname ()
  (merge-pathnames ".svn/"
                   (make-pathname :directory (pathname-directory (ccl::lisp-command-pathname)))))

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
;;; Another piece of information we want opos up here, too: the
;;; current revision, on a line of the form:
;;; Revision: foobar

(defun parse-svn-info (info-string)
  info-string)

(defun get-svn-info (p)
  (parse-svn-info
   (with-output-to-string (out)
     (run-program "svn" `("info" ,(namestring p)) :output out))))