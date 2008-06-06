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
(require :sequence-utils)

;;; -----------------------------------------------------------------
;;; svn metadata utils
;;; -----------------------------------------------------------------

;;; VALIDATE-SVN-DATA-PATHNAME p
;;; -----------------------------------------------------------------
;;; returns TRUE if P is really an existing directory that appears to
;;; contain valid Subversion metadata; NIL otherwise

(defmethod validate-svn-data-pathname ((p pathname))
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

(defmethod validate-svn-data-pathname ((p string))
  (validate-svn-data-pathname (pathname p)))

;;; -----------------------------------------------------------------
;;; url utils
;;; -----------------------------------------------------------------

;;; URL-P thing
;;; -----------------------------------------------------------------
;;; returns true if THING is a string that appears to contain a URL,
;;; NIL otherwise

(defmethod url-p (thing)
  (declare (ignore thing))
  nil)

(defmethod url-p ((url string))
  (if (find-matching-subsequence "://" url)
      t
      nil))

;;; URL-PROTOCOL url
;;; -----------------------------------------------------------------
;;; returns the protocol pprtion of the URL, or NIL if none
;;; can be identified

(defmethod url-protocol ((url string))
  (let ((index (find-matching-subsequence "://" url)))
    (if index
        (subseq url 0 index)
        nil)))

;;; URL-HOST url
;;; -----------------------------------------------------------------
;;; returns two values:
;;; 1. the hostname of the URL
;;; 2. the username portion of the host segment, if any, or NIL

(defmethod url-host ((url string))
  (let* ((protocol-marker "://")
         (protocol-marker-index (find-matching-subsequence protocol-marker url)))
    (if protocol-marker-index
        (let* ((protocol-end-index (+ protocol-marker-index (length protocol-marker)))
               (host-end-index (find-matching-subsequence "/" url :start protocol-end-index))
               (host-segment (subseq url protocol-end-index host-end-index))
               (username-terminus-index (find-matching-subsequence "@" host-segment))
               (username (if username-terminus-index
                             (subseq host-segment 0 username-terminus-index)
                             nil))
               (host (if username-terminus-index
                         (subseq host-segment (1+ username-terminus-index))
                         host-segment)))
          (values host username))
        nil)))

;;; URL-PATH url
;;; -----------------------------------------------------------------
;;; returns the pathname portion of a URL, or NIL if none can be identified

(defmethod url-path ((url string))
  (let* ((protocol-marker "://")
         (protocol-marker-index (find-matching-subsequence protocol-marker url)))
    (if protocol-marker-index
        (let* ((protocol-end-index (+ protocol-marker-index (length protocol-marker)))
               (host-end-index (find-matching-subsequence "/" url :start protocol-end-index)))
          (if host-end-index
              (subseq url host-end-index)
              nil))
        nil)))

;;; -----------------------------------------------------------------
;;; getting svn info
;;; -----------------------------------------------------------------

(defmethod svn-info ((p string))
  (with-output-to-string (out)
     (run-program "svn" `("info" ,p) :output out)))

(defmethod svn-info ((p pathname))
  (svn-info (namestring p)))

(defmethod split-svn-info-line ((line string))
  (let* ((split-sequence ": ")
         (split-index (find-matching-subsequence split-sequence line :test #'char=))
         (prefix (subseq line 0 split-index))
         (suffix (subseq line (if split-index
                                  (+ split-index (length split-sequence))
                                  (length line)))))
    (list prefix suffix)))

(defmethod parse-svn-info ((info-string string))
  (let ((info-lines (split-lines info-string)))
    (mapcar #'split-svn-info-line info-lines)))

(defmethod get-svn-info ((p string))
  (parse-svn-info (svn-info p)))

(defmethod get-svn-info ((p pathname))
  (get-svn-info (namestring p)))

(defmethod svn-repository ((p string))
  (let* ((info (get-svn-info p))
         (repo-entry (assoc "URL" info :test #'string=)))
    (when repo-entry (second repo-entry))))

(defmethod svn-repository ((p pathname))
  (svn-repository (namestring p)))

(defmethod svn-revision ((p string))
  (let* ((info (get-svn-info p))
         (revision-entry (assoc "Revision" info :test #'string=)))
    (when revision-entry (second revision-entry))))

(defmethod svn-revision ((p pathname))
  (svn-revision (namestring p)))

;;; -----------------------------------------------------------------
;;; authentication utils, for use with source control
;;; -----------------------------------------------------------------
;;; NOTE: currently unused, because we do not update from the GUI
;;;       in the case that authentication is required. code left here
;;;       for future reference

(defparameter *authentication-window-controller* nil)

(defclass authentication-window-controller (ns:ns-window-controller)
    ((authentication-window :foreign-type :id :reader authentication-window)
     (username-field :foreign-type :id :reader authentication-window-username-field)
     (password-field :foreign-type :id :reader authentication-window-password-field))
  (:metaclass ns:+ns-object))

(objc:defmethod #/windowNibName ((self authentication-window-controller))
  #@"Authenticate")

(objc:defmethod (#/authOkay: :void) ((self authentication-window-controller) sender)
  (declare (ignore sender))
  (#/stopModalWithCode: (#/sharedApplication (@class ns-application)) 1)
  (#/orderOut: (authentication-window *authentication-window-controller*) +null-ptr+))

(objc:defmethod (#/authCancel: :void) ((self authentication-window-controller) sender)
  (declare (ignore sender))
  (#/stopModalWithCode: (#/sharedApplication (@class ns-application)) 0)
  (#/orderOut: (authentication-window *authentication-window-controller*) +null-ptr+))

(defun get-auth-window ()
  (unless *authentication-window-controller*
    (setf *authentication-window-controller* 
          (make-instance 'authentication-window-controller))
    (#/initWithWindowNibName: *authentication-window-controller* #@"Authenticate"))
  (unless (#/isWindowLoaded *authentication-window-controller*)
    (#/loadWindow *authentication-window-controller*))
  (let ((window (authentication-window *authentication-window-controller*)))
    (if (or (null window)
            (%null-ptr-p window))
        nil
        window)))

(defun get-svn-auth-data ()
  (let ((auth-window (get-auth-window)))
    (if auth-window
        (let ((window-status (#/runModalForWindow: (#/sharedApplication (@class ns-application))
                                                   auth-window)))
          (if (zerop window-status)
              nil
              (let  ((username (lisp-string-from-nsstring (#/stringValue (authentication-window-username-field 
                                                                          *authentication-window-controller*))))
                     (password (lisp-string-from-nsstring (#/stringValue (authentication-window-password-field 
                                                                          *authentication-window-controller*)))))
                (cons username password))))
        nil)))

;;; -----------------------------------------------------------------
;;; svn updates
;;; -----------------------------------------------------------------

(defun valid-revision-number-for-svn-update? (rev)
  (and (stringp rev)
       (plusp (length rev))))

(defun valid-repository-for-svn-update? (url)
  (url-p url))

(defun valid-directory-for-svn-update? (dir)
  (and dir
       (probe-file dir)
       (directoryp dir)
       (validate-svn-data-pathname (merge-pathnames ".svn/" dir))))

(defun svn-update-ccl (&key directory repository last-revision)
  (cond
    ((not (valid-directory-for-svn-update? directory)) 
     (gui::alert-window :title "Update Failed"
                   :message (format nil 
                                    "Subversion update failed. CCL directory '~A' doesn't exist, or lacks valid Subversion metadata."
                                    directory)))
    ((not (valid-repository-for-svn-update? repository))
     (gui::alert-window :title "Update Failed"
                   :message (format nil "Subversion update failed. The supplied repository URL is invalid: '~A'"
                                    repository)))
    ((not (valid-revision-number-for-svn-update? last-revision))
     (gui::alert-window :title "Update Failed"
                   :message (format nil "Subversion update failed. CCL found an invalid revision number for the current working copy: '~A'"
                                    last-revision)))
    (t (gui::alert-window :title "Update Succeeded"
                     :message "Subversion update succeeded. Soon we will actually run the update when it succeeds."))))

(defun run-svn-update-for-directory (dir)
  (let* ((svn-info (get-svn-info dir))
         (revision-entry (assoc "Revision" svn-info :test #'string=))
         (revision (and revision-entry (second revision-entry)))
         (url-entry (assoc "URL" svn-info :test #'string=))
         (url (and url-entry (second url-entry))))
    (svn-update-ccl :directory dir :repository url :last-revision revision)))
  
(defun run-svn-update ()
  (run-svn-update-for-directory (gui::find-ccl-directory)))

(defun svn-update-available-p ()
  (let ((ccl-dir (gui::find-ccl-directory)))
    (if (valid-directory-for-svn-update? ccl-dir)
        ;; compare revision number of working copy with repo
        (let* ((repo (svn-repository ccl-dir))
               (local-revision (read-from-string (svn-revision ccl-dir)))
               (repo-revision (read-from-string (svn-revision repo))))
          (< local-revision repo-revision))
        nil)))

;;; -----------------------------------------------------------------
;;; app delegate extensions to handle self-update UI
;;; -----------------------------------------------------------------

(defparameter *update-ccl-window-controller* nil)

(defclass update-ccl-window-controller (ns:ns-window-controller)
    ((update-window :foreign-type :id :reader update-window))
  (:metaclass ns:+ns-object))

(objc:defmethod #/windowNibName ((self update-ccl-window-controller))
  #@"updateCCL")

(objc:defmethod (#/updateCCLOkay: :void) ((self update-ccl-window-controller) sender)
  (declare (ignore sender))
  (#/stopModalWithCode: (#/sharedApplication (@class ns-application)) 1)
  (run-svn-update)
  (#/orderOut: (update-window *update-ccl-window-controller*) +null-ptr+))

(objc:defmethod (#/updateCCLCancel: :void) ((self update-ccl-window-controller) sender)
  (declare (ignore sender))
  (#/stopModalWithCode: (#/sharedApplication (@class ns-application)) 0)
  (#/orderOut: (update-window *update-ccl-window-controller*) +null-ptr+))

(objc:defmethod (#/updateCCL: :void) ((self lisp-application-delegate)
                                      sender)
  (declare (ignore sender))
  (if (svn-update-available-p)
      ;; newer version in the repo; display the update window
      (progn
        (when (null *update-ccl-window-controller*)
          (setf *update-ccl-window-controller*
                (make-instance 'update-ccl-window-controller))
          (#/initWithWindowNibName: *update-ccl-window-controller* #@"updateCCL"))
        (unless (#/isWindowLoaded *update-ccl-window-controller*)
          (#/loadWindow *update-ccl-window-controller*))
        (#/runModalForWindow: (#/sharedApplication (@class ns-application)) 
                              (update-window *update-ccl-window-controller*)))
      ;; no newer version available; display an informative alert window
      (gui::alert-window :title "No Update Available"
                         :message "No update is available. Your copy of CCL is up-to-date.")))

