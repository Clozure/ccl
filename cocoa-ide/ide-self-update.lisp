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
;;; IDE automated self-rebuild
;;; -----------------------------------------------------------------
;;; normally we run the self-rebuild after an update from the
;;; source repo. the steps are:
;;; 1. rename Clozure CL.app to Clozure CL-last.app
;;;    (check for older versions and rename with a numbering scheme)
;;; 2. run an external process that starts ccl and evaluates (rebuild-ccl :full t)
;;; 3. run an external process that starts ccl and evaluates (require :cocoa-application)
;;; 4. quit the current IDE (with a farewell message to the effect that the IDE has been rebuilt)
;;; 5. relaunch the IDE (?) 
;;; (for a simple way to quit and relaunch, see http://www.cocoabuilder.com/archive/message/cocoa/2008/3/3/200352)

(defun ide-self-rebuild ()
  (let* ((ccl-dir (gui::find-ccl-directory))
         (bundle (probe-file (merge-pathnames "Clozure CL.app" ccl-dir))))
    (if bundle
        ;; found the bundle; proceed with rebuilding...
        (let* ((result-status nil)
               (lisp (merge-pathnames (standard-kernel-name) ccl-dir)))
          (gui::with-modal-progress-dialog "Rebuilding" "Rebuilding Clozure CL (please wait)..."
                                           (run-program lisp `("-e" "(rebuild-ccl :full t)") 
                                                        ::status-hook (lambda (ep) 
                                                                        (multiple-value-bind (status status-code) 
                                                                            (external-process-status ep)
                                                                          (when (eql status :exited)
                                                                            (setf result-status status-code))))))
          (if (zerop result-status)
              ;; rebuild succeeded; continue...
              (let* ((old-bundle (merge-pathnames "Clozure CL-last.app" ccl-dir)))
                ;; if there is already an old bundle, delete it
                (when (probe-file old-bundle)
                  (recursive-delete-directory old-bundle))
                ;; rename the current bundle to the old-bundle
                (rename-file bundle old-bundle)
                ;; rebuild the IDE
                (setf result-status nil)
                (gui::with-modal-progress-dialog "Rebuilding" "Rebuilding the IDE (please wait)..."
                                                 (run-program lisp `("-e" "(require :cocoa-application)") 
                                                              ::status-hook (lambda (ep) 
                                                                              (multiple-value-bind (status status-code) 
                                                                                  (external-process-status ep)
                                                                                (when (eql status :exited)
                                                                                  (setf result-status status-code))))))
                (if (zerop result-status)
                    ;; inform the user that the IDE is rebuilt and we will quit
                    (progn
                      (gui::alert-window :title "Rebuilding IDE Succeeded"
                                 :message (format nil 
                                                  "Clozure CL is rebuilt; you can start the new IDE after this copy quits."))
                      (quit))
                    ;; warn the user that the IDE rebuild failed and we will quit
                    (progn
                      (gui::alert-window :title "Rebuilding IDE Failed"
                                 :message (format nil 
                                                  "Rebuilding the IDE failed with error code ~A. The previous IDE has been moved to ~A."
                                                  result-status old-bundle))
                      (quit))))
              ;; warn the user that rebuilding failed and exit
              (gui::alert-window :title "Rebuilding CCL Failed"
                                 :message (format nil 
                                                  "Clozure CL exited with error status = ~A"
                                                  result-status))))
        ;; else: the bundle doesn't seem to be there
        (gui::alert-window :title "Rebuilding CCL Failed"
                        :message (format nil 
                                         "Can't find the application '~A'."
                                         bundle)))))

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
;;; running svn commands
;;; -----------------------------------------------------------------

(defmethod svn-info ((p string))
  (let* ((result-status nil)
         (info (with-output-to-string (out)
                 (run-program "svn" `("info" ,p) 
                              :output out
                              :status-hook (lambda (ep) 
                                             (multiple-value-bind (status status-code) 
                                                 (external-process-status ep)
                                               (when (eql status :exited)
                                                 (setf result-status status-code))))))))
    (values info result-status)))

(defmethod svn-info ((p pathname))
  (svn-info (namestring p)))

(defmethod svn-update ((p string))
  (let ((result-status nil))
    (run-program "svn" `("update" ,p) 
               :status-hook (lambda (ep) 
                              (multiple-value-bind (status status-code) 
                                  (external-process-status ep)
                                (when (eql status :exited)
                                  (setf result-status status-code)))))
    result-status))

(defmethod svn-update ((p pathname))
  (svn-update (namestring p)))

;;; -----------------------------------------------------------------
;;; parsing info
;;; -----------------------------------------------------------------

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

(defun svn-revision ()
  (svn-info-component "Revision:"))

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
                                         "Subversion update failed. CCL directory '~A' is not a valid working copy."
                                         directory)))
    ((not (valid-repository-for-svn-update? repository))
     (gui::alert-window :title "Update Failed"
                        :message (format nil "Subversion update failed. The supplied repository URL is invalid: '~A'"
                                         repository)))
    ((not (valid-revision-number-for-svn-update? last-revision))
     (gui::alert-window :title "Update Failed"
                        :message (format nil "Subversion update failed. CCL found an invalid revision number ('~A') for '~A'"
                                         last-revision directory)))
    (t (let ((status (svn-update directory)))
         (if (zerop status)
             (progn
               ;; notify the user that the update succeeded and we'll now rebuild
               (gui::alert-window :title "Update Succeeded"
                        :message (format nil "Subversion updated CCL source directory '~A'. CCL needs to be rebuilt."
                                         directory))
               (ide-self-rebuild))
             (gui::alert-window :title "Update Failed"
                        :message (format nil "Subversion update of CCL directory '~A' failed with error code ~A."
                                         directory status)))))))

(defun run-svn-update-for-directory (dir)
  (let* ((revision (svn-info-component "Revision:"))
         (url (svn-url)))
    (svn-update-ccl :directory dir :repository url :last-revision revision)))
  
(defun run-svn-update ()
  (run-svn-update-for-directory (gui::find-ccl-directory)))

(defun svn-update-available-p ()
  (let ((ccl-dir (gui::find-ccl-directory)))
    (if (valid-directory-for-svn-update? ccl-dir)
        ;; compare revision number of working copy with repo
        (let* ((local-revision (read-from-string (svn-revision)))
               (repo (svn-repository))
               (repo-info (parse-svn-info (svn-info repo)))
               (repo-revision-entry (assoc "Revision" repo-info :test #'string=))
               (repo-revision (or (and repo-revision-entry
                                       (read-from-string (second repo-revision-entry)))
                                  0)))
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
  (#/orderOut: (update-window *update-ccl-window-controller*) +null-ptr+)
  (gui::with-modal-progress-dialog "Updating..."
    "Getting changes from the CCL Repository..."
   (run-svn-update))
  (ide-self-rebuild))

(objc:defmethod (#/updateCCLCancel: :void) ((self update-ccl-window-controller) sender)
  (declare (ignore sender))
  (#/stopModalWithCode: (#/sharedApplication (@class ns-application)) 0)
  (#/orderOut: (update-window *update-ccl-window-controller*) +null-ptr+))

(objc:defmethod (#/updateCCL: :void) ((self gui::lisp-application-delegate)
                                      sender)
  (declare (ignore sender))
  (if (gui::with-modal-progress-dialog "Checking for Updates..."
        "Checking for new CCL changes..."
       (svn-update-available-p))
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

