;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2011 Clozure Associates
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

;;  Use the IDE to debug a remote ccl.
;;  **** THIS IS NOT COMPLETE AND NOT HOOKED UP TO ANYTHING YET *****
;;

(in-package "GUI")

#+debug ;; For testing, start a ccl running swank, then call this in the ide.
(defun cl-user::rlisp-test (port &optional host)
  (declare (special conn thread))
  (when (boundp 'conn) (close conn))
  (setq conn (ccl::connect-to-swank (or host "localhost") port))
  (setq thread (ccl::make-rrepl-thread conn "IDE Listener"))
  (let* ((old ccl::*inhibit-greeting*)
         (listener (unwind-protect
                       (progn
                         (setq ccl::*inhibit-greeting* t)
                         (new-listener))
                     (setq ccl::*inhibit-greeting* old))))
    (connect-listener-to-remote listener thread)))


(defclass remote-cocoa-listener-process (cocoa-listener-process)
  ((remote-thread :initarg :remote-thread :reader process-remote-thread)))

;; in the future, there should be something like a "New Remote Listener" command
;; which should pass relevant info through to new-cocoa-listener-process.
;; But this will do for testing: take an existing normal listener and convert it.
(defmethod connect-listener-to-remote (object rthread)
  (let ((view (hemlock-view object)))
    (connect-listener-to-remote (or view (require-type object 'hi:hemlock-view)) rthread)))

(defmethod connect-listener-to-remote ((view hi:hemlock-view) (rthread ccl::remote-lisp-thread))
  (let* ((doc (hi::buffer-document (hi:hemlock-view-buffer view)))
         (process (or (hemlock-document-process doc)
                      (error "Not a listener: ~s" view)))
         (name (process-name process))
         (window (cocoa-listener-process-window process)))
    (when (eq process *current-process*)
      (error "Cannot connect current listener"))
    (setf (hemlock-document-process doc) nil) ;; so killing the process doesn't close the window
    (process-kill process)
    (let ((pos (search " [Remote " name :from-end t)))
      (when pos
        (setq name (subseq name 0 pos))))
    (setf (hemlock-document-process doc)
          (new-cocoa-listener-process (format nil "~a [Remote ~a(~a)]"
                                              name (ccl::rlisp-host-description rthread) (ccl::rlisp-thread-id rthread))
                                      window
                                      :class 'remote-cocoa-listener-process
                                      :initargs  `(:remote-thread ,rthread)
                                      :initial-function
                                      (lambda ()
                                        (setf (hemlock-document-process doc) *current-process*)
                                        (ccl::remote-listener-function rthread))))))

(defmethod ccl::output-stream-for-remote-lisp ((app cocoa-application))
  (hemlock-ext:top-listener-output-stream))

(defmethod ccl::input-stream-for-remote-lisp ((app cocoa-application))
  (hemlock-ext:top-listener-input-stream))

(defmethod ccl::toplevel-form-text ((stream cocoa-listener-input-stream))
  (with-slots (read-lock queue-lock queue queue-semaphore text-semaphore) stream
    (with-lock-grabbed (read-lock)
      (assert (with-slots (cur-sstream) stream (null cur-sstream)))
      (wait-on-semaphore queue-semaphore nil "Toplevel Read")
      (let ((val (with-lock-grabbed (queue-lock) (pop queue))))
        (cond ((stringp val) ;; listener input
               (assert (with-slots (text-semaphore) stream
                         (timed-wait-on-semaphore text-semaphore 0))
                       ()
                       "text/queue mismatch!")
               (values val nil t))
              (t
               ;; TODO: this is bogus, the package may not exist on this side, so must be a string,
               ;; but we can't bind *package* to a string.  So this assumes the caller will know
               ;; not to progv the env.
               (destructuring-bind (string package-name pathname offset) val ;; queued form
                 (declare (ignore offset))
                 (let ((env (cons '(*loading-file-source-file*)
                                  (list pathname))))
                   (when package-name
                     (push '*package* (car env))
                     (push package-name (cdr env)))
                   (values string env)))))))))
