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
;;;

(in-package :ccl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Standard swank startup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (export '(load-swank start-swank-server start-swank-loader stop-swank-loader))

(defun swankvar (name &optional (package :swank))
  (symbol-value (find-symbol name package)))

(defun (setf swankvar) (value name &optional (package :swank))
  (let ((sym (find-symbol name package)))
    (if (null sym)
      (warn "Couldn't find ~a::~a" package name)
      (set sym value))))

(defun load-swank (load-path)
  (when (find-package :swank-loader) (delete-package :swank-loader)) ;; so can tell if loaded
  (load (merge-pathnames load-path "swank-loader.lisp"))
  (unless (and (find-package :swank-loader)
               (find-symbol "INIT" :swank-loader))
    (error "~s is not a swank loader path" load-path))
  (funcall (find-symbol "INIT" :swank-loader))
  (unless (and (find-package :swank)
               (find-symbol "CREATE-SERVER" :swank))
    (error "Incompatible swank version loaded from ~s" load-path)))

(defun start-swank-server (&key
                           (port (swankvar "DEFAULT-SERVER-PORT"))
                           (debug (swankvar "*LOG-EVENTS*"))
                           (dedicated-output-port (and (swankvar "*USE-DEDICATED-OUTPUT-STREAM*")
                                                       (swankvar "*DEDICATED-OUTPUT-STREAM-PORT*")))
                           (globally-redirect-io (swankvar "*GLOBALLY-REDIRECT-IO*"))
                           (global-debugger (swankvar "*GLOBAL-DEBUGGER*"))
                           (indentation-updates (swankvar "*CONFIGURE-EMACS-INDENTATION*"))
                           (dont-close (swankvar "*DONT-CLOSE*"))
                           (coding-system "iso-latin-1-unix")
                           (style :spawn))
  "Assuming SWANK is already loaded, create a swank server on the specified port"
  (when debug
    (setf (swankvar "*LOG-EVENTS*" :swank-rpc) t)
    (setf (swankvar "*SWANK-DEBUG-P*") t)
    (setf (swankvar "*DEBUG-ON-SWANK-PROTOCOL-ERROR*") t))
  (when (setf (swankvar "*USE-DEDICATED-OUTPUT-STREAM*") (not (null dedicated-output-port)))
    (setf (swankvar "*DEDICATED-OUTPUT-STREAM-PORT*") dedicated-output-port))
  (setf (swankvar "*GLOBALLY-REDIRECT-IO*") globally-redirect-io)
  (setf (swankvar "*GLOBAL-DEBUGGER*") global-debugger)
  (setf (swankvar "*CONFIGURE-EMACS-INDENTATION*") indentation-updates)
  (funcall (find-symbol "CREATE-SERVER" :swank)
           :style style
           :port port
           :dont-close dont-close
           :coding-system coding-system))


(defun swank-port-active? (port)
  (and (find-package :swank) (getf (swankvar "*LISTENER-SOCKETS*") port)))


;; Special ccl slime extension to allow the client to specify the swank path

(defvar *swank-loader-process* nil)
(defparameter $emacs-ccl-swank-request-marker "[emacs-ccl-swank-request]")
(defparameter *default-swank-loader-port* 4884)

(defun stop-swank-loader ()
  (when *swank-loader-process*
    (process-kill (shiftf *swank-loader-process* nil))))

(defun start-swank-loader (&optional (port *default-swank-loader-port*))
  (ignore-errors (stop-swank-loader))
  (let ((semaphore (make-semaphore))
        (errorp nil))
    (setq *swank-loader-process*
          ;; Wait for either a swank client to connect or the special ccl slime kludge
          (process-run-function "Swank Loader"
                                (lambda (sem)
                                  (setq *swank-loader-process* *current-process*)
                                  (unwind-protect
                                      (with-open-socket (socket :connect :passive :local-port port
                                                                :reuse-address t)
                                        (signal-semaphore (shiftf sem nil))
                                        (loop
                                          (let* ((stream (accept-connection socket))
                                                 (line (read-line stream nil)))
                                            (multiple-value-bind (path port)
                                                                 (parse-emacs-ccl-swank-request line)
                                              (let ((message (handler-case
                                                                 (if (swank-port-active? port)
                                                                   (format nil "Swank is already active on port ~s" port)
                                                                   (progn
                                                                     (load-swank path)
                                                                     (start-swank-server :port port)
                                                                     nil))
                                                               (error (c) (princ-to-string c)))))
                                                (prin1 `(:active (and (swank-port-active? port) t)
                                                                 :loader ,path
                                                                 :message ,message
                                                                 :port ,port)
                                                       stream)
                                                (finish-output stream))))))
                                    (when sem ;; in case exit before finished startup
                                      (setq errorp t)
                                      (signal-semaphore sem))))
                                semaphore))
    (wait-on-semaphore semaphore)
    (when errorp
      (ignore-errors (process-kill (shiftf *swank-loader-process* nil))))
    *swank-loader-process*))

(defun parse-emacs-ccl-swank-request (line)
  (let ((start (length $emacs-ccl-swank-request-marker)))
    (when (and (< start (length line))
               (string= $emacs-ccl-swank-request-marker line :end2 start))
      (let* ((split-pos (position #\: line :start start))
             (port (parse-integer line :junk-allowed nil :start start :end split-pos))
             (path-pos (position-if-not #'whitespacep line
                                        :start (if split-pos (1+ split-pos) start)))
             (path (subseq line path-pos
                           (1+ (position-if-not #'whitespacep line :from-end t)))))
        (values path port)))))




