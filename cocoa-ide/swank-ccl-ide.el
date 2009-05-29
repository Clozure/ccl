;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          swank-ccl-ide.el
;;;; Project:       CCL IDE
;;;; Purpose:       swank extensions for use with the CCL IDE
;;;;
;;;; ***********************************************************************

;;; ABOUT
;;; ------------------------------------------------------------------------

;;; this file implements an extension to SLIME that talks to CCL's
;;; Cocoa app 
;;; it provides utilities that:
;;; 1. tell CCL to find and load the swank-loader currently in use by
;;;    the running Emacs
;;; 2.  start a swank server on a specified port.
;;; 3. tell SLIME to connect to swank on the specified port

(defun swank-loader-path () (concat slime-path "swank-loader.lisp"))

(defvar *ccl-swank-listener-host* "127.0.0.1")
(defvar *ccl-swank-listener-port* 4884)
(defvar *ccl-swank-listener-proc* nil)


;;; TODO: make this filter function start up a connection to
;;;       the CCL swank server if it reads a success message,
;;;       or display an informative error if it reads a
;;;       failure message
(defun slime-ccl-swank-filter (process string)
  (message (concat "CCL swank listener: " string)))

(defvar $emacs-ccl-swank-request-marker "[emacs-ccl-swank-request]")

(defun request-ccl-load-swank (&optional host listener-port connection-port )
  (let* ((host (or host *ccl-swank-listener-host*))
         (listener-port (or listener-port *ccl-swank-listener-port*))
         (connection-port (or connection-port slime-port))
         (ping (concat $emacs-ccl-swank-request-marker (format "%d" connection-port) ":" (swank-loader-path) "\n"))
         (ccl-proc (open-network-stream "SLIME CCL Swank" nil host listener-port)))
    (setq *ccl-swank-listener-proc* ccl-proc)
    (set-process-filter ccl-proc 'slime-ccl-swank-filter)
    ;; send ping
    (process-send-string ccl-proc ping)
    ccl-proc))

