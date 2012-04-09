;; -*- Mode: Lisp; tab-width: 2; indent-tabs-mode: nil -*-

;; Local host Web server for browsing code coverage test run results

(require ':hunchentoot)

(in-package :code-cover-test-server)

;; Port and host name for socket binding 
(defvar *server-port* 9090.)
(defvar *server-host* "localhost")

;; Locating code coverage output files

(defun server-files-path (filename)
  (check-type filename (or pathname string))
  (output-path filename))

(defvar *server-message-log* #P"message.log")
(defvar *server-access-log* #P"access.log")

;; Server handles these URIs

(defvar *results-uri* "/code-cover-test/")
(defvar *results-partial-uri* "/code-cover-test")
(defvar *results-full-uri* "/code-cover-test/index.html")

;; Debugging 
(defvar _acceptor)
(defvar _request)

;; Handlers for URIs to get code coverage test run results

(defvar *report-file-dispatcher*
  (hunchentoot:create-static-file-dispatcher-and-handler
   *results-full-uri* (index-file-path)))

(defvar *report-folder-dispatcher*
  (hunchentoot:create-folder-dispatcher-and-handler
   *results-uri* (make-pathname :defaults (index-file-path) :name nil :type nil)))

(defun match-uri-p (request) 
  (member (print (hunchentoot:script-name request))
          (list *results-uri* *results-partial-uri*)
          :test #'equal))

(hunchentoot:define-easy-handler (report-index :uri 'match-uri-p) ()
  (hunchentoot:redirect *results-full-uri*))

;; Initialize server - create acceptor

(defun init-server ()
  (let ((message-log-pathname (server-files-path *server-message-log*))
        (access-log-pathname (server-files-path *server-access-log*)))
    ;; Returns
    (setq _acceptor
          (make-instance 'hunchentoot:easy-acceptor :port *server-port* :address *server-host*
                         :message-log-destination message-log-pathname
                         :access-log-destination access-log-pathname))))

;; Install dispatcher/handlers and start server 

(defun start-server (&key (initialize-p t))
  (and initialize-p (init-server))
  (setq hunchentoot:*dispatch-table*
        (append hunchentoot:*dispatch-table*
                (list *report-file-dispatcher* *report-folder-dispatcher*)))
  (hunchentoot:start _acceptor))

;; Remove dispatcher/handlers and stop server

(defun stop-server ()
  (setf hunchentoot:*dispatch-table*
        (remove-if #'(lambda (handler)
                       (member handler (list *report-file-dispatcher* *report-folder-dispatcher*)))
                   hunchentoot:*dispatch-table*))
  (hunchentoot:stop _acceptor))
