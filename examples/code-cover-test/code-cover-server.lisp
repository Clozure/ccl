;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

(require ':hunchentoot)

(in-package :code-cover-test)

(defvar *server-port* 9090.)
(defvar *server-host* "localhost")

(defun server-files-path (&optional filename)
  (merge-pathnames (or filename (make-pathname)) *output-directory-path*))

(defvar *server-message-log* #P"message.log")
(defvar *server-access-log* #P"access.log")

(defvar *results-uri* "/code-cover-test/")
(defvar *results-partial-uri* "/code-cover-test")
(defvar *results-full-uri* "/code-cover-test/index.html")

(defvar _acceptor)                      ;one acceptor listening for this server
(defvar _request)                       ;debugging

(let* ((report-folder-handler
        (hunchentoot:create-folder-dispatcher-and-handler
         *results-uri* (make-pathname :defaults (index-file-path) :name nil :type nil)))
       (report-folder-verbose-handler
        (lambda (request)
          (let (script result)
            (setq script (hunchentoot::script-name request))
            (hunchentoot::log-message :debug "Folder handler called for ~a" script)
            (setq result (funcall report-folder-handler request))
            (hunchentoot::log-message :debug "Folder handler result for ~a - ~s" script result)
            ;; Returns
            result)))
       (report-special-handler
        (lambda (request)
          (let ((script (hunchentoot::script-name request)))
            (hunchentoot::log-message :debug "Special handler called for ~A" script)
            (when (member script (list *results-partial-uri* *results-uri*) :test #'equal)
              (hunchentoot::redirect *results-full-uri*)))))
       (handlers (list report-special-handler report-folder-verbose-handler)))
  (defun report-dispatch-handler (request)
    (let ((hunchentoot:*dispatch-table* handlers))
      (hunchentoot::list-request-dispatcher request))))

(defun init-code-coverage-test-server ()
  (unless hunchentoot:*message-log-pathname*
    (setf hunchentoot:*message-log-pathname* (server-files-path *server-message-log*)))
  (unless hunchentoot:*access-log-pathname*
    (setf hunchentoot:*access-log-pathname* (server-files-path *server-access-log*)))
  ;; Returns
  (setq _acceptor (make-instance 'hunchentoot:acceptor :port *server-port* :address *server-host*)))

(defun start-code-coverage-test-server (&key (initialize-p t))
  (and initialize-p (init-code-coverage-test-server))
  (pushnew 'report-dispatch-handler hunchentoot:*dispatch-table*)
  (hunchentoot:start _acceptor))

(defun stop-code-coverage-test-server ()
  (setf hunchentoot:*dispatch-table*
        (remove 'report-dispatch-handler hunchentoot:*dispatch-table*))
  (hunchentoot:stop _acceptor))
