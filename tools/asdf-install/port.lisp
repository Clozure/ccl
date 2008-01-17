(in-package :asdf-install)

(defvar *temporary-files*)

(eval-when (:load-toplevel :compile-toplevel :execute)
  #+:lispworks
  (require "comm")
  #+:allegro
  (require :osi)
  #+:allegro
  (require :socket)
  #+:digitool
  (require :opentransport))

(defun get-env-var (name)
  #+:sbcl (sb-ext:posix-getenv name)
  #+:cmu (cdr (assoc (intern (substitute #\_ #\- name)
                            :keyword)
                    ext:*environment-list*))
  #+:allegro (sys:getenv name)
  #+:lispworks (lw:environment-variable name)
  #+:clisp (ext:getenv name)
  #+(or :mcl :openmcl) (ccl::getenv name))

#-:digitool
(defun system-namestring (pathname)
  (namestring (truename pathname)))

#+:digitool
(defvar *start-up-volume*
  (second (pathname-directory (truename "ccl:"))))

#+:digitool
(defun system-namestring (pathname)
  ;; this tries to adjust the root directory to eliminate the spurious
  ;; volume name for the boot file system; it also avoids use of
  ;; TRUENAME as some applications are for not yet existent files
  (let ((truename (probe-file pathname)))
    (unless truename
      (setf truename
              (translate-logical-pathname
               (merge-pathnames pathname *default-pathname-defaults*))))
    (let ((directory (pathname-directory truename)))
      (flet ((string-or-nil (value) (when (stringp value) value))
             (absolute-p (directory) (eq (first directory) :absolute))
             (root-volume-p (directory)
               (equal *start-up-volume* (second directory))))
        (format nil "~:[~;/~]~{~a/~}~@[~a~]~@[.~a~]"
                (absolute-p directory)
                (if (root-volume-p directory) (cddr directory) (cdr directory))
                (string-or-nil (pathname-name truename))
                (string-or-nil (pathname-type truename)))))))

#+:digitool
(progn
  (defun |read-linefeed-eol-comment|
         (stream char &optional (eol '(#\return #\linefeed)))
    (loop (setf char (read-char stream nil nil))
          (unless char (return))
          (when (find char eol) (return)))
    (values))
  
  (set-syntax-from-char #\linefeed #\space)
  (set-macro-character #\; #'|read-linefeed-eol-comment| nil *readtable*))

;; for non-SBCL we just steal this from SB-EXECUTABLE
#-(or :sbcl :digitool)
(defvar *stream-buffer-size* 8192)
#-(or :sbcl :digitool)
(defun copy-stream (from to)
  "Copy into TO from FROM until end of the input stream, in blocks of
*stream-buffer-size*.  The streams should have the same element type."
  (unless (subtypep (stream-element-type to) (stream-element-type from))
    (error "Incompatible streams ~A and ~A." from to))
  (let ((buf (make-array *stream-buffer-size*
			 :element-type (stream-element-type from))))
    (loop
     (let ((pos #-(or :clisp :cmu) (read-sequence buf from)
                #+:clisp (ext:read-byte-sequence buf from :no-hang nil)
                #+:cmu (sys:read-n-bytes from buf 0 *stream-buffer-size* nil)))
       (when (zerop pos) (return))
       (write-sequence buf to :end pos)))))

#+:digitool
(defun copy-stream (from to)
  "Perform copy and map EOL mode."
  (multiple-value-bind (reader reader-arg) (ccl::stream-reader from)
    (multiple-value-bind (writer writer-arg) (ccl::stream-writer to)
      (let ((datum nil))
        (loop (unless (setf datum (funcall reader reader-arg))
                (return))
              (funcall writer writer-arg datum))))))

#+:sbcl
(declaim (inline copy-stream))
#+:sbcl
(defun copy-stream (from to)
  (sb-executable:copy-stream from to))

(defun make-stream-from-url (url)
  #+:sbcl
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                          :type :stream
                          :protocol :tcp)))
    (sb-bsd-sockets:socket-connect
     s (car (sb-bsd-sockets:host-ent-addresses
             (sb-bsd-sockets:get-host-by-name (url-host url))))
     (url-port url))
    (sb-bsd-sockets:socket-make-stream s :input t :output t :buffering :full))
  #+:cmu
  (sys:make-fd-stream (ext:connect-to-inet-socket (url-host url) (url-port url))
                      :input t :output t :buffering :full)
  #+:lispworks
  (comm:open-tcp-stream (url-host url) (url-port url)
                        #+(and :lispworks :win32) :element-type
                        #+(and :lispworks :win32) '(unsigned-byte 8))
  #+:allegro
  (socket:make-socket :remote-host (url-host url)
                      :remote-port (url-port url))
  #+:clisp
  (socket:socket-connect (url-port url) (url-host url)
                         :external-format
                         (ext:make-encoding :charset 'charset:iso-8859-1 :line-terminator :unix))
  #+:openmcl
  (ccl:make-socket :remote-host (url-host url)
                   :remote-port (url-port url))
  #+:digitool
  (ccl::open-tcp-stream (url-host url) (url-port url)
                        :element-type 'unsigned-byte))

#+(or :sbcl :cmu)
(defun make-stream-from-gpg-command (string file-name)
  (#+:sbcl sb-ext:process-output
   #+:cmu ext:process-output
   (#+:sbcl sb-ext:run-program
    #+:cmu ext:run-program
    "gpg"
    (list
     "--status-fd" "1" "--verify" "-"
     (namestring file-name))
    :output :stream
    :error nil
    #+sbcl :search #+sbcl t
    :input (make-string-input-stream string)
    :wait t)))

#+(and :lispworks (not :win32))
(defun make-stream-from-gpg-command (string file-name)
  ;; kludge - we can't separate the in and out streams
  (let ((stream (sys:open-pipe (format nil "echo '~A' | gpg --status-fd 1 --verify - ~A"
                                       string
                                       (namestring file-name)))))
    stream))

(defun make-temp-sig (file-name content)
  (let ((name (format nil "~A.asc" (namestring (truename file-name)))))
    (with-open-file (out name
                         :direction :output
                         :if-exists :supersede)
      (write-string content out))
    (pushnew name *temporary-files*)
    name))

#+(and :lispworks :win32)
(defun make-stream-from-gpg-command (string file-name)
  (sys:open-pipe (format nil "gpg --status-fd 1 --verify \"~A\" \"~A\""
                         (make-temp-sig file-name string)
                         (namestring file-name))))

#+(and :clisp (not (or :win32 :cygwin)))
(defun make-stream-from-gpg-command (string file-name)
  (let ((stream
          (ext:run-shell-command (format nil "echo '~A' | gpg --status-fd 1 --verify - ~A"
                                         string
                                         (namestring file-name))
                           :output :stream
                           :wait nil)))
    stream))

#+(and :clisp (or :win32 :cygwin))
(defun make-stream-from-gpg-command (string file-name)
  (ext:run-shell-command (format nil "gpg --status-fd 1 --verify \"~A\" \"~A\""
                                 (make-temp-sig file-name string)
                                 (namestring file-name))
                         :output :stream
                         :wait nil))

#+:allegro
(defun make-stream-from-gpg-command (string file-name)
  (multiple-value-bind (in-stream out-stream)
      (excl:run-shell-command
       #-:mswindows
       (concatenate 'vector
                    #("gpg" "gpg" "--status-fd" "1" "--verify" "-")
                    (make-sequence 'vector 1
                                   :initial-element (namestring file-name)))
       #+:mswindows
       (format nil "gpg --status-fd 1 --verify - \"~A\"" (namestring file-name))
       :input :stream
       :output :stream
       :separate-streams t
       :wait nil)
    (write-string string in-stream)
    (finish-output in-stream)
    (close in-stream)
    out-stream))

#+:openmcl
(defun make-stream-from-gpg-command (string file-name)
  (let ((proc (ccl:run-program "gpg" (list "--status-fd" "1" "--verify" "-" (namestring file-name))
                               :input :stream
                               :output :stream
                               :wait nil)))
    (write-string string (ccl:external-process-input-stream proc))
    (close (ccl:external-process-input-stream proc))
    (ccl:external-process-output-stream proc)))

#+:digitool
(defun make-stream-from-gpg-command (string file-name)
  (make-instance 'popen-input-stream
                 :command (format nil "echo '~A' | gpg --status-fd 1 --verify - '~A'"
                                  string
                                  (system-namestring file-name))))

#+:sbcl
(defun return-output-from-program (program args)
  (with-output-to-string (out-stream)
    (let ((proc (sb-ext:run-program
                 program
                 args
                 :output out-stream
                 :wait t)))
      (when (or (null proc)
                (and (member (sb-ext:process-status proc) '(:exited :signaled))
                     (not (zerop (sb-ext:process-exit-code proc)))))
        (return-from return-output-from-program nil)))))

#+:cmu
(defun return-output-from-program (program args)
  (with-output-to-string (out-stream)
    (let ((proc (ext:run-program
                 program
                 args
                 :output out-stream
                 :wait t)))
      (when (or (null proc)
                (and (member (ext:process-status proc) '(:exited :signaled))
                     (not (zerop (ext:process-exit-code proc)))))
        (return-from return-output-from-program nil)))))

#+:lispworks
(defun return-output-from-program (program args)
  (with-output-to-string (out-stream)
    (unless (zerop (sys:call-system-showing-output
                    (format nil #-:win32 "~A~{ '~A'~}"
                                #+:win32 "~A~{ ~A~}"
                                program args)
                    :prefix ""
                    :show-cmd nil
                    :output-stream out-stream))
      (return-from return-output-from-program nil))))

#+(and :clisp (not :win32))
(defun return-output-from-program (program args)
  (with-output-to-string (out-stream)
    (let ((stream
            (ext:run-program program
                             :arguments args
                             :output :stream
                             :wait nil)))
      (loop for line = (read-line stream nil)
            while line
            do (write-line line out-stream)))))

#+(and :clisp :win32)
(defun return-output-from-program (program args)
  (with-output-to-string (out-stream)
    (let ((stream
            (ext:run-shell-command
             (format nil "~A~{ ~A~}" program args
                     :output :stream
                     :wait nil))))
      (loop for line = (ignore-errors (read-line stream nil))
            while line
            do (write-line line out-stream)))))

#+:allegro
(defun return-output-from-program (program args)
  (with-output-to-string (out-stream)
    (let ((stream
            (excl:run-shell-command
             #-:mswindows
             (concatenate 'vector
                          (list program)
                          (cons program args))
             #+:mswindows
             (format nil "~A~{ ~A~}" program args)
             :output :stream
             :wait nil)))
      (loop for line = (read-line stream nil)
            while line
            do (write-line line out-stream)))))

#+:openmcl
(defun return-output-from-program (program args)
  (with-output-to-string (out-stream)
    (let ((proc (ccl:run-program program args
                                 :input nil
                                 :output :stream
                                 :wait nil)))
      (loop for line = (read-line (ccl:external-process-output-stream proc) nil nil nil)
            while line
            do (write-line line out-stream)))))

#+:digitool
(defun return-output-from-program (program args)
  (ccl::call-system (format nil "~A~{ '~A'~} 2>&1" program args)))

;; why not just use DELETE-FILE?
(defun unlink-file (pathname)
  #+:sbcl
  (sb-posix:unlink pathname)
  #+:cmu
  (unix:unix-unlink (namestring pathname))
  #+:allegro
  (excl.osi:unlink pathname)
  #+(or :lispwork :clisp :openmcl :digitool)
  (delete-file pathname))

(defun symlink-files (old new)
  #+:sbcl
  (sb-posix:symlink old new)
  #+:cmu
  (unix:unix-symlink (namestring old)
                     (namestring new))
  #+:allegro
  (excl.osi:symlink old new)
  #+:lispworks
  ;; we loose if the pathnames contain apostrophes...
  (sys:call-system (format nil "ln -s '~A' '~A'"
                           (namestring old)
                           (namestring new)))
  #+:clisp
  (ext:run-program "ln"
                   :arguments (append '("-s")
                                      (list (format nil "~A" (namestring old))
                                            (format nil "~A" (namestring new)))))
  #+:openmcl
  (ccl:run-program "ln" (list "-s" (namestring old) (namestring new)))
  #+:digitool
  (ccl::call-system (format nil "ln -s '~A' '~A'"
                            (system-namestring old)
                            (system-namestring new))))
