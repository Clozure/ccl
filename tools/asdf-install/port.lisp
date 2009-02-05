(in-package #:asdf-install)

(defvar *temporary-files*)

(defparameter *shell-path* "/bin/sh"
  "The path to a Bourne compatible command shell in physical pathname notation.")

(eval-when (:load-toplevel :compile-toplevel :execute)
  #+:allegro
  (require :osi)
  #+:allegro
  (require :socket)
  #+:digitool
  (require :opentransport)
  #+:ecl
  (require :sockets)
  #+:lispworks
  (require "comm")
  )

(defun get-env-var (name)
  #+:allegro (sys:getenv name)
  #+:clisp (ext:getenv name)
  #+:cmu (cdr (assoc (intern (substitute #\_ #\- name)
                             :keyword)
                     ext:*environment-list*))
  #+:ecl (ext:getenv name)
  #+:lispworks (lw:environment-variable name)
  #+(or :mcl :openmcl) (ccl::getenv name)
  #+:sbcl (sb-ext:posix-getenv name)
  #+:scl (cdr (assoc name ext:*environment-list* :test #'string=))
  )

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
#-(or :digitool)
(defvar *stream-buffer-size* 8192)
#-(or :digitool)
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

(defun make-stream-from-url (url)
  #+(or :sbcl :ecl)
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket
             :type :stream
             :protocol :tcp)))
    (sb-bsd-sockets:socket-connect
     s (car (sb-bsd-sockets:host-ent-addresses
             (sb-bsd-sockets:get-host-by-name (url-host url))))
     (url-port url))
    (sb-bsd-sockets:socket-make-stream 
     s
     :input t 
     :output t
     :buffering :full
     :external-format :iso-8859-1))
  #+:cmu
  (sys:make-fd-stream (ext:connect-to-inet-socket (url-host url) (url-port url))
                      :input t :output t :buffering :full)
  #+:scl
  (sys:make-fd-stream (ext:connect-to-inet-socket (url-host url) (url-port url))
                      :input t :output t :buffering :full
		      :external-format :iso-8859-1)
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


#+:sbcl
(defun return-output-from-program (program args)
  (with-output-to-string (out-stream)
    (let ((proc (sb-ext:run-program
                 program
                 args
                 :output out-stream
                 :search t
                 :wait t)))
      (when (or (null proc)
                (and (member (sb-ext:process-status proc) '(:exited :signaled))
                     (not (zerop (sb-ext:process-exit-code proc)))))
        (return-from return-output-from-program nil)))))

#+(or :cmu :scl)
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

#+:ecl
(defun return-output-from-program (program args)
  (with-output-to-string (out-stream)
    (let ((stream (ext:run-program program args :output :stream)))
      (when stream
	(loop for line = (ignore-errors (read-line stream nil))
	      while line
	      do (write-line line out-stream))))))

#+:openmcl
(defun return-output-from-program (program args)
  (with-output-to-string (out-stream)
    (let ((proc (ccl:run-program program args
                                 :input nil
                                 :output :stream
                                 :wait nil)))
      (loop for line = (read-line
			(ccl:external-process-output-stream proc) nil nil nil)
            while line
            do (write-line line out-stream)))))

#+:digitool
(defun return-output-from-program (program args)
  (ccl::call-system (format nil "~A~{ '~A'~} 2>&1" program args)))

(defun unlink-file (pathname)
  ;; 20070208 gwking@metabang.com - removed lisp-specific os-level calls
  ;; in favor of a simple delete
  (delete-file pathname))

(defun symlink-files (old new)
  (let* ((old (#-scl namestring #+scl ext:unix-namestring old))
	 (new (#-scl namestring #+scl ext:unix-namestring new #+scl nil))
	 ;; 20070811 - thanks to Juan Jose Garcia-Ripoll for pointing
	 ;; that ~a would wreck havoc if the working directory had a space
	 ;; in the pathname
	 (command (format nil "ln -s ~s ~s" old new)))
    (format t "~S~%" command)
    (shell-command command)))

(defun maybe-symlink-sysfile (system sysfile)
  (declare (ignorable system sysfile))
  #-(or :win32 :mswindows)
  (let ((target (merge-pathnames
                 (make-pathname :name (pathname-name sysfile)
                                :type (pathname-type sysfile))
                 system)))
    (when (probe-file target)
      (unlink-file target))
    (symlink-files sysfile target)))

;;; ---------------------------------------------------------------------------
;;; read-header-line
;;; ---------------------------------------------------------------------------

#-:digitool
(defun read-header-line (stream)
  (read-line stream))

#+:digitool
(defun read-header-line (stream &aux (line (make-array 16
                                                       :element-type 'character
                                                       :adjustable t
                                                       :fill-pointer 0))
                                (byte nil))
  (print (multiple-value-bind (reader arg)
                              (ccl::stream-reader stream)
           (loop (setf byte (funcall reader arg))
                 (case byte
                   ((nil)
                    (return))
                   ((#.(char-code #\Return)
                     #.(char-code #\Linefeed))
                    (case (setf byte (funcall reader arg))
                      ((nil #.(char-code #\Return) #.(char-code #\Linefeed)))
                      (t (ccl:stream-untyi stream byte)))
                    (return))
                   (t
                    (vector-push-extend (code-char byte) line))))
           (when (or byte (plusp (length line)))
             line))))

(defun open-file-arguments ()
  (append 
   #+sbcl
   '(:external-format :latin1)
   #+:scl
   '(:external-format :iso-8859-1)
   #+(or :clisp :digitool (and :lispworks :win32))
   '(:element-type (unsigned-byte 8))))

(defun download-url-to-file (url file-name)
  "Resolves url and then downloads it to file-name; returns the url actually used."
  (multiple-value-bind (response headers stream)
      (loop
       (destructuring-bind (response headers stream)
	   (url-connection url)
	 (unless (member response '(301 302))
	   (return (values response headers stream)))
	 (close stream)
	 (setf url (header-value :location headers))))
    (when (>= response 400)
      (error 'download-error :url url :response response))
    (let ((length (parse-integer (or (header-value :content-length headers) "")
				 :junk-allowed t)))
      (installer-msg t "Downloading ~A bytes from ~A to ~A ..."
		     (or length "some unknown number of")
		     url
		     file-name)
      (force-output)
      #+:clisp (setf (stream-element-type stream)
		     '(unsigned-byte 8))
      (let ((ok? nil) (o nil))
	(unwind-protect
	     (progn
	       (setf o (apply #'open file-name 
			      :direction :output :if-exists :supersede
			      (open-file-arguments)))
	       #+(or :cmu :digitool)
	       (copy-stream stream o)
	       #-(or :cmu :digitool)
	       (if length
		   (let ((buf (make-array length
					  :element-type
					  (stream-element-type stream))))
		     #-:clisp (read-sequence buf stream)
		     #+:clisp (ext:read-byte-sequence buf stream :no-hang nil)
		     (write-sequence buf o))
		   (copy-stream stream o))
	       (setf ok? t))
	  (when o (close o :abort (null ok?))))))
    (close stream))
  (values url))

(defun download-url-to-temporary-file (url)
  "Attempts to download url to a new, temporary file. Returns the resolved url and the file name \(as multiple values\)."
  (let ((tmp (temp-file-name url)))
    (pushnew tmp *temporary-files*)
    (values (download-url-to-file url tmp) tmp)))

(defun gpg-results (package signature)
  (let ((tags nil))
    (with-input-from-string
	(gpg-stream 
	 (shell-command (format nil "gpg --status-fd 1 --verify ~s ~s"
				(namestring signature) (namestring package))))
      (loop for l = (read-line gpg-stream nil nil)
	 while l
	 do (print l)
	 when (> (mismatch l "[GNUPG:]") 6)
	 do (destructuring-bind (_ tag &rest data)
		(split-sequence-if (lambda (x)
				     (find x '(#\Space #\Tab)))
				   l)
	      (declare (ignore _))
	      (pushnew (cons (intern (string-upcase tag) :keyword)
			     data) tags)))
      tags)))

#+allegro
(defun shell-command (command)
  (multiple-value-bind (output error status)
	               (excl.osi:command-output command :whole t)
    (values output error status)))

#+clisp
(defun shell-command (command)
  ;; BUG: CLisp doesn't allow output to user-specified stream
  (values
   nil
   nil
   (ext:run-shell-command  command :output :terminal :wait t)))

#+(or :cmu :scl)
(defun shell-command (command)
  (let* ((process (ext:run-program
                   *shell-path*
                   (list "-c" command)
                   :input nil :output :stream :error :stream))
         (output (file-to-string-as-lines (ext::process-output process)))
         (error (file-to-string-as-lines (ext::process-error process))))
    (close (ext::process-output process))
    (close (ext::process-error process))
    (values
     output
     error
     (ext::process-exit-code process))))

#+ecl
(defun shell-command (command)
  ;; If we use run-program, we do not get exit codes
  (values nil nil (ext:system command)))

#+lispworks
(defun shell-command (command)
  ;; BUG: Lispworks combines output and error streams
  (let ((output (make-string-output-stream)))
    (unwind-protect
      (let ((status
             (system:call-system-showing-output
              command
              :prefix ""
              :show-cmd nil
              :output-stream output)))
        (values (get-output-stream-string output) nil status))
      (close output))))

#+openmcl
(defun shell-command (command)
  (let* ((process (create-shell-process command t))
         (output (file-to-string-as-lines 
                  (ccl::external-process-output-stream process)))
         (error (file-to-string-as-lines
                 (ccl::external-process-error-stream process))))
    (close (ccl::external-process-output-stream process))
    (close (ccl::external-process-error-stream process))
    (values output
            error
            (process-exit-code process))))

#+openmcl
(defun create-shell-process (command wait)
  (ccl:run-program
   *shell-path*
   (list "-c" command)
   :input nil :output :stream :error :stream
   :wait wait))

#+openmcl
(defun process-exit-code (process)
  (nth-value 1 (ccl:external-process-status process)))

#+digitool
(defun shell-command (command)
  ;; BUG: I have no idea what this returns
  (ccl::call-system command))

#+sbcl
(defun shell-command (command)
  (let* ((process (sb-ext:run-program
                   *shell-path*
                   (list "-c" command)
                   :input nil :output :stream :error :stream))
         (output (file-to-string-as-lines (sb-impl::process-output process)))
         (error (file-to-string-as-lines (sb-impl::process-error process))))
    (close (sb-impl::process-output process))
    (close (sb-impl::process-error process))
    (values
     output
     error
     (sb-impl::process-exit-code process))))

(defgeneric file-to-string-as-lines (pathname)
  (:documentation ""))

(defmethod file-to-string-as-lines ((pathname pathname))
  (with-open-file (stream pathname :direction :input)
    (file-to-string-as-lines stream)))

(defmethod file-to-string-as-lines ((stream stream))
  (with-output-to-string (s)
    (loop for line = (read-line stream nil :eof nil) 
	 until (eq line :eof) do
	 (princ line s)
	 (terpri s))))

;; copied from ASDF
(defun pathname-sans-name+type (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME and TYPE components"
  (make-pathname :name nil :type nil :defaults pathname))

