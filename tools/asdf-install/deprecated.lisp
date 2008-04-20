(in-package asdf-install)

#+(and ignore sbcl) ; Deprecated.
(define-symbol-macro *sbcl-home* *asdf-install-dirs*)

#+(and ignore sbcl) ; Deprecated.
(define-symbol-macro *dot-sbcl* *private-asdf-install-dirs*)

#+(or)
;; uncalled
(defun read-until-eof (stream)
  (with-output-to-string (o)
    (copy-stream stream o)))


#+(or)
(defun verify-gpg-signature/string (string file-name)
  (block verify
    (loop
      (restart-case
        (let ((gpg-stream (make-stream-from-gpg-command string file-name))
              tags)
          (unwind-protect
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
            (ignore-errors
             (close gpg-stream)))
          ;; test that command returned something 
          (unless tags
            (error 'gpg-shell-error))
          ;; test for obvious key/sig problems
          (let ((errsig (header-value :errsig tags)))
            (and errsig (error 'key-not-found :key-id errsig)))
          (let ((badsig (header-value :badsig tags)))
            (and badsig (error 'key-not-found :key-id badsig)))
          (let* ((good (header-value :goodsig tags))
	         (id (first good))
	         (name (format nil "~{~A~^ ~}" (rest good))))
            ;; good signature, but perhaps not trusted
            (restart-case
              (let ((trusted? (or (header-pair :trust_ultimate tags)
                                  (header-pair :trust_fully tags)))
                    (in-list? (assoc id *trusted-uids* :test #'equal)))
                (cond ((or trusted? in-list?)
                       ;; ok
                       )
                      ((not trusted?)
                       (error 'key-not-trusted :key-user-name name :key-id id))
                      ((not in-list?)
                       (error 'author-not-trusted
                         :key-user-name name :key-id id))
                      (t
                       (error "Boolean logic gone bad. Run for the hills"))))
              (add-key (&rest rest)
                       :report "Add to package supplier list"
                       (declare (ignore rest))
                       (pushnew (list id name) *trusted-uids*))))
          (return-from verify t))
        #+Ignore
        (install-anyways (&rest rest)
	                       :report "Don't check GPG signature for this package"
                               (declare (ignore rest))
	                       (return-from verify t))
        (retry-gpg-check (&rest args)
                         :report "Retry GPG check \(e.g., after downloading the key\)"
                         (declare (ignore args))
                         nil)))))

#+(or)
(defun verify-gpg-signature/url (url file-name)
  (block verify
    (loop
      (restart-case
        (when (verify-gpg-signatures-p url)
          (let ((sig-url (concatenate 'string url ".asc")))
            (destructuring-bind (response headers stream)
                                (url-connection sig-url)
              (unwind-protect
                (flet (#-:digitool
                       (read-signature (data stream)
                         (read-sequence data stream))
                       #+:digitool
                       (read-signature (data stream)
                         (multiple-value-bind (reader arg)
                                              (ccl:stream-reader stream)
                           (let ((byte 0))
                             (dotimes (i (length data))
                               (unless (setf byte (funcall reader arg))
                                 (error 'download-error :url sig-url
                                        :response 200))
                               (setf (char data i) (code-char byte)))))))
                  (if (= response 200)
                    (let ((data (make-string (parse-integer
                                              (header-value :content-length headers)
                                              :junk-allowed t))))
                      (read-signature data stream)
                      (verify-gpg-signature/string data file-name))
                    (error 'download-error :url sig-url
                           :response response)))
                (close stream)
                (return-from verify t)))))
        (install-anyways (&rest rest)
                         :report "Don't check GPG signature for this package"
                         (declare (ignore rest))
                         (return-from verify t))
        (retry-gpg-check (&rest args)
                         :report "Retry GPG check \(e.g., after fixing the network connection\)"
                         (declare (ignore args))
                         nil)))))


#+(or :sbcl :cmu :scl)
(defun make-stream-from-gpg-command (string file-name)
  (#+:sbcl sb-ext:process-output
   #+(or :cmu :scl) ext:process-output
   (#+:sbcl sb-ext:run-program
    #+(or :cmu :scl) ext:run-program
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

#+(or)
(defun make-temp-sig (file-name content)
  (let ((name (format nil "~A.asc" (namestring (truename file-name)))))
    (with-open-file (out name
                         :direction :output
                         :if-exists :supersede)
      (write-string content out))
    (pushnew name *temporary-files*)
    name))

