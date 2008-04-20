;;; -*- package: asdf-install; -*-
;;;
;;; Digitool-specific bootstrapping
;;;
;;; 2004-01-18 james.anderson@setf.de additions for MCL
;;; 2008-01-22 added exit-code checks to call-system

(in-package #:asdf-install)

#+:digitool
(let ((getenv-fn 0)
      (setenv-fn 0)
      (unsetenv-fn 0)
      (popen-fn 0)
      (pclose-fn 0)
      (fread-fn 0)
      (feof-fn 0))
  (ccl::with-cfstrs ((framework "System.framework"))
    (let ((err 0)
          (baseURL nil)
          (bundleURL nil)
          (bundle nil))
      (ccl::rlet ((folder :fsref))
        ;; Find the folder holding the bundle
        (setf err (ccl::require-trap traps::_FSFindFolder
                                     (ccl::require-trap-constant traps::$kOnAppropriateDisk)
                                     (ccl::require-trap-constant traps::$kFrameworksFolderType)
                                     t folder))
        ;; if everything's cool, make a URL for it
        (when (zerop err)
          (setf baseURL (ccl::require-trap traps::_CFURLCreateFromFSRef (ccl::%null-ptr) folder)))
        (if (ccl::%null-ptr-p baseURL)
          (setf err (ccl::require-trap-constant traps::$coreFoundationUnknownErr))))
      ;; if everything's cool, make a URL for the bundle
      (when (zerop err)
        (setf bundleURL (ccl::require-trap traps::_CFURLCreateCopyAppendingPathComponent (ccl::%null-ptr) baseURL framework nil))
        (if (ccl::%null-ptr-p bundleURL)
          (setf err (ccl::require-trap-constant traps::$coreFoundationUnknownErr))))
      ;; if everything's cool, create it
      (when (zerop err)
        (setf bundle (ccl::require-trap traps::_CFBundleCreate (ccl::%null-ptr) bundleURL))
        (if (ccl::%null-ptr-p bundle)
          (setf err (ccl::require-trap-constant traps::$coreFoundationUnknownErr))))
      ;; if everything's cool, load it
      (when (zerop err)
        (if (not (ccl::require-trap traps::_CFBundleLoadExecutable bundle))
          (setf err (ccl::require-trap-constant traps::$coreFoundationUnknownErr))))
      ;; if there's an error, but we've got a pointer, free it and clear result
      (when (and (not (zerop err)) (not (ccl::%null-ptr-p bundle)))
        (ccl::require-trap traps::_CFRelease bundle)
        (setf bundle nil))
      ;; free the URLs if here non-null
      (when (not (ccl::%null-ptr-p bundleURL))
        (ccl::require-trap traps::_CFRelease bundleURL))
      (when (not (ccl::%null-ptr-p baseURL))
        (ccl::require-trap traps::_CFRelease baseURL))
      (cond (bundle
             ;; extract the necessary function id's
             (flet ((get-addr (name)
                      (ccl::with-cfstrs ((c-name name))
                        (let* ((addr (ccl::require-trap traps::_CFBundleGetFunctionPointerForName bundle c-name)))
                          (when (ccl::%null-ptr-p addr)
                            (error "Couldn't resolve address of foreign function ~s" name))
                          (ccl::rlet ((buf :long))
                            (setf (ccl::%get-ptr buf) addr)
                            (ash (ccl::%get-signed-long buf) -2))))))
               (setf getenv-fn (get-addr "getenv"))
               (setf setenv-fn (get-addr "setenv"))
               (setf unsetenv-fn (get-addr "unsetenv"))
               (setf popen-fn (get-addr "popen"))
               (setf pclose-fn (get-addr "pclose"))
               (setf fread-fn (get-addr "fread"))
               (setf feof-fn (get-addr "feof")))
             (ccl::require-trap traps::_CFRelease bundle)
             (setf bundle nil))
            (t
             (error "can't resolve core framework entry points.")))))
  
  (defun ccl::getenv (variable-name)
    (ccl::with-cstrs ((c-variable-name variable-name))
      (let* ((env-ptr (ccl::%null-ptr)))
        (declare (dynamic-extent env-ptr))
        (ccl::%setf-macptr env-ptr (ccl::ppc-ff-call getenv-fn
                                                     :address c-variable-name
                                                     :address))
        (unless (ccl::%null-ptr-p env-ptr)
          (ccl::%get-cstring env-ptr)))))

  (defun ccl::setenv (variable-name variable-value)
    (ccl::with-cstrs ((c-variable-name variable-name)
                      (c-variable-value variable-value))
      (ccl::ppc-ff-call setenv-fn
                        :address c-variable-name
                        :address c-variable-value
                        :signed-fullword 1
                        :signed-fullword)))

  (defun ccl::unsetenv (variable-name)
    (ccl::with-cstrs ((c-variable-name variable-name))
      (ccl::ppc-ff-call unsetenv-fn
                        :address c-variable-name
                        :void)))
  
  (labels ((fread (fp buffer length)
             (ccl::ppc-ff-call fread-fn
                               :address buffer
                               :unsigned-fullword 1
                               :unsigned-fullword length
                               :address fp
                               :signed-fullword))
           (feof-p (fp)
             (not (zerop (ccl::ppc-ff-call feof-fn
                                           :address fp
                                           :signed-fullword))))
           (popen (command)
             (ccl::with-cstrs  ((read "r")
                                (cmd command))
               (ccl::ppc-ff-call popen-fn
                                 :address cmd
                                 :address read
                                 :address)))
           (pclose (fp)
             (ccl::ppc-ff-call pclose-fn
                               :address fp
                               :signed-fullword))
           
           (fread-decoded (fp io-buffer io-buffer-length string-buffer script)
             (cond ((feof-p fp)
                    (values nil string-buffer))
                   (t
                    (let ((io-count (fread fp io-buffer io-buffer-length)))
                      (cond ((and io-count (plusp io-count))
                             (if script
                               (multiple-value-bind (chars fatp) (ccl::pointer-char-length io-buffer io-count script)
                                 (cond ((not fatp)
                                        (ccl::%copy-ptr-to-ivector io-buffer 0 string-buffer 0 io-count))
                                       (t
                                        (unless (>= (length string-buffer) chars)
                                          (setf string-buffer (make-string chars :element-type 'base-character)))
                                        (ccl::pointer-to-string-in-script io-buffer string-buffer io-count script)
                                        (setf io-count chars))))
                               (ccl::%copy-ptr-to-ivector io-buffer 0 string-buffer 0 io-count))
                             (values io-count string-buffer))
                            (t
                             (values 0 string-buffer))))))))
    
    (defun ccl::call-system (command)
      (let* ((script (ccl::default-script nil))
             (table (ccl::get-char-byte-table script))
             (result (make-array 128 :element-type 'character :adjustable t :fill-pointer 0))
             (string-buffer (unless table (make-string 512 :element-type 'base-character)))
             (io-count 0)
             (fp (popen command))
             (exit-code 0))
        (unless (ccl::%null-ptr-p fp)
          (unwind-protect
            (ccl::%stack-block ((io-buffer 512))
              (loop (multiple-value-setq (io-count string-buffer)
                      (fread-decoded fp io-buffer 512 string-buffer (when table script)))
                    (unless io-count (return))
                    (let ((char #\null))
                      (dotimes (i io-count)
                        (case (setf char (schar string-buffer i))
                          ((#\return #\linefeed) (setf char #\newline)))
                        (vector-push-extend char result)))))
            (setf exit-code (pclose fp))
            (setf fp nil))
          (if (zerop exit-code)
            (values result 0)
            (values nil exit-code result)))))
    
    ;; need a function to avoid both the reader macro and the compiler
    (setf (symbol-function '%new-ptr) #'ccl::%new-ptr) 
    
    (defclass popen-input-stream (ccl::input-stream)
      ((io-buffer :initform nil)
       (fp :initform nil )
       (string-buffer :initform nil)
       (length :initform 0)
       (index :initform 0)
       (script :initarg :script :initform (ccl::default-script nil)))
      (:default-initargs :direction :input))
    
    (defmethod initialize-instance :after ((instance popen-input-stream) &key command)
      (with-slots (io-buffer string-buffer fp script) instance
        (setf fp (popen command)
              io-buffer (%new-ptr 512 nil)
              string-buffer (make-string 512 :element-type 'base-character))
        (when script (unless (ccl::get-char-byte-table script) (setf script nil)))))
    
    (defmethod ccl::stream-close ((stream popen-input-stream))
      (declare (ignore abort))
      (with-slots (io-buffer string-buffer fp ccl::direction) stream
        (when (and fp (not (ccl::%null-ptr-p fp)))
          (pclose fp)
          (setf fp nil)
          (setf ccl::direction :closed)
          (ccl::disposeptr io-buffer)
          (setf io-buffer nil))))
    
    (defmethod stream-element-type ((stream popen-input-stream))
      'character)
    
    (defmethod ccl::stream-tyi ((stream popen-input-stream))
      ;; despite the decoding provisions, unix input comes with linefeeds
      ;; and i don't know what decoding one would need.
      (with-slots (io-buffer fp string-buffer length index script) stream
        (when fp
          (when (>= index length)
            (multiple-value-setq (length string-buffer)
              (fread-decoded fp io-buffer 512 string-buffer script))
            (unless (and length (plusp length))
              (setf length -1)
              (return-from ccl::stream-tyi nil))
            (setf index 0))
          (let ((char (schar string-buffer index)))
            (incf index)
            (case char
              ((#\return #\linefeed) #\newline)
              (t char))))))
    
    (defmethod ccl::stream-untyi ((stream popen-input-stream) char)
      (with-slots (string-buffer length index) stream
        (unless (and (plusp index) (eql char (schar (decf index) string-buffer)))
          (error "invalid tyi character: ~s." char))
        char))

    (defmethod ccl::stream-eofp ((stream popen-input-stream))
      (with-slots (length) stream
        (minusp length)))))
