;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2008 Clozure Associates

(in-package "GUI")


(defclass console-window (typeout-window)
  ((syslog-in :foreign-type :id :accessor syslog-in)
   (syslog-out :foreign-type :id :accessor syslog-out)
   (nextra :foreign-type :int)
   (translatebuf :foreign-type :address)
   (bufsize :foreign-type :int)
   (hidden-by-user :initform t :accessor console-window-hidden-by-user))
  (:metaclass ns:+ns-object))

(defconstant $system-console-menu-item-tag 1)


;;; Insert/append a string to the console-window's text view,
;;; activating the window if necessary.

(objc:defmethod (#/insertString: :void) ((self console-window) string)
  (with-slots ((tv typeout-view)) self
    (if (console-window-hidden-by-user self)
      (mark-console-output-available self t)
      (#/makeKeyAndOrderFront: self +null-ptr+))
    (#/insertString: (typeout-view-text-view tv) string)))

(defmethod mark-console-output-available ((self console-window) available-p)
  #+cocotron (declare (ignore available-p))
  #-cocotron
  (let* ((menu (#/windowsMenu *nsapp*))
         (menu-ref (ccl::external-call "__NSGetCarbonMenu" :address menu :address))
         (index (#/indexOfItemWithTag: menu $system-console-menu-item-tag)))
    (when (< index 0)
      (setq index (#/indexOfItemWithTitle: menu #@"Show System Console")))
    (when (> index 0)
      (ccl::external-call "_SetItemMark" :id menu-ref :integer (1+ index)
                          :integer (if available-p 19 0))))) ;19 is diamondMark

;;; Process a chunkful of data
(objc:defmethod (#/processData: :void) ((self console-window) data)
  (with-slots (syslog-in syslog-out nextra translatebuf bufsize) self
    (let* ((encoding (load-time-value (get-character-encoding :utf-8)))
	   (data-length (#/length data))
           (n nextra)
           (cursize bufsize)
           (need (+ n data-length))
           (xlate translatebuf))
      (#/writeData: syslog-out data)
      (when (> need cursize)
        (let* ((new (#_malloc need)))
          (dotimes (i n) (setf (%get-unsigned-byte new i)
                               (%get-unsigned-byte xlate i)))
          (#_free xlate)
          (setq xlate new translatebuf new bufsize need)))
      #+debug (#_NSLog #@"got %d bytes of data" :int data-length)
      (with-macptrs ((target (%inc-ptr xlate n)))
        (#/getBytes:range: data target (ns:make-ns-range 0 data-length)))
      (let* ((total (+ n data-length))
             (noctets-used (nth-value 1
                                      (funcall (ccl::character-encoding-length-of-memory-encoding-function encoding)
                                               xlate
                                               total
                                               0)))
             (string (make-instance ns:ns-string
                                    :with-bytes xlate
                                    :length noctets-used
                                    :encoding #$NSUTF8StringEncoding)))
         (unless (zerop (setq n (- total noctets-used)))
              ;; By definition, the number of untranslated octets
              ;; can't be more than 3.
              (dotimes (i n)
                (setf (%get-unsigned-byte xlate i)
                      (%get-unsigned-byte xlate (+ noctets-used i)))))
            (setq nextra n)
            (#/insertString: self string)))))

;;; We want to be able to capture and display process-level
;;; output to file descriptors 1 and 2, including messages
;;; logged via #_NSLog/#_CFLog and variants.  Logging messages
;;; may only be echoed to fd 2 if that fd is open to a file
;;; (rather than to a socket/pty/pipe/...).  Unless/until
;;; the the file has data written to it, reading from
;;; it will return EOF, and waiting via mechanisms like
;;; #_poll/#_select/#/readInBackgroundAndNotify will indicate
;;; that the file can be read without blocking.  True, but
;;; we'd rather not see it as being constantly at EOF ...
;;; So, we have a timer-driven method wake up every second
;;; or so, and see if there's actually any unread data
;;; to process.

(objc:defmethod (#/checkForData: :void) ((self console-window) timer)
  (declare (ignorable timer))
  (let* ((in (syslog-in self)))
    (loop
      (let* ((data (#/availableData in))
             (n (#/length data)))
        (declare (fixnum n))
        (if (zerop n)
          (return)
          (#/processData: self data))))))

;;; Open file descriptor to a temporary file.  The write-fd will be
;;; open for reading and writing and the file will have mode #o600
;;; (readable/ writable by owner, not accessible to others.)  Unlink
;;; the file as soon as it's opened, to help avoid exposing its contents
;;; (and to ensure that the file gets deleted when the application
;;; quits.)
#-windows-target
(defun open-logging-fds ()
  (with-cstrs ((template "/tmp/logfileXXXXXX"))
    (let* ((write-fd (#_mkstemp template)))
      (when (>= write-fd 0)
        (let* ((read-fd (#_open template #$O_RDONLY)))
          (#_unlink template)
          (values write-fd read-fd))))))



(objc:defmethod #/redirectStandardOutput ((self console-window))
  (with-slots (syslog-out syslog-in) self
    (multiple-value-bind (write-fd read-fd) (open-logging-fds)
      (when write-fd
        (setq syslog-out
              (make-instance 'ns:ns-file-handle :with-file-descriptor (#_dup 1)
                             :close-on-dealloc t))
        (let* ((log-fh (make-instance 'ns:ns-file-handle
                                      :with-file-descriptor read-fd
                                      :close-on-dealloc t)))
          (setq syslog-in log-fh)
          (let* ((bufsize #$BUFSIZ)
                 (buffer (#_malloc bufsize)))
            (setf (slot-value self 'translatebuf) buffer
                  (slot-value self 'bufsize) bufsize
                  (slot-value self 'nextra) 0))
          (#_dup2 write-fd 1)
          (#_dup2 write-fd 2)
          (#/scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:
           ns:ns-timer
           1.0d0
           self
           (@selector #/checkForData:)
           +null-ptr+
           t)))))
  self)

(objc:defmethod #/init ((self console-window))
  (#/release self)
  #+windows-target +null-ptr+
  #-windows-target
  (flet ((path-inode (path)
           (nth-value 4 (ccl::%stat path)))
         (fd-inode (fd)
           (nth-value 4 (ccl::%fstat fd))))
    (cond ((and nil
                (eql (fd-inode 0) (path-inode "/dev/null"))
                (eql (fd-inode 1) (fd-inode 2))
                (rlet ((pflags :long))
                  (#_fcntl 2 #$F_GETFL :address pflags)
                  (let* ((accmode (logand #$O_ACCMODE (pref flags :long))))
                    (or (eql #$O_RDONLY accmode)
                        (eql #$O_RDWR accmode)))))
           (let* ((win (#/typeoutWindowWithTitle: (find-class 'console-window) #@"Console")))


             (#/redirectStandardOutput win)
             (let* ((tv (typeout-view-text-view (typeout-window-typeout-view win))))
               (#/setTypingAttributes: tv
                                       (create-text-attributes
                                        :font (default-font
                                                  :name #+darwin-target "Monaco"
                                                  #-darwin-target "Courier"
                                                :size 10)
                                        :color (#/redColor ns:ns-color))))
             (#/setFrameOrigin: win (ns:make-ns-point 20 20))
             win))
          (t +null-ptr+))))

