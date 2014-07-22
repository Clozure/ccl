;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2010 Clozure Associates
;;;   Copyright (C) 1994-2001 Digitool, Inc
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

(in-package "CCL")

;;; Real (RS-232) serial devices are pretty rare these days;
;;; USB to serial devices are fairly common.

(defclass serial-stream (fd-stream
                         buffered-binary-io-stream-mixin
                         buffered-character-io-stream-mixin)
    ())

(defmethod select-stream-class ((s serial-stream) in-p out-p char-p)
  (declare (ignore in-p out-p char-p))
  'serial-stream)


(defun ttyname (fd)
  (let* ((p (#_ttyname fd)))
    (if (%null-ptr-p p)
      "<unknown>"
      (%get-cstring p))))

(defvar *baud-rates*
  '((50 . #.#$B50)
    (75 . #.#$B75)
    (110 . #.#$B110)
    (134 . #.#$B134)
    (150 . #.#$B150)
    (200 . #.#$B200)
    (300 . #.#$B300)
    (600 . #.#$B600)
    (1200 . #.#$B1200)
    (1800 . #.#$B1800)
    (2400 . #.#$B2400)
    (4800 . #.#$B4800)
    (9600 . #.#$B9600)
    (19200 . #.#$B19200)
    (38400 . #.#$B38400)
    (57600 . #.#$B57600)
    (115200 . #.#$B115200)
    (230400 . #.#$B230400)
    ;; It's the world's most advanced Operating System!
    #-darwin-target
    (460800 . #.#$B460800)))

(defun encode-baud-rate (rate)
  (or (cdr (assoc rate *baud-rates*))
      (error "Unsupported baud rate - ~s." rate)))

;;; There are supposedly ~60 flags that can be set in a termios
;;; structure; this only allows a few of them to be set explicitly,
;;; and is otherwise oriented towards "communicating with a device
;;; via a serial port" rather than "comminicating with a user via
;;; a serial port."
(defun setup-serial-device (fd &key (baud-rate nil baud-rate-p)
                               (parity nil parity-p)
                               (char-bits nil char-bits-p)
                               (stop-bits nil stop-bits-p)
                               (flow-control nil flow-control-p)
                               &allow-other-keys)
  (rlet ((settings :termios))
    (flet ((check-error (result operation)
             (if (< result 0)
               (error "Error trying to ~a for fd ~d : ~a" operation fd (%strerror (%get-errno)))
               result)))
      (check-error (#_tcgetattr fd settings) "get serial settings")
      (let* ((orig-cflag (pref settings :termios.c_cflag))
             (cflag orig-cflag)
             (orig-iflag (pref settings :termios.c_cflag))
             (iflag orig-iflag)
             (orig-oflag (pref settings :termios.c_oflag))
             (oflag orig-oflag)
             (orig-lflag (pref settings :termios.c_lflag))
             (lflag orig-lflag))
        ;; Inhibit input/output translation/canonicalization
        (setq oflag 0)
        (setq iflag (logandc2 iflag
                              (logior #$IGNBRK #$BRKINT #$ICRNL
                                      #$INLCR #$PARMRK #$INPCK #$ISTRIP)))
        (setq lflag (logandc2 lflag
                              (logior #$ECHO #$ECHONL #$ICANON #$IEXTEN #$ISIG)))
        (when baud-rate-p
          (check-error (#_cfsetspeed settings (encode-baud-rate baud-rate))
                       "set baud rate"))

        (when parity-p
          (setq cflag
                (ecase parity
                  (:even (logior #$PARENB (logandc2 cflag #$PARODD)))
                  (:odd  (logior #$PARENB (logior cflag #$PARODD)))
                  ((:none nil) (logandc2 cflag #$PARENB)))))
        (when char-bits-p
          (setq cflag
                (logior (logandc2 cflag #$CSIZE)
                        (ecase char-bits
                          (5 #$CS5)
                          (6 #$CS6)
                          (7 #$CS7)
                          (8 #$CS8)))))
        (when flow-control-p
          (setq iflag (logandc2 iflag (logior #$IXON #$IXOFF))
                cflag (logandc2 cflag #$CRTSCTS))
          
          (ecase flow-control
            ((:hardware :rts/cts) (setq cflag (logior cflag #$CRTSCTS)))
            ((:software :xon/xoff) (setq iflag (logior iflag #$IXON #$IXOFF)))
            ((:none nil))))
        (when stop-bits-p
          (setq cflag
                (ecase stop-bits
                  (1 (logandc2 cflag #$CSTOPB))
                  (2 (logior cflag #$CSTOPB)))))
        (unless (eql cflag orig-cflag)
          (setf (pref settings :termios.c_cflag) cflag))
        (unless (eql iflag orig-iflag)
          (setf (pref settings :termios.c_iflag) iflag))
        (unless (eq lflag orig-lflag)
          (setf (pref settings :termios.c_lflag) lflag))
        (unless (eq oflag orig-oflag)
          (setf (pref settings :termios.c_lflag) oflag))
        (check-error (#_tcsetattr fd #$TCSANOW settings) "set serial settings")))))

(defmethod setup-serial-stream ((s serial-stream) &rest initargs)
  (apply #'setup-serial-device (stream-device s :input) initargs))
      

(defun get-serial-attributes (fd)
  (flet ((check-error (result operation)
           (if (< result 0)
             (error "Error trying to ~a for fd ~d : ~a" operation fd (%strerror (%get-errno)))
             result)))
    (rlet ((settings :termios))
      (check-error (#_tcgetattr fd settings) "get serial attributes")
      (let* ((cflag (pref settings :termios.c_cflag)))
        (values
         (car (rassoc
                   (check-error (#_cfgetispeed settings) "determine baud rate")
                   *baud-rates*))
         (if (logtest #$PARENB cflag)
                (if (logtest #$PARODD cflag)
                  :odd
                  :even)
                :none)
         (case (logand #$CSIZE cflag)
                (#.#$CS5 5)
                (#.#$CS6 6)
                (#.#$CS7 7)
                (#.#$CS8 8))
         (if (logtest cflag #$CSTOPB)
                2
                1)
         (if (logtest cflag #$CRTSCTS)
           :rts/cts
           (if (logtest (pref settings :termios.c_iflag)
                        (logior #$IXON #$IXOFF))
             :xon/xoff
             :none)))))))

(defmethod print-object ((s serial-stream) out)
  (print-unreadable-object (s out :type t :identity t)
    (let* ((ioblock (stream-ioblock s nil))
           (fd (and ioblock (ioblock-device ioblock)))
           (encoding (and ioblock (encoding-name (ioblock-encoding ioblock)))))
      (if fd
        (multiple-value-bind (baud parity char-bits stop-bits flow-control)
            (ignore-errors (get-serial-attributes fd))
          (format out "~s (~a [~d]) ~d,~c~d~d ~a"
                  encoding
                  (ttyname fd)
                  fd
                  baud
                  (case parity
                    (:even #\E)
                    (:odd #\O)
                    (t #\N))
                  char-bits
                  stop-bits
                  (string-downcase flow-control)))
        (format out "~s" :closed)))))

(defun make-serial-stream (device-name
                           &rest initargs
                           &key (format :bivalent)
                           external-format
                           (class 'serial-stream)
                           sharing
                           (basic nil)
                           (auto-close t)
                           input-timeout
                           output-timeout
                           deadline
                           &allow-other-keys)
  
  (let* ((external-format (normalize-external-format t external-format))
         (element-type (ecase format
                         ((nil :text) 'character)
                         ((:binary :bivalent) '(unsigned-byte 8))))
         (fd (fd-open device-name (logior #$O_RDWR #$O_NOCTTY))))
    (when (< fd 0)
      (error "Error opening ~s: ~a" device-name (%strerror (%get-errno))))
    (unless (isatty fd)
      (fd-close fd)
      (error "Not a serial device: ~s." device-name))
    (let* ((stream (make-fd-stream fd
                                   :class class
                                   :direction :io
                                   :element-type element-type
                                   :sharing sharing
                                   :character-p (not (eq format :binary))
                                   :encoding (external-format-character-encoding external-format)
                                   :line-termination (external-format-line-termination external-format)
                                   :basic basic
                                   :auto-close auto-close
                                   :input-timeout input-timeout
                                   :output-timeout output-timeout
                                   :deadline deadline)))
      (apply #'setup-serial-stream stream initargs)
      stream)))
