; -*- Mode: Lisp; Package: CCL; -*-
;;;
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




(defun utf-8-octets-in-string (string start end)
  (if (>= end start)
    (do* ((noctets 0)
          (i start (1+ i)))
         ((= i end) noctets)
      (declare (fixnum noctets))
      (let* ((code (char-code (schar string i))))
        (declare (type (mod #x110000) code))
        (incf noctets
              (if (< code #x80)
                1
                (if (< code #x800)
                  2
                  (if (< code #x10000)
                    3
                    4))))))
    0))

(defun utf-16-octets-in-string (string start end)
  (if (>= end start)
    (do* ((noctets 0)
          (i start (1+ i)))
         ((= i end) noctets)
      (declare (fixnum noctets))
      (let* ((code (char-code (schar string i))))
        (declare (type (mod #x110000) code))
        (incf noctets
              (if (< code #x10000)
                2
                4))))
    0))

(defun utf-8-memory-encode (string pointer idx start end)
  (declare (fixnum idx))
  (do* ((i start (1+ i)))
       ((>= i end) idx)
    (let* ((code (char-code (schar string i))))
      (declare (type (mod #x110000) code))
      (cond ((< code #x80)
             (setf (%get-unsigned-byte pointer idx) code)
             (incf idx))
            ((< code #x800)
             (setf (%get-unsigned-byte pointer idx)
                   (logior #xc0 (the fixnum (ash code -6))))
             (setf (%get-unsigned-byte pointer (the fixnum (1+ idx)))
                   (logior #x80 (the fixnum (logand code #x3f))))
             (incf idx 2))
            ((< code #x10000)
             (setf (%get-unsigned-byte pointer idx)
                   (logior #xe0 (the fixnum (ash code -12))))
             (setf (%get-unsigned-byte pointer (the fixnum (1+ idx)))
                   (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6))))))
             (setf (%get-unsigned-byte pointer (the fixnum (+ idx 2)))
                   (logior #x80 (the fixnum (logand code #x3f))))
             (incf idx 3))
            (t
             (setf (%get-unsigned-byte pointer idx)
                   (logior #xf0
                           (the fixnum (logand #x7 (the fixnum (ash code -18))))))
             (setf (%get-unsigned-byte pointer (the fixnum (1+ idx)))
                   (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -12))))))
             (setf (%get-unsigned-byte pointer (the fixnum (+ idx 2)))
                   (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6))))))
             (setf (%get-unsigned-byte pointer (the fixnum (+ idx 3)))
                   (logior #x80 (logand #x3f code)))
             (incf idx 4))))))

(defun native-utf-16-memory-encode (string pointer idx start end)
  (declare (fixnum idx))
  (do* ((i start (1+ i)))
       ((>= i end) idx)
    (let* ((code (char-code (schar string i)))
           (highbits (- code #x10000)))
      (declare (type (mod #x110000) code)
               (fixnum  highbits))
      (cond ((< highbits 0)
             (setf (%get-unsigned-word pointer idx) code)
             (incf idx 2))
            (t
             (setf (%get-unsigned-word pointer idx) (logior #xd800 (the fixnum (ash highbits -10))))
             (incf idx 2)
             (setf (%get-unsigned-word pointer idx) (logior #xdc00 (the fixnum (logand highbits #x3ff))))
             (incf idx 2))))))

(defun utf-8-memory-decode (pointer noctets idx string)
  (declare (fixnum noctets idx))
  (do* ((i 0 (1+ i))
        (end (+ idx noctets))
        (index idx (1+ index)))
       ((>= index end) (if (= index end) index 0))
    (let* ((1st-unit (%get-unsigned-byte pointer index)))
      (declare (type (unsigned-byte 8) 1st-unit))
      (let* ((char (if (< 1st-unit #x80)
                     (code-char 1st-unit)
                     (if (>= 1st-unit #xc2)
                       (let* ((2nd-unit (%get-unsigned-byte pointer (incf index))))
                         (declare (type (unsigned-byte 8) 2nd-unit))
                         (if (< 1st-unit #xe0)
                           (if (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                             (code-char
                              (logior
                               (the fixnum (ash (the fixnum (logand #x1f 1st-unit)) 6))
                               (the fixnum (logxor 2nd-unit #x80)))))
                           (let* ((3rd-unit (%get-unsigned-byte pointer (incf index))))
                             (declare (type (unsigned-byte 8) 3rd-unit))
                             (if (< 1st-unit #xf0)
                               (if (and (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                                        (< (the fixnum (logxor 3rd-unit #x80)) #x40)
                                        (or (>= 1st-unit #xe1)
                                            (>= 2nd-unit #xa0)))
                                 (code-char (the fixnum
                                              (logior (the fixnum
                                                        (ash (the fixnum (logand 1st-unit #xf))
                                                             12))
                                                      (the fixnum
                                                        (logior
                                                         (the fixnum
                                                           (ash (the fixnum (logand 2nd-unit #x3f))
                                                                6))
                                                         (the fixnum (logand 3rd-unit #x3f))))))))
                               (if (< 1st-unit #xf8)
                                 (let* ((4th-unit (%get-unsigned-byte pointer (incf index))))
                                   (declare (type (unsigned-byte 8) 4th-unit))
                                   (if (and (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                                            (< (the fixnum (logxor 3rd-unit #x80)) #x40)
                                            (< (the fixnum (logxor 4th-unit #x80)) #x40)
                                            (or (>= 1st-unit #xf1)
                                                (>= 2nd-unit #x90)))
                                     (code-char
                                      (logior
                                       (the fixnum
                                         (logior
                                          (the fixnum
                                            (ash (the fixnum (logand 1st-unit 7)) 18))
                                          (the fixnum
                                            (ash (the fixnum (logxor 2nd-unit #x80)) 12))))
                                       (the fixnum
                                         (logior
                                          (the fixnum
                                            (ash (the fixnum (logxor 3rd-unit #x80)) 6))
                                          (the fixnum (logxor 4th-unit #x80)))))))))))))))))
        (setf (schar string i) (or char #\Replacement_Character))))))

(defun utf-8-length-of-memory-encoding (pointer noctets start)
  (do* ((i start)
        (end (+ start noctets))
        (nchars 0 (1+ nchars)))
       ((= i end) (values nchars (- i start)))
    (let* ((code (%get-unsigned-byte pointer i))
           (nexti (+ i (cond ((< code #xc2) 1)
                             ((< code #xe0) 2)
                             ((< code #xf0) 3)
                             ((< code #xf8) 4)
                             (t 1)))))
      (declare (type (unsigned-byte 8) code))
      (if (> nexti end)
        (return (values nchars (- i start)))
        (setq i nexti)))))



;;; write nbytes bytes from buffer buf to file-descriptor fd.
(defun fd-write (fd buf nbytes)
  (ignoring-eintr
   (int-errno-ffcall
    (%kernel-import target::kernel-import-lisp-write)
             :int fd :address buf :ssize_t nbytes :ssize_t)))

(defun fd-read (fd buf nbytes)
  (ignoring-eintr
   (int-errno-ffcall
    (%kernel-import target::kernel-import-lisp-read)
             :int fd :address buf :ssize_t nbytes :ssize_t)))


(let* ((pathname-encoding-name ()))
  (declare (ignorable pathname-encoding-name))
  (defun pathname-encoding-name ()
    #+darwin-target :utf-8
    #+windows-target :utf-16le
    #-(or darwin-target windows-target) pathname-encoding-name)
  (defun set-pathname-encoding-name (new)
    #+(or darwin-target windows-target) (declare (ignore new))
    #+darwin-target :utf-8
    #+windows-target :utf-16le
    #-(or darwin-target windows-target)
    (let* ((encoding (ensure-character-encoding new)))
      (setq pathname-encoding-name
            (unless (eq encoding (get-character-encoding nil))
              (character-encoding-name encoding))))))


(defun fd-open-path (p flags create-mode)
  (let* ((fd (int-errno-ffcall
              (%kernel-import target::kernel-import-lisp-open)
              :address p :int flags :mode_t create-mode :int)))
    (declare (fixnum fd))
    (when (or (= fd (- #$EMFILE))
              (= fd (- #$ENFILE)))
      (gc)
      (drain-termination-queue)
      (setq fd (int-errno-ffcall
                (%kernel-import target::kernel-import-lisp-open)
                :address p :int flags :mode_t create-mode :int)))
    fd))

(defun fd-open (path flags &optional (create-mode #o666))
  #+darwin-target (with-utf-8-cstrs ((p path))
                    (fd-open-path p flags create-mode))
  #+windows-target (with-native-utf-16-cstrs ((p path))
                     (fd-open-path p flags create-mode))
  #-(or darwin-target windows-target)
  (let* ((encoding (pathname-encoding-name)))
    (if encoding
      (with-encoded-cstrs encoding ((p path))
        (fd-open-path p flags create-mode))
      (with-cstrs ((p path))
        (fd-open-path p flags create-mode)))))

(defun fd-chmod (fd mode)
  (int-errno-ffcall (%kernel-import target::kernel-import-lisp-fchmod)
                    :int fd
                    :mode_t mode
                    :int))

(defun fd-lseek (fd offset whence)
  (int-errno-ffcall
   (%kernel-import target::kernel-import-lisp-lseek)
   :int fd
   :signed-doubleword offset
   :int whence
   :signed-doubleword))

(defun fd-close (fd)
  (int-errno-ffcall (%kernel-import target::kernel-import-lisp-close)
                    :int fd
                    :int)) 

(defun fd-tell (fd)
  (fd-lseek fd 0 #$SEEK_CUR))

;;; Kernels prior to 2.4 don't seem to have a "stat" variant
;;; that handles 64-bit file offsets.
(defun fd-size (fd)
  (rlet ((stat #+win64-target #>_stat64 #+win32-target #>__stat64 #-windows-target :stat))
    (if (eql 0 (ff-call (%kernel-import target::kernel-import-lisp-fstat)
                        :int fd
                        :address stat
                        :int))
      (pref stat
            #-windows-target :stat.st_size
            #+win64-target #>_stat64.st_size
            #+win32-target #>__stat64.st_size)
      -1)))


(defun fd-ftruncate (fd new)
  (int-errno-ffcall (%kernel-import target::kernel-import-lisp-ftruncate)
                    :int fd :off_t new :int))

(defun %string-to-stderr (str)
  (with-cstrs ((s str))
    (fd-write 2 s (length str))))

(defun pdbg (string)
  (%string-to-stderr string)
  (%string-to-stderr #.(string #\LineFeed)))



;;; Not really I/O, but ...
(defun malloc (size)
  (ff-call 
   (%kernel-import target::kernel-import-malloc)
   :unsigned-fullword size :address))

(defun free (ptr)
  (let* ((size (uvsize ptr))
         (flags (if (= size target::xmacptr.size)
                  (uvref ptr target::xmacptr.flags-cell)
                  $flags_DisposPtr)))
    (declare (fixnum size flags))
    (if (= flags $flags_DisposPtr)
      (with-macptrs ((addr ptr))
        (when (= size target::xmacptr.size)
          (%setf-macptr ptr (%null-ptr))
          (setf (uvref ptr target::xmacptr.flags-cell) $flags_Normal))
        (ff-call 
         (%kernel-import target::kernel-import-free)
         :address addr :void)))))




