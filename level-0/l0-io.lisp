; -*- Mode: Lisp; Package: CCL; -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

(eval-when (:compile-toplevel)
  #+linuxppc-target
  (require "PPC-LINUX-SYSCALLS")
  #+linuxx8664-target
  (require "X8664-LINUX-SYSCALLS")
  #+darwinppc-target
  (require "DARWINPPC-SYSCALLS")
  #+darwinx8632-target
  (require "DARWINX8632-SYSCALLS")
  #+darwinx8664-target
  (require "DARWINX8664-SYSCALLS")
  #+freebsd-target
  (require "X8664-FREEBSD-SYSCALLS")
  #+solarisx8664-target
  (require "X8664-SOLARIS-SYSCALLS")
  )


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
                   (logand #x3f code))
             (incf idx 4))))))

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
       ((= i end) (values nchars i))
    (let* ((code (%get-unsigned-byte pointer i))
           (nexti (+ i (cond ((< code #x80) 1)
                             ((< code #xe0) 2)
                             ((< code #xf0) 3)
                             (t 4)))))
      (declare (type (unsigned-byte 8) code))
      (if (> nexti end)
        (return (values nchars i))
        (setq i nexti)))))



;;; write nbytes bytes from buffer buf to file-descriptor fd.
(defun fd-write (fd buf nbytes)
  (ignoring-eintr (syscall syscalls::write fd buf nbytes)))

(defun fd-read (fd buf nbytes)
  (ignoring-eintr (syscall syscalls::read fd buf nbytes)))


(defun fd-open (path flags &optional (create-mode #o666))
  (#+darwin-target with-utf-8-cstrs #-darwin-target with-cstrs ((p path))
    (let* ((fd (syscall syscalls::open p flags create-mode)))
      (declare (fixnum fd))
      (when (or (= fd (- #$EMFILE))
                (= fd (- #$EMFILE)))
        (gc)
        (drain-termination-queue)
        (setq fd (syscall syscalls::open p flags create-mode)))
      fd)))

(defun fd-chmod (fd mode)
  (syscall syscalls::fchmod fd mode))

;;; This should really be conditionalized on whether the seek system
;;; call supports 64-bit offsets or on whether one has to use some
;;; variant.
#+(and ppc32-target linux-target)
(defun fd-lseek (fd offset whence)
  (let* ((high (ldb (byte 32 32) offset))
	 (low (ldb (byte 32 0) offset)))
    (declare (type (unsigned-byte 32) high low))
    (%stack-block ((pos 8))
      (let* ((res (syscall syscalls::_llseek fd high low pos whence)))
	(declare (fixnum res))
	(if (< res 0)
	  res
	  (let* ((pos-high (%get-unsigned-long pos 0))
		 (pos-low (%get-unsigned-long pos 4)))
	    (declare (type (unsigned-byte 32) pos-high pos-low))
	    (if (zerop pos-high)
	      pos-low
	      (dpb pos-high (byte 32 32) pos-low))))))))

#-(and ppc32-target linux-target)
(defun fd-lseek (fd offset whence)
  #+freebsd-target
  (syscall syscalls::lseek fd 0 offset whence)
  #-freebsd-target
  (syscall syscalls::lseek fd offset whence))

(defun fd-close (fd)
  (syscall syscalls::close fd)) 

(defun fd-tell (fd)
  (fd-lseek fd 0 #$SEEK_CUR))

;;; Kernels prior to 2.4 don't seem to have a "stat" variant
;;; that handles 64-bit file offsets.
(defun fd-size (fd)
  (without-interrupts
   (let* ((curpos (fd-lseek fd 0 #$SEEK_CUR)))
     (unwind-protect
	  (fd-lseek fd 0 #$SEEK_END)
       (fd-lseek fd curpos #$SEEK_SET)))))

(defun fd-ftruncate (fd new)
  #-solaris-target
  (syscall syscalls::ftruncate fd new)
  #+solaris-target
  (rlet ((lck #>flock))
    (setf (pref lck :flock.l_whence) 0
          (pref lck :flock.l_start) new
          (pref lck :flock.l_type) #$F_WRLCK
          (pref lck :flock.l_len) 0)
    (syscall syscalls::fcntl fd #$F_FREESP lck)))

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




