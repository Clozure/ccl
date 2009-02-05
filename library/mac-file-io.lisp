;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Opensourced MCL.
;;;
;;;   Opensourced MCL is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Lesser General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2.1 of the License, or (at your option) any later version.
;;;
;;;   Opensourced MCL is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;   Lesser General Public License for more details.
;;;
;;;   You should have received a copy of the GNU Lesser General Public
;;;   License along with this library; if not, write to the Free Software
;;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mac-file-io.lisp
;;

;; This file implements something similar to the high-level file I/O
;; primitives in Inside Macintosh.
;; It does NOT support asynchronous I/O (and neither does the Macintosh, really).

;; Routines that take an errorp parameter will signal an error if
;; the parameter is unspecified or true, otherwise, if there is an
;; error they return two values: NIL & the error number.
;; If there is no error, routines return one or more values the
;; first of which is non-NIL.

;;;;;;;;;;;;;
;;
;; Modification History
;;

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require 'sysequ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(with-FSOpen-file FSOpen FSClose FSRead FSWrite setFPos getFPos getEOF)))

(defmacro with-FSOpen-file ((pb filename &optional read-write-p (vrefnum 0))
                            &body body)
  `(let ((,pb (FSOpen ,filename ,read-write-p ,vrefnum)))
     (unwind-protect
       (progn ,@body)
       (FSClose ,pb))))

(defmacro with-FSOpen-file-noerr ((pb filename &optional read-write-p (vrefnum 0))
                                  &body body)
  `(let ((,pb (ignore-errors
               (FSOpen ,filename ,read-write-p ,vrefnum))))
     (when ,pb
       (unwind-protect
         (progn ,@body)
         (FSClose ,pb)))))

; Returns a paramBlock for doing furthur I/O with the file
(defun FSOpen (filename &optional read-write-p (vrefnum 0) (errorp t)
                        (resolve-aliases-p t))
  (when resolve-aliases-p (setq filename (truename filename)))
  (let ((paramBlock (make-record :hparamblockrec))
        ok)
    (unwind-protect
      (with-pstrs ((pname (mac-namestring filename)))
        (setf (pref paramblock :hparamblockrec.ioNameptr) pname
              (pref paramblock :hparamblockrec.ioVrefnum) vrefnum
              (pref paramblock :hparamblockrec.ioVersNum) 0
              (pref paramblock :hparamblockrec.ioPermssn) (if read-write-p #$fsRdWrPerm #$fsRdPerm)
              (pref paramblock :hparamblockrec.ioMisc) (%null-ptr))
        (#_PBOpenSync paramBlock)
        (let ((res (pref paramBlock :hparamblockrec.ioResult)))
          (if (eql #$NoErr res)
            (progn
              (setf (pref paramblock :hparamblockrec.ioPosOffSet) 0
                    (pref paramblock :hparamblockrec.ioPosMode) #$fsAtMark)
              (setq ok t)
              paramBlock)
            (maybe-file-error errorp res filename))))
      (unless ok
        (#_DisposePtr paramBlock)))))

(defun FSClose (paramBlock &optional (errorp t))
  (#_PBCloseSync paramBlock)
  (let ((errnum (pref paramBlock :hparamblockrec.ioResult)))
    (#_DisposePtr paramBlock)
    (or (eql errnum #$noErr)
        (maybe-file-error errorp errnum))))

; Returns two values: the number of bytes actually read, and the
; location of the file mark.
(defun fsRead (paramBlock count buffer &optional (offset 0) (errorp t))
  (setf (pref paramBlock :hparamblockrec.ioBuffer) (%inc-ptr buffer offset)
        (pref paramBlock :hparamblockrec.ioReqCount) count)
  (#_PBReadSync paramBlock)
  (setf (pref paramBlock :hparamblockrec.ioPosMode) #$fsAtMark)
  (let ((errnum (pref paramBlock :hparamblockrec.ioResult)))
    (if (or (eql #$noErr errnum) (eql #$eofErr errnum))
      (values (pref paramBlock :hparamblockrec.ioActCount)
              (pref paramBlock :hparamblockrec.ioPosOffset))
      (maybe-file-error errorp errnum))))

; Returns two values: the number of bytes actually written, and the
; location of the file mark.
(defun fsWrite (paramBlock count buffer &optional (offset 0) (errorp t))
  (setf (pref paramBlock :hparamblockrec.ioBuffer) (%inc-ptr buffer offset)
        (pref paramBlock :hparamblockrec.ioReqCount) count)
  (#_PBWriteSync paramBlock)
  (setf (pref paramBlock :hparamblockrec.ioPosMode) #$fsAtMark)
  (let ((errnum (pref paramBlock :hparamblockrec.ioResult)))
    (if (or (eql #$noErr errnum) (eql #$eofErr errnum))
      (values (pref paramBlock :hparamblockrec.ioActCount)
              (pref paramBlock :hparamblockrec.ioPosOffset))
      (maybe-file-error errorp errnum))))

(defun setFPos (paramBlock pos)
  (setf (pref paramBlock :hparamblockrec.ioPosOffset) pos
        (pref paramblock :hparamblockrec.ioPosMode) #$fsFromStart)
  pos)

(defun getFPos (paramBlock)
  (pref paramBlock :hparamblockrec.ioPosOffset))

(defun getEOF (paramBlock &optional (errorp t))
  (let* ((errnum (#_PBGetEOFSync paramBlock)))
    (if (eql #$noErr errnum)
      (%ptr-to-int (pref paramblock :hparamblockrec.ioMisc))
      (maybe-file-error errorp errnum))))

(defun GetVInfo (&key (volName "") (vRefNum 0))
  (let* ((vol-pathname (truename (make-pathname :type nil :name nil :defaults volName)))
         (directory    (pathname-directory vol-pathname)))
    (assert (and directory (eq :absolute (car directory))))
    (rlet ((paramBlock :hparamblockrec))
      (with-returned-pstrs ((pname (cadr directory)))
        (setf (pref paramblock :hparamblockrec.ioCompletion) (%null-ptr)
              (pref paramblock :hparamblockrec.ioNamePtr)    pname
              (pref paramblock :hparamblockrec.ioVRefNum)    vRefNum
              (pref paramblock :hparamblockrec.ioVolIndex)   0)
        (values (#_PBHGetVInfoSync paramBlock)
                (* (%get-unsigned-long paramblock $ioVAlBlkSiz)         ; see IM:Files 2-46
                   (pref paramblock :hparamblockrec.ioVFrBlk))
                (pref paramblock :hparamblockrec.ioVRefNum)
                (%get-string (pref paramblock :hparamblockrec.ioNamePtr)))))))

(defun maybe-file-error (errorp errnum &optional filename)
  (if errorp
    (%err-disp errnum filename)
    (values nil errnum)))

(provide :mac-file-io)

; End of mac-file-io.lisp
