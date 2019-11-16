;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 2002-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

(defun image-write-u32 (w f)
  (cond (*xload-target-big-endian*
         (write-byte (ldb (byte 8 24) w) f)
         (write-byte (ldb (byte 8 16) w) f)
         (write-byte (ldb (byte 8 8) w) f)
         (write-byte (ldb (byte 8 0) w) f))
        (t
         (write-byte (ldb (byte 8 0) w) f)
         (write-byte (ldb (byte 8 8) w) f)
         (write-byte (ldb (byte 8 16) w) f)
         (write-byte (ldb (byte 8 24) w) f))))

(defun image-write-u64 (dw f)
  (cond (*xload-target-big-endian*
         (image-write-u32 (ldb (byte 32 32) dw) f)
         (image-write-u32 (ldb (byte 32 0) dw) f))
        (t
         (image-write-u32 (ldb (byte 32 0) dw) f)
         (image-write-u32 (ldb (byte 32 32) dw) f))))

(defun image-write-natural (n f)
  (target-word-size-case
   (32 (image-write-u32 n f))
   (64 (image-write-u64 n f))))

(defparameter image-file-header
  "OpenMCLImageFile")

(defun image-write-string (file string count)
  (loop for (char-a char-b char-c char-d) on (coerce string 'list) by #'cddddr
        for i from 0 below count by 4
        for byte = (dpb (char-code char-a)
			                   (byte 8 24)
			                   (dpb (char-code char-b)
				                      (byte 8 16)
				                      (dpb (char-code char-c)
				                           (byte 8 8)
				                           (char-code char-d))))
        do (image-write-u32 byte file)))

(defun image-write-file-header (file)
  (image-write-string file image-file-header 16))

(defun image-write-file-trailer (file header-pos)
  (image-write-string file image-file-header 12)
  (let* ((pos (+ 4 (file-position file))))
    (image-write-u32 (- header-pos pos) file)))

(defun image-align-output-position (f)
  (file-position f (logand (lognot 4095)
			   (+ 4095 (file-position f)))))

(defun target-image-abi-version ()
  (let* ((pkg (pkg-arg "TARGET"))
         (sym (find-symbol "*IMAGE-ABI-VERSION*" pkg)))
    (or (and sym (boundp sym) (symbol-value sym))
        (error "*IMAGE-ABI-VERSION* not defined in ~s" pkg))))

(defun write-image-file (pathname image-base spaces)
  (let* ((abi-version (target-image-abi-version))
         (image-header-size (* 4 16))
         (image-section-size (* 4 (target-word-size-case (32 4) (64 8))))
         (nsections (length spaces))
         (header-pos (- 4096 (+ image-header-size (* nsections image-section-size)))))
    (with-open-file (f pathname
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
      (flet ((write-u32 (x) (image-write-u32 x f))
             (write-u64 (x) (image-write-u64 x f))
             (write-nat (x) (image-write-natural x f)))
        (file-position f header-pos)
        (image-write-file-header f)
        (write-u32 (get-universal-time)) ;; FIXME: Year 2038 problem.
        (write-u32 (target-word-size-case
                    (32 *xload-image-base-address*)
                    (64 0)))
        (write-u32 (target-word-size-case
                    (32 image-base)
                    (64 0)))
        (write-u32 nsections)
        (write-u32 abi-version)
        (write-u32 0)
        (write-u32 0)
        (write-u32 (backend-target-platform *target-backend*))
        (write-u64 (target-word-size-case
                    (32 0)
                    (64 *xload-image-base-address*)))
        (write-u64 (target-word-size-case
                    (32 0)
                    (64 image-base)))
        (dolist (sect spaces)
          (write-nat (ash (xload-space-code sect)
                          *xload-target-fixnumshift*))
          (write-nat 0)
          (let* ((size (xload-space-lowptr sect)))
            (write-nat size)
            (write-nat 0))) ; static dnodes.
        (dolist (sect spaces)
          (image-align-output-position f)
          (stream-write-ivector f
                                (xload-space-data sect)
                                0
                                (xload-space-lowptr sect)))
        (image-write-file-trailer f header-pos)
        nil))))
