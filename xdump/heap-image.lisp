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
(defconstant image-sig0 (dpb (char-code #\O)
			     (byte 8 24)
			     (dpb (char-code #\p)
				  (byte 8 16)
				  (dpb (char-code #\e)
				       (byte 8 8)
				       (char-code #\n)))))
(defconstant image-sig1 (dpb (char-code #\M)
			     (byte 8 24)
			     (dpb (char-code #\C)
				  (byte 8 16)
				  (dpb (char-code #\L)
				       (byte 8 8)
				       (char-code #\I)))))
(defconstant image-sig2 (dpb (char-code #\m)
			     (byte 8 24)
			     (dpb (char-code #\a)
				  (byte 8 16)
				  (dpb (char-code #\g)
				       (byte 8 8)
				       (char-code #\e)))))
(defconstant image-sig3 (dpb (char-code #\F)
			     (byte 8 24)
			     (dpb (char-code #\i)
				  (byte 8 16)
				  (dpb (char-code #\l)
				       (byte 8 8)
				       (char-code #\e)))))

#|
(def-foreign-type
    openmcl-image-section-header
    (:struct nil
	     (:code :unsigned-long)
	     (:area (:* t))
	     (:memory-size :unsigned-long)
	     (:static-dnodes :unsigned-long)))
|#

(defun image-write-fullword (w f &optional force-big-endian)
  (cond ((or force-big-endian *xload-target-big-endian*)
         (write-byte (ldb (byte 8 24) w) f)
         (write-byte (ldb (byte 8 16) w) f)
         (write-byte (ldb (byte 8 8) w) f)
         (write-byte (ldb (byte 8 0) w) f))
        (t
         (write-byte (ldb (byte 8 0) w) f)
         (write-byte (ldb (byte 8 8) w) f)
         (write-byte (ldb (byte 8 16) w) f)
         (write-byte (ldb (byte 8 24) w) f))))

(defun image-write-doubleword (dw f)
  (cond (*xload-target-big-endian*
         (image-write-fullword (ldb (byte 32 32) dw) f)
         (image-write-fullword (ldb (byte 32 0) dw) f))
        (t
         (image-write-fullword (ldb (byte 32 0) dw) f)
         (image-write-fullword (ldb (byte 32 32) dw) f))))

(defun image-write-natural (n f)
  (target-word-size-case
   (32 (image-write-fullword n f))
   (64 (image-write-doubleword n f))))

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
         (image-section-size (* 4 (target-word-size-case (32 4) (64 8)))))
    (with-open-file (f pathname
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
      (let* ((nsections (length spaces))
             (header-pos (- 4096 (+ image-header-size
                                    (* nsections image-section-size)))))
        (file-position f header-pos)
        (image-write-fullword image-sig0 f)
        (image-write-fullword image-sig1 f)
        (image-write-fullword image-sig2 f)
        (image-write-fullword image-sig3 f)
        (image-write-fullword (get-universal-time) f) ;; FIXME: Year 2038 problem.
        (image-write-fullword (target-word-size-case
                               (32 *xload-image-base-address*)
                               (64 0)) f)
        (image-write-fullword (target-word-size-case
                               (32 image-base)
                               (64 0)) f)
        (image-write-fullword nsections f)
        (image-write-fullword abi-version f)
        (target-word-size-case
         (32
          (dotimes (i 2) (image-write-fullword 0 f))
          (image-write-fullword (backend-target-platform *target-backend*) f)
          (dotimes (i 4) (image-write-fullword 0 f)))
         (64
          (image-write-fullword 0 f)
          (image-write-fullword 0 f)
          (image-write-fullword (backend-target-platform *target-backend*) f)
          (image-write-doubleword *xload-image-base-address* f)
          (image-write-doubleword image-base f)))
        (dolist (sect spaces)
          (image-write-natural (ash (xload-space-code sect)
                                    *xload-target-fixnumshift*)
                               f)
          (image-write-natural 0 f)
          (let* ((size (xload-space-lowptr sect)))
            (image-write-natural size f)
            (image-write-natural 0 f))) ; static dnodes.
        (dolist (sect spaces)
          (image-align-output-position f)
          (stream-write-ivector f
                                (xload-space-data sect)
                                0
                                (xload-space-lowptr sect)))
        ;; Write an openmcl_image_file_trailer.
        (image-write-fullword image-sig0 f)
        (image-write-fullword image-sig1 f)
        (image-write-fullword image-sig2 f)
        (let* ((pos (+ 4 (file-position f))))
          (image-write-fullword (- header-pos pos) f))
        nil))))
