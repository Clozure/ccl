;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2007, Clozure Associates and contributors
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

;;;; helper functions
(defvar *eucjp-to-ucs-hash* (make-hash-table))
(defvar *ucs-to-eucjp-hash* (make-hash-table))
(defvar *cp932-to-ucs-hash* (make-hash-table))
(defvar *ucs-to-cp932-hash* (make-hash-table))

(dolist (i `((,*cp932-only*
	      ,*cp932-to-ucs-hash*
	      ,*ucs-to-cp932-hash*)
	     (,*eucjp-only*
	      ,*eucjp-to-ucs-hash*
	      ,*ucs-to-eucjp-hash*)
	     (,*eucjp*
	      ,*eucjp-to-ucs-hash*
	      ,*ucs-to-eucjp-hash*)))
  (dolist (j (first i))
    (setf (gethash (car j) (second i)) (cadr j))
    (setf (gethash (cadr j) (third i)) (car j))))

(flet ((euc-cp932 (x)
	 (let ((high (ash x -16))
	       (mid (logand (ash x -8) 255))
	       (low (logand x 255)))
	   (cond ((not (zerop high))
		  nil)
		 ((= mid #x8e)
		  (logand x 255))
		 ((zerop mid)
		  x)
		 ((decf mid #xa1)
		  (decf low #x80)
		  (incf low (if (zerop (logand mid 1)) #x1f #x7e))
		  (incf low (if (<= #x7f low #x9d) 1 0))
		  (setq mid (ash mid -1))
		  (incf mid (if (<= mid #x1e) #x81 #xc1))
		  (+ (ash mid 8) low))))))
  (dolist (i *eucjp*)
    (let ((cp932 (euc-cp932 (first i))))
      (setf (gethash cp932 *cp932-to-ucs-hash*) (second i))
      (setf (gethash (second i) *ucs-to-cp932-hash*) cp932))))

;; ascii
(loop for i from #x00 to #x7f
      do
   (setf (gethash i *cp932-to-ucs-hash*) i)
   (setf (gethash i *eucjp-to-ucs-hash*) i)
   (setf (gethash i *ucs-to-eucjp-hash*) i)
   (setf (gethash i *ucs-to-cp932-hash*) i))

;; half-width katakana
(loop for i from #xa1 to #xdf
      do
   (setf (gethash i *cp932-to-ucs-hash*) (+ #xff61 #x-a1 i))
   (setf (gethash (+ #xff61 #x-a1 i) *ucs-to-cp932-hash*) i)
   (setf (gethash (+ #x8e00 i) *eucjp-to-ucs-hash*) (+ #xff61 #x-a1 i))
   (setf (gethash (+ #xff61 #x-a1 i) *ucs-to-eucjp-hash*) (+ #x8e00 i)))

(defun eucjp-to-ucs (code)
  (values (gethash code *eucjp-to-ucs-hash*)))

(defun ucs-to-eucjp (code)
  (values (gethash code *ucs-to-eucjp-hash*)))

(defun cp932-to-ucs (code)
  (values (gethash code *cp932-to-ucs-hash*)))

(defun ucs-to-cp932 (code)
  (values (gethash code *ucs-to-cp932-hash*)))


(defmacro define-jp-encoding (name document max-units-per-char
                              from-ucs
                              to-ucs
                              length-by-code
                              length-by-1st-unit)
  `(define-character-encoding ,name
       ,document
     :native-endianness nil
     :max-units-per-char ,max-units-per-char
     :stream-encode-function
     (lambda (char write-function stream)
       (let ((code (,from-ucs (char-code char))))
         (cond ((null code)
                (funcall write-function stream #.(char-code #\?))
                1)
               ((< code #x100)
                (funcall write-function stream code)
                1)
               ((< code #x10000)
                (funcall write-function stream (logand #xff (ash code -8)))
                (funcall write-function stream (logand code #xff))
                2)
               (t
                (funcall write-function stream (logand #xff (ash code -16)))
                (funcall write-function stream (logand #xff (ash code -8)))
                (funcall write-function stream (logand code #xff))
                3))))
     :stream-decode-function
     (lambda (1st-unit next-unit-function stream)
       (declare (type (unsigned-byte 8) 1st-unit))
       (let ((code
              (case ,length-by-1st-unit
                (3 (let ((2nd-unit (funcall next-unit-function stream)))
                     (if (eq 2nd-unit :eof)
                         :eof
                         (let ((3rd-unit (funcall next-unit-function stream)))
                           (if (eq 3rd-unit :eof)
                               :eof
                               (logior #x8f0000
                                       (ash 2nd-unit 8)
                                       3rd-unit))))))
                (2 (let ((2nd-unit (funcall next-unit-function stream)))
                     (if (eq 2nd-unit :eof)
                         :eof
                         (logior (ash 1st-unit 8)
                                 2nd-unit))))
                (1 1st-unit))))
         (if (eq code :eof)
             :eof
             (let ((ucs (,to-ucs code)))
               (if ucs
                   (code-char ucs)
                   #\?)))))
     :vector-encode-function
     (lambda (string vector idx start end)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                (fixnum idx))
       (do* ((i start (1+ i)))
            ((>= i end) idx)
         (let* ((char (schar string i))
                (code (,from-ucs (char-code char))))
           (cond ((null code)
                  (setf (aref vector idx) #.(char-code #\?))
                  (incf idx))
                 ((< code #x100)
                  (setf (aref vector idx) code)
                  (incf idx))
                 ((< code #x10000)
                  (setf (aref vector idx) (logand #xff (ash code -8)))
                  (setf (aref vector (the fixnum (1+ idx))) (logand code #xff))
                  (incf idx 2))
                 (t
                  (setf (aref vector idx) (logand #xff (ash code -16)))
                  (setf (aref vector (the fixnum (1+ idx)))
                        (logand #xff (ash code -8)))
                  (setf (aref vector (the fixnum (+ idx 2))) (logand code #xff))
                  (incf idx 3))))))
     :vector-decode-function
     (lambda (vector idx noctets string)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                (type index idx))
       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx (1+ index)))
            ((= index end) index)
         (let* ((1st-unit (aref vector index)))
           (declare (type (unsigned-byte 8) 1st-unit))
           (let* ((code (,to-ucs
                         (case ,length-by-1st-unit
                           (3 (logior
                               #x8f0000
                               (ash (aref vector (incf index)) 8)
                               (aref vector (incf index))))
                           (2 (logior
                               (ash 1st-unit 8)
                               (aref vector (incf index))))
                           (1 1st-unit))))
                  (char (and code (code-char code))))
             (setf (schar string i) (or char #\?))))))
     :memory-encode-function
     (lambda (string pointer idx start end)
       (declare (fixnum idx))
       (do* ((i start (1+ i)))
            ((>= i end) idx)
         (let* ((code (,from-ucs (char-code (schar string i)))))
           (cond ((null code)
                  (setf (%get-unsigned-byte pointer idx) #.(char-code #\?))
                  (incf idx))
                 ((< code #x100)
                  (setf (%get-unsigned-byte pointer idx) code)
                  (incf idx))
                 ((< code #x10000)
                  (setf (%get-unsigned-byte pointer idx)
                        (logand #xff (ash code -8)))
                  (setf (%get-unsigned-byte pointer (the fixnum (1+ idx)))
                        (logand code #xff))
                  (incf idx 2))
                 (t
                  (setf (%get-unsigned-byte pointer idx)
                        (logand #xff (ash code -16)))
                  (setf (%get-unsigned-byte pointer (the fixnum (1+ idx)))
                        (logand #xff (ash code -8)))
                  (setf (%get-unsigned-byte pointer (the fixnum (+ 2 idx)))
                        (logand code #xff))
                  (incf idx 3))))))
     :memory-decode-function
     (lambda (pointer noctets idx string)
       (declare (fixnum noctets idx))
       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx (1+ index)))
            ((>= index end) (if (= index end) index 0))
         (let* ((1st-unit (%get-unsigned-byte pointer index)))
           (declare (type (unsigned-byte 8) 1st-unit))
           (let* ((code
                   (,to-ucs
                    (case ,length-by-1st-unit
                      (3 (logior
                          #x8f0000
                          (ash (%get-unsigned-byte
                                pointer (incf index)) 8)
                          (%get-unsigned-byte pointer (incf index))))
                      (2 (logior
                          (ash 1st-unit 8)
                          (%get-unsigned-byte pointer (incf index))))
                      (1 1st-unit))))
                  (char (if code (code-char code) #\?)))
             (setf (schar string i) char)))))
     :octets-in-string-function
     (lambda (string start end)
       (if (>= end start)
           (do* ((noctets 0)
                 (i start (1+ i)))
                ((= i end) noctets)
             (declare (fixnum noctets))
             (let* ((code (,from-ucs (char-code (schar string i)))))
               (if code
                   (incf noctets ,length-by-code)
                   (incf noctets))))
           0))
     :length-of-vector-encoding-function
     (lambda (vector start end)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector))
       (do* ((i start)
             (nchars 0))
            ((>= i end)
             (values nchars i))
         (declare (fixnum i))
         (let* ((1st-unit (aref vector i))
                (nexti (+ i ,length-by-1st-unit)))
           (declare (type (unsigned-byte 8) 1st-unit))
           (if (> nexti end)
               (return (values nchars i))
               (setq nchars (1+ nchars) i nexti)))))
     :length-of-memory-encoding-function
     (lambda (pointer noctets start)
       (do* ((i start)
             (end (+ start noctets))
             (nchars 0 (1+ nchars)))
            ((= i end) (values nchars (- i start)))
         (let* ((1st-unit (%get-unsigned-byte pointer i))
                (nexti (+ i ,length-by-1st-unit)))
           (declare (type (unsigned-byte 8) 1st-unit))
           (if (> nexti end)
               (return (values nchars (- i start)))
               (setq i nexti)))))
     :decode-literal-code-unit-limit #x80
     :encode-literal-char-code-limit #x80
     :character-size-in-octets-function
     (lambda (c)
       (let ((code (,from-ucs (char-code c))))
         (if code
             ,length-by-code
             1)))))


(define-jp-encoding :eucjp
    "An 8-bit, variable-length character encoding in which
character code points in the range #x00-#x7f can be encoded in a
single octet; characters with larger code values can be encoded
in 2 to 3 bytes."
  3
  ucs-to-eucjp
  eucjp-to-ucs
  (cond ((< code #x100) 1)
        ((< code #x10000) 2)
        (t 3))
  (cond ((= 1st-unit #x8f)
         3)
        ((or (= 1st-unit #x8e)
             (< #xa0 1st-unit #xff))
         2)
        (t 1)))

(define-jp-encoding :cp932
    "An 8-bit, variable-length character encoding in which
character code points in the range #x00-#x7f can be encoded in a
single octet; characters with larger code values can be encoded
in 2 bytes."
  2
  ucs-to-cp932
  cp932-to-ucs
  (cond ((< code #x100) 1)
        (t 2))
  (cond ((or (<= #x81 1st-unit #x9f)
             (<= #xe0 1st-unit #xfc))
         2)
        (t 1)))
