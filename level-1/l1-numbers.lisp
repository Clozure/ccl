;;-*-Mode: LISP; Package: CCL -*-
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

(eval-when (:compile-toplevel :execute)
  (require "NUMBER-MACROS")
)

(defun %parse-number-token (string &optional start end radix)
  (if end (require-type end 'fixnum)(setq end (length string)))
  (if start (require-type start 'fixnum)(setq start 0))
  (multiple-value-bind (string offset)(array-data-and-offset string)
    (new-numtoken string (+ start offset)(- end start) (%validate-radix (or radix 10)))))

(defun new-numtoken (string start len radix &optional no-rat no-sign)
  (declare (fixnum start len radix))
  (if (eq 0 len)
    nil
    (let ((c (%scharcode string start))
          (nstart start)
          (end (+ start len))
          (hic (if (<= radix 10)
                 (+ (char-code #\0) (1- radix))
                 (+ (char-code #\A) (- radix 11))))
          dot dec dgt)
      (declare (fixnum nstart end hic))
      (when (or (eq c (char-code #\+))(eq c (char-code #\-)))
        (if no-sign
          (return-from new-numtoken nil)
          (setq nstart (1+ nstart))))
      (when (eq nstart end)(return-from new-numtoken nil)) ; just a sign
      (do ((i nstart (1+ i)))
          ((eq i end))
        (let ()
          (setq c (%scharcode string i))
          (cond
           ((eq c (char-code #\.))
            (when dot (return-from new-numtoken nil))
            (setq dot t)
            (when dec (return-from new-numtoken nil))
            (setq hic (char-code #\9)))
           ((< c (char-code #\0)) 
            (when (and (eq c (char-code #\/))(not dot)(not no-rat))
              (let ((top (new-numtoken string start (- i start) radix)))
                (when top 
                  (let ((bottom (new-numtoken string (+ start i 1) (- len i 1) radix t t)))
                    (when bottom 
                      (return-from new-numtoken (/ top bottom)))))))
            (return-from new-numtoken nil))
           ((<= c (char-code #\9))
            (when (> c hic)
              ; seen a decimal digit above base.
              (setq dgt t)))
           (t (when (>= c (char-code #\a))(setq c (- c 32)))
              ;; don't care about *read-base* if float
              (cond ((or (< c (char-code #\A))(> c hic))
                     (when (and (neq i nstart) ; need some digits first
                                (memq c '#.(list (char-code #\E)(char-code #\F)
                                                 (char-code #\D)(char-code #\L)
                                                 (char-code #\S))))
                       (return-from new-numtoken (parse-float string len start)))
                     (return-from new-numtoken nil))
                    (t     ; seen a "digit" in base that ain't decimal
                     (setq dec t)))))))
      (when (and dot (or (and (neq nstart start)(eq len 2))
                         (eq len 1)))  ;. +. or -.
        (return-from new-numtoken nil))
      (when dot 
        (if (eq c (char-code #\.))
          (progn (setq len (1- len) end (1- end))
                 (when dec (return-from new-numtoken nil))
                 ; make #o9. work (should it)
                 (setq radix 10 dgt nil))
          (return-from new-numtoken (parse-float string len start))))
      (when dgt (return-from new-numtoken nil)) ; so why didnt we quit at first sight of it?
      ; and we ought to accumulate as we go until she gets too big - maybe
      (cond (nil ;(or (and (eq radix 10)(< (- end nstart) 9))(and (eq radix 8)(< (- end nstart) 10)))
             (let ((num 0))
               (declare (fixnum num))
               (do ((i nstart (1+ i)))
                   ((eq i end))
                 (setq num (%i+ (%i* num radix)(%i- (%scharcode string i) (char-code #\0)))))
               (if (eq (%scharcode string start) (char-code #\-)) (setq num (- num)))
               num))                         
            (t (token2int string start len radix))))))


;; Will Clingers number 1.448997445238699
;; Doug Curries numbers 214748.3646, 1073741823/5000
;; My number: 12.
;; Your number:





(defun logand (&lexpr numbers)
  "Return the bit-wise and of its arguments. Args must be integers."
  (let* ((count (%lexpr-count numbers)))
    (declare (fixnum count))
    (if (zerop count)
      -1
      (let* ((n0 (%lisp-word-ref numbers count)))
        (if (= count 1)
          (require-type n0 'integer)
          (do* ((i 1 (1+ i)))
               ((= i count) n0)
            (declare (fixnum i))
            (declare (optimize (speed 3) (safety 0)))
            (setq n0 (logand (%lexpr-ref numbers count i) n0))))))))


(defun logior (&lexpr numbers)
  "Return the bit-wise or of its arguments. Args must be integers."
  (let* ((count (%lexpr-count numbers)))
    (declare (fixnum count))
    (if (zerop count)
      0
      (let* ((n0 (%lisp-word-ref numbers count)))
        (if (= count 1)
          (require-type n0 'integer)
          (do* ((i 1 (1+ i)))
               ((= i count) n0)
            (declare (fixnum i))
            (declare (optimize (speed 3) (safety 0)))
            (setq n0 (logior (%lexpr-ref numbers count i) n0))))))))

(defun logxor (&lexpr numbers)
  "Return the bit-wise exclusive or of its arguments. Args must be integers."
  (let* ((count (%lexpr-count numbers)))
    (declare (fixnum count))
    (if (zerop count)
      0
      (let* ((n0 (%lisp-word-ref numbers count)))
        (if (= count 1)
          (require-type n0 'integer)
          (do* ((i 1 (1+ i)))
               ((= i count) n0)
            (declare (fixnum i))
            (declare (optimize (speed 3) (safety 0)))
            (setq n0 (logxor (%lexpr-ref numbers count i) n0))))))))

(defun logeqv (&lexpr numbers)
  "Return the bit-wise equivalence of its arguments. Args must be integers."
  (let* ((count (%lexpr-count numbers))
         (result (if (zerop count)
                   0
                   (let* ((n0 (%lisp-word-ref numbers count)))
                     (if (= count 1)
                       (require-type n0 'integer)
                       (do* ((i 1 (1+ i)))
                            ((= i count) n0)
                         (declare (fixnum i))
                         (declare (optimize (speed 3) (safety 0)))
                         (setq n0 (logxor (%lexpr-ref numbers count i) n0))))))))
    (declare (fixnum count))
    (if (evenp count)
      (lognot result)
      result)))




(defun = (num &lexpr more)
  "Return T if all of its arguments are numerically equal, NIL otherwise."
  (let* ((count (%lexpr-count more)))
    (declare (fixnum count))
    (if (zerop count)
      (progn
        (require-type num 'number)
        t)
      (dotimes (i count t)
        (unless (=-2 (%lexpr-ref more count i) num) (return))))))

(defun /= (num &lexpr more)
  "Return T if no two of its arguments are numerically equal, NIL otherwise."
  (let* ((count (%lexpr-count more)))
    (declare (fixnum count))
    (if (zerop count)
      (progn
        (require-type num 'number)
        t)
      (dotimes (i count t)
        (declare (fixnum i))
        (do ((j i (1+ j)))
            ((= j count))
          (declare (fixnum j))
          (when (=-2 num (%lexpr-ref more count j))
            (return-from /= nil)))
        (setq num (%lexpr-ref more count i))))))

(defun - (num &lexpr more)
  "Subtract the second and all subsequent arguments from the first; 
  or with one argument, negate the first argument."
  (let* ((count (%lexpr-count more)))
    (declare (fixnum count))
    (if (zerop count)
      (- num)
      (dotimes (i count num)
        (setq num (--2 num (%lexpr-ref more count i)))))))

(defun / (num &lexpr more)
  "Divide the first argument by each of the following arguments, in turn.
  With one argument, return reciprocal."
  (let* ((count (%lexpr-count more)))
    (declare (fixnum count))
    (if (zerop count)
      (%quo-1 num)
      (dotimes (i count num)
        (setq num (/-2 num (%lexpr-ref more count i)))))))

(defun + (&lexpr numbers)
  "Return the sum of its arguments. With no args, returns 0."
  (let* ((count (%lexpr-count numbers)))
    (declare (fixnum count))
    (if (zerop count)
      0
      (let* ((n0 (%lisp-word-ref numbers count)))
        (if (= count 1)
          (require-type n0 'number)
          (do* ((i 1 (1+ i)))
               ((= i count) n0)
            (declare (fixnum i))
            (setq n0 (+-2 (%lexpr-ref numbers count i) n0))))))))



(defun * (&lexpr numbers)
  "Return the product of its arguments. With no args, returns 1."
  (let* ((count (%lexpr-count numbers)))
    (declare (fixnum count))
    (if (zerop count)
      1
      (let* ((n0 (%lisp-word-ref numbers count)))
        (if (= count 1)
          (require-type n0 'number)
          (do* ((i 1 (1+ i)))
               ((= i count) n0)
            (declare (fixnum i))
            (declare (optimize (speed 3) (safety 0)))
            (setq n0 (*-2 (%lexpr-ref numbers count i) n0))))))))


(defun < (num &lexpr more)
  "Return T if its arguments are in strictly increasing order, NIL otherwise."
  (let* ((count (%lexpr-count more)))
    (declare (fixnum count))
    (if (zerop count)
      (progn
        (require-type num 'real)
        t)
      (dotimes (i count t)
        (declare (optimize (speed 3) (safety 0)))
        (unless (< num (setq num (%lexpr-ref more count i)))
          (return))))))

(defun <= (num &lexpr more)
  "Return T if arguments are in strictly non-decreasing order, NIL otherwise."
  (let* ((count (%lexpr-count more)))
    (declare (fixnum count))
    (if (zerop count)
      (progn
        (require-type num 'real)
        t)
      (dotimes (i count t)
        (declare (optimize (speed 3) (safety 0)))
        (unless (<= num (setq num (%lexpr-ref more count i)))
          (return))))))


(defun > (num &lexpr more)
  "Return T if its arguments are in strictly decreasing order, NIL otherwise."
  (let* ((count (%lexpr-count more)))
    (declare (fixnum count))
    (if (zerop count)
      (progn
        (require-type num 'real)
        t)
      (dotimes (i count t)
        (declare (optimize (speed 3) (safety 0)))
        (unless (> num (setq num (%lexpr-ref more count i)))
          (return))))))

(defun >= (num &lexpr more)
  "Return T if arguments are in strictly non-increasing order, NIL otherwise."
  (let* ((count (%lexpr-count more)))
    (declare (fixnum count))
    (if (zerop count)
      (progn
        (require-type num 'real)
        t)
      (dotimes (i count t)
        (declare (optimize (speed 3) (safety 0)))
        (unless (>= num (setq num (%lexpr-ref more count i)))
          (return))))))

(defun max-2 (n0 n1)
  (if (> n0 n1) n0 n1))

(defun max (num &lexpr more)
  "Return the greatest of its arguments; among EQUALP greatest, return
   the first."
  (let* ((count (%lexpr-count more)))
    (declare (fixnum count))
    (if (zerop count)
      (require-type num 'real)
      (dotimes (i count num)
        (declare (optimize (speed 3) (safety 0)))
        (setq num (max-2 (%lexpr-ref more count i) num))))))

(defun min-2 (n0 n1)
  (if (< n0 n1) n0 n1))

(defun min (num &lexpr more)
  "Return the least of its arguments; among EQUALP least, return
  the first."
  (let* ((count (%lexpr-count more)))
    (declare (fixnum count))
    (if (zerop count)
      (require-type num 'real)
      (dotimes (i count num)
        (declare (optimize (speed 3) (safety 0)))
        (setq num (min-2 (%lexpr-ref more count i) num))))))
 


;Not CL. Used by transforms.
(defun deposit-byte (value size position integer)
  (let ((mask (byte-mask size)))
    (logior (ash (logand value mask) position)
            (logandc1 (ash mask position) integer))))

(defun deposit-field (value bytespec integer)
  "Return new integer with newbyte in specified position, newbyte is not right justified."
  (if (> bytespec 0)    
    (logior (logandc1 bytespec integer) (logand bytespec value))
    (progn
      (require-type value 'integer)
      (require-type integer 'integer))))

;;;;;;;;;;  Byte field functions ;;;;;;;;;;;;;;;;

;;; Size = 0, position = 0 -> 0
;;; size = 0, position > 0 -> -position
;;; else ->  (ash (byte-mask size) position)
(defun byte (size position)
  "Return a byte specifier which may be used by other byte functions
  (e.g. LDB)."
  (unless (and (typep size 'integer)
	       (>= size 0))
    (report-bad-arg size 'unsigned-byte))
  (unless (and (typep position 'integer)
	       (>= position 0))
    (report-bad-arg position 'unsigned-byte))
  (if (eql 0 size)
    (if (eql 0 position)
      0
      (- position))
    (ash (byte-mask size) position)))



(defun byte-size (bytespec)
  "Return the size part of the byte specifier bytespec."
  (if (> bytespec 0)
    (logcount bytespec)
    0))

(defun ldb (bytespec integer)
  "Extract the specified byte from integer, and right justify result."
  (if (and (fixnump bytespec) (> (the fixnum bytespec) 0)  (fixnump integer))
    (%ilsr (byte-position bytespec) (%ilogand bytespec integer))
    (let ((size (byte-size bytespec))
          (position (byte-position bytespec)))
      (if (eql size 0)
	(progn
	  (require-type integer 'integer)
	  0)
	(if (and (bignump integer)
		 (<= size  (- (1- target::nbits-in-word)  target::fixnumshift))
		 (fixnump position))
          (%ldb-fixnum-from-bignum integer size position)
          (ash (logand bytespec integer) (- position)))))))

(defun mask-field (bytespec integer)
  "Extract the specified byte from integer, but do not right justify result."
  (if (>= bytespec 0)
    (logand bytespec integer)
    (logand integer 0)))

(defun dpb (value bytespec integer)
  "Return new integer with newbyte in specified position, newbyte is right justified."
  (if (and (fixnump value)
	   (fixnump bytespec)
	   (> (the fixnum bytespec) 0)
	   (fixnump integer))
    (%ilogior (%ilogand bytespec (%ilsl (byte-position bytespec) value))
              (%ilogand (%ilognot bytespec) integer))
    (deposit-field (ash value (byte-position bytespec)) bytespec integer)))

(defun ldb-test (bytespec integer)
  "Return T if any of the specified bits in integer are 1's."
  (if (> bytespec 0)
    (logtest bytespec integer)
    (progn
      (require-type integer 'integer)
      nil)))

(defun %cons-random-state (seed-1 seed-2)
  #+32-bit-target
  (%istruct 'random-state seed-1 seed-2)
  #+64-bit-target
  (%istruct 'random-state (the fixnum (+ (the fixnum seed-2)
                          (the fixnum (ash (the fixnum seed-1) 16))))))

;;; random associated stuff except for the print-object method which
;;; is still in "lib;numbers.lisp"
(defun initialize-random-state (seed-1 seed-2)
  (unless (and (fixnump seed-1) (%i<= 0 seed-1) (%i< seed-1 #x10000))
    (report-bad-arg seed-1 '(unsigned-byte 16)))
  (unless (and (fixnump seed-2) (%i<= 0 seed-2) (%i< seed-2 #x10000))
    (report-bad-arg seed-2 '(unsigned-byte 16)))
    (%cons-random-state seed-1 seed-2))

(defun make-random-state (&optional state)
  "Make a random state object. If STATE is not supplied, return a copy
  of the default random state. If STATE is a random state, then return a
  copy of it. If STATE is T then return a random state generated from
  the universal time."
  (let* ((seed-1 0)
         (seed-2 0))
    (if (eq state t)
      (multiple-value-setq (seed-1 seed-2) (init-random-state-seeds))
      (progn
        (setq state (require-type (or state *random-state*) 'random-state))
        #+32-bit-target
        (setq seed-1 (random.seed-1 state) seed-2 (random.seed-2 state))
        #+64-bit-target
        (let* ((seed (random.seed-1 state)))
          (declare (type (unsigned-byte 32) seed))
          (setq seed-1 (ldb (byte 16 16) seed)
                seed-2 (ldb (byte 16 0) seed)))))
    (%cons-random-state seed-1 seed-2)))

(defun random-state-p (thing) (istruct-typep thing 'random-state))

;;; transcendental stuff.  Should go in level-0;l0-float
;;; but shleps don't work in level-0.  Or do they ?
; Destructively set z to x^y and return z.
(defun %double-float-expt! (b e result)
  (declare (double-float b e result))
  (with-stack-double-floats ((temp))
    (%setf-double-float temp (#_pow b e))
    (%df-check-exception-2 'expt b e (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-expt! (b e result)
  (declare (single-float b e result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float temp (#_powf b e))
    (%sf-check-exception-2 'expt b e (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-expt (b e)
  (declare (single-float b e))
  (let* ((result (#_powf b e)))
    (%sf-check-exception-2 'expt b e (%ffi-exception-status))
    result))

(defun %double-float-sin! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_sin n))
    (%df-check-exception-1 'sin n (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-sin! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_sinf n))
    (%sf-check-exception-1 'sin n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-sin (n)
  (declare (single-float n))
  (let* ((result (#_sinf n)))
    (%sf-check-exception-1 'sin n (%ffi-exception-status))
    result))

(defun %double-float-cos! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_cos n))
    (%df-check-exception-1 'cos n (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-cos! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_cosf n))
    (%sf-check-exception-1 'cos n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-cos (n)
  (declare (single-float n))
  (let* ((result (#_cosf n)))
    (%sf-check-exception-1 'cos n (%ffi-exception-status))
    result))

(defun %double-float-acos! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_acos n))
    (%df-check-exception-1 'acos n (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-acos! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_acosf n))
    (%sf-check-exception-1 'acos n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-acos (n)
  (declare (single-float n))
  (let* ((result (#_acosf n)))
    (%sf-check-exception-1 'acos n (%ffi-exception-status))
    result))

(defun %double-float-asin! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_asin n))
    (%df-check-exception-1 'asin n (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-asin! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_asinf n))
    (%sf-check-exception-1 'asin n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-asin (n)
  (declare (single-float n))
  (let* ((result (#_asinf n)))
    (%sf-check-exception-1 'asin n (%ffi-exception-status))
    result))

(defun %double-float-cosh! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_cosh n))
    (%df-check-exception-1 'cosh n (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-cosh! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_coshf n))
    (%sf-check-exception-1 'cosh n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-cosh (n)
  (declare (single-float n))
  (let* ((result (#_coshf n)))
    (%sf-check-exception-1 'cosh n (%ffi-exception-status))
    result))

(defun %double-float-log! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_log n))
    (%df-check-exception-1 'log n (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-log! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_logf n))
    (%sf-check-exception-1 'log n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-log (n)
  (let* ((result (#_logf n)))
    (%sf-check-exception-1 'log n (%ffi-exception-status))
    result))

(defun %double-float-tan! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_tan n))
    (%df-check-exception-1 'tan n (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-tan! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_tanf n))
    (%sf-check-exception-1 'tan n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-tan (n)
  (declare (single-float n))
  (let* ((result (#_tanf n)))
    (%sf-check-exception-1 'tan n (%ffi-exception-status))
    result))

(defun %double-float-atan! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_atan n))
    (%df-check-exception-1 'atan n (%ffi-exception-status))
    (%setf-double-float result TEMP)))


#+32-bit-target
(defun %single-float-atan! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_atanf n))
    (%sf-check-exception-1 'atan n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-atan (n)
  (declare (single-float n))
  (let* ((temp (#_atanf n)))
    (%sf-check-exception-1 'atan n (%ffi-exception-status))
    temp))

(defun %double-float-atan2! (x y result)
  (declare (double-float x y result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_atan2 x y))
    (%df-check-exception-2 'atan2 x y (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-atan2! (x y result)
  (declare (single-float x y result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_atan2f x y))
    (%sf-check-exception-2 'atan2 x y (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-atan2 (x y)
  (declare (single-float x y))
  (let* ((result (#_atan2f x y)))
    (%sf-check-exception-2 'atan2 x y (%ffi-exception-status))
    result))

(defun %double-float-exp! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_exp n))
    (%df-check-exception-1 'exp n (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-exp! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_expf n))
    (%sf-check-exception-1 'exp n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-exp (n)
  (declare (single-float n))
  (let* ((result (#_expf n)))
    (%sf-check-exception-1 'exp n (%ffi-exception-status))
    result))

(defun %double-float-sinh! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_sinh n))
    (%df-check-exception-1 'sinh n (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-sinh! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_sinhf n))
    (%sf-check-exception-1 'sinh n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-sinh (n)
  (declare (single-float n))
  (let* ((result (#_sinhf n)))
    (%sf-check-exception-1 'sinh n (%ffi-exception-status))
    result))

(defun %double-float-tanh! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_tanh n))
    (%df-check-exception-1 'tanh n (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-tanh! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_tanhf n))
    (%sf-check-exception-1 'tanh n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-tanh (n)
  (declare (single-float n))
  (let* ((result (#_tanhf n)))
    (%sf-check-exception-1 'tanh n (%ffi-exception-status))
    result))

#+windows-target
(progn
(defun %double-float-asinh! (n result)
  (%setf-double-float
   result
   (log (+ n (sqrt (1+ (* n n)))))))

#+32-bit-target
(defun %single-float-asinh! (n result)
  (%setf-short-float
   result
   (log (+ n (sqrt (1+ (* n n)))))))

#+64-bit-target
(defun %single-float-asinh (n)
  (log (+ n (sqrt (1+ (* n n)))))))

#-windows-target
(progn
(defun %double-float-asinh! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_asinh n))
    (%df-check-exception-1 'asinh n (%ffi-exception-status))
    (%setf-double-float result TEMP)))


#+32-bit-target
(defun %single-float-asinh! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_asinhf n))
    (%sf-check-exception-1 'asinh n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-asinh (n)
  (declare (single-float n))
  (let* ((result (#_asinhf n)))
    (%sf-check-exception-1 'asinh n (%ffi-exception-status))
    result))
)

#+windows-target
(progn
(defun %double-float-acosh! (n result)
  (%setf-double-float
   result
   (* 2 (log (+ (sqrt (/ (+ n 1) 2)) (sqrt (/ (- n 1) 2)))))))

#+32-bit-target
(defun %single-float-acosh! (n result)
  (%setf-short-float
   result
   (* 2 (log (+ (sqrt (/ (+ n 1) 2)) (sqrt (/ (- n 1) 2)))))))

#+64-bit-target
(defun %single-float-acosh (n)
  (* 2 (log (+ (sqrt (/ (+ n 1) 2)) (sqrt (/ (- n 1) 2))))))

)

#-windows-target
(progn
(defun %double-float-acosh! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_acosh n))
    (%df-check-exception-1 'acosh n (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-acosh! (n result)
  (declare (single-float n result))
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_acoshf n))
    (%sf-check-exception-1 'acosh n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-acosh (n)
  (declare (single-float n))
  (let* ((result (#_acoshf n)))
    (%sf-check-exception-1 'acosh n (%ffi-exception-status))
    result))
)

#+windows-target
(progn
(defun %double-float-atanh! (n result)
  (%setf-double-float
   result
   (/ (- (log (1+ n))
         (log (- 1 n)))
      2)))

#+32-bit-target
(defun %single-float-atanh! (n result)
  (%setf-short-float
   result
   (/ (- (log (1+ n))
         (log (- 1 n)))
      2)))

#+64-bit-target
(defun %single-float-atanh (n)
  (/ (- (log (1+ n))
        (log (- 1 n)))
     2))

)

#-windows-target
(progn
(defun %double-float-atanh! (n result)
  (declare (double-float n result))
  (with-stack-double-floats ((temp))
    (%setf-double-float TEMP (#_atanh n))
    (%df-check-exception-1 'atanh n (%ffi-exception-status))
    (%setf-double-float result TEMP)))

#+32-bit-target
(defun %single-float-atanh! (n result)
  (declare (single-float n result)) 
  (target::with-stack-short-floats ((temp))
    (%setf-short-float TEMP (#_atanhf n))
    (%sf-check-exception-1 'atanh n (%ffi-exception-status))
    (%setf-short-float result TEMP)))

#+64-bit-target
(defun %single-float-atanh (n)
  (declare (single-float n)) 
  (let* ((result (#_atanhf n)))
    (%sf-check-exception-1 'atanh n (%ffi-exception-status))
    result))
)