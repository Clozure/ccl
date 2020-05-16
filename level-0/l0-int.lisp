;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
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

(eval-when (:compile-toplevel :execute)
  (require "NUMBER-MACROS")
  (require "NUMBER-CASE-MACRO")
)


(defun lsh (fixnum count)
  (require-type fixnum 'fixnum)
  (require-type count 'fixnum)
  (if (> count 0) 
    (%ilsl count fixnum)
    (%ilsr (- count) fixnum)))

; this called with fixnum
(defun %iabs  (n)
  (declare (fixnum n))
  (if (minusp  n) (- n) n))

; called with any integer - is there a cmu version of integer/bignum-abs?
(defun %integer-abs (n)
  (number-case n
    (fixnum
     (locally
	 (declare (fixnum n))
       (if (minusp n)
         (if (eql n most-negative-fixnum)
           (- most-negative-fixnum)
           (the fixnum (- n)))
         n)))
    (bignum
     (if (minusp n) (- n) n))))


(eval-when (:compile-toplevel :execute)
  (assert (< (char-code #\9) (char-code #\A) (char-code #\a))))

(defun token2int (string start len radix)
  ; simple minded in case you hadn't noticed
  (let* ((n start)
         (end (+ start len))
         (char0 (schar string n))
         (val 0)
         minus)
    (declare (fixnum n end start len radix)) ; as if it mattered
    (when (or (eq char0 #\+)(eq char0 #\-))
      (setq n (1+ n))
      (if (eq char0 #\-)(setq minus t)))
    (while (< n end)
      (let ((code (%scharcode string n)))
        (if (<= code (char-code #\9)) 
          (setq code (- code (char-code #\0)))
          (progn
            (when (>= code (char-code #\a))
              (setq code (- code (- (char-code #\a) (char-code #\A)))))
            (setq code (- code (- (char-code #\A) 10)))))
        (setq val (+ (* val radix) code))
        (setq n (1+ n))))
    (if minus (- val) val)))
  

(defun %integer-to-string (int &optional (radix 10))
  (%pr-integer int radix nil t))


;;; it may be hard to believe, but this is much faster than the lap
;;; version (3 or 4X) for fixnums that is (stream-write-string vs
;;; stream-tyo ???)

(defun %pr-integer (int &optional (radix 10) (stream *standard-output*) return-it  negate-it)
  (declare (fixnum radix)) ; assume caller has checked
  (if stream 
    (if (eq stream t) (setq stream *terminal-io*))
    (setq stream *standard-output*))
  (let ((digit-string "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))    
    (cond ((fixnump int)  ; ugh                      
           (let ((temstring (make-string (- target::nbits-in-word target::fixnumshift) :element-type 'base-char))
                 (i (- target::nbits-in-word  target::fixnumshift 1))
                 (neg (< int 0))
                 (rem 0))
             (declare (fixnum i rem))
             (declare (dynamic-extent temstring))
             (when neg (setq int (- int)))
             (when (not (fixnump int))
               (return-from %pr-integer (%pr-integer int radix stream return-it t)))
             (locally (declare (fixnum int))  
               (loop
                 (multiple-value-setq  (int rem) (%fixnum-truncate int radix))                 
                 (setf (%schar temstring i)(%schar digit-string rem))
                 (when (eq 0 int)
                   (return))
                 (setq i (1- i)))
               (when neg 
                 (setf (%schar temstring (setq i (1- i))) #\-))
               (if return-it
                 (%substr temstring i (- target::nbits-in-word
                                         target::fixnumshift))
                 (write-string temstring stream :start i :end (- target::nbits-in-word target::fixnumshift))))))          
          (t (let* ((size-vect #(nil nil 32 21 16 14 13 12 11
                                 11   10 10  9  9  9  9  8  8
                                 8     8  8  8  8  8  7  7  7
                                 7     7  7  7  7  7  7  7  7 7))
                    ;; overestimate # digits by a little for weird
                    ;; radix
                    (bigwords (uvsize int))
                    (strlen (1+ (* bigwords (svref size-vect radix))))
                    (temstring (make-string strlen :element-type 'base-char))
                    (i (1- strlen))
                    (neg (< int 0))
                    ; ;(rem 0)
                    ;; ;better-bignum-print?
                    )  ; warn
               (declare (dynamic-extent temstring)
                        (fixnum i strlen))
               (flet ((do-it (newbig)
                        (print-bignum-2 newbig radix temstring digit-string)))
                 (declare (dynamic-extent #'do-it))
                 (setq i (with-one-negated-bignum-buffer int do-it)))                            
               (when (or neg negate-it) 
                 (setf (%schar temstring (setq i (1- i))) #\-))
               (if return-it
                 (%substr temstring i strlen)
                 (write-string temstring stream :start i :end strlen)))))))



;;; *BASE-POWER* holds the number that we keep dividing into the bignum for
;;; each *print-base*.  We want this number as close to *most-positive-fixnum*
;;; as possible, i.e. (floor (log most-positive-fixnum *print-base*)).
;;; 
(defparameter *base-power* ())

;;; *FIXNUM-POWER--1* holds the number of digits for each *print-base* that
;;; fit in the corresponding *base-power*.
;;; 
(defparameter *fixnum-power--1* ())

(do* ((b (make-array 37 :initial-element nil))
      (f (make-array 37 :initial-element nil))
      (base 2 (1+ base)))
     ((= base 37) (setq *base-power* b *fixnum-power--1* f))
  (do ((power-1 -1 (1+ power-1))
       (new-divisor base (* new-divisor base))
       (divisor 1 new-divisor))
      ((not (fixnump new-divisor))
       (setf (aref b base) divisor)
       (setf (aref f base) power-1))))


(defun print-bignum-2 (big radix string digit-string)
  (declare (optimize (speed 3) (safety 0))
           (simple-base-string string digit-string))
  (let* ((divisor (aref *base-power* radix))
         (power (aref *fixnum-power--1* radix))
         (index (1- (length string)))
         (rem 0))
    (declare (fixnum index divisor power))
    ;;(print index)
    (loop
      (multiple-value-setq (big rem) (truncate big divisor))
      (let* ((int rem)
             (rem 0)
             (final-index (- index power 1)))
        (loop
          (multiple-value-setq (int rem) (%fixnum-truncate int radix))
          (setf (schar string index)(schar digit-string rem))
          (when (eql 0 int)
            (return index))
          (setq index (1- index)))
        (if (zerop big)
          (return index)
          (dotimes (i (- index final-index) index)
            (declare (fixnum i))
            (setq index (1- index))
            (setf (schar string index) #\0)))))))

#+x8664-target
(defun %bignum-hex-digits (b string)
  (let* ((size (uvsize b))
	 (temp-string (make-string 8))
	 (end 0))
    (declare (type fixnum size end))
    (declare (dynamic-extent temp-string))
    (locally (declare (optimize (speed 3) (safety 0)))
      (loop for i of-type fixnum from (the fixnum (1- size)) downto 0
	    for start2 of-type fixnum by 32
	    do (%ub-fixnum-hex-digits 7 (bignum-ref b i) temp-string)
	    (setq end (%i+ end 8))
	    (%copy-ivector-to-ivector temp-string 0 string start2 32)))
    (values string end)))

(defun write-unsigned-byte-hex-digits (u stream)
  (setq u (require-type u '(unsigned-byte)))
  #-x8664-target
  (write u :stream stream :base 16)
  #+x8664-target
  (if (fixnump u)
    (let* ((scratch (make-string 15)))
      (declare (dynamic-extent scratch))
      (%ub-fixnum-hex-digits 14 u scratch)
      (let ((start (dotimes (i 15 nil)
		     (declare (fixnum i)
			      (optimize (speed 3) (safety 0)))
		     (unless (char= #\0 (schar scratch i))
		       (return i)))))
	(if start
	  (write-string scratch stream :start start)
	  (write-string "0" stream))))
    (let* ((scratch (make-string (the fixnum (ash (the fixnum (uvsize u)) 3))))
	   (start 0))
      (declare (dynamic-extent scratch)
	       (fixnum start))
      (multiple-value-bind (string end)
	  (%bignum-hex-digits u scratch)
	;; skip leading zeros (there will be a non-zero digit)
	(let ((i 0))
	  (declare (type (unsigned-byte 56) i))
	  (loop
	    (if (char= #\0 (schar string i))
	      (setq start (%i+ start 1))
	      (return))
	    (setq i (%i+ i 1))))
	(write-string string stream :start start :end end))))
  nil)
