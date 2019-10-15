;; decimal.lisp

#|
The MIT license.

Copyright (c) 2010 Paul L. Krueger

Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
and associated documentation files (the "Software"), to deal in the Software without restriction, 
including without limitation the rights to use, copy, modify, merge, publish, distribute, 
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT 
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :ns-string-utils))

(defpackage :interface-utilities
  (:nicknames :iu)
  (:export lisp-to-ns-decimal lisp-from-ns-decimal))

(in-package :iu)

;; This contains a set of utility functions for converting back and forth between 
;; NSDecimalNumber objects and lisp integers.
;; Lisp does a good job representing large integers, so in general we can do things
;; like manipulate dollar amounts just by using integers to represent the total
;; cents value and doing something like:
;;       (floor lisp-value 100) 
;; to retrieve dollars and cents values.

;; Using float values for dollars and cents is problematic. Rounding errors will get
;; you anytime you convert to or from float-values and input or display strings. Apple
;; has created a special class NSDecimalNumber to represent long decimal numbers. They
;; defined an array of arithmetic functions to manipulate instance of this class. While
;; we could probably just use those functions in lisp and manipulate NSDecimalNumber
;; objects directly, it seems preferable to convert back and forth to lisp integers and
;; use normal arithmetic operations when in Lisp. Of course it will be up to the Lisp
;; programmer to understand how many decimal digits are being represented whereas this
;; value is represented within NSDecimalNumberObjects.

(defun lisp-to-ns-decimal (int-val &key (decimals 2))
  ;; construct an NSDecimalNumber object with the given int-val and number of decimals
  ;; For example if you have a dollar amount 123.45 represented by the fixnum 12345
  ;; you would call (make-ns-decimal 12345 :decimals 2) to get a corresponding
  ;; NSDecimalNumber object. This object is the responsibility of the caller and a 
  ;; call to #/release must be made when the caller is done with it.
  (unless (typep int-val 'fixnum)
    (error "Input must be a fixnum"))
  (#/decimalNumberWithMantissa:exponent:isNegative:
   ns:ns-decimal-number
   (abs int-val)
   (- decimals)
   (if (minusp int-val) #$YES #$NO)))

(defun lisp-from-ns-decimal (ns-dec-obj &key (decimals 2))
  ;; This function returns a fixnum that corresponds to the NSDecimalNumber 
  ;; or NSNumber that is passed in as the first argument. 
  ;; The result will be scaled and rounded to represent the desired
  ;; number of decimal digits as specified by the :decimals keyword argument.
  ;; For example, if an NSNumber is passed in which is something like 123.45678
  ;; and you ask for 2 decimal digits, the returned value will be the integer 12346.
  (let* ((loc (#/currentLocale ns:ns-locale))
         (lisp-str (ccl::lisp-string-from-nsstring 
                    (#/descriptionWithLocale: ns-dec-obj loc)))
         (str-len-1 (1- (length lisp-str)))
         (dec-pos (or (position #\. lisp-str) str-len-1))
         (dec-digits (- str-len-1 dec-pos))
         (dec-diff (- decimals dec-digits))
         (mantissa-str (delete #\. lisp-str)))
    (cond ((zerop dec-diff)
           (read-from-string mantissa-str))
          ((plusp dec-diff)
           (read-from-string (concatenate 'string 
                                          mantissa-str
                                          (make-string dec-diff :initial-element #\0))))
          (t ;; minusp dec-diff
           (let ((first-dropped (+ (length mantissa-str) dec-diff)))
             (+ (if (> (char-code (elt mantissa-str first-dropped)) (char-code #\4)) 1 0)
                (read-from-string (subseq mantissa-str 0 first-dropped))))))))
    

