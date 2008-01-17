; -*- Mode: Lisp; Package: CCL -*-
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
;; chars.lisp

(in-package "CCL")

; If object is a character, it is returned.  If it is an integer, its INT-CHAR
; is returned. If it is a string of length 1, then the sole element of the
; string is returned.  If it is a symbol whose pname is of length 1, then
; the sole element of the pname is returned. Else error.

(defun character (arg)
  "Coerce OBJECT into a CHARACTER if possible. Legal inputs are 
  characters, strings and symbols of length 1."
  (if (typep arg 'character)
    arg
    (if (typep arg 'fixnum)
      (code-char arg)
      (if (and (typep arg 'string)
               (= (the fixnum (length arg)) 1))
        (char arg 0)
        (let* ((pname (if (typep arg 'symbol) (symbol-name arg))))
          (if (and pname (= (the fixnum (length pname)) 1))
            (char pname 0)
            (%err-disp $xcoerce arg 'character)))))))



(defun digit-char (weight &optional radix)
  "All arguments must be integers. Returns a character object that
  represents a digit of the given weight in the specified radix. Returns
  NIL if no such character exists."
  (let* ((r (if radix (require-type radix 'integer) 10)))
    (if (and (typep (require-type weight 'integer) 'fixnum)
             (>= r 2)
             (<= r 36)
             (>= weight 0)
             (< weight r))
      (locally (declare (fixnum weight))
        (if (< weight 10)
          (code-char (the fixnum (+ weight (char-code #\0))))
          (code-char (the fixnum (+ weight (- (char-code #\A) 10)))))))))



;True for ascii codes 32-126 inclusive.
; and for guys >= 128. Its really a function of the font of the moment.
(defun graphic-char-p (c)
  "The argument must be a character object. GRAPHIC-CHAR-P returns T if the
  argument is a printing character (space through ~ in ASCII), otherwise
  returns NIL."
  (let* ((code (char-code c)))
    (unless (eq c #\rubout)
      (>= code (char-code #\space)))))


;True for ascii codes 10 and 32-126 inclusive.
(defun standard-char-p (c)
  "The argument must be a character object. STANDARD-CHAR-P returns T if the
   argument is a standard character -- one of the 95 ASCII printing characters
   or <return>."
  (let* ((code (char-code c)))
    (or (eq c #\newline)
        (and 
         (>= code (char-code #\space))
         (< code (char-code #\rubout))))))



(defun %non-standard-lower-case-equivalent (char)
  (gethash char *non-standard-upper-to-lower*))



(defun upper-case-p (c)
  "The argument must be a character object; UPPER-CASE-P returns T if the
   argument is an upper-case character, NIL otherwise."
  (let* ((code (char-code c)))
    (declare (type (mod #x110000) code))
    (or (and (>= code (char-code #\A))
             (<= code (char-code #\Z)))
        (and (>= code #x80)
             (not (null (%non-standard-lower-case-equivalent c)))))))




(defun both-case-p (c)
  "The argument must be a character object. BOTH-CASE-P returns T if the
  argument is an alphabetic character and if the character exists in
  both upper and lower case. For ASCII, this is the same as ALPHA-CHAR-P."
  (let* ((code (char-code c)))
    (declare (type (mod #x110000) code))
    (or (and (>= code (char-code #\A))
             (<= code (char-code #\Z)))
        (and (>= code (char-code #\a))
             (<= code (char-code #\z))))))
  
(defun alphanumericp (c)
  "Given a character-object argument, ALPHANUMERICP returns T if the
   argument is either numeric or alphabetic."
  (let ((code (char-code c)))
    (declare (type (mod #x110000) code))
    (or
     (and (>= code (char-code #\0))
          (<= code (char-code #\9)))
     (and (>= code (char-code #\a))
          (<= code (char-code #\z)))
     (and (>= code (char-code #\A))
          (<= code (char-code #\Z)))
     (and (> code #x80)
          (or (not (null (%non-standard-upper-case-equivalent c)))
              (not (null (%non-standard-lower-case-equivalent c))))))))

(defun char= (ch &rest others)
  "Return T if all of the arguments are the same character."
  (declare (dynamic-extent others))
  (unless (typep ch 'character)
    (setq ch (require-type ch 'character)))
  (dolist (other others t)
    (unless (eq other ch)
      (unless (typep other 'character)
        (setq other (require-type other 'character)))
      (return))))

(defun char/= (ch &rest others)
  "Return T if no two of the arguments are the same character."
  (declare (dynamic-extent others))
  (unless (typep ch 'character)
    (setq ch (require-type ch 'character)))
  (do* ((rest others (cdr rest)))
       ((null rest) t)
    (let ((other (car rest)))
      (if (eq other ch) (return))
      (unless (typep other 'character)
        (setq other (require-type other 'character)))
      (dolist (o2 (cdr rest))
        (if (eq o2 other)(return-from char/= nil))))))


(defun char-equal (char &rest others)
  "Return T if all of the arguments are the same character.
  Font, bits, and case are ignored."
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (dolist (c others t)
      (when (not (eq c char))
        (unless (eq (char-upcase char) (char-upcase c))
          (return))))))

;;; Compares each char against all following chars, not just next one. Tries
;;; to be fast for one or two args.
(defun char-not-equal (char &rest others)
  "Return T if no two of the arguments are the same character.
   Font, bits, and case are ignored."
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3) (safety 0)))
    (let* ((rest (cdr others)))
      (cond 
       (rest                   
        (setq char (char-code (char-upcase char)))
        (do ((list others (cdr list)))
            ((null list))
          (rplaca list (char-code (char-upcase (car list)))))
        (while others
          (when (memq char others)
            (return-from char-not-equal nil))
	  (setq char (car others)
		others rest
		rest (cdr others)))
        t)
       (others                     ;  2 args, no table
        (not (eq (char-upcase char) (char-upcase (car others)))))
       (t t)))))


(defun char-lessp (char &rest others)
  "Return T if the arguments are in strictly increasing alphabetic order.
   Font, bits, and case are ignored."
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (let* ((code (char-code (char-upcase char))))
      (dolist (c others t)
        (unless (< code (setq code (char-code (char-upcase c))))
          (return))))))

(defun char-not-lessp (char &rest others)
  "Return T if the arguments are in strictly non-increasing alphabetic order.
   Font, bits, and case are ignored."
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (let* ((code (char-code (char-upcase char))))
      (dolist (c others t)
        (when (< code (setq code (char-code (char-upcase c))))
          (return))))))

(defun char-greaterp (char &rest others)
  "Return T if the arguments are in strictly decreasing alphabetic order.
   Font, bits, and case are ignored."
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (let* ((code (char-code (char-upcase char))))
      (dolist (c others t)
        (unless (> code (setq code (char-code (char-upcase c))))
          (return))))))

(defun char-not-greaterp (char &rest others)
  "Return T if the arguments are in strictly non-decreasing alphabetic order.
   Font, bits, and case are ignored."
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (let* ((code (char-code (char-upcase char))))
      (dolist (c others t)
        (when (> code (setq code (char-code (char-upcase c))))
          (return))))))


(defun char> (char &rest others)
  "Return T if the arguments are in strictly decreasing alphabetic order."
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (let* ()      
      (setq char (char-code char))
      (dolist (c others t)
        (let ((code (char-code c)))
          (when (not (%i> char (setq char code)))
            (return)))))))

(defun char>= (char &rest others)
  "Return T if the arguments are in strictly non-increasing alphabetic order."
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (let* ()      
      (setq char (char-code char))
      (dolist (c others t)
        (let ((code (char-code c)))
          (when (not (%i>= char (setq char code)))
            (return)))))))


(defun char< (char &rest others)
  "Return T if the arguments are in strictly increasing alphabetic order."
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (let* ()      
      (setq char (char-code char))
      (dolist (c others t)
        (let ((code (char-code c)))
          (when (not (%i< char (setq char code)))
            (return)))))))

(defun char<= (char &rest others)
  "Return T if the arguments are in strictly non-decreasing alphabetic order."
  (declare (dynamic-extent others))
  (locally (declare (optimize (speed 3)(safety 0)))
    (let* ()      
      (setq char (char-code char))
      (dolist (c others t)
        (let ((code (char-code c)))
          (when (not (%i<= char (setq char code)))
            (return)))))))

; This is Common Lisp
(defun char-int (c)
  "Return the integer code of CHAR."
  (char-code c))


;If char has an entry in the *NAME-CHAR-ALIST*, return first such entry.
;Otherwise, if char is a graphics character, return NIL
;Otherwise, if char code is < 128, return "^C", otherwise "1nn"

(defun char-name (c)
  "Return the name (a STRING) for a CHARACTER object."
  (let* ((code (char-code c)))
    (declare (type (mod #x110000) code))
    (or (gethash c *char->name*)
        (cond ((< code #x7f)
               (when (< code (char-code #\space))
                 (let ((str (make-string 2 :element-type 'base-char)))
                   (declare (simple-base-string str))
                   (setf (schar str 0) #\^)
                   (setf (schar str 1)(code-char (logxor code #x40)))
                   str)))
              ((and (< code #x100)(graphic-char-p c)) nil)
              (t (format nil "U+~4,'0x" code))))))


(defun string-downcase (string &key start end)
  (setq string (copy-string-arg string))
  (if (not start) (setq start 0)(require-type start 'fixnum))
  (if (not end)(setq end (length string))(require-type end 'fixnum))
  (%strdown string start end))


(defun %strdown (string start end)
  (declare (fixnum start end)
           (optimize (speed 3) (safety 0)))
  (unless (typep string 'simple-string)
    (check-type string simple-string))
  (do* ((i start (1+ i)))
       ((>= i end) string)
    (declare (fixnum i))
    (let* ((ch (schar string i))
           (code (char-code ch))
           (lower (if (and (char<= ch #\Z)
                           (char>= ch #\A))
                    (%code-char (the (unsigned-byte 8)
                                  (+ code (- (char-code #\a)(char-code #\A)))))
                    (if (>= code #x80)
                      (%non-standard-lower-case-equivalent ch)))))
      (declare (character ch) (type (mod #x11000) code))
      (when lower
        (setf (schar string i) lower)))))




(defun copy-string-arg (string &aux (org 0) len)
  (etypecase string
    (string
     (setq len (length string))
     (multiple-value-setq (string org)(array-data-and-offset string)))
    (symbol
     (setq string (symbol-name string))
     (setq len (length string)))
    (character
     (return-from copy-string-arg
                    (make-string 1 :initial-element string :element-type (type-of string)))))
  (%substr string org (+ len org)))     

(defun string-upcase (string &key start end)
  (setq string (copy-string-arg string))
  (if (not start) (setq start 0)(require-type start 'fixnum))
  (if (not end)(setq end (length string))(require-type end 'fixnum))
  (%strup string start end))

(defun %strup (string start end)
  (declare (fixnum start end)
           (optimize (speed 3) (safety 0)))
  (unless (typep string 'simple-string)
    (check-type string simple-string))
  (do* ((i start (1+ i)))
       ((>= i end) string)
    (declare (fixnum i))
    (let* ((ch (schar string i))
           (code (char-code ch))
           (upper (if (and (char<= ch #\z)
                           (char>= ch #\a))
                    (%code-char (the (unsigned-byte 8)
                                  (- code (- (char-code #\a)(char-code #\A)))))
                    (if (>= code #x80)
                      (%non-standard-upper-case-equivalent ch)))))
      (declare (character ch) (type (mod #x11000) code))
      (when upper
        (setf (schar string i) upper)))))



(defun string-capitalize (string &key start end)
  (setq string (copy-string-arg string))
  (if (not start) (setq start 0)(require-type start 'fixnum))
  (if (not end)(setq end (length string))(require-type end 'fixnum))
  (%strcap string start end))

(defun %strcap (string start end)
  (declare (fixnum start end))
  (let ((state :up)
        (i start))
    (declare (fixnum i))
    (while (< i end)
      (let* ((c (%schar string i))
             (alphap (alphanumericp c))) ; makes no sense
        (if alphap
          (progn
            (setf (%schar string i)
                  (case state
                    (:up (char-upcase c))
                    (t (char-downcase c))))
            (setq state :down))
          (setq state :up)))
      (setq i (1+ i)))
    string))




(defun nstring-downcase (string &key start end)
  (etypecase string
    (string
     (if (not start) (setq start 0)(require-type start 'fixnum))
     (if (not end)(setq end (length string))(require-type end 'fixnum))
     (multiple-value-bind (sstring org) (array-data-and-offset string)
       (%strdown sstring (+ start org)(+ end org)))
     string)))

(defun nstring-upcase (string &key start end)
  (etypecase string
    (string
     (if (not start) (setq start 0)(require-type start 'fixnum))
     (if (not end)(setq end (length string))(require-type end 'fixnum))
     (multiple-value-bind (sstring org) (array-data-and-offset string)
       (%strup sstring (+ start org)(+ end org)))
     string)))


(defun nstring-capitalize (string &key start end)
  (etypecase string
    (string
     (if (not start) (setq start 0)(require-type start 'fixnum))
     (if (not end)(setq end (length string))(require-type end 'fixnum))
     (multiple-value-bind (sstring org) (array-data-and-offset string)
       (%strcap sstring (+ start org)(+ end org)))
     string)))



(defun nstring-studlify (string &key start end)
  (declare (ignore start end))
  string)

  
(defun string-compare (string1 start1 end1 string2 start2 end2)
  (let ((istart1 (or start1 0)))
    (if (and (typep string1 'simple-string)(null start1)(null end1))
      (setq start1 0 end1 (length string1))
      (multiple-value-setq (string1 start1 end1)(string-start-end string1 start1 end1)))
    (if (and (typep string2 'simple-string)(null start2)(null end2))
      (setq start2 0 end2 (length string2))
      (multiple-value-setq (string2 start2 end2)(string-start-end string2 start2 end2)))
    (setq istart1 (%i- start1 istart1))        
    (let* ((val t))
      (declare (optimize (speed 3)(safety 0)))
      (do* ((i start1 (%i+ 1 i))
            (j start2 (%i+ 1 j)))
           ()
        (when (eq i end1)
          (when (neq j end2)(setq val -1))
          (return))
        (when (eq j end2)
          (setq end1 i)
          (setq val 1)(return))
        (let ((code1 (%scharcode string1 i))
              (code2 (%scharcode string2 j)))
          (declare (fixnum code1 code2))
          (if (and (>= code1 (char-code #\a))
                   (<= code1 (char-code #\z)))
            (setq code1 (- code1 (- (char-code #\a) (char-code #\A)))))
          (if (and (>= code2 (char-code #\a))
                   (<= code2 (char-code #\z)))
            (setq code2 (- code2 (- (char-code #\a) (char-code #\A)))))
          (unless (= code1 code2)            
            (setq val (if (%i< code1 code2) -1 1))
            (setq end1 i)
            (return))))
      (values val (%i- end1 istart1)))))


(defun string-greaterp (string1 string2 &key start1 end1 start2 end2)
  "Given two strings, if the first string is lexicographically greater than
  the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (multiple-value-bind (result pos) (string-compare string1 start1 end1 string2 start2 end2)
    (if (eq result 1) pos nil)))

(defun string-not-greaterp (string1 string2 &key start1 end1 start2 end2)
  "Given two strings, if the first string is lexicographically less than
  or equal to the second string, returns the longest common prefix
  (using char-equal) of the two strings. Otherwise, returns ()."
  (multiple-value-bind (result pos) (string-compare string1 start1 end1 string2 start2 end2)
    (if (eq result 1) nil pos)))

(defun string-not-equal (string1 string2 &key start1 end1 start2 end2)
  "Given two strings, if the first string is not lexicographically equal
  to the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (multiple-value-bind (result pos) (string-compare string1 start1 end1 string2 start2 end2)
    (if (eq result t) nil pos)))

(defun string-not-lessp (string1 string2 &key start1 end1 start2 end2)
  "Given two strings, if the first string is lexicographically greater
  than or equal to the second string, returns the longest common prefix
  (using char-equal) of the two strings. Otherwise, returns ()."
  (multiple-value-bind (result pos) (string-compare string1 start1 end1 string2 start2 end2)
    (if (eq result -1) nil pos)))

(defun string-equal (string1 string2 &key start1 end1 start2 end2)
  "Given two strings (string1 and string2), and optional integers start1,
  start2, end1 and end2, compares characters in string1 to characters in
  string2 (using char-equal)."
  (eq t (string-compare string1 start1 end1 string2 start2 end2)))


(defun string-lessp (string1 string2 &key start1 end1 start2 end2)
  "Given two strings, if the first string is lexicographically less than
  the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (multiple-value-bind (result pos)(string-compare string1 start1 end1 string2 start2 end2)
    (if (eq result -1) pos nil)))

; forget script-manager - just do codes
(defun string-cmp (string1 start1 end1 string2 start2 end2)
  (let ((istart1 (or start1 0)))
    (if (and (typep string1 'simple-string)(null start1)(null end1))
      (setq start1 0 end1 (length string1))
      (multiple-value-setq (string1 start1 end1)(string-start-end string1 start1 end1)))
    (if (and (typep string2 'simple-string)(null start2)(null end2))
      (setq start2 0 end2 (length string2))
      (multiple-value-setq (string2 start2 end2)(string-start-end string2 start2 end2)))
    (setq istart1 (%i- start1 istart1))        
    (let* ((val t))
      (declare (optimize (speed 3)(safety 0)))
      (do* ((i start1 (%i+ 1 i))
            (j start2 (%i+ 1 j)))
           ()
        (when (eq i end1)
          (when (neq j end2)(setq val -1))
          (return))
        (when (eq j end2)
          (setq end1 i)
          (setq val 1)(return))
        (let ((code1 (%scharcode string1 i))
              (code2 (%scharcode string2 j)))
          (declare (fixnum code1 code2))
          (unless (= code1 code2)            
            (setq val (if (%i< code1 code2) -1 1))
            (setq end1 i)
            (return))))
      (values val (%i- end1 istart1)))))

(defun string> (string1 string2 &key start1 end1 start2 end2)
  "Given two strings, if the first string is lexicographically greater than
  the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (multiple-value-bind (result pos) (string-cmp string1 start1 end1 string2 start2 end2)
    (if (eq result 1) pos nil)))

(defun string>= (string1 string2 &key start1 end1 start2 end2)
  "Given two strings, if the first string is lexicographically greater
  than or equal to the second string, returns the longest common prefix
  (using char=) of the two strings. Otherwise, returns ()."
  (multiple-value-bind (result pos) (string-cmp string1 start1 end1 string2 start2 end2)
    (if (eq result -1) nil pos)))

(defun string< (string1 string2 &key start1 end1 start2 end2)
  "Given two strings, if the first string is lexicographically less than
  the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (multiple-value-bind (result pos) (string-cmp string1 start1 end1 string2 start2 end2)
    (if (eq result -1) pos nil)))

(defun string<= (string1 string2 &key start1 end1 start2 end2)
  "Given two strings, if the first string is lexicographically less than
  or equal to the second string, returns the longest common prefix
  (using char=) of the two strings. Otherwise, returns ()."
  (multiple-value-bind (result pos) (string-cmp string1 start1 end1 string2 start2 end2)
    (if (eq result 1) nil pos)))

; this need not be so fancy?
(defun string/= (string1 string2 &key start1 end1 start2 end2)
  "Given two strings, if the first string is not lexicographically equal
  to the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (multiple-value-bind (result pos) (string-cmp string1 start1 end1 string2 start2 end2)
    (if (eq result t) nil pos)))



(provide 'chars)
