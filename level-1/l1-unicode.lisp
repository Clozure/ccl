;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2006 Clozure Associates and contributors.
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


;;; Unicode translation stuff, mostly in support of I/O.

(in-package "CCL")


(defvar *character-encodings* (make-hash-table :test #'eq))

(defun lookup-character-encoding (name)
  (gethash name *character-encodings*))

(defun get-character-encoding (name)
  (or (lookup-character-encoding name)
      (error "Unknown character encoding: ~s." name)))

(defun (setf get-character-encoding) (new name)
  (setf (gethash name *character-encodings*) new))

(defun ensure-character-encoding (thing)
  (if (typep thing 'character-encoding)
    thing
    (or (lookup-character-encoding thing)
        (error "~s is not a character-encoding or the name of a character-encoding."
               thing))))


(defstruct character-encoding
  (name ())                             ;canonical name
  (code-unit-size 8)                    ;in bits: 8, 16, 32
  (native-endianness t)                 ;if nil, need to swap 16,32-bit units
  (max-units-per-char 1)                ;usually 1-4

  ;; Writes CHAR (or a replacement character if CHAR can't be encoded)
  ;; to STREAM and returns the number of code-units written.
  stream-encode-function                ;(CHAR WRITE-FUNCTION STREAM)
  
  ;; Returns a charcter (possibly #\Replacement_Character) or :EOF.
  stream-decode-function                ;(1ST-UNIT NEXT-UNIT STREAM)

  ;; Sets 1 or more units in a vector argument and returns a value 1
  ;; greater than the index of the last octet written to the vector
  vector-encode-function                ;(STRING VECTOR INDEX START END)
  
  ;; Returns a value 1 greater than the last octet index consumed from
  ;; the vector argument.
  vector-decode-function                ;(VECTOR INDEX NOCTETS STRING)
  
  ;; Sets one or more units in memory at the address denoted by
  ;; the pointer and index arguments and returns (+ idx number of
  ;; units written to memory), else returns NIL if any character
  ;; can't be encoded.
  memory-encode-function                ;(STRING POINTER INDEX START END)

  
  ;; Returns (as multiple values) the  string encoded in memory
  ;; at the address denoted by the address and index args and the
  ;; sum of the index arg and the number of octets consumed.
  memory-decode-function                ;(POINTER NOCTETS INDEX STRING)
  
  ;; Returns the number of octets needed to encode STRING between START and END
  octets-in-string-function              ;(STRING START END)

  ;; Returns the number of (full) characters encoded in VECTOR, and
  ;; the index the index of the first octet not used to encode
  ;; them. (The second value may be less than END.
  length-of-vector-encoding-function    ;(VECTOR START END) 

  ;; Returns the number of (full) characters encoded in memory at (+ POINTER START)
  ;; and the number of octets used to encode them.  (The second value may be less
  ;; than NOCTETS.)
  length-of-memory-encoding-function    ;(POINTER NOCTETS START)

  ;; Code units less than this value map to themselves on input.
  (decode-literal-code-unit-limit 0)

  ;; Does a byte-order-mark determine the endianness of input ?
  ;; Should we prepend a BOM to output ?
  ;; If non-nil, the value should be the name of the an encoding
  ;; that implements this encoding with swapped byte order.
  (use-byte-order-mark nil)
  ;; What alternate line-termination conventions can be encoded ?  (This basically
  ;; means "can #\Line_Separator be encoded?", since :CR and :CRLF can always
  ;; be encoded.)
  (alternate-line-termination-conventions '(:cr :crlf))
  ;; By what other MIME names is this encoding known ?
  (aliases nil)
  (documentation nil)
  ;; What does a native byte-order-mark look like (as a sequence of octets)
  ;; in this encoding ? (NIL if a BOM can't be encoded.)
  (bom-encoding nil)
  ;; How is #\NUL encoded, as a sequence of octets ?  (Typically, as a minimal-
  ;; length sequenve of 0s, but there are exceptions.)
  (nul-encoding #(0))
  ;; Char-codes less than  this value map to themselves on output.
  (encode-literal-char-code-limit 0)
  )

(defconstant byte-order-mark #\u+feff)
(defconstant byte-order-mark-char-code (char-code byte-order-mark))
(defconstant swapped-byte-order-mark-char-code #xfffe)


(defmethod default-character-encoding ((domain t))
  (character-encoding-name (get-character-encoding nil)))

(defun decode-character-encoded-vector (encoding vector start-index noctets string)
  (setq encoding (ensure-character-encoding encoding))
  (unless (= (the (unsigned-byte 8) (typecode vector))
             target::subtag-u8-vector)
    (report-bad-arg vector '(simple-array (unsigned-byte 8) (*))))
  (unless (= (the (unsigned-byte 8) (typecode string))
             target::subtag-simple-base-string)
    (report-bad-arg vector 'simple-string))
  (let* ((len (length vector)))
    (declare (type index len))
    (unless (and (typep start-index 'fixnum)
                 (>= (the fixnum start-index) 0)
                 (< (the fixnum start-index) len))
      (error "~s is an invalid start index for ~s" start-index vector))
    (unless (and (typep noctets 'fixnum)
                 (>= (the fixnum noctets) 0)
                 (<= (+ (the fixnum start-index) (the fixnum noctets)) len))
      (error "~S is an invalid octet count for ~s at ~s" noctets vector start-index))
    (funcall (character-encoding-vector-decode-function encoding)
             vector
             start-index
             noctets
             string)))


(defmethod print-object ((ce character-encoding) stream)
  (print-unreadable-object (ce stream :type t :identity t)
    (format stream "~a" (character-encoding-name ce))))

;;; N.B.  (ccl:nfunction <name> (lambda (...) ...)) is just  like
;;;       (cl:function (lambda (...) ...)), except that the resulting
;;; function will have "name" <name> (this is often helpful when debugging.)

(defmacro define-character-encoding (name doc &rest args &key &allow-other-keys)
  (setq name (intern (string name) "KEYWORD"))
  (let* ((encoding (gensym))
         (alias (gensym)))
  `(let* ((,encoding (make-character-encoding :name ,name :documentation ,doc ,@args)))
    (setf (get-character-encoding ,name) ,encoding)
    (dolist (,alias (character-encoding-aliases ,encoding))
      (setf (get-character-encoding ,alias) ,encoding))
    ',name)))

(defun encoding-name (encoding)
  (character-encoding-name (or encoding (get-character-encoding nil))))

;;; ISO-8859-1 is trivial, though of course it can't really encode characters
;;; whose CHAR-CODE is >= 256

(defun 8-bit-fixed-width-octets-in-string (string start end)
  (declare (ignore string))
  (if (>= end start)
    (- end start)
    0))

(defun 8-bit-fixed-width-length-of-vector-encoding (vector start end)
  (declare (ignore vector))
  (if (>= end start)
    (values (- end start) end)
    (values 0 start)))

(defun 8-bit-fixed-width-length-of-memory-encoding (pointer noctets start)
  (declare (ignore pointer start))
  (values noctets noctets))

(define-character-encoding :iso-8859-1
  "An 8-bit, fixed-width character encoding in which all character
codes map to their Unicode equivalents. Intended to support most
characters used in most Western European languages."

  ;; The NIL alias is used internally to mean that ISO-8859-1 is
  ;; the "null" 8-bit encoding
  :aliases '(nil :iso_8859-1 :latin1 :l1 :ibm819 :cp819 :csISOLatin1)
  :stream-encode-function
  (nfunction
   iso-8859-1-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char)))
       (declare (type (mod #x110000) code))
       (if (>= code 256)
         (setq code (char-code #\Sub)))
       (funcall write-function stream code)
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-1-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (code-char 1st-unit)))
  :vector-encode-function
  (nfunction
   iso-8859-1-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (if (>= code 256)
           (setq code (char-code #\Sub)))
         (progn
           (setf (aref vector idx) code)
           (incf idx))))))
  :vector-decode-function
  (nfunction
   iso-8859-1-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (setf (schar string i) (code-char (the (unsigned-byte 8)
                                             (aref vector index)))))))
  :memory-encode-function
  (nfunction
   iso-8859-1-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (if (>= code 256)
           (setq code (char-code #\Sub)))
         (setf (%get-unsigned-byte pointer idx) code)
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-1-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
         (setf (schar string i) (code-char (the (unsigned-byte 8)
                                             (%get-unsigned-byte pointer index)))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit 256
  :encode-literal-char-code-limit 256
  )

(define-character-encoding :us-ascii
  "A 7-bit, fixed-width character encoding in which all character
codes map to their Unicode equivalents."

  :aliases '(:csASCII :cp637 :IBM637 :us :ISO646-US :ascii :ISO-ir-6)
  :stream-encode-function
  (nfunction
   ascii-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char)))
       (declare (type (mod #x110000) code))
       (when (>= code 128)
         (setq code (char-code #\Sub)))
       (funcall write-function stream code)
       1)))
  :stream-decode-function
  (nfunction
   ascii-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit 128)
       (code-char 1st-unit)
       #\Replacement_Character)))
  :vector-encode-function
  (nfunction
   ascii-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (if (>= code 128)
           (setq code (char-code #\Sub)))
         (setf (aref vector idx) code)
         (incf idx)))))
  :vector-decode-function
  (nfunction
   ascii-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((code (aref vector index)))
         (declare (type (unsigned-byte 8) code))
         (when (>= code 128)
           (setq code (char-code #\Sub)))
         (setf (schar string i) (code-char code))))))
  :memory-encode-function
  (nfunction
   ascii-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (if (>= code 128)
           (setq code (char-code #\Sub)))
         (setf (%get-unsigned-byte pointer idx) code)
         (incf idx)))))
  :memory-decode-function
  (nfunction
   ascii-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((code (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) code))
         (if (>= code 128)
           (setf (schar string i) #\sub)
           (setf (schar string i) (code-char code)))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit 128
  :encode-literal-char-code-limit 128
  )



;;; Other 1-byte, fixed-width encodings.  Typically, codes in the range
;;; #x00-#x9f maps straight through, while codes #xa0-#xff select arbitrary
;;; Unicode characters that are commonly used in some locale.  (Sometimes
;;; the break is at #x80 instead of #xa0).

(defstatic *iso-8859-2-to-unicode*
  #(
  ;; #xa0
  #\u+00a0 #\u+0104 #\u+02d8 #\u+0141 #\u+00a4 #\u+013d #\u+015a #\u+00a7
  #\u+00a8 #\u+0160 #\u+015e #\u+0164 #\u+0179 #\u+00ad #\u+017d #\u+017b
  ;; #xb0 
  #\u+00b0 #\u+0105 #\u+02db #\u+0142 #\u+00b4 #\u+013e #\u+015b #\u+02c7
  #\u+00b8 #\u+0161 #\u+015f #\u+0165 #\u+017a #\u+02dd #\u+017e #\u+017c
  ;; #xc0 
  #\u+0154 #\u+00c1 #\u+00c2 #\u+0102 #\u+00c4 #\u+0139 #\u+0106 #\u+00c7
  #\u+010c #\u+00c9 #\u+0118 #\u+00cb #\u+011a #\u+00cd #\u+00ce #\u+010e
  ;; #xd0 
  #\u+0110 #\u+0143 #\u+0147 #\u+00d3 #\u+00d4 #\u+0150 #\u+00d6 #\u+00d7
  #\u+0158 #\u+016e #\u+00da #\u+0170 #\u+00dc #\u+00dd #\u+0162 #\u+00df
  ;; #xe0 
  #\u+0155 #\u+00e1 #\u+00e2 #\u+0103 #\u+00e4 #\u+013a #\u+0107 #\u+00e7
  #\u+010d #\u+00e9 #\u+0119 #\u+00eb #\u+011b #\u+00ed #\u+00ee #\u+010f
  ;; #xf0 
  #\u+0111 #\u+0144 #\u+0148 #\u+00f3 #\u+00f4 #\u+0151 #\u+00f6 #\u+00f7
  #\u+0159 #\u+016f #\u+00fa #\u+0171 #\u+00fc #\u+00fd #\u+0163 #\u+02d9
))

(defstatic *unicode-00a0-0180-to-iso-8859-2*
  #(
    #xa0 nil nil nil #xa4 nil nil #xa7 ; #xa0-#xa7 
    #xa8 nil nil nil nil #xad nil nil ; #xa8-#xaf 
    #xb0 nil nil nil #xb4 nil nil nil ; #xb0-#xb7 
    #xb8 nil nil nil nil nil nil nil  ; #xb8-#xbf 
    nil #xc1 #xc2 nil #xc4 nil nil #xc7 ; #xc0-#xc7 
    nil #xc9 nil #xcb nil #xcd #xce nil ; #xc8-#xcf 
    nil nil nil #xd3 #xd4 nil #xd6 #xd7 ; #xd0-#xd7 
    nil nil #xda nil #xdc #xdd nil #xdf ; #xd8-#xdf 
    nil #xe1 #xe2 nil #xe4 nil nil #xe7 ; #xe0-#xe7 
    nil #xe9 nil #xeb nil #xed #xee nil ; #xe8-#xef 
    nil nil nil #xf3 #xf4 nil #xf6 #xf7 ; #xf0-#xf7 
    nil nil #xfa nil #xfc #xfd nil nil ; #xf8-#xff 
    ;; #x0100 
    nil nil #xc3 #xe3 #xa1 #xb1 #xc6 #xe6 ; #x100-#x107 
    nil nil nil nil #xc8 #xe8 #xcf #xef ; #x108-#x10f 
    #xd0 #xf0 nil nil nil nil nil nil ; #x110-#x117 
    #xca #xea #xcc #xec nil nil nil nil ; #x118-#x11f 
    nil nil nil nil nil nil nil nil     ; #x120-#x127 
    nil nil nil nil nil nil nil nil     ; #x128-#x12f 
    nil nil nil nil nil nil nil nil     ; #x130-#x137 
    nil #xc5 #xe5 nil nil #xa5 #xb5 nil ; #x138-#x13f 
    nil #xa3 #xb3 #xd1 #xf1 nil nil #xd2 ; #x140-#x147 
    #xf2 nil nil nil nil nil nil nil  ; #x148-#x14f 
    #xd5 #xf5 nil nil #xc0 #xe0 nil nil ; #x150-#x157 
    #xd8 #xf8 #xa6 #xb6 nil nil #xaa #xba ; #x158-#x15f 
    #xa9 #xb9 #xde #xfe #xab #xbb nil nil ; #x160-#x167 
    nil nil nil nil nil nil #xd9 #xf9 ; #x168-#x16f 
    #xdb #xfb nil nil nil nil nil nil ; #x170-#x177 
    nil #xac #xbc #xaf #xbf #xae #xbe nil ; #x178-#x17f 
    ))

(defstatic *unicode-00c0-00e0-to-iso-8859-2*
  #(
    nil nil nil nil nil nil nil #xb7  ; #xc0-#xc7 
    nil nil nil nil nil nil nil nil     ; #xc8-#xcf 
    nil nil nil nil nil nil nil nil     ; #xd0-#xd7 
    #xa2 #xff nil #xb2 nil #xbd nil nil ; #xd8-#xdf
    ))

(define-character-encoding :iso-8859-2
  "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in most
languages used in Central/Eastern Europe."
  :aliases '(:iso_8859-2 :latin-2 :l2 :csISOLatin2)
  :stream-encode-function
  (nfunction
   iso-8859-2-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-00a0-0180-to-iso-8859-2*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x2c0) (< code #x2e0))
                       (svref *unicode-00c0-00e0-to-iso-8859-2*
                                      (the fixnum (- code #x2c0)))))))
                      
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-2-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-2-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-2-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                          ((< code #x180)
                           (svref *unicode-00a0-0180-to-iso-8859-2*
                                  (the fixnum (- code #xa0))))
                          ((and (>= code #x2c0) (< code #x2e0))
                           (svref *unicode-00c0-00e0-to-iso-8859-2*
                                  (the fixnum (- code #x2c0)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-2-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
           (declare (type (unsigned-byte 8) 1st-unit))
           (setf (schar string i)
            (if (< 1st-unit #xa0)
              (code-char 1st-unit)
              (svref *iso-8859-2-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-2-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                        ((< code #x180)
                         (svref *unicode-00a0-0180-to-iso-8859-2*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x2c0) (< code #x2e0))
                         (svref *unicode-00c0-00e0-to-iso-8859-2*
                                (the fixnum (- code #x2c0)))))))
       (declare (type (mod #x110000) code))
       (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
       (1+ idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-2-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-2-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0
  )

(defstatic *iso-8859-3-to-unicode*
  #(
    ;; #xa0 
    #\u+00a0 #\u+0126 #\u+02d8 #\u+00a3 #\u+00a4 #\u+fffd #\u+0124 #\u+00a7
    #\u+00a8 #\u+0130 #\u+015e #\u+011e #\u+0134 #\u+00ad #\u+fffd #\u+017b
    ;; #xb0 
    #\u+00b0 #\u+0127 #\u+00b2 #\u+00b3 #\u+00b4 #\u+00b5 #\u+0125 #\u+00b7
    #\u+00b8 #\u+0131 #\u+015f #\u+011f #\u+0135 #\u+00bd #\u+fffd #\u+017c
    ;; #xc0 
    #\u+00c0 #\u+00c1 #\u+00c2 #\u+fffd #\u+00c4 #\u+010a #\u+0108 #\u+00c7
    #\u+00c8 #\u+00c9 #\u+00ca #\u+00cb #\u+00cc #\u+00cd #\u+00ce #\u+00cf
    ;; #xd0 
    #\u+fffd #\u+00d1 #\u+00d2 #\u+00d3 #\u+00d4 #\u+0120 #\u+00d6 #\u+00d7
    #\u+011c #\u+00d9 #\u+00da #\u+00db #\u+00dc #\u+016c #\u+015c #\u+00df
    ;; #xe0 
    #\u+00e0 #\u+00e1 #\u+00e2 #\u+fffd #\u+00e4 #\u+010b #\u+0109 #\u+00e7
    #\u+00e8 #\u+00e9 #\u+00ea #\u+00eb #\u+00ec #\u+00ed #\u+00ee #\u+00ef
    ;; #xf0 
    #\u+fffd #\u+00f1 #\u+00f2 #\u+00f3 #\u+00f4 #\u+0121 #\u+00f6 #\u+00f7
    #\u+011d #\u+00f9 #\u+00fa #\u+00fb #\u+00fc #\u+016d #\u+015d #\u+02d9
    ))

(defstatic *unicode-a0-100-to-iso-8859-3*
  #(
    #xa0 nil nil #xa3 #xa4 nil nil #xa7 ; #xa0-#xa7 
    #xa8 nil nil nil nil #xad nil nil   ; #xa8-#xaf 
    #xb0 nil #xb2 #xb3 #xb4 #xb5 nil #xb7 ; #xb0-#xb7 
    #xb8 nil nil nil nil #xbd nil nil   ; #xb8-#xbf 
    #xc0 #xc1 #xc2 nil #xc4 nil nil #xc7 ; #xc0-#xc7 
    #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #xc8-#xcf 
    nil #xd1 #xd2 #xd3 #xd4 nil #xd6 #xd7 ; #xd0-#xd7 
    nil #xd9 #xda #xdb #xdc nil nil #xdf ; #xd8-#xdf 
    #xe0 #xe1 #xe2 nil #xe4 nil nil #xe7 ; #xe0-#xe7 
    #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #xe8-#xef 
    nil #xf1 #xf2 #xf3 #xf4 nil #xf6 #xf7 ; #xf0-#xf7 
    nil #xf9 #xfa #xfb #xfc nil nil nil ; #xf8-#xff 
    ))

(defstatic *unicode-108-180-to-iso-8859-3*
  #(
    #xc6 #xe6 #xc5 #xe5 #x00 #x00 #x00 #x00 ; #x108-#x10f 
    nil nil nil nil nil nil nil nil     ; #x110-#x117 
    nil nil nil nil #xd8 #xf8 #xab #xbb ; #x118-#x11f 
    #xd5 #xf5 nil nil #xa6 #xb6 #xa1 #xb1 ; #x120-#x127 
    nil nil nil nil nil nil nil nil     ; #x128-#x12f 
    #xa9 #xb9 nil nil #xac #xbc nil nil ; #x130-#x137 
    nil nil nil nil nil nil nil nil     ; #x138-#x13f 
    nil nil nil nil nil nil nil nil     ; #x140-#x147 
    nil nil nil nil nil nil nil nil     ; #x148-#x14f 
    nil nil nil nil nil nil nil nil     ; #x150-#x157 
    nil nil nil nil #xde #xfe #xaa #xba ; #x158-#x15f 
    nil nil nil nil nil nil nil nil     ; #x160-#x167 
    nil nil nil nil #xdd #xfd nil nil   ; #x168-#x16f 
    nil nil nil nil nil nil nil nil     ; #x170-#x177 
    nil nil nil #xaf #xbf nil nil nil   ; #x178-#x17f 
    ))

(defstatic *unicode-2d8-2e0-to-iso-8859-3*
  #(
    #xa2 #xff nil nil nil nil nil nil   ; #x2d8-#x2df 
    ))


    
(define-character-encoding :iso-8859-3
  "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in most
languages used in Southern Europe."

  :aliases '(:iso_8859-3 :latin3 :l3 :csisolatin3)
  :stream-encode-function
  (nfunction
   iso-8859-3-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #x100)
                       (svref *unicode-a0-100-to-iso-8859-3*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x108) (< code #x180))
                       (svref *unicode-108-180-to-iso-8859-3*
                              (the fixnum (- code #x108))))
                      ((and (>= code #x2d8) (< code #x2e0))
                       (svref *unicode-2d8-2e0-to-iso-8859-3*
                              (the fixnum (- code #x2d8)))))))
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-3-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-3-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-3-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                        ((< code #x100)
                         (svref *unicode-a0-100-to-iso-8859-3*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x108) (< code #x180))
                         (svref *unicode-108-180-to-iso-8859-3*
                                (the fixnum (- code #x108))))
                        ((and (>= code #x2d8) (< code #x2e0))
                         (svref *unicode-2d8-2e0-to-iso-8859-3*
                 
               (the fixnum (- code #x2d8)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-3-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
         (let* ((1st-unit (aref vector index)))
           (declare (type (unsigned-byte 8) 1st-unit))
           (setf (schar string i)
                 (if (< 1st-unit #xa0)
                   (code-char 1st-unit)
                   (svref *iso-8859-3-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-3-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                        ((< code #x100)
                         (svref *unicode-a0-100-to-iso-8859-3*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x108) (< code #x180))
                         (svref *unicode-108-180-to-iso-8859-3*
                                (the fixnum (- code #x108))))
                        ((and (>= code #x2d8) (< code #x2e0))
                         (svref *unicode-2d8-2e0-to-iso-8859-3*
                                (the fixnum (- code #x2d8)))))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-3-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-3-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0  
  )


(defstatic *iso-8859-4-to-unicode*
  #(
    ;; #xa0 
    #\u+00a0 #\u+0104 #\u+0138 #\u+0156 #\u+00a4 #\u+0128 #\u+013b #\u+00a7
    #\u+00a8 #\u+0160 #\u+0112 #\u+0122 #\u+0166 #\u+00ad #\u+017d #\u+00af
    ;; #xb0 
    #\u+00b0 #\u+0105 #\u+02db #\u+0157 #\u+00b4 #\u+0129 #\u+013c #\u+02c7
    #\u+00b8 #\u+0161 #\u+0113 #\u+0123 #\u+0167 #\u+014a #\u+017e #\u+014b
    ;; #xc0 
    #\u+0100 #\u+00c1 #\u+00c2 #\u+00c3 #\u+00c4 #\u+00c5 #\u+00c6 #\u+012e
    #\u+010c #\u+00c9 #\u+0118 #\u+00cb #\u+0116 #\u+00cd #\u+00ce #\u+012a
    ;; #xd0 
    #\u+0110 #\u+0145 #\u+014c #\u+0136 #\u+00d4 #\u+00d5 #\u+00d6 #\u+00d7
    #\u+00d8 #\u+0172 #\u+00da #\u+00db #\u+00dc #\u+0168 #\u+016a #\u+00df
    ;; #xe0 
    #\u+0101 #\u+00e1 #\u+00e2 #\u+00e3 #\u+00e4 #\u+00e5 #\u+00e6 #\u+012f
    #\u+010d #\u+00e9 #\u+0119 #\u+00eb #\u+0117 #\u+00ed #\u+00ee #\u+012b
    ;; #xf0 
    #\u+0111 #\u+0146 #\u+014d #\u+0137 #\u+00f4 #\u+00f5 #\u+00f6 #\u+00f7
    #\u+00f8 #\u+0173 #\u+00fa #\u+00fb #\u+00fc #\u+0169 #\u+016b #\u+02d9
    ))


(defstatic *unicode-a0-180-to-iso-8859-4*
  #(
    #xa0 nil nil nil #xa4 nil nil #xa7  ; #xa0-#xa7 
    #xa8 nil nil nil nil #xad nil #xaf  ; #xa8-#xaf 
    #xb0 nil nil nil #xb4 nil nil nil   ; #xb0-#xb7 
    #xb8 nil nil nil nil nil nil nil    ; #xb8-#xbf 
    nil #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 nil ; #xc0-#xc7 
    nil #xc9 nil #xcb nil #xcd #xce nil ; #xc8-#xcf 
    nil nil nil nil #xd4 #xd5 #xd6 #xd7 ; #xd0-#xd7 
    #xd8 nil #xda #xdb #xdc nil nil #xdf ; #xd8-#xdf 
    nil #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 nil ; #xe0-#xe7 
    nil #xe9 nil #xeb nil #xed #xee nil ; #xe8-#xef 
    nil nil nil nil #xf4 #xf5 #xf6 #xf7 ; #xf0-#xf7 
    #xf8 nil #xfa #xfb #xfc nil nil nil ; #xf8-#xff 
    #xc0 #xe0 nil nil #xa1 #xb1 nil nil ; #x100-#x107 
    nil nil nil nil #xc8 #xe8 nil nil   ; #x108-#x10f 
    #xd0 #xf0 #xaa #xba nil nil #xcc #xec ; #x110-#x117 
    #xca #xea nil nil nil nil nil nil   ; #x118-#x11f 
    nil nil #xab #xbb nil nil nil nil   ; #x120-#x127 
    #xa5 #xb5 #xcf #xef nil nil #xc7 #xe7 ; #x128-#x12f 
    nil nil nil nil nil nil #xd3 #xf3   ; #x130-#x137 
    #xa2 nil nil #xa6 #xb6 nil nil nil  ; #x138-#x13f 
    nil nil nil nil nil #xd1 #xf1 nil   ; #x140-#x147 
    nil nil #xbd #xbf #xd2 #xf2 nil nil ; #x148-#x14f 
    nil nil nil nil nil nil #xa3 #xb3   ; #x150-#x157 
    nil nil nil nil nil nil nil nil     ; #x158-#x15f 
    #xa9 #xb9 nil nil nil nil #xac #xbc ; #x160-#x167 
    #xdd #xfd #xde #xfe nil nil nil nil ; #x168-#x16f 
    nil nil #xd9 #xf9 nil nil nil nil   ; #x170-#x177 
    nil nil nil nil nil #xae #xbe nil   ; #x178-#x17f 
    ))

(defstatic *unicode-2c0-2e0-to-iso-8859-4*
  #(
    nil nil nil nil nil nil nil #xb7    ; #x2c0-#x2c7
    nil nil nil nil nil nil nil nil     ; #x2c8-#x2cf
    nil nil nil nil nil nil nil nil     ; #x2d0-#x2d7
    nil #xff nil #xb2 nil nil nil nil   ; #x2d8-#x2df
    ))



(define-character-encoding :iso-8859-4
  "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in most
languages used in Northern Europe."

  :aliases '(:iso_8859-4 :latin4 :l4 :csisolatin4)
  :stream-encode-function
  (nfunction
   iso-8859-4-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso-8859-4*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x2d8) (< code #x2e0))
                       (svref *unicode-2c0-2e0-to-iso-8859-4*
                              (the fixnum (- code #x2c0)))))))
                      
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-4-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-4-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-4-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                        ((< code #x180)
                         (svref *unicode-a0-180-to-iso-8859-4*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x2d8) (< code #x2e0))
                         (svref *unicode-2c0-2e0-to-iso-8859-4*
                                (the fixnum (- code #x2c0)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-4-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-4-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-4-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                        ((< code #x180)
                         (svref *unicode-a0-180-to-iso-8859-4*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x2d8) (< code #x2e0))
                         (svref *unicode-2c0-2e0-to-iso-8859-4*
                                (the fixnum (- code #x2c0)))))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-4-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-4-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0  
  )

(defstatic *iso-8859-5-to-unicode*
  #(
    ;; #xa0
    #\u+00a0 #\u+0401 #\u+0402 #\u+0403 #\u+0404 #\u+0405 #\u+0406 #\u+0407
    #\u+0408 #\u+0409 #\u+040a #\u+040b #\u+040c #\u+00ad #\u+040e #\u+040f
    ;; #xb0
    #\u+0410 #\u+0411 #\u+0412 #\u+0413 #\u+0414 #\u+0415 #\u+0416 #\u+0417
    #\u+0418 #\u+0419 #\u+041a #\u+041b #\u+041c #\u+041d #\u+041e #\u+041f
    ;; #xc0
    #\u+0420 #\u+0421 #\u+0422 #\u+0423 #\u+0424 #\u+0425 #\u+0426 #\u+0427
    #\u+0428 #\u+0429 #\u+042a #\u+042b #\u+042c #\u+042d #\u+042e #\u+042f
    ;; #xd0
    #\u+0430 #\u+0431 #\u+0432 #\u+0433 #\u+0434 #\u+0435 #\u+0436 #\u+0437
    #\u+0438 #\u+0439 #\u+043a #\u+043b #\u+043c #\u+043d #\u+043e #\u+043f
    ;; #xe0
    #\u+0440 #\u+0441 #\u+0442 #\u+0443 #\u+0444 #\u+0445 #\u+0446 #\u+0447
    #\u+0448 #\u+0449 #\u+044a #\u+044b #\u+044c #\u+044d #\u+044e #\u+044f
    ;; #xf0
    #\u+2116 #\u+0451 #\u+0452 #\u+0453 #\u+0454 #\u+0455 #\u+0456 #\u+0457
    #\u+0458 #\u+0459 #\u+045a #\u+045b #\u+045c #\u+00a7 #\u+045e #\u+045f
    ))


(defstatic *unicode-a0-b0-to-iso-8859-5*
  #(
    #xa0 nil nil nil nil nil nil #xfd   ; #xa0-#xa7
    nil nil nil nil nil #xad nil nil    ; #xa8-#xaf
    ))

(defstatic *unicode-400-460-to-iso-8859-5*
  #(
    nil #xa1 #xa2 #xa3 #xa4 #xa5 #xa6 #xa7 ; #x400-#x407
    #xa8 #xa9 #xaa #xab #xac nil #xae #xaf ; #x408-#x40f
    #xb0 #xb1 #xb2 #xb3 #xb4 #xb5 #xb6 #xb7 ; #x410-#x417
    #xb8 #xb9 #xba #xbb #xbc #xbd #xbe #xbf ; #x418-#x41f
    #xc0 #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 ; #x420-#x427
    #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #x428-#x42f
    #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7 ; #x430-#x437
    #xd8 #xd9 #xda #xdb #xdc #xdd #xde #xdf ; #x438-#x43f
    #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #x440-#x447
    #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #x448-#x44f
    nil #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 ; #x450-#x457
    #xf8 #xf9 #xfa #xfb #xfc nil #xfe #xff ; #x458-#x45f
    ))


(define-character-encoding :iso-8859-5
  "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in the
Cyrillic alphabet."

  :aliases '(:iso_8859-5 :cyrillic :csISOLatinCyrillic :iso-ir-144)
  :stream-encode-function
  (nfunction
   iso-8859-5-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #xb0)
                       (svref *unicode-a0-b0-to-iso-8859-5*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x400) (< code #x460))
                       (svref *unicode-400-460-to-iso-8859-5*
                              (the fixnum (- code #x400)))))))
                      
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-5-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-5-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-5-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                        ((< code #xb0)
                         (svref *unicode-a0-b0-to-iso-8859-5*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x400) (< code #x460))
                         (svref *unicode-400-460-to-iso-8859-5*
                                (the fixnum (- code #x400)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-5-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-5-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-5-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                        ((< code #xb0)
                         (svref *unicode-a0-b0-to-iso-8859-5*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x400) (< code #x460))
                         (svref *unicode-400-460-to-iso-8859-5*
                                (the fixnum (- code #x400)))))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-5-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-5-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0
  )

(defstatic *iso-8859-6-to-unicode*
  #(
    ;; #xa0 
    #\u+00a0 #\u+fffd #\u+fffd #\u+fffd #\u+00a4 #\u+fffd #\u+fffd #\u+fffd
    #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+060c #\u+00ad #\u+fffd #\u+fffd
    ;; #xb0 
    #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd
    #\u+fffd #\u+fffd #\u+fffd #\u+061b #\u+fffd #\u+fffd #\u+fffd #\u+061f
    ;; #xc0 
    #\u+fffd #\u+0621 #\u+0622 #\u+0623 #\u+0624 #\u+0625 #\u+0626 #\u+0627
    #\u+0628 #\u+0629 #\u+062a #\u+062b #\u+062c #\u+062d #\u+062e #\u+062f
    ;; #xd0 
    #\u+0630 #\u+0631 #\u+0632 #\u+0633 #\u+0634 #\u+0635 #\u+0636 #\u+0637
    #\u+0638 #\u+0639 #\u+063a #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd
    ;; #xe0 
    #\u+0640 #\u+0641 #\u+0642 #\u+0643 #\u+0644 #\u+0645 #\u+0646 #\u+0647
    #\u+0648 #\u+0649 #\u+064a #\u+064b #\u+064c #\u+064d #\u+064e #\u+064f
    ;; #xf0 
    #\u+0650 #\u+0651 #\u+0652 #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd
    #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd
    ))

(defstatic *unicode-a0-b0-to-iso-8859-6*
  #(
    0xa0 nil nil nil 0xa4 nil nil nil   ; #xa0-#xa7
    nil nil nil nil nil #xad nil nil    ; #xa8-#xaf
    ))


(defstatic *unicode-608-658-to-iso-8859-6*
  #(
    nil nil nil nil #xac nil nil nil    ; #x608-#x60f
    nil nil nil nil nil nil nil nil     ; #x610-#x617
    nil nil nil #xbb nil nil nil #xbf   ; #x618-#x61f
    nil #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 ; #x620-#x627
    #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #x628-#x62f
    #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7 ; #x630-#x637
    #xd8 #xd9 #xda nil nil nil nil nil  ; #x638-#x63f
    #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #x640-#x647
    #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #x648-#x64f
    #xf0 #xf1 #xf2 nil nil nil nil nil  ; #x650-#x657
    ))

(define-character-encoding :iso-8859-6
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in the
Arabic alphabet."

  :aliases '(:iso_8859-6 :arabic :csISOLatinArabic :iso-ir-127)
  :stream-encode-function
  (nfunction
   iso-8859-6-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #xb0)
                       (svref *unicode-a0-b0-to-iso-8859-6*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x608) (< code #x658))
                       (svref *unicode-608-658-to-iso-8859-6*
                              (the fixnum (- code #x608)))))))
                      
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-6-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-6-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-6-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                        ((< code #xb0)
                         (svref *unicode-a0-b0-to-iso-8859-6*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x608) (< code #x658))
                         (svref *unicode-608-658-to-iso-8859-6*
                                (the fixnum (- code #x608)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-6-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-6-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-6-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                        ((< code #xb0)
                         (svref *unicode-a0-b0-to-iso-8859-6*
                                (the fixnum (- code #xa0))))
                        ((and (>= code #x608) (< code #x658))
                         (svref *unicode-608-658-to-iso-8859-6*
                                (the fixnum (- code #x608)))))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-6-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-6-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0  
  )

(defstatic *iso-8859-7-to-unicode*
  #(
    ;; #xa0
    #\u+00a0 #\u+2018 #\u+2019 #\u+00a3 #\u+20ac #\u+20af #\u+00a6 #\u+00a7
    #\u+00a8 #\u+00a9 #\u+037a #\u+00ab #\u+00ac #\u+00ad #\u+fffd #\u+2015
    ;; #xb0
    #\u+00b0 #\u+00b1 #\u+00b2 #\u+00b3 #\u+0384 #\u+0385 #\u+0386 #\u+00b7
    #\u+0388 #\u+0389 #\u+038a #\u+00bb #\u+038c #\u+00bd #\u+038e #\u+038f
    ;; #xc0
    #\u+0390 #\u+0391 #\u+0392 #\u+0393 #\u+0394 #\u+0395 #\u+0396 #\u+0397
    #\u+0398 #\u+0399 #\u+039a #\u+039b #\u+039c #\u+039d #\u+039e #\u+039f
    ;; #xd0
    #\u+03a0 #\u+03a1 #\u+fffd #\u+03a3 #\u+03a4 #\u+03a5 #\u+03a6 #\u+03a7
    #\u+03a8 #\u+03a9 #\u+03aa #\u+03ab #\u+03ac #\u+03ad #\u+03ae #\u+03af
    ;; #xe0
    #\u+03b0 #\u+03b1 #\u+03b2 #\u+03b3 #\u+03b4 #\u+03b5 #\u+03b6 #\u+03b7
    #\u+03b8 #\u+03b9 #\u+03ba #\u+03bb #\u+03bc #\u+03bd #\u+03be #\u+03bf
    ;; #xf0
    #\u+03c0 #\u+03c1 #\u+03c2 #\u+03c3 #\u+03c4 #\u+03c5 #\u+03c6 #\u+03c7
    #\u+03c8 #\u+03c9 #\u+03ca #\u+03cb #\u+03cc #\u+03cd #\u+03ce #\u+fffd
    ))

(defstatic *unicode-a0-c0-to-iso-8859-7*
  #(
    #xa0 nil nil #xa3 nil nil #xa6 #xa7 ; #xa0-#xa7
    #xa8 #xa9 nil #xab #xac #xad nil nil ; #xa8-#xaf
    #xb0 #xb1 #xb2 #xb3 nil nil nil #xb7 ; #xb0-#xb7
    nil nil nil #xbb nil #xbd nil nil   ; #xb8-#xbf
    ))

(defstatic *unicode-378-3d0-to-iso-8859-7*
  #(
    nil nil #xaa nil nil nil nil nil    ; #x378-#x37f 
    nil nil nil nil #xb4 #xb5 #xb6 nil  ; #x380-#x387 
    #xb8 #xb9 #xba nil #xbc nil #xbe #xbf ; #x388-#x38f 
    #xc0 #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 ; #x390-#x397 
    #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #x398-#x39f 
    #xd0 #xd1 nil #xd3 #xd4 #xd5 #xd6 #xd7 ; #x3a0-#x3a7 
    #xd8 #xd9 #xda #xdb #xdc #xdd #xde #xdf ; #x3a8-#x3af 
    #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #x3b0-#x3b7 
    #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #x3b8-#x3bf 
    #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 ; #x3c0-#x3c7 
    #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe nil ; #x3c8-#x3cf 
    ))

(defstatic *unicode-2010-2020-to-iso-8859-7*
  #(
    nil nil nil nil nil #xaf nil nil    ; #x2010-#x2017 
    #xa1 #xa2 nil nil nil nil nil nil   ; #x2018-#x201f 
    ))

(defstatic *unicode-20ac-20b0-to-iso-8859-7*
  #(
    #xa4 nil nil #xa5
    ))

(define-character-encoding :iso-8859-7
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in the
Greek alphabet."

  :aliases '(:iso_8859-7 :greek  :greek8 :csISOLatinGreek :iso-ir-126 :ELOT_928 :ecma-118)
  :stream-encode-function
  (nfunction
   iso-8859-7-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #xc0)
                       (svref *unicode-a0-c0-to-iso-8859-7*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x378) (< code #x3d0))
                       (svref *unicode-378-3d0-to-iso-8859-7*
                              (the fixnum (- code #x378))))
                      ((and (>= code #x2010) (< code #x2020))
                       (svref *unicode-2010-2020-to-iso-8859-7*
                              (the fixnum (- code #x2010))))
                      ((and (>= code #x20ac) (< code #x20b0))
                       (svref *unicode-20ac-20b0-to-iso-8859-7*
                              (the fixnum (- code #x20ac)))))))
              
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-7-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-7-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-7-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                      ((< code #xc0)
                       (svref *unicode-a0-c0-to-iso-8859-7*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x378) (< code #x3d0))
                       (svref *unicode-378-3d0-to-iso-8859-7*
                              (the fixnum (- code #x378))))
                      ((and (>= code #x2010) (< code #x2020))
                       (svref *unicode-2010-2020-to-iso-8859-7*
                              (the fixnum (- code #x2010))))
                      ((and (>= code #x20ac) (< code #x20b0))
                       (svref *unicode-20ac-20b0-to-iso-8859-7*
                              (the fixnum (- code #x20ac)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-7-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-7-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-7-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                      ((< code #xc0)
                       (svref *unicode-a0-c0-to-iso-8859-7*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x378) (< code #x3d0))
                       (svref *unicode-378-3d0-to-iso-8859-7*
                              (the fixnum (- code #x378))))
                      ((and (>= code #x2010) (< code #x2020))
                       (svref *unicode-2010-2020-to-iso-8859-7*
                              (the fixnum (- code #x2010))))
                      ((and (>= code #x20ac) (< code #x20b0))
                       (svref *unicode-20ac-20b0-to-iso-8859-7*
                              (the fixnum (- code #x20ac)))))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-7-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-7-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0  
  )

(defstatic *iso-8859-8-to-unicode*
  #(
    ;; #xa0
    #\u+00a0 #\u+fffd #\u+00a2 #\u+00a3 #\u+00a4 #\u+00a5 #\u+00a6 #\u+00a7
    #\u+00a8 #\u+00a9 #\u+00d7 #\u+00ab #\u+00ac #\u+00ad #\u+00ae #\u+00af
    ;; #xb0
    #\u+00b0 #\u+00b1 #\u+00b2 #\u+00b3 #\u+00b4 #\u+00b5 #\u+00b6 #\u+00b7
    #\u+00b8 #\u+00b9 #\u+00f7 #\u+00bb #\u+00bc #\u+00bd #\u+00be #\u+fffd
    ;; #xc0
    #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd
    #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd
    ;; #xd0
    #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd
    #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+fffd #\u+2017
    ;; #xe0
    #\u+05d0 #\u+05d1 #\u+05d2 #\u+05d3 #\u+05d4 #\u+05d5 #\u+05d6 #\u+05d7
    #\u+05d8 #\u+05d9 #\u+05da #\u+05db #\u+05dc #\u+05dd #\u+05de #\u+05df
    ;; #xf0
    #\u+05e0 #\u+05e1 #\u+05e2 #\u+05e3 #\u+05e4 #\u+05e5 #\u+05e6 #\u+05e7
    #\u+05e8 #\u+05e9 #\u+05ea #\u+fffd #\u+fffd #\u+200e #\u+200f #\u+fffd
    ))

(defstatic *unicode-a0-f8-to-iso-8859-8*
  #(
    #xa0 nil #xa2 #xa3 #xa4 #xa5 #xa6 #xa7 ; #xa0-#xa7 
    #xa8 #xa9 nil #xab #xac #xad #xae #xaf ; #xa8-#xaf 
    #xb0 #xb1 #xb2 #xb3 #xb4 #xb5 #xb6 #xb7 ; #xb0-#xb7 
    #xb8 #xb9 nil #xbb #xbc #xbd #xbe nil ; #xb8-#xbf 
    nil nil nil nil nil nil nil nil     ; #xc0-#xc7 
    nil nil nil nil nil nil nil nil     ; #xc8-#xcf 
    nil nil nil nil nil nil nil #xaa    ; #xd0-#xd7 
    nil nil nil nil nil nil nil nil     ; #xd8-#xdf 
    nil nil nil nil nil nil nil nil     ; #xe0-#xe7 
    nil nil nil nil nil nil nil nil     ; #xe8-#xef 
    nil nil nil nil nil nil nil #xba    ; #xf0-#xf7 
    ))

(defstatic *unicode-5d0-5f0-to-iso-8859-8*
  #(
    #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #x5d0-#x5d7
    #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #x5d8-#x5df
    #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 ; #x5e0-#x5e7
    #xf8 #xf9 #xfa nil nil nil nil nil  ; #x5e8-#x5ef
    ))

(defstatic *unicode-2008-2018-to-iso-8859-8*
  #(
    nil nil nil nil nil nil #xfd #xfe   ; #x2008-#x200f 
    nil nil nil nil nil nil nil #xdf    ; #x2010-#x2017 
    ))    

(define-character-encoding :iso-8859-8
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in the
Hebrew alphabet."

  :aliases '(:iso_8859-8 :hebrew :csISOLatinHebrew :iso-ir-138)
  :stream-encode-function
  (nfunction
   iso-8859-8-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #xf8)
                       (svref *unicode-a0-f8-to-iso-8859-8*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x5d0) (< code #x5f0))
                       (svref *unicode-5d0-5f0-to-iso-8859-8*
                              (the fixnum (- code #x5d0))))
                      ((and (>= code #x2008) (< code #x2018))
                       (svref *unicode-2008-2018-to-iso-8859-8*
                              (the fixnum (- code #x2008)))))))
              
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-8-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-8-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-8-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                      ((< code #xf8)
                       (svref *unicode-a0-f8-to-iso-8859-8*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x5d0) (< code #x5f0))
                       (svref *unicode-5d0-5f0-to-iso-8859-8*
                              (the fixnum (- code #x5d0))))
                      ((and (>= code #x2008) (< code #x2018))
                       (svref *unicode-2008-2018-to-iso-8859-8*
                              (the fixnum (- code #x2008)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-8-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-8-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-8-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                      ((< code #xf8)
                       (svref *unicode-a0-f8-to-iso-8859-8*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x5d0) (< code #x5f0))
                       (svref *unicode-5d0-5f0-to-iso-8859-8*
                              (the fixnum (- code #x5d0))))
                      ((and (>= code #x2008) (< code #x2018))
                       (svref *unicode-2008-2018-to-iso-8859-8*
                              (the fixnum (- code #x2008)))))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-8-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-8-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0  
  )

(defstatic *iso-8859-9-to-unicode*
  #(
    ;; #xd0
    #\u+011e #\u+00d1 #\u+00d2 #\u+00d3 #\u+00d4 #\u+00d5 #\u+00d6 #\u+00d7
    #\u+00d8 #\u+00d9 #\u+00da #\u+00db #\u+00dc #\u+0130 #\u+015e #\u+00df
    ;; #xe0
    #\u+00e0 #\u+00e1 #\u+00e2 #\u+00e3 #\u+00e4 #\u+00e5 #\u+00e6 #\u+00e7
    #\u+00e8 #\u+00e9 #\u+00ea #\u+00eb #\u+00ec #\u+00ed #\u+00ee #\u+00ef
    ;; #xf0
    #\u+011f #\u+00f1 #\u+00f2 #\u+00f3 #\u+00f4 #\u+00f5 #\u+00f6 #\u+00f7
    #\u+00f8 #\u+00f9 #\u+00fa #\u+00fb #\u+00fc #\u+0131 #\u+015f #\u+00ff
    ))

(defstatic *unicode-d0-100-to-iso-8859-9*
  #(
    nil #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7 ; #xd0-#xd7
    #xd8 #xd9 #xda #xdb #xdc nil nil #xdf ; #xd8-#xdf
    #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #xe0-#xe7
    #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #xe8-#xef
    nil #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 ; #xf0-#xf7
    #xf8 #xf9 #xfa #xfb #xfc nil nil #xff ; #xf8-#xff
    ))

(defstatic *unicode-118-160-to-iso-8859-9*
  #(
    nil nil nil nil nil nil #xd0 #xf0   ; #x118-#x11f 
    nil nil nil nil nil nil nil nil     ; #x120-#x127 
    nil nil nil nil nil nil nil nil     ; #x128-#x12f 
    #xdd #xfd nil nil nil nil nil nil   ; #x130-#x137 
    nil nil nil nil nil nil nil nil     ; #x138-#x13f 
    nil nil nil nil nil nil nil nil     ; #x140-#x147 
    nil nil nil nil nil nil nil nil     ; #x148-#x14f 
    nil nil nil nil nil nil nil nil     ; #x150-#x157 
    nil nil nil nil nil nil #xde #xfe   ; #x158-#x15f 
    ))


(define-character-encoding :iso-8859-9
    "An 8-bit, fixed-width character encoding in which codes #x00-#xcf
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in the
Turkish alphabet."

  :aliases '(:iso_8859-9 :latin5 :csISOLatin5 :iso-ir-148)
  :stream-encode-function
  (nfunction
   iso-8859-9-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xd0) code)
                      ((< code #x100)
                       (svref *unicode-d0-100-to-iso-8859-9*
                              (the fixnum (- code #xd0))))
                      ((and (>= code #x118) (< code #x160))
                       (svref *unicode-118-160-to-iso-8859-9*
                              (the fixnum (- code #x118)))))))
              
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-9-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-9-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-9-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xd0) code)
                      ((< code #x100)
                       (svref *unicode-d0-100-to-iso-8859-9*
                              (the fixnum (- code #xd0))))
                      ((and (>= code #x118) (< code #x160))
                       (svref *unicode-118-160-to-iso-8859-9*
                              (the fixnum (- code #x118)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-9-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-9-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-9-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xd0) code)
                      ((< code #x100)
                       (svref *unicode-d0-100-to-iso-8859-9*
                              (the fixnum (- code #xd0))))
                      ((and (>= code #x118) (< code #x160))
                       (svref *unicode-118-160-to-iso-8859-9*
                              (the fixnum (- code #x118)))))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-9-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-9-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xd0
  :encode-literal-char-code-limit #xa0
  )

(defstatic *iso-8859-10-to-unicode*
  #(
    ;; #xa0
    #\u+00a0 #\u+0104 #\u+0112 #\u+0122 #\u+012a #\u+0128 #\u+0136 #\u+00a7
    #\u+013b #\u+0110 #\u+0160 #\u+0166 #\u+017d #\u+00ad #\u+016a #\u+014a
    ;; #xb0
    #\u+00b0 #\u+0105 #\u+0113 #\u+0123 #\u+012b #\u+0129 #\u+0137 #\u+00b7
    #\u+013c #\u+0111 #\u+0161 #\u+0167 #\u+017e #\u+2015 #\u+016b #\u+014b
    ;; #xc0
    #\u+0100 #\u+00c1 #\u+00c2 #\u+00c3 #\u+00c4 #\u+00c5 #\u+00c6 #\u+012e
    #\u+010c #\u+00c9 #\u+0118 #\u+00cb #\u+0116 #\u+00cd #\u+00ce #\u+00cf
    ;; #xd0
    #\u+00d0 #\u+0145 #\u+014c #\u+00d3 #\u+00d4 #\u+00d5 #\u+00d6 #\u+0168
    #\u+00d8 #\u+0172 #\u+00da #\u+00db #\u+00dc #\u+00dd #\u+00de #\u+00df
    ;; #xe0
    #\u+0101 #\u+00e1 #\u+00e2 #\u+00e3 #\u+00e4 #\u+00e5 #\u+00e6 #\u+012f
    #\u+010d #\u+00e9 #\u+0119 #\u+00eb #\u+0117 #\u+00ed #\u+00ee #\u+00ef
    ;; #xf0
    #\u+00f0 #\u+0146 #\u+014d #\u+00f3 #\u+00f4 #\u+00f5 #\u+00f6 #\u+0169
    #\u+00f8 #\u+0173 #\u+00fa #\u+00fb #\u+00fc #\u+00fd #\u+00fe #\u+0138
    ))

(defstatic *unicode-a0-180-to-iso-8859-10*
  #(
    #xa0 nil nil nil nil nil nil #xa7   ; #xa0-#xa7 
    nil nil nil nil nil #xad nil nil    ; #xa8-#xaf 
    #xb0 nil nil nil nil nil nil #xb7   ; #xb0-#xb7 
    nil nil nil nil nil nil nil nil     ; #xb8-#xbf 
    nil #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 nil ; #xc0-#xc7 
    nil #xc9 nil #xcb nil #xcd #xce #xcf ; #xc8-#xcf 
    #xd0 nil nil #xd3 #xd4 #xd5 #xd6 nil ; #xd0-#xd7 
    #xd8 nil #xda #xdb #xdc #xdd #xde #xdf ; #xd8-#xdf 
    nil #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 nil ; #xe0-#xe7 
    nil #xe9 nil #xeb nil #xed #xee #xef ; #xe8-#xef 
    #xf0 nil nil #xf3 #xf4 #xf5 #xf6 nil ; #xf0-#xf7 
    #xf8 nil #xfa #xfb #xfc #xfd #xfe nil ; #xf8-#xff 
    #xc0 #xe0 nil nil #xa1 #xb1 nil nil ; #x100-#x107 
    nil nil nil nil #xc8 #xe8 nil nil   ; #x108-#x10f 
    #xa9 #xb9 #xa2 #xb2 nil nil #xcc #xec ; #x110-#x117 
    #xca #xea nil nil nil nil nil nil   ; #x118-#x11f 
    nil nil #xa3 #xb3 nil nil nil nil   ; #x120-#x127 
    #xa5 #xb5 #xa4 #xb4 nil nil #xc7 #xe7 ; #x128-#x12f 
    nil nil nil nil nil nil #xa6 #xb6   ; #x130-#x137 
    #xff nil nil #xa8 #xb8 nil nil nil  ; #x138-#x13f 
    nil nil nil nil nil #xd1 #xf1 nil   ; #x140-#x147 
    nil nil #xaf #xbf #xd2 #xf2 nil nil ; #x148-#x14f 
    nil nil nil nil nil nil nil nil     ; #x150-#x157 
    nil nil nil nil nil nil nil nil     ; #x158-#x15f 
    #xaa #xba nil nil nil nil #xab #xbb ; #x160-#x167 
    #xd7 #xf7 #xae #xbe nil nil nil nil ; #x168-#x16f 
    nil nil #xd9 #xf9 nil nil nil nil   ; #x170-#x177 
    nil nil nil nil nil #xac #xbc nil   ; #x178-#x17f 
    ))

(define-character-encoding :iso-8859-10
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in Nordic
alphabets."

  :aliases '(:iso_8859-10 :latin6 :csISOLatin6 :iso-ir-157)
  :stream-encode-function
  (nfunction
   iso-8859-10-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso-8859-10*
                              (the fixnum (- code #xa0)))))))
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-10-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-10-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-10-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso-8859-10*
                              (the fixnum (- code #xa0)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-10-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-10-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-10-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso-8859-10*
                              (the fixnum (- code #xa0)))))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-10-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-10-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0  
  )

(define-character-encoding :iso-8859-11
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found the  Thai
alphabet."
  :aliases '()
  :stream-encode-function
  (nfunction
   iso-8859-11-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa1) code)
                      ((and (<= code #xfb)
                            (not (and (>= code #xdb) (<= code #xde))))
                       (+ code #x0d60)))))
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-11-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa1)
       (code-char 1st-unit)
       (if (and (>= 1st-unit #xe01)
                (<= 1st-unit #xe5b)
                (not (and (>= 1st-unit #xe3b)
                          (<= 1st-unit #xe3e))))
         (code-char (- 1st-unit #xd60))
         #\Replacement_Character))))
  :vector-encode-function
  (nfunction
   iso-8859-11-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa1) code)
                      ((and (<= code #xfb)
                            (not (and (>= code #xdb) (<= code #xde))))
                       (+ code #x0d60)))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-11-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa1)
                 (code-char 1st-unit)
                 (if (and (>= 1st-unit #xe01)
                          (<= 1st-unit #xe5b)
                          (not (and (>= 1st-unit #xe3b)
                                    (<= 1st-unit #xe3e))))
                   (code-char (- 1st-unit #xd60))
                   #\Replacement_Character)))))))
  :memory-encode-function
  (nfunction
   iso-8859-11-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa1) code)
                      ((and (<= code #xfb)
                            (not (and (>= code #xdb) (<= code #xde))))
                       (+ code #x0d60)))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-11-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa1)
                 (code-char 1st-unit)
                 (if (and (>= 1st-unit #xe01)
                          (<= 1st-unit #xe5b)
                          (not (and (>= 1st-unit #xe3b)
                                    (<= 1st-unit #xe3e))))
                   (code-char (- 1st-unit #xd60))
                   #\Replacement_Character)))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0  
  )

;;; There is no iso-8859-12 encoding.

(defstatic *iso-8859-13-to-unicode*
  #(
    ;; #xa0
    #\u+00a0 #\u+201d #\u+00a2 #\u+00a3 #\u+00a4 #\u+201e #\u+00a6 #\u+00a7
    #\u+00d8 #\u+00a9 #\u+0156 #\u+00ab #\u+00ac #\u+00ad #\u+00ae #\u+00c6
    ;; #xb0
    #\u+00b0 #\u+00b1 #\u+00b2 #\u+00b3 #\u+201c #\u+00b5 #\u+00b6 #\u+00b7
    #\u+00f8 #\u+00b9 #\u+0157 #\u+00bb #\u+00bc #\u+00bd #\u+00be #\u+00e6
    ;; #xc0
    #\u+0104 #\u+012e #\u+0100 #\u+0106 #\u+00c4 #\u+00c5 #\u+0118 #\u+0112
    #\u+010c #\u+00c9 #\u+0179 #\u+0116 #\u+0122 #\u+0136 #\u+012a #\u+013b
    ;; #xd0
    #\u+0160 #\u+0143 #\u+0145 #\u+00d3 #\u+014c #\u+00d5 #\u+00d6 #\u+00d7
    #\u+0172 #\u+0141 #\u+015a #\u+016a #\u+00dc #\u+017b #\u+017d #\u+00df
    ;; #xe0
    #\u+0105 #\u+012f #\u+0101 #\u+0107 #\u+00e4 #\u+00e5 #\u+0119 #\u+0113
    #\u+010d #\u+00e9 #\u+017a #\u+0117 #\u+0123 #\u+0137 #\u+012b #\u+013c
    ;; #xf0
    #\u+0161 #\u+0144 #\u+0146 #\u+00f3 #\u+014d #\u+00f5 #\u+00f6 #\u+00f7
    #\u+0173 #\u+0142 #\u+015b #\u+016b #\u+00fc #\u+017c #\u+017e #\u+2019
    ))

(defstatic *unicode-a0-180-to-iso-8859-13*
  #(
    #xa0 nil #xa2 #xa3 #xa4 nil #xa6 #xa7 ; #xa0-#xa7
    nil #xa9 nil #xab #xac #xad #xae nil ; #xa8-#xaf
    #xb0 #xb1 #xb2 #xb3 nil #xb5 #xb6 #xb7 ; #xb0-#xb7
    nil #xb9 nil #xbb #xbc #xbd #xbe nil ; #xb8-#xbf
    nil nil nil nil #xc4 #xc5 #xaf nil ; #xc0-#xc7
    nil #xc9 nil nil nil nil nil nil ; #xc8-#xcf
    nil nil nil #xd3 nil #xd5 #xd6 #xd7 ; #xd0-#xd7
    #xa8 nil nil nil #xdc nil nil #xdf ; #xd8-#xdf
    nil nil nil nil #xe4 #xe5 #xbf nil ; #xe0-#xe7
    nil #xe9 nil nil nil nil nil nil ; #xe8-#xef
    nil nil nil #xf3 nil #xf5 #xf6 #xf7 ; #xf0-#xf7
    #xb8 nil nil nil #xfc nil nil nil ; #xf8-#xff
    #xc2 #xe2 nil nil #xc0 #xe0 #xc3 #xe3 ; #x100-#x107
    nil nil nil nil #xc8 #xe8 nil nil ; #x108-#x10f
    nil nil #xc7 #xe7 nil nil #xcb #xeb ; #x110-#x117
    #xc6 #xe6 nil nil nil nil nil nil ; #x118-#x11f
    nil nil #xcc #xec nil nil nil nil ; #x120-#x127
    nil nil #xce #xee nil nil #xc1 #xe1 ; #x128-#x12f
    nil nil nil nil nil nil #xcd #xed ; #x130-#x137
    nil nil nil #xcf #xef nil nil nil ; #x138-#x13f
    nil #xd9 #xf9 #xd1 #xf1 #xd2 #xf2 nil ; #x140-#x147
    nil nil nil nil #xd4 #xf4 nil nil ; #x148-#x14f
    nil nil nil nil nil nil #xaa #xba ; #x150-#x157
    nil nil #xda #xfa nil nil nil nil ; #x158-#x15f
    #xd0 #xf0 nil nil nil nil nil nil ; #x160-#x167
    nil nil #xdb #xfb nil nil nil nil ; #x168-#x16f
    nil nil #xd8 #xf8 nil nil nil nil ; #x170-#x177
    nil #xca #xea #xdd #xfd #xde #xfe nil ; #x178-#x17f
    ))

(defstatic *unicode-2018-2020-to-iso-8859-13*
  #(
    nil #xff nil nil #xb4 #xa1 #xa5 nil ; #x2018-#x201f */
    ))


(define-character-encoding :iso-8859-13
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in Baltic
alphabets."

  :aliases '()
  :stream-encode-function
  (nfunction
   iso-8859-13-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso-8859-13*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x2018)
                            (< code #x2020))
                       (svref *unicode-2018-2020-to-iso-8859-13*
                              (the fixnum (- code #x2018)))))))
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-13-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-13-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-13-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso-8859-13*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x2018)
                            (< code #x2020))
                       (svref *unicode-2018-2020-to-iso-8859-13*
                              (the fixnum (- code #x2018)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-13-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-13-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-13-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso-8859-13*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x2018)
                            (< code #x2020))
                       (svref *unicode-2018-2020-to-iso-8859-13*
                              (the fixnum (- code #x2018)))))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-13-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-13-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0  
  )

(defstatic *iso-8859-14-to-unicode*
  #(
    ;; #xa0
    #\u+00a0 #\u+1e02 #\u+1e03 #\u+00a3 #\u+010a #\u+010b #\u+1e0a #\u+00a7
    #\u+1e80 #\u+00a9 #\u+1e82 #\u+1e0b #\u+1ef2 #\u+00ad #\u+00ae #\u+0178
    ;; #xb0
    #\u+1e1e #\u+1e1f #\u+0120 #\u+0121 #\u+1e40 #\u+1e41 #\u+00b6 #\u+1e56
    #\u+1e81 #\u+1e57 #\u+1e83 #\u+1e60 #\u+1ef3 #\u+1e84 #\u+1e85 #\u+1e61
    ;; #xc0
    #\u+00c0 #\u+00c1 #\u+00c2 #\u+00c3 #\u+00c4 #\u+00c5 #\u+00c6 #\u+00c7
    #\u+00c8 #\u+00c9 #\u+00ca #\u+00cb #\u+00cc #\u+00cd #\u+00ce #\u+00cf
    ;; #xd0
    #\u+0174 #\u+00d1 #\u+00d2 #\u+00d3 #\u+00d4 #\u+00d5 #\u+00d6 #\u+1e6a
    #\u+00d8 #\u+00d9 #\u+00da #\u+00db #\u+00dc #\u+00dd #\u+0176 #\u+00df
    ;; #xe0
    #\u+00e0 #\u+00e1 #\u+00e2 #\u+00e3 #\u+00e4 #\u+00e5 #\u+00e6 #\u+00e7
    #\u+00e8 #\u+00e9 #\u+00ea #\u+00eb #\u+00ec #\u+00ed #\u+00ee #\u+00ef
    ;; #xf0
    #\u+0175 #\u+00f1 #\u+00f2 #\u+00f3 #\u+00f4 #\u+00f5 #\u+00f6 #\u+1e6b
    #\u+00f8 #\u+00f9 #\u+00fa #\u+00fb #\u+00fc #\u+00fd #\u+0177 #\u+00ff
    ))

(defstatic *unicode-a0-100-to-iso-8859-14*
  #(
    #xa0 nil nil #xa3 nil nil nil #xa7  ; #xa0-#xa7
    nil #xa9 nil nil nil #xad #xae nil  ; #xa8-#xaf
    nil nil nil nil nil nil #xb6 nil    ; #xb0-#xb7
    nil nil nil nil nil nil nil nil     ; #xb8-#xbf
    #xc0 #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 ; #xc0-#xc7
    #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #xc8-#xcf
    nil #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 nil ; #xd0-#xd7
    #xd8 #xd9 #xda #xdb #xdc #xdd nil #xdf ; #xd8-#xdf
    #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #xe0-#xe7
    #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #xe8-#xef
    nil #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 nil ; #xf0-#xf7
    #xf8 #xf9 #xfa #xfb #xfc #xfd nil #xff ; #xf8-#xff
    ))

(defstatic *unicode-108-128-to-iso-8859-14*
  #(
    nil nil #xa4 #xa5 nil nil nil nil   ; #x108-#x10f
    nil nil nil nil nil nil nil nil     ; #x110-#x117
    nil nil nil nil nil nil nil nil     ; #x118-#x11f
    #xb2 #xb3 nil nil nil nil nil nil   ; #x120-#x127
    ))

(defstatic *unicode-170-180-to-iso-8859-14*
  #(
    nil nil nil nil #xd0 #xf0 #xde #xfe ; #x170-#x177
    #xaf nil nil nil nil nil nil nil    ; #x178-#x17f
    ))    

(defstatic *unicode-1e00-1e88-to-iso-8859-14*
  #(
    nil nil #xa1 #xa2 nil nil nil nil   ; #x1e00-#x1e07
    nil nil #xa6 #xab nil nil nil nil   ; #x1e08-#x1e0f
    nil nil nil nil nil nil nil nil     ; #x1e10-#x1e17
    nil nil nil nil nil nil #xb0 #xb1   ; #x1e18-#x1e1f
    nil nil nil nil nil nil nil nil     ; #x1e20-#x1e27
    nil nil nil nil nil nil nil nil     ; #x1e28-#x1e2f
    nil nil nil nil nil nil nil nil     ; #x1e30-#x1e37
    nil nil nil nil nil nil nil nil     ; #x1e38-#x1e3f
    #xb4 #xb5 nil nil nil nil nil nil   ; #x1e40-#x1e47
    nil nil nil nil nil nil nil nil     ; #x1e48-#x1e4f
    nil nil nil nil nil nil #xb7 #xb9   ; #x1e50-#x1e57
    nil nil nil nil nil nil nil nil     ; #x1e58-#x1e5f
    #xbb #xbf nil nil nil nil nil nil   ; #x1e60-#x1e67
    nil nil #xd7 #xf7 nil nil nil nil   ; #x1e68-#x1e6f
    nil nil nil nil nil nil nil nil     ; #x1e70-#x1e77
    nil nil nil nil nil nil nil nil     ; #x1e78-#x1e7f
    #xa8 #xb8 #xaa #xba #xbd #xbe nil nil ; #x1e80-#x1e87
    ))

(defstatic *unicode-1ef0-1ef8-to-iso-8859-14*
  #(
    nil nil #xac #xbc nil nil nil nil   ; #x1ef0-#x1ef7
    ))

(define-character-encoding :iso-8859-14
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in Celtic
languages."
  :aliases '(:iso_8859-14 :iso-ir-199 :latin8 :l8 :iso-celtic)
  :stream-encode-function
  (nfunction
   iso-8859-14-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #x100)
                       (svref *unicode-a0-100-to-iso-8859-14*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x108) (< code #x128))
                       (svref *unicode-108-128-to-iso-8859-14*
                              (the fixnum (- code #x108))))
                      ((and (>= code #x170) (< code #x180))
                       (svref *unicode-170-180-to-iso-8859-14*
                              (the fixnum (- code #x170))))
                      ((and (>= code #x1e00) (< code #x1e88))
                       (svref *unicode-1e00-1e88-to-iso-8859-14*
                              (the fixnum (- code #x1e00))))
                      ((and (>= code #x1ef0) (< code #x1ef8))
                       (svref *unicode-1ef0-1ef8-to-iso-8859-14*
                              (the fixnum (- code #x1ef0)))))))
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-14-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-14-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-14-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                      ((< code #x100)
                       (svref *unicode-a0-100-to-iso-8859-14*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x108) (< code #x128))
                       (svref *unicode-108-128-to-iso-8859-14*
                              (the fixnum (- code #x108))))
                      ((and (>= code #x170) (< code #x180))
                       (svref *unicode-170-180-to-iso-8859-14*
                              (the fixnum (- code #x170))))
                      ((and (>= code #x1e00) (< code #x1e88))
                       (svref *unicode-1e00-1e88-to-iso-8859-14*
                              (the fixnum (- code #x1e00))))
                      ((and (>= code #x1ef0) (< code #x1ef8))
                       (svref *unicode-1ef0-1ef8-to-iso-8859-14*
                              (the fixnum (- code #x1ef0)))))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-14-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-14-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-14-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                      ((< code #x100)
                       (svref *unicode-a0-100-to-iso-8859-14*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x108) (< code #x128))
                       (svref *unicode-108-128-to-iso-8859-14*
                              (the fixnum (- code #x108))))
                      ((and (>= code #x170) (< code #x180))
                       (svref *unicode-170-180-to-iso-8859-14*
                              (the fixnum (- code #x170))))
                      ((and (>= code #x1e00) (< code #x1e88))
                       (svref *unicode-1e00-1e88-to-iso-8859-14*
                              (the fixnum (- code #x1e00))))
                      ((and (>= code #x1ef0) (< code #x1ef8))
                       (svref *unicode-1ef0-1ef8-to-iso-8859-14*
                              (the fixnum (- code #x1ef0)))))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-14-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-14-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0  
  )

(defstatic *iso-8859-15-to-unicode*
  #(
    ;; #xa0
    #\u+00a0 #\u+00a1 #\u+00a2 #\u+00a3 #\u+20ac #\u+00a5 #\u+0160 #\u+00a7
    #\u+0161 #\u+00a9 #\u+00aa #\u+00ab #\u+00ac #\u+00ad #\u+00ae #\u+00af
    ;; #xb0
    #\u+00b0 #\u+00b1 #\u+00b2 #\u+00b3 #\u+017d #\u+00b5 #\u+00b6 #\u+00b7
    #\u+017e #\u+00b9 #\u+00ba #\u+00bb #\u+0152 #\u+0153 #\u+0178 #\u+00bf
    ;; #xc0
    #\u+00c0 #\u+00c1 #\u+00c2 #\u+00c3 #\u+00c4 #\u+00c5 #\u+00c6 #\u+00c7 
    ;; #xc8
    #\u+00c8 #\u+00c9 #\u+00ca #\u+00cb #\u+00cc #\u+00cd #\u+00ce #\u+00cf 
    ;; #xd0
    #\u+00d0 #\u+00d1 #\u+00d2 #\u+00d3 #\u+00d4 #\u+00d5 #\u+00d6 #\u+00d7 
    ;; #xd8
    #\u+00d8 #\u+00d9 #\u+00da #\u+00db #\u+00dc #\u+00dd #\u+00de #\u+00df 
    ;; #xe0
    #\u+00e0 #\u+00e1 #\u+00e2 #\u+00e3 #\u+00e4 #\u+00e5 #\u+00e6 #\u+00e7 
    ;; #xe8
    #\u+00e8 #\u+00e9 #\u+00ea #\u+00eb #\u+00ec #\u+00ed #\u+00ee #\u+00ef 
    ;; #xf0
    #\u+00f0 #\u+00f1 #\u+00f2 #\u+00f3 #\u+00f4 #\u+00f5 #\u+00f6 #\u+00f7 
    ;; #xf8
    #\u+00f8 #\u+00f9 #\u+00fa #\u+00fb #\u+00fc #\u+00fd #\u+00fe #\u+00ff 
    ))

(defstatic *unicode-a0-100-to-iso-8859-15*
  #(
    #xa0 #xa1 #xa2 #xa3 nil #xa5 nil #xa7 ; #xa0-#xa7
    nil #xa9 #xaa #xab #xac #xad #xae #xaf ; #xa8-#xaf
    #xb0 #xb1 #xb2 #xb3 nil #xb5 #xb6 #xb7 ; #xb0-#xb7
    nil #xb9 #xba #xbb nil nil nil #xbf ; #xb8-0xbf
    #xc0 #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 ; #xc0-#xc7
    #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #xc8-#xcf
    #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7 ; #xd0-#xd7
    #xd8 #xd9 #xda #xdb #xdc #xdd #xde #xdf ; #xd8-#xdf
    #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 ; #xe0-#xe7
    #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #xe8-#xef
    #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 ; #xf0-#xf7
    #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe #xff ; #xf8-#xff
    ))

(defstatic *unicode-150-180-to-iso-8859-15*
  #(
    nil nil #xbc #xbd nil nil nil nil   ; #x150-#x157
    nil nil nil nil nil nil nil nil     ; #x158-#x15f
    #xa6 #xa8 nil nil nil nil nil nil   ; #x160-#x167
    nil nil nil nil nil nil nil nil     ; #x168-#x16f
    nil nil nil nil nil nil nil nil     ; #x170-#x177
    #xbe nil nil nil nil #xb4 #xb8 nil  ; #x178-#x17f
    ))

(define-character-encoding :iso-8859-15
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in Western
European languages (including the Euro sign and some other characters
missing from ISO-8859-1."
  :aliases '(:iso_8859-15 :latin9)
  :stream-encode-function
  (nfunction
   iso-8859-15-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #x100)
                       (svref *unicode-a0-100-to-iso-8859-15*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x150) (< code #x180))
                       (svref *unicode-150-180-to-iso-8859-15*
                              (the fixnum (- code #x150))))
                      ((= code #x20ac) #xa4))))
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-15-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-15-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-15-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                      ((< code #x100)
                       (svref *unicode-a0-100-to-iso-8859-15*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x150) (< code #x180))
                       (svref *unicode-150-180-to-iso-8859-15*
                              (the fixnum (- code #x150))))
                      ((= code #x20ac) #xa4))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-15-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-15-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-15-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                      ((< code #x100)
                       (svref *unicode-a0-100-to-iso-8859-15*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x150) (< code #x180))
                       (svref *unicode-150-180-to-iso-8859-15*
                              (the fixnum (- code #x150))))
                      ((= code #x20ac) #xa4))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-15-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-15-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0  
  )

(defstatic *iso-8859-16-to-unicode*
  #(
    ;; #xa0
    #\u+00a0 #\u+0104 #\u+0105 #\u+0141 #\u+20ac #\u+201e #\u+0160 #\u+00a7
    #\u+0161 #\u+00a9 #\u+0218 #\u+00ab #\u+0179 #\u+00ad #\u+017a #\u+017b
    ;; #xb0
    #\u+00b0 #\u+00b1 #\u+010c #\u+0142 #\u+017d #\u+201d #\u+00b6 #\u+00b7
    #\u+017e #\u+010d #\u+0219 #\u+00bb #\u+0152 #\u+0153 #\u+0178 #\u+017c
    ;; #xc0
    #\u+00c0 #\u+00c1 #\u+00c2 #\u+0102 #\u+00c4 #\u+0106 #\u+00c6 #\u+00c7
    #\u+00c8 #\u+00c9 #\u+00ca #\u+00cb #\u+00cc #\u+00cd #\u+00ce #\u+00cf
    ;; #xd0
    #\u+0110 #\u+0143 #\u+00d2 #\u+00d3 #\u+00d4 #\u+0150 #\u+00d6 #\u+015a
    #\u+0170 #\u+00d9 #\u+00da #\u+00db #\u+00dc #\u+0118 #\u+021a #\u+00df
    ;; #xe0
    #\u+00e0 #\u+00e1 #\u+00e2 #\u+0103 #\u+00e4 #\u+0107 #\u+00e6 #\u+00e7
    #\u+00e8 #\u+00e9 #\u+00ea #\u+00eb #\u+00ec #\u+00ed #\u+00ee #\u+00ef
    ;; #xf0
    #\u+0111 #\u+0144 #\u+00f2 #\u+00f3 #\u+00f4 #\u+0151 #\u+00f6 #\u+015b
    #\u+0171 #\u+00f9 #\u+00fa #\u+00fb #\u+00fc #\u+0119 #\u+021b #\u+00ff
    ))

(defstatic *unicode-a0-180-to-iso-8859-16*
  #(
    #xa0 nil nil nil nil nil nil #xa7   ; #xa0-#xa7 
    nil #xa9 nil #xab nil #xad nil nil  ; #xa8-#xaf 
    #xb0 #xb1 nil nil nil nil #xb6 #xb7 ; #xb0-#xb7 
    nil nil nil #xbb nil nil nil nil    ; #xb8-#xbf 
    #xc0 #xc1 #xc2 nil #xc4 nil #xc6 #xc7 ; #xc0-#xc7 
    #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf ; #xc8-#xcf 
    nil nil #xd2 #xd3 #xd4 nil #xd6 nil ; #xd0-#xd7 
    nil #xd9 #xda #xdb #xdc nil nil #xdf ; #xd8-#xdf 
    #xe0 #xe1 #xe2 nil #xe4 nil #xe6 #xe7 ; #xe0-#xe7 
    #xe8 #xe9 #xea #xeb #xec #xed #xee #xef ; #xe8-#xef 
    nil nil #xf2 #xf3 #xf4 nil #xf6 nil ; #xf0-#xf7 
    nil #xf9 #xfa #xfb #xfc nil nil #xff ; #xf8-#xff 
    nil nil #xc3 #xe3 #xa1 #xa2 #xc5 #xe5 ; #x100-#x107 
    nil nil nil nil #xb2 #xb9 nil nil   ; #x108-#x10f 
    #xd0 #xf0 nil nil nil nil nil nil   ; #x110-#x117 
    #xdd #xfd nil nil nil nil nil nil   ; #x118-#x11f 
    nil nil nil nil nil nil nil nil     ; #x120-#x127 
    nil nil nil nil nil nil nil nil     ; #x128-#x12f 
    nil nil nil nil nil nil nil nil     ; #x130-#x137 
    nil nil nil nil nil nil nil nil     ; #x138-#x13f 
    nil #xa3 #xb3 #xd1 #xf1 nil nil nil ; #x140-#x147 
    nil nil nil nil nil nil nil nil     ; #x148-#x14f 
    #xd5 #xf5 #xbc #xbd nil nil nil nil ; #x150-#x157 
    nil nil #xd7 #xf7 nil nil nil nil   ; #x158-#x15f 
    #xa6 #xa8 nil nil nil nil nil nil   ; #x160-#x167 
    nil nil nil nil nil nil nil nil     ; #x168-#x16f 
    #xd8 #xf8 nil nil nil nil nil nil   ; #x170-#x177 
    #xbe #xac #xae #xaf #xbf #xb4 #xb8 nil ; #x178-#x17f 
    ))

(defstatic *unicode-218-220-to-iso-8859-16*
  #(
    #xaa #xba #xde #xfe nil nil nil nil ; #x218-#x21f
    ))

(defstatic *unicode-2018-2020-to-iso-8859-16*
  #(
    nil nil nil nil nil #xb5 #xa5 nil   ; #x2018-#x201f
    ))
  

(define-character-encoding :iso-8859-16
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in Southeast
European languages."
  :aliases '(:iso_8859-16 :latin10 :l1 :iso-ir-226)
  :stream-encode-function
  (nfunction
   iso-8859-16-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso-8859-16*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x218) (< code #x220))
                       (svref *unicode-218-220-to-iso-8859-16*
                              (the fixnum (- code #x218))))
                      ((and (>= code #x2018) (< code #x2020))
                       (svref *unicode-2018-2020-to-iso-8859-16*
                              (the fixnum (- code #x2018))))
                      ((= code #x20ac) #xa4))))
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-16-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa0)
       (code-char 1st-unit)
       (svref *iso-8859-16-to-unicode* (the fixnum (- 1st-unit #xa0))))))
  :vector-encode-function
  (nfunction
   iso-8859-16-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso-8859-16*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x218) (< code #x220))
                       (svref *unicode-218-220-to-iso-8859-16*
                              (the fixnum (- code #x218))))
                      ((and (>= code #x2018) (< code #x2020))
                       (svref *unicode-2018-2020-to-iso-8859-16*
                              (the fixnum (- code #x2018))))
                      ((= code #x20ac) #xa4))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   iso-8859-16-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-16-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :memory-encode-function
  (nfunction
   iso-8859-16-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (c2 (cond ((< code #xa0) code)
                      ((< code #x180)
                       (svref *unicode-a0-180-to-iso-8859-16*
                              (the fixnum (- code #xa0))))
                      ((and (>= code #x218) (< code #x220))
                       (svref *unicode-218-220-to-iso-8859-16*
                              (the fixnum (- code #x218))))
                      ((and (>= code #x2018) (< code #x2020))
                       (svref *unicode-2018-2020-to-iso-8859-16*
                              (the fixnum (- code #x2018))))
                      ((= code #x20ac) #xa4))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   iso-8859-16-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #xa0)
                 (code-char 1st-unit)
                 (svref *iso-8859-16-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #xa0
  :encode-literal-char-code-limit #xa0  
  )

(defstatic *macintosh-to-unicode*
  #(
    ;; #x80 
    #\u+00c4 #\u+00c5 #\u+00c7 #\u+00c9 #\u+00d1 #\u+00d6 #\u+00dc #\u+00e1
    #\u+00e0 #\u+00e2 #\u+00e4 #\u+00e3 #\u+00e5 #\u+00e7 #\u+00e9 #\u+00e8
    ;; #x90 
    #\u+00ea #\u+00eb #\u+00ed #\u+00ec #\u+00ee #\u+00ef #\u+00f1 #\u+00f3
    #\u+00f2 #\u+00f4 #\u+00f6 #\u+00f5 #\u+00fa #\u+00f9 #\u+00fb #\u+00fc
    ;; #xa0 
    #\u+2020 #\u+00b0 #\u+00a2 #\u+00a3 #\u+00a7 #\u+2022 #\u+00b6 #\u+00df
    #\u+00ae #\u+00a9 #\u+2122 #\u+00b4 #\u+00a8 #\u+2260 #\u+00c6 #\u+00d8
    ;; #xb0 
    #\u+221e #\u+00b1 #\u+2264 #\u+2265 #\u+00a5 #\u+00b5 #\u+2202 #\u+2211
    #\u+220f #\u+03c0 #\u+222b #\u+00aa #\u+00ba #\u+2126 #\u+00e6 #\u+00f8
    ;; #xc0 
    #\u+00bf #\u+00a1 #\u+00ac #\u+221a #\u+0192 #\u+2248 #\u+2206 #\u+00ab
    #\u+00bb #\u+2026 #\u+00a0 #\u+00c0 #\u+00c3 #\u+00d5 #\u+0152 #\u+0153
    ;; #xd0 
    #\u+2013 #\u+2014 #\u+201c #\u+201d #\u+2018 #\u+2019 #\u+00f7 #\u+25ca
    #\u+00ff #\u+0178 #\u+2044 #\u+00a4 #\u+2039 #\u+203a #\u+fb01 #\u+fb02
    ;; #xe0 
    #\u+2021 #\u+00b7 #\u+201a #\u+201e #\u+2030 #\u+00c2 #\u+00ca #\u+00c1
    #\u+00cb #\u+00c8 #\u+00cd #\u+00ce #\u+00cf #\u+00cc #\u+00d3 #\u+00d4
    ;; #xf0 
    #\u+f8ff #\u+00d2 #\u+00da #\u+00db #\u+00d9 #\u+0131 #\u+02c6 #\u+02dc
    #\u+00af #\u+02d8 #\u+02d9 #\u+02da #\u+00b8 #\u+02dd #\u+02db #\u+02c7
    ))


(defstatic *unicode-a0-100-to-macintosh*
  #(
    #xca #xc1 #xa2 #xa3 #xdb #xb4 nil #xa4 ; #xa0-#xa7 
    #xac #xa9 #xbb #xc7 #xc2 nil #xa8 #xf8 ; #xa8-#xaf 
    #xa1 #xb1 nil nil #xab #xb5 #xa6 #xe1 ; #xb0-#xb7 
    #xfc nil #xbc #xc8 nil nil nil #xc0 ; #xb8-#xbf 
    #xcb #xe7 #xe5 #xcc #x80 #x81 #xae #x82 ; #xc0-#xc7 
    #xe9 #x83 #xe6 #xe8 #xed #xea #xeb #xec ; #xc8-#xcf 
    nil #x84 #xf1 #xee #xef #xcd #x85 nil ; #xd0-#xd7 
    #xaf #xf4 #xf2 #xf3 #x86 nil nil #xa7 ; #xd8-#xdf 
    #x88 #x87 #x89 #x8b #x8a #x8c #xbe #x8d ; #xe0-#xe7 
    #x8f #x8e #x90 #x91 #x93 #x92 #x94 #x95 ; #xe8-#xef 
    nil #x96 #x98 #x97 #x99 #x9b #x9a #xd6 ; #xf0-#xf7 
    #xbf #x9d #x9c #x9e #x9f nil nil #xd8 ; #xf8-#xff 
    ))

(defstatic *unicode-130-198-to-macintosh*
  #(
    nil #xf5 nil nil nil nil nil nil ; #x130-#x137 
    nil nil nil nil nil nil nil nil ; #x138-#x13f 
    nil nil nil nil nil nil nil nil ; #x140-#x147 
    nil nil nil nil nil nil nil nil ; #x148-#x14f 
    nil nil #xce #xcf nil nil nil nil ; #x150-#x157 
    nil nil nil nil nil nil nil nil ; #x158-#x15f 
    nil nil nil nil nil nil nil nil ; #x160-#x167 
    nil nil nil nil nil nil nil nil ; #x168-#x16f 
    nil nil nil nil nil nil nil nil ; #x170-#x177 
    #xd9 nil nil nil nil nil nil nil ; #x178-#x17f 
    nil nil nil nil nil nil nil nil ; #x180-#x187 
    nil nil nil nil nil nil nil nil ; #x188-#x18f 
    nil nil #xc4 nil nil nil nil nil ; #x190-#x197 
    ))

(defstatic *unicode-2c0-2e0-to-macintosh*
  #(
    nil nil nil nil nil nil #xf6 #xff   ; #x2c0-#x2c7 
    nil nil nil nil nil nil nil nil     ; #x2c8-#x2cf 
    nil nil nil nil nil nil nil nil     ; #x2d0-#x2d7 
    #xf9 #xfa #xfb #xfe #xf7 #xfd nil nil ; #x2d8-#x2df 
    ))

(defstatic *unicode-2010-2048-to-macintosh*
  #(
  nil nil nil #xd0 #xd1 nil nil nil ; #x2010-#x2017 
  #xd4 #xd5 #xe2 nil #xd2 #xd3 #xe3 nil ; #x2018-#x201f 
  #xa0 #xe0 #xa5 nil nil nil #xc9 nil ; #x2020-#x2027 
  nil nil nil nil nil nil nil nil ; #x2028-#x202f 
  #xe4 nil nil nil nil nil nil nil ; #x2030-#x2037 
  nil #xdc #xdd nil nil nil nil nil ; #x2038-#x203f 
  nil nil nil nil #xda nil nil nil ; #x2040-#x2047 
    ))

(defstatic *unicode-2120-2128-to-macintosh*
  #(
    nil nil #xaa nil nil nil #xbd nil   ; #x2120-#x2127
    ))

(defstatic *unicode-2200-2268-to-macintosh*
  #(
    nil nil #xb6 nil nil nil #xc6 nil   ; #x2200-#x2207 
    nil nil nil nil nil nil nil #xb8    ; #x2208-#x220f 
    nil #xb7 nil nil nil nil nil nil    ; #x2210-#x2217 
    nil nil #xc3 nil nil nil #xb0 nil   ; #x2218-#x221f 
    nil nil nil nil nil nil nil nil     ; #x2220-#x2227 
    nil nil nil #xba nil nil nil nil    ; #x2228-#x222f 
    nil nil nil nil nil nil nil nil     ; #x2230-#x2237 
    nil nil nil nil nil nil nil nil     ; #x2238-#x223f 
    nil nil nil nil nil nil nil nil     ; #x2240-#x2247 
    #xc5 nil nil nil nil nil nil nil    ; #x2248-#x224f 
    nil nil nil nil nil nil nil nil     ; #x2250-#x2257 
    nil nil nil nil nil nil nil nil     ; #x2258-#x225f 
    #xad nil nil nil #xb2 #xb3 nil nil  ; #x2260-#x2267 
    ))

(defstatic *unicode-fb00-fb08-to-macintosh*
  #(
    nil #xde #xdf nil nil nil nil nil ; #xfb00-#xfb07
    ))

(define-character-encoding :macintosh
    "An 8-bit, fixed-width character encoding in which codes #x00-#x7f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Traditionally used on Classic MacOS to encode characters
used in western languages."
  :aliases '(:macos-roman :macosroman :mac-roman :macroman)

  :stream-encode-function
  (nfunction
   macintosh-stream-encode
   (lambda (char write-function stream)
     (let* ((code (char-code char))
            (c2 (cond ((< code #x80) code)
                      ((and (>= code #xa0) (< code #x100)
                       (svref *unicode-a0-100-to-macintosh*
                              (the fixnum (- code #xa0)))))
                      ((and (>= code #x130) (< code #x198))
                       (svref *unicode-130-198-to-macintosh*
                              (the fixnum (- code #x130))))
                      ((and (>= code #x2c0) (< code #x2e0))
                       (svref *unicode-2c0-2e0-to-macintosh*
                              (the fixnum (- code #x2c0))))
                      ((= code #x3c0) #xb9)
                      ((and (>= code #x2010) (< code #x2048))
                       (svref *unicode-2010-2048-to-macintosh*
                              (the fixnum (- code #x2010))))
                      ((and (>= code #x2120) (< code #x2128))
                       (svref *unicode-2120-2128-to-macintosh*
                              (the fixnum (- code #x2120))))
                      ((and (>= code #x2200) (< code #x2268))
                       (svref *unicode-2200-2268-to-macintosh*
                              (the fixnum (- code #x2200))))
                      ((= code #x25ca) #xd7)
                      ((and (>= code #xfb00) (< code #xfb08))
                       (svref *unicode-fb00-fb08-to-macintosh*
                              (the fixnum (- code #xfb00))))
                      ((= code #xf8ff) #xf0))))
       (declare (type (mod #x110000) code))
       (funcall write-function stream (or c2 (char-code #\Sub)))
       1)))
  :stream-decode-function
  (nfunction
   macintosh-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function stream)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #x80)
       (code-char 1st-unit)
       (svref *macintosh-to-unicode* (the fixnum (- 1st-unit #x80))))))
  :vector-encode-function
  (nfunction
   macintosh-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
            (c2 (cond ((< code #x80) code)
                      ((and (>= code #xa0) (< code #x100)
                       (svref *unicode-a0-100-to-macintosh*
                              (the fixnum (- code #xa0)))))
                      ((and (>= code #x130) (< code #x198))
                       (svref *unicode-130-198-to-macintosh*
                              (the fixnum (- code #x130))))
                      ((and (>= code #x2c0) (< code #x2e0))
                       (svref *unicode-2c0-2e0-to-macintosh*
                              (the fixnum (- code #x2c0))))
                      ((= code #x3c0) #xb9)
                      ((and (>= code #x2010) (< code #x2048))
                       (svref *unicode-2010-2048-to-macintosh*
                              (the fixnum (- code #x2010))))
                      ((and (>= code #x2120) (< code #x2128))
                       (svref *unicode-2120-2128-to-macintosh*
                              (the fixnum (- code #x2120))))
                      ((and (>= code #x2200) (< code #x2268))
                       (svref *unicode-2200-2268-to-macintosh*
                              (the fixnum (- code #x2200))))
                      ((= code #x25ca) #xd7)
                      ((and (>= code #xfb00) (< code #xfb08))
                       (svref *unicode-fb00-fb08-to-macintosh*
                              (the fixnum (- code #xfb00))))
                      ((= code #xf8ff) #xf0))))
         (declare (type (mod #x110000) code))
         (setf (aref vector idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :vector-decode-function
  (nfunction
   macintosh-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (aref vector index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #x80)
                 (code-char 1st-unit)
                 (svref *macintosh-to-unicode* (the fixnum (- 1st-unit #x80)))))))))
  :memory-encode-function
  (nfunction
   macintosh-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
            (c2 (cond ((< code #x80) code)
                      ((and (>= code #xa0) (< code #x100)
                       (svref *unicode-a0-100-to-macintosh*
                              (the fixnum (- code #xa0)))))
                      ((and (>= code #x130) (< code #x198))
                       (svref *unicode-130-198-to-macintosh*
                              (the fixnum (- code #x130))))
                      ((and (>= code #x2c0) (< code #x2e0))
                       (svref *unicode-2c0-2e0-to-macintosh*
                              (the fixnum (- code #x2c0))))
                      ((= code #x3c0) #xb9)
                      ((and (>= code #x2010) (< code #x2048))
                       (svref *unicode-2010-2048-to-macintosh*
                              (the fixnum (- code #x2010))))
                      ((and (>= code #x2120) (< code #x2128))
                       (svref *unicode-2120-2128-to-macintosh*
                              (the fixnum (- code #x2120))))
                      ((and (>= code #x2200) (< code #x2268))
                       (svref *unicode-2200-2268-to-macintosh*
                              (the fixnum (- code #x2200))))
                      ((= code #x25ca) #xd7)
                      ((and (>= code #xfb00) (< code #xfb08))
                       (svref *unicode-fb00-fb08-to-macintosh*
                              (the fixnum (- code #xfb00))))
                      ((= code #xf8ff) #xf0))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (char-code #\Sub)))
         (incf idx)))))
  :memory-decode-function
  (nfunction
   macintosh-memory-decode
   (lambda (pointer noctets idx string)
     (do* ((i 0 (1+ i))
           (index idx (1+ index)))
          ((>= i noctets) index)
       (let* ((1st-unit (%get-unsigned-byte pointer index)))
         (declare (type (unsigned-byte 8) 1st-unit))
         (setf (schar string i)
               (if (< 1st-unit #x80)
                 (code-char 1st-unit)
                 (svref *macintosh-to-unicode* (the fixnum (- 1st-unit #xa0)))))))))
  :octets-in-string-function
  #'8-bit-fixed-width-octets-in-string
  :length-of-vector-encoding-function
  #'8-bit-fixed-width-length-of-vector-encoding
  :length-of-memory-encoding-function 
  #'8-bit-fixed-width-length-of-memory-encoding
  :decode-literal-code-unit-limit #x80
  :encode-literal-char-code-limit #x80  
  )

;;; UTF-8.  Decoding checks for malformed sequences; it might be faster (and
;;; would certainly be simpler) if it didn't.
(define-character-encoding :utf-8
    "An 8-bit, variable-length character encoding in which characters
with CHAR-CODEs in the range #x00-#x7f can be encoded in a single
octet; characters with larger code values can be encoded in 2 to 4
bytes."
    :max-units-per-char 4
    :stream-encode-function
    (nfunction
     utf-8-stream-encode
     (lambda (char write-function stream)
       (let* ((code (char-code char)))
         (declare (type (mod #x110000) code))
         (cond ((< code #x80)
                (funcall write-function stream code)
                1)
               ((< code #x800)
                (let* ((y (ldb (byte 5 6) code))
                       (z (ldb (byte 6 0) code)))
                  (declare (fixnum y z))
                  (funcall write-function stream (logior #xc0 y))
                  (funcall write-function stream (logior #x80 z))
                  2))
               ((< code #x10000)
                (let* ((x (ldb (byte 4 12) code))
                       (y (ldb (byte 6 6) code))
                       (z (ldb (byte 6 0) code)))
                  (declare (fixnum x y z))
                  (funcall write-function stream (logior #xe0 x))
                  (funcall write-function stream (logior #x80 y))
                  (funcall write-function stream (logior #x80 z))
                  3))
               (t
                (let* ((w (ldb (byte 3 18) code))
                       (x (ldb (byte 6 12) code))
                       (y (ldb (byte 6 6) code))
                       (z (ldb (byte 6 0) code)))
                  (declare (fixnum w x y z))
                  (funcall write-function stream (logior #xf0 w))
                  (funcall write-function stream (logior #x80 x))
                  (funcall write-function stream (logior #x80 y))
                  (funcall write-function stream (logior #x80 z))
                  4))))))
    :stream-decode-function
    (nfunction
     utf-8-stream-decode
     (lambda (1st-unit next-unit-function stream)
       (declare (type (unsigned-byte 8) 1st-unit))
       (if (< 1st-unit #x80)
         (code-char 1st-unit)
         (if (>= 1st-unit #xc2)
           (let* ((s1 (funcall next-unit-function stream)))
             (if (eq s1 :eof)
               s1
               (locally
                   (declare (type (unsigned-byte 8) s1))
                 (if (< 1st-unit #xe0)
                   (if (< (the fixnum (logxor s1 #x80)) #x40)
                     (code-char
                      (logior
                       (the fixnum (ash (the fixnum (logand #x1f 1st-unit)) 6))
                       (the fixnum (logxor s1 #x80))))
                     #\Replacement_Character)
                   (let* ((s2 (funcall next-unit-function stream)))
                     (if (eq s2 :eof)
                       s2
                       (locally
                           (declare (type (unsigned-byte 8) s2))
                         (if (< 1st-unit #xf0)
                           (if (and (< (the fixnum (logxor s1 #x80)) #x40)
                                    (< (the fixnum (logxor s2 #x80)) #x40)
                                    (or (>= 1st-unit #xe1)
                                        (>= s1 #xa0)))
                             (or (code-char (the fixnum
                                          (logior (the fixnum
                                                    (ash (the fixnum (logand 1st-unit #xf))
                                                         12))
                                                  (the fixnum
                                                    (logior
                                                     (the fixnum
                                                       (ash (the fixnum (logand s1 #x3f))
                                                            6))
                                                     (the fixnum (logand s2 #x3f)))))))
                                 #\Replacement_Character)
                             #\Replacement_Character)
                           (if (< 1st-unit #xf8)
                             (let* ((s3 (funcall next-unit-function stream)))
                               (if (eq s3 :eof)
                                 s3
                                 (locally
                                     (declare (type (unsigned-byte 8) s3))
                                   (if (and (< (the fixnum (logxor s1 #x80)) #x40)
                                            (< (the fixnum (logxor s2 #x80)) #x40)
                                            (< (the fixnum (logxor s3 #x80)) #x40)
                                            (or (>= 1st-unit #xf1)
                                                (>= s1 #x90)))
                                     (code-char
                                      (logior
                                       (the fixnum
                                         (logior
                                          (the fixnum
                                            (ash (the fixnum (logand 1st-unit 7)) 18))
                                          (the fixnum
                                            (ash (the fixnum (logxor s1 #x80)) 12))))
                                       (the fixnum
                                         (logior
                                          (the fixnum
                                            (ash (the fixnum (logxor s2 #x80)) 6))
                                          (the fixnum (logxor s3 #x80))))))
                                     #\Replacement_Character))))
                             #\Replacement_Character)))))))))
           #\Replacement_Character))))
    :vector-encode-function
    (nfunction
     utf-8-vector-encode
     (lambda (string vector idx start end)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                (fixnum idx))
       (do* ((i start (1+ i)))
            ((>= i end) idx)
         (let* ((char (schar string i))
                (code (char-code char)))
           (declare (type (mod #x110000) code))
           (cond ((< code #x80)
                  (setf (aref vector idx) code)
                  (incf idx))
                 ((< code #x800)
                  (setf (aref vector idx)
                        (logior #xc0 (the fixnum (ash code -6))))
                  (setf (aref vector (the fixnum (1+ idx)))
                        (logior #x80 (the fixnum (logand code #x3f))))
                  (incf idx 2))
                 ((< code #x10000)
                  (setf (aref vector idx)
                        (logior #xe0 (the fixnum (ash code -12))))
                  (setf (aref vector (the fixnum (1+ idx)))
                        (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6))))))
                  (setf (aref vector (the fixnum (+ idx 2)))
                        (logior #x80 (the fixnum (logand code #x3f))))
                  (incf idx 3))
                 (t
                   (setf (aref vector idx)
                         (logior #xf0
                                 (the fixnum (logand #x7 (the fixnum (ash code -18))))))
                   (setf (aref vector (the fixnum (1+ idx)))
                         (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -12))))))
                   (setf (aref vector (the fixnum (+ idx 2)))
                         (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6))))))
                   (setf (aref vector (the fixnum (+ idx 3)))
                         (logior #x80 (logand #x3f code)))
                   (incf idx 4)))))))
    :vector-decode-function
    (nfunction
     utf-8-vector-decode
     (lambda (vector idx noctets string)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                (type index idx))
       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx (1+ index)))
            ((= index end) index)
           (let* ((1st-unit (aref vector index)))
             (declare (type (unsigned-byte 8) 1st-unit))
             (let* ((char 
                     (if (< 1st-unit #x80)
                       (code-char 1st-unit)
                       (if (>= 1st-unit #xc2)
                           (let* ((2nd-unit (aref vector (incf index))))
                             (declare (type (unsigned-byte 8) 2nd-unit))
                             (if (< 1st-unit #xe0)
                               (if (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                                 (code-char
                                  (logior
                                   (the fixnum (ash (the fixnum (logand #x1f 1st-unit)) 6))
                                   (the fixnum (logxor 2nd-unit #x80)))))
                               (let* ((3rd-unit (aref vector (incf index))))
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
                                   (let* ((4th-unit (aref vector (incf index))))
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
                                            (the fixnum (logxor 4th-unit #x80))))))))))))))))
               (setf (schar string i) (or char #\Replacement_Character)))))))
    :memory-encode-function
    #'utf-8-memory-encode
    :memory-decode-function
    #'utf-8-memory-decode
    :octets-in-string-function
    #'utf-8-octets-in-string
    :length-of-vector-encoding-function
    (nfunction
     utf-8-length-of-vector-encoding
     (lambda (vector start end)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector))
       (do* ((i start)
             (nchars 0))
            ((>= i end)
             (values nchars i))
         (declare (fixnum i))
         (let* ((code (aref vector i))
                (nexti (+ i (cond ((< code #xc2) 1)
                                  ((< code #xe0) 2)
                                  ((< code #xf0) 3)
                                  ((< code #xf8) 4)
                                  (t 1)))))
           (declare (type (unsigned-byte 8) code))
           (if (> nexti end)
             (return (values nchars i))
             (setq nchars (1+ nchars) i nexti))))))
    :length-of-memory-encoding-function
    #'utf-8-length-of-memory-encoding
    :decode-literal-code-unit-limit #x80
    :encode-literal-char-code-limit #x80    
    :bom-encoding #(#xef #xbb #xbf)
    )


;;; For a code-unit-size greater than 8: the stream-encode function's write-function
;;; accepts a code-unit in native byte order and swaps it if necessary and the
;;; stream-decode function receives a first-unit in native byte order and its
;;; next-unit-function returns a unit in native byte order.  The memory/vector
;;; functions have to do their own byte swapping.


(defmacro utf-16-combine-surrogate-pairs (a b)
  `(code-char
    (the (unsigned-byte 21)
      (+ #x10000
         (the (unsigned-byte 20)
           (logior
            (the (unsigned-byte 20) (ash (the (unsigned-byte 10)
                                           (- ,a #xd800))
                                         10))
            (the (unsigned-byte 10) (- ,b #xdc00))))))))
    
(defun utf-16-stream-encode (char write-function stream)
  (let* ((code (char-code char))
         (highbits (- code #x10000)))
    (declare (type (mod #x110000) code)
             (fixnum highbits))
    (if (< highbits 0)
      (progn
        (funcall write-function stream code)
        1)
      (progn
        (funcall write-function stream (logior #xd800 (the fixnum (ash highbits -10))))
        (funcall write-function stream (logior #xdc00 (the fixnum (logand highbits #x3ff))))
        2))))

(defun utf-16-stream-decode (1st-unit next-unit-function stream)
  (declare (type (unsigned-byte 16) 1st-unit))
  (if (or (< 1st-unit #xd800)
          (>= 1st-unit #xe000))
    (code-char 1st-unit)
    (if (< 1st-unit #xdc00)
      (let* ((2nd-unit (funcall next-unit-function stream)))
        (if (eq 2nd-unit :eof)
          2nd-unit
          (locally (declare (type (unsigned-byte 16) 2nd-unit))
            (if (and (>= 2nd-unit #xdc00)
                     (< 2nd-unit #xe000))
              (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)
              #\Replacement_Character))))
      #\Replacement_Character)))


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


(declaim (inline %big-endian-u8-ref-u16 %little-endian-u8-ref-u16))
(defun %big-endian-u8-ref-u16 (u8-vector idx)
  (declare (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (fixnum idx))
  (logior (the (unsigned-byte 16) (ash (the (unsigned-byte 8) (aref u8-vector idx)) 8))
          (the (unsigned-byte 8) (aref u8-vector (the fixnum (1+ idx))))))

(defun %little-endian-u8-ref-u16 (u8-vector idx)
  (declare (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (fixnum idx))
  (logior (the (unsigned-byte 16) (ash (the (unsigned-byte 8)
                                         (aref u8-vector (the fixnum (1+ idx)))) 8))
          (the (unsigned-byte 8) (aref u8-vector idx))))

#+big-endian-target
(progn
(defmacro %native-u8-ref-u16 (vector idx)
  `(%big-endian-u8-ref-u16 ,vector ,idx))

(defmacro %reversed-u8-ref-u16 (vector idx)
  `(%little-endian-u8-ref-u16 ,vector ,idx))
)

#+little-endian-target
(progn
(defmacro %native-u8-ref-u16 (vector idx)
  `(%little-endian-u8-ref-u16 ,vector ,idx))

(defmacro %reversed-u8-ref-u16 (vector idx)
  `(%big-endian-u8-ref-u16 ,vector ,idx))
)


(declaim (inline (setf %big-endian-u8-ref-u16) (setf %little-endian-u8-ref-u16)))
(defun (setf %big-endian-u8-ref-u16) (val u8-vector idx)
  (declare (type (unsigned-byte 16) val)
           (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (fixnum idx))
  (setf (aref u8-vector idx) (ldb (byte 8 8) val)
        (aref u8-vector (the fixnum (1+ idx))) (ldb (byte 8 0) val))
  val)

(defun (setf %little-endian-u8-ref-u16) (val u8-vector idx)
  (declare (type (unsigned-byte 16) val)
           (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (fixnum idx))
  (setf (aref u8-vector idx) (ldb (byte 8 0) val)
        (aref u8-vector (the fixnum (1+ idx))) (ldb (byte 8 8) val))
  val)


;;; utf-16, native byte order.
(define-character-encoding #+big-endian-target :utf-16be #-big-endian-target :utf-16le
    #+big-endian-target
    "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
big-endian word and characters with larger codes can be encoded in a
pair of 16-bit big-endian words.  The endianness of the encoded data
is implicit in the encoding; byte-order-mark characters are not
interpreted on input or prepended to output."
    #+little-endian-target
    "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
little-endian word and characters with larger codes can be encoded in
a pair of 16-bit little-endian words.  The endianness of the encoded
data is implicit in the encoding; byte-order-mark characters are not
interpreted on input or prepended to output."
    :max-units-per-char 2
    :code-unit-size 16
    :native-endianness t
    :stream-encode-function
    #'utf-16-stream-encode
    :stream-decode-function
    #'utf-16-stream-decode
    :vector-encode-function
    (nfunction
     native-utf-16-vector-encode
     (lambda (string vector idx start end)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                (fixnum idx start end))
       (do* ((i start (1+ i)))
            ((>= i end) idx)
         (declare (fixnum i))
         (let* ((char (schar string i))
                (code (char-code char))
                (highbits (- code #x10000)))
           (declare (type (mod #x110000) code)
                    (fixnum highbits))
           (cond ((< highbits 0)
                  (setf (%native-u8-ref-u16 vector idx) code)
                  (incf idx 2))
                 (t
                  (let* ((firstword (logior #xd800 (the fixnum (ash highbits -10))))
                         (secondword (logior #xdc00 (the fixnum (logand highbits #x3ff)))))
                    (declare (type (unsigned-byte 16) firstword secondword))
                    (setf (%native-u8-ref-u16 vector idx) firstword
                          (%native-u8-ref-u16 vector (the fixnum (+ idx 2))) secondword)
                    (incf idx 4))))))))
    :vector-decode-function
    (nfunction
     native-utf-16-vector-decode
     (lambda (vector idx noctets string)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                (type index idx))
       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx))
            ((= index end) index)
         (declare (fixnum i end index))
         (let* ((1st-unit (%native-u8-ref-u16 vector index)))
           (declare (type (unsigned-byte 16) 1st-unit))
           (incf index 2)
           (let* ((char
                   (if (or (< 1st-unit #xd800)
                           (>= 1st-unit #xe000))
                     (code-char 1st-unit)
                     (if (< 1st-unit #xdc00)
                       (let* ((2nd-unit (%native-u8-ref-u16 vector index)))
                         (declare (type (unsigned-byte 16) 2nd-unit))
                         (incf index 2)
                         (if (and (>= 2nd-unit #xdc00)
                                  (< 2nd-unit #xe000))
                           (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)))))))
             (setf (schar string i) (or char #\Replacement_Character)))))))
    :memory-encode-function
    (nfunction
     native-utf-16-memory-encode
     (lambda (string pointer idx start end)
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
                (incf idx 2)))))))
    :memory-decode-function
    (nfunction
     native-utf-16-memory-decode
     (lambda (pointer noctets idx string)
       (declare (fixnum noctets idx))
       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx))
            ((>= index end) index)
         (declare (fixnum i index end))
         (let* ((1st-unit (%get-unsigned-word pointer index)))
           (declare (type (unsigned-byte 16) 1st-unit))
           (incf index 2)
           (let* ((char
                   (if (or (< 1st-unit #xd800)
                           (>= 1st-unit #xe000))
                     (code-char 1st-unit)
                     (if (< 1st-unit #xdc00)
                       (let* ((2nd-unit (%get-unsigned-word pointer index)))
                           (declare (type (unsigned-byte 16) 2nd-unit))
                           (incf index)
                           (if (and (>= 2nd-unit #xdc00)
                                    (< 2nd-unit #xe000))
                             (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)))))))
            (setf (schar string i) (or char #\Replacement_Character)))))))
    :octets-in-string-function
    #'utf-16-octets-in-string
    :length-of-vector-encoding-function
    (nfunction
     native-utf-16-length-of-vector-encoding
     (lambda (vector start end)
       (declare (type (simple-array (unsigned-byte 8) (*)) vector))
       (declare (fixnum start end))
       (do* ((i start)
             (j (+ 2 i) (+ 2 i))
             (nchars 0))
            ((> j end) (values nchars i))
         (declare (fixnum i j nchars))
         (let* ((code (%native-u8-ref-u16 vector i))
                (nexti (+ i (if (or (< code #xd800)
                                    (>= code #xdc00))
                              2
                              4))))
           (declare (type (unsigned-byte 16) code)
                    (fixnum nexti))
           (if (> nexti end)
             (return (values nchars i))
             (setq i nexti nchars (1+ nchars)))))))
    :length-of-memory-encoding-function
    (nfunction
     native-utf-16-length-of-memory-encoding
     (lambda (pointer noctets start)
       (do* ((i start)
             (j (+ i 2) (+ i 2))
             (end (+ start noctets))
             (nchars 0))
            ((> j end) (values nchars i))
         (let* ((code (%get-unsigned-word pointer i))
                (nexti (+ i (if (or (< code #xd800)
                                    (>= code #xdc00))
                              2
                              4))))
           (declare (type (unsigned-byte 16) code)
                    (fixnum nexti))
           (if (> nexti end)
             (return (values nchars i))
             (setq i nexti nchars (1+ nchars)))))))
    :decode-literal-code-unit-limit #xd800  
    :encode-literal-char-code-limit #x10000
    :nul-encoding #(0 0)
    )

;;; utf-16, reversed byte order
(define-character-encoding #+big-endian-target :utf-16le #-big-endian-target :utf-16be
   #+little-endian-target
   "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
big-endian word and characters with larger codes can be encoded in a
pair of 16-bit big-endian words.  The endianness of the encoded data
is implicit in the encoding; byte-order-mark characters are not
interpreted on input or prepended to output."
  #+big-endian-target
  "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
little-endian word and characters with larger codes can be encoded in
a pair of 16-bit little-endian words.  The endianness of the encoded
data is implicit in the encoding; byte-order-mark characters are not
interpreted on input or prepended to output."
  :max-units-per-char 2
  :code-unit-size 16
  :native-endianness nil
  :stream-encode-function
  #'utf-16-stream-encode
  :stream-decode-function
  #'utf-16-stream-decode
  :vector-encode-function
  (nfunction
   reversed-utf-16-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx start end))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (declare (fixnum i))
       (let* ((char (schar string i))
              (code (char-code char))
              (highbits (- code #x10000)))
         (declare (type (mod #x110000) code)
                  (fixnum highbits))
         (cond ((< highbits 0)
                (setf (%reversed-u8-ref-u16 vector idx) code)
                (incf idx 2))
               (t
                (let* ((firstword (logior #xd800 (the fixnum (ash highbits -10))))
                       (secondword (logior #xdc00 (the fixnum (logand highbits #x3ff)))))
                  (declare (type (unsigned-byte 16) firstword secondword))
                  (setf (%reversed-u8-ref-u16 vector idx) firstword
                        (%reversed-u8-ref-u16 vector (the fixnum (+ idx 2))) secondword)
                  (incf idx 4))))))))
  :vector-decode-function
  (nfunction
   reversed-utf-16-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (type index idx))
     (do* ((i 0 (1+ i))
           (end (+ idx noctets))
           (index idx))
          ((= index end) index)
       (declare (fixnum i end index))
       (let* ((1st-unit (%reversed-u8-ref-u16 vector index)))
         (declare (type (unsigned-byte 16) 1st-unit))
         (incf index 2)
         (let* ((char
                 (if (or (< 1st-unit #xd800)
                         (>= 1st-unit #xe000))
                   (code-char 1st-unit)
                   (if (< 1st-unit #xdc00)
                     (let* ((2nd-unit (%reversed-u8-ref-u16 vector index)))
                       (declare (type (unsigned-byte 16) 2nd-unit))
                       (incf index 2)
                       (if (and (>= 2nd-unit #xdc00)
                                (< 2nd-unit #xe000))
                         (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)))))))
           (setf (schar string i) (or char #\Replacement_Character)))))))
  :memory-encode-function
  (nfunction
   reversed-utf-16-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (highbits (- code #x10000)))
         (declare (type (mod #x110000) code)
                  (fixnum  highbits))
         (cond ((< highbits 0)
                (setf (%get-unsigned-word pointer idx) (%swap-u16 code))
                (incf idx 2))
               (t
                (setf (%get-unsigned-word pointer idx) (%swap-u16 (logior #xd800 (the fixnum (ash highbits -10)))))
                (incf idx 2)
                (setf (%get-unsigned-word pointer idx) (%swap-u16 (logior #xdc00 (the fixnum (logand highbits #x3ff)))))
                (incf idx 2)))))))
  :memory-decode-function
  (nfunction
   reversed-utf-16-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum noctets idx))
     (do* ((i 0 (1+ i))
           (end (+ idx noctets))
           (index idx))
          ((>= index end) index)
       (declare (fixnum i index end))
       (let* ((1st-unit (%swap-u16 (%get-unsigned-word pointer index))))
         (declare (type (unsigned-byte 16) 1st-unit))
         (incf index 2)
         (let* ((char
                 (if (or (< 1st-unit #xd800)
                         (>= 1st-unit #xe000))
                   (code-char 1st-unit)
                   (if (< 1st-unit #xdc00)
                     (let* ((2nd-unit (%swap-u16 (%get-unsigned-word pointer index))))
                       (declare (type (unsigned-byte 16) 2nd-unit))
                       (incf index)
                       (if (and (>= 2nd-unit #xdc00)
                                (< 2nd-unit #xe000))
                         (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)))))))
           (setf (schar string i) (or char #\Replacement_Character)))))))
  :octets-in-string-function
  #'utf-16-octets-in-string
  :length-of-vector-encoding-function
  (nfunction
   reversed-utf-16-length-of-vector-encoding
   (lambda (vector start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector))
     (declare (fixnum start end))
     (do* ((i start)
           (j (+ 2 i) (+ 2 i))
           (nchars 0))
          ((> j end) (values nchars i))
       (declare (fixnum i j nchars))
       (let* ((code (%reversed-u8-ref-u16 vector i))
              (nexti (+ i (if (or (< code #xd800)
                                  (>= code #xdc00))
                            2
                            4))))
         (declare (type (unsigned-byte 16) code)
                  (fixnum nexti))
         (if (> nexti end)
           (return (values nchars i))
           (setq i nexti nchars (1+ nchars)))))))
  :length-of-memory-encoding-function
  (nfunction
   reversed-utf-16-length-of-memory-encoding
   (lambda (pointer noctets start)
     (do* ((i start)
           (j (+ i 2) (+ i 2))
           (end (+ start noctets))
           (nchars 0))
          ((> j end) (values nchars i))
       (let* ((code (%swap-u16 (%get-unsigned-word pointer i)))
              (nexti (+ i (if (or (< code #xd800)
                                  (>= code #xdc00))
                            2
                            4))))
         (declare (type (unsigned-byte 16) code)
                  (fixnum nexti))
         (if (> nexti end)
           (return (values nchars i))
           (setq i nexti nchars (1+ nchars)))))))
  :decode-literal-code-unit-limit #xd800
  :encode-literal-char-code-limit #x10000
  :nul-encoding #(0 0)
  )

;;; UTF-16.  Memory and vector functions determine endianness of
;;; input by the presence of a byte-order mark (or swapped BOM)
;;; at the beginning of input, and assume big-endian order
;;; if this mark is missing; on output, a BOM is prepended and
;;; things are written in native byte order.
;;; The endianness of stream-io operations is determined by
;;; stream content; new output streams are written in native
;;; endianness with a BOM character prepended.  Input streams
;;; are read in native byte order if the initial character is
;;; a BOM, in reversed byte order if the initial character is
;;; a swapped BOM, and in big-endian order (per RFC 2781) if
;;; there is no BOM.

(define-character-encoding :utf-16
    "A 16-bit, variable-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
word and characters with larger codes can be encoded in a
pair of 16-bit words.  The endianness of the encoded data is
indicated by the endianness of a byte-order-mark character (#\u+feff)
prepended to the data; in the absence of such a character on input,
the data is assumed to be in big-endian order. Output is written
in native byte-order with a leading byte-order mark."    
  :max-units-per-char 2
  :code-unit-size 16
  :native-endianness t                  ;not necessarily true.
  :stream-encode-function
  #'utf-16-stream-encode
  :stream-decode-function
  #'utf-16-stream-decode
  :vector-encode-function
  (nfunction
   utf-16-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (setf (%native-u8-ref-u16 vector idx) byte-order-mark-char-code)
     (incf idx 2)
     (do* ((i start (1+ i)))
            ((>= i end) idx)
         (declare (fixnum i))
         (let* ((char (schar string i))
                (code (char-code char))
                (highbits (- code #x10000)))
           (declare (type (mod #x110000) code)
                    (fixnum highbits))
           (cond ((< highbits 0)
                  (setf (%native-u8-ref-u16 vector idx) code)
                  (incf idx 2))
                 (t
                  (let* ((firstword (logior #xd800 (the fixnum (ash highbits -10))))
                         (secondword (logior #xdc00 (the fixnum (logand highbits #x3ff)))))
                    (declare (type (unsigned-byte 16) firstword secondword))
                    (setf (%native-u8-ref-u16 vector idx) firstword
                          (%native-u8-ref-u16 vector (the fixnum (+ idx 2))) secondword)
                    (incf idx 4))))))))
  :vector-decode-function
  (nfunction
   utf-16-vector-decode 
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 16) (*)) vector)
              (type index idx))
     (let* ((origin idx)
            (swap (if (>= noctets 2)
                    (case (%native-u8-ref-u16 vector idx)
                      (#.byte-order-mark-char-code
                       (incf idx 2) nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf idx 2) t)
                      (t #+little-endian-target t)))))
       (do* ((i 0 (1+ i))
             (end (+ origin noctets))
             (index idx))
            ((= index end) index)
         (declare (fixnum i end index))
         (let* ((1st-unit (if swap
                            (%reversed-u8-ref-u16 vector index)
                            (%native-u8-ref-u16 vector index))))
           (declare (type (unsigned-byte 16) 1st-unit))
           (incf index 2)
           (let* ((char
                   (if (or (< 1st-unit #xd800)
                           (>= 1st-unit #xe000))
                     (code-char 1st-unit)
                     (if (< 1st-unit #xdc00)
                       (let* ((2nd-unit (if swap
                                          (%reversed-u8-ref-u16 vector index)
                                          (%native-u8-ref-u16 vector index))))
                         (declare (type (unsigned-byte 16) 2nd-unit))
                         (incf index 2)
                         (if (and (>= 2nd-unit #xdc00)
                                  (< 2nd-unit #xe000))
                           (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)))))))
             (setf (schar string i) (or char #\Replacement_Character))))))))
  :memory-encode-function
  (nfunction
   utf-16-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     ;; Output a BOM.
     (setf (%get-unsigned-word pointer idx) byte-order-mark-char-code)
     (incf idx 2)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i)))
              (highbits (- code #x10000)))
         (declare (type (mod #x110000) code)
                  (fixnum highbits))
         (cond ((< highbits 0)
                (setf (%get-unsigned-word pointer idx) code)
                (incf idx 2))
               (t
                (setf (%get-unsigned-word pointer idx) (logior #xd800 (the fixnum (ash highbits -10))))
                (setf (%get-unsigned-word pointer (the fixnum (+ idx 2)))
                      (logior #xdc00 (the fixnum (logand highbits #x3ff))))
                (incf idx 4)))))))
  :memory-decode-function
  (nfunction
   utf-16-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum noctets idx))
     (let* ((swap (when (> noctets 1)
                    (case (%get-unsigned-word pointer idx)
                      (#.byte-order-mark-char-code
                       (incf idx 2)
                       (decf noctets 2)
                       nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf idx 2)
                       (decf noctets 2)
                       t)
                      (t #+little-endian-target t)))))
       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx ))
            ((>= index end) index)
         (declare (fixnum i index end))
         (let* ((1st-unit (%get-unsigned-word pointer index)))
           (declare (type (unsigned-byte 16) 1st-unit))
           (incf index 2)
           (if swap (setq 1st-unit (%swap-u16 1st-unit)))
           (let* ((char
                   (if (or (< 1st-unit #xd800)
                           (>= 1st-unit #xe000))
                     (code-char 1st-unit)
                     (if (< 1st-unit #xdc00)
                       (let* ((2nd-unit (%get-unsigned-byte pointer index)))
                         (declare (type (unsigned-byte 16) 2nd-unit))
                         (if swap (setq 2nd-unit (%swap-u16 2nd-unit)))
                         (incf index 2)
                         (if (and (>= 2nd-unit #xdc00)
                                  (< 2nd-unit #xe000))
                           (utf-16-combine-surrogate-pairs 1st-unit 2nd-unit)))))))
             (setf (schar string i) (or char #\Replacement_Character))))))))
  :octets-in-string-function
  (nfunction
   utf-16-bom-octets-in-string
   (lambda (string start end)
     (+ 2 (utf-16-octets-in-string string start end))))
  :length-of-vector-encoding-function
  (nfunction
   utf-16-length-of-vector-encoding
   (lambda (vector start end)
     (declare (type (simple-array (unsigned-byte 16) (*)) vector))
     (let* ((swap (when (>= end (+ start 2))
                    (case (%native-u8-ref-u16 vector start)
                      (#.byte-order-mark-char-code
                       (incf start 2)
                       nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf start 2)
                       t)
                      (t #+little-endian-target t)))))
       (do* ((i start)
             (j (+ 2 i) (+ 2 j))
             (nchars 0))
            ((> j end)
             (values nchars i))
         (let* ((code (if swap
                        (%reversed-u8-ref-u16 vector i)
                        (%native-u8-ref-u16 vector i)))
                (nexti (+ i (if (or (< code #xd800)
                                    (>= code #xdc00))
                              2
                              4))))
           (declare (type (unsigned-byte 16) code)
                    (fixnum nexti))
           (if (> nexti end)
             (return (values nchars i))
             (setq i nexti nchars (1+ nchars))))))))
  :length-of-memory-encoding-function
  (nfunction
   utf-16-length-of-memory-encoding
   (lambda (pointer noctets start)
     (declare (fixnum noctets start))
     (when (oddp noctets)
       (setq noctets (1- noctets)))
     (let* ((origin start)
            (swap (when (>= noctets 2)
                    (case (%get-unsigned-word pointer (+ start start))
                      (#.byte-order-mark-char-code
                       (incf start 2)
                       nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf start 2)
                       t)
                      (t #+little-endian-target t)))))
       (declare (fixnum origin))
       (do* ((i start)
             (j (+ i 2) (+ i 2))
             (end (+ origin noctets))
             (nchars 0 (1+ nchars)))
            ((> j end) (values nchars (- i origin)))
         (declare (fixnum (i j end nchars)))
         (let* ((code (%get-unsigned-word pointer i)))
           (declare (type (unsigned-byte 16) code))
           (if swap (setq code (%swap-u16 code)))
           (let* ((nexti (+ i (if (or (< code #xd800)
                                      (>= code #xdc00))
                                2
                                4))))
             (declare (fixnum nexti))
             (if (> nexti end)
               (return (values nchars (- i origin)))
               (setq i nexti))))))))
  :decode-literal-code-unit-limit #xd800
  :encode-literal-char-code-limit #x10000  
  :use-byte-order-mark
  #+big-endian-target :utf-16le
  #+little-endian-target :utf-16be
  :bom-encoding #+big-endian-target #(#xfe #xff) #+little-endian-target #(#xff #xfe)
  :nul-encoding #(0 0)
  )


(defun ucs-2-stream-encode (char write-function stream)
  (let* ((code (char-code char)))
    (declare (type (mod #x110000) code))
    (if (>= code #x10000)
      (setq code (char-code #\Replacement_Character)))
    (funcall write-function stream code)
    1))

(defun ucs-2-stream-decode (1st-unit next-unit-function stream)
  (declare (type (unsigned-byte 16) 1st-unit)
           (ignore next-unit-function stream))
  ;; CODE-CHAR returns NIL on either half of a surrogate pair.
  (or (code-char 1st-unit)
      #\Replacement_Character))


(defun ucs-2-octets-in-string (string start end)
  (declare (ignore string))
  (if (>= end start)
    (* 2 (- end start))
    0))

(defun ucs-2-length-of-vector-encoding (vector start end)
  (declare (ignore vector))
  (let* ((noctets (max (- end start) 0)))
    (values (ash noctets -1) (+ start (logandc2 noctets 1)))))

(defun ucs-2-length-of-memory-encoding (pointer noctets start)
  (declare (ignore pointer start))
  (values (ash noctets -1) (logandc2 noctets 1)))



;;; UCS-2, native byte order
(define-character-encoding #+big-endian-target :ucs-2be #-big-endian-target :ucs-2le
  #+big-endian-target
  "A 16-bit, fixed-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
big-endian word. The encoded data is implicitly big-endian;
byte-order-mark characters are not interpreted on input or prepended
to output."
  #+little-endian-target
  "A 16-bit, fixed-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
little-endian word. The encoded data is implicitly little-endian;
byte-order-mark characters are not interpreted on input or prepended
to output."
  :max-units-per-char 1
  :code-unit-size 16
  :native-endianness t
  :stream-encode-function
  #'ucs-2-stream-encode
  :stream-decode-function
  #'ucs-2-stream-decode
  :vector-encode-function
  (nfunction
   native-ucs-2-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (when (>= code #x10000)
           (setq code (char-code #\Replacement_Character)))
         (setf (%native-u8-ref-u16 vector idx) code)
         (incf idx 2)))))
  :vector-decode-function
  (nfunction
   native-ucs-2-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (type index idx))
     (do* ((i 0 (1+ i))
           (end (+ idx noctets))
           (index idx (+ 2 index)))
          ((>= index end) index)
       (declare (fixnum i end index))
       (setf (schar string i)
             (or (code-char (%native-u8-ref-u16 vector index))
                 #\Replacement_Character)))))
  :memory-encode-function
  (nfunction
   native-ucs-2-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-word pointer idx)
                      (if (>= code #x10000)
                        (char-code #\Replacement_Character)
                        code))
         (incf idx 2)))))
  :memory-decode-function
  (nfunction
   native-ucs-2-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum noctets idx))
     (do* ((i 0 (1+ i))
           (index idx (+ index 2)))
          ((>= i noctets) index)
       (declare (fixnum i index))
       (let* ((1st-unit (%get-unsigned-word pointer index)))
         (declare (type (unsigned-byte 16) 1st-unit))
         (setf (schar string i) (or (char-code 1st-unit) #\Replacement_Character))))))
  :octets-in-string-function
  #'ucs-2-octets-in-string
  :length-of-vector-encoding-function
  #'ucs-2-length-of-vector-encoding
  :length-of-memory-encoding-function
  #'ucs-2-length-of-memory-encoding
  :decode-literal-code-unit-limit #x10000
  :encode-literal-char-code-limit #x10000  
  :nul-encoding #(0 0)
  )

;;; UCS-2, reversed byte order
(define-character-encoding #+big-endian-target :ucs-2le #-big-endian-target :ucs-2be
  #+little-endian-target
  "A 16-bit, fixed-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit
big-endian word. The encoded data is implicitly big-endian;
byte-order-mark characters are not interpreted on input or prepended
to output."
  #+big-endian-target
  "A 16-bit, fixed-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit

little-endian word. The encoded data is implicitly little-endian;
byte-order-mark characters are not interpreted on input or prepended
to output."
  :max-units-per-char 1
  :code-unit-size 16
  :native-endianness nil
  :stream-encode-function
  #'ucs-2-stream-encode
  :stream-decode-function
  #'ucs-2-stream-decode
  :vector-encode-function
  (nfunction
   reversed-ucs-2-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (when (>= code #x10000)
           (setq code (char-code #\Replacement_Character)))
         (setf (%reversed-u8-ref-u16 vector idx) code)
         (incf idx 2)))))
  :vector-decode-function
  (nfunction
   reversed-ucs-2-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (type index idx))
     (do* ((i 0 (1+ i))
           (end (+ idx noctets))
           (index idx (+ 2 index)))
          ((>= index end) index)
       (declare (fixnum i end index))
       (setf (schar string i)
             (or (code-char (%reversed-u8-ref-u16 vector index))
                 #\Replacement_Character)))))
  :memory-encode-function
  (nfunction
   reversed-ucs-2-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-word pointer idx)
               (if (>= code #x10000)
                 (%swap-u16 (char-code #\Replacement_Character))
                 (%swap-u16 code)))
         (incf idx 2)))))
  :memory-decode-function
  (nfunction
   reversed-ucs-2-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum noctets idx))
     (do* ((i 0 (1+ i))
           (index idx (+ index 2)))
          ((>= i noctets) index)
       (declare (fixnum i index))
       (let* ((1st-unit (%swap-u16 (%get-unsigned-word pointer index))))
         (declare (type (unsigned-byte 16) 1st-unit))
         (setf (schar string i) (or (code-char 1st-unit) #\Replacement_Character))))))
  :octets-in-string-function
  #'ucs-2-octets-in-string
  :length-of-vector-encoding-function
  #'ucs-2-length-of-vector-encoding
  :length-of-memory-encoding-function
  #'ucs-2-length-of-memory-encoding
  :decode-literal-code-unit-limit #x10000
  :encode-literal-char-code-limit #x10000
  :nul-encoding #(0 0)
  )

(define-character-encoding :ucs-2
    "A 16-bit, fixed-length encoding in which characters with
CHAR-CODEs less than #x10000 can be encoded in a single 16-bit word.
The endianness of the encoded data is indicated by the endianness of a
byte-order-mark character (#\u+feff) prepended to the data; in the
absence of such a character on input, the data is assumed to be in
big-endian order."
  :max-units-per-char 1
  :code-unit-size 16
  :native-endianness t                  ;not necessarily true.
  :stream-encode-function
  #'ucs-2-stream-encode
  :stream-decode-function
  #'ucs-2-stream-decode
  :vector-encode-function
  (nfunction
   ucs-2-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (setf (%native-u8-ref-u16 vector idx) byte-order-mark-char-code)
     (incf idx 2)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (when (>= code #x10000)
           (setq code (char-code #\Replacement_Character)))
         (setf (%native-u8-ref-u16 vector idx) code)
         (incf idx 2)))))
  :vector-decode-function
  (nfunction
   ucs-2-vector-decode 
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (type index idx)
              (fixnum noctets))
     (let* ((swap (if (> noctets 1)
                    (case (%native-u8-ref-u16 vector idx)
                      (#.byte-order-mark-char-code
                       (incf idx 2) (decf noctets 2) nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf idx 2) (decf noctets 2) t)
                       (t #+little-endian-target t)))))

       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx (1+ index)))
            ((>= index end) index)
         (declare (fixnum i end index))
         (let* ((1st-unit (if swap
                            (%reversed-u8-ref-u16 vector index)
                            (%native-u8-ref-u16 vector index))))
             (declare (type (unsigned-byte 16) 1st-unit))
             (setf (schar string i) (or (code-char 1st-unit) #\Replacement_Character)))))))
  :memory-encode-function
  (nfunction
   ucs-2-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (setf (%get-unsigned-word pointer idx) byte-order-mark-char-code)
     (incf idx 2)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-word pointer idx)
                      (if (>= code #x10000)
                        (char-code #\Replacement_Character)
                        code))
         (incf idx 2)))))
  :memory-decode-function
  (nfunction
   ucs-2-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum noctets idx))
     (let* ((swap (when (> noctets 1)
                    (case (%get-unsigned-word pointer idx)
                      (#.byte-order-mark-char-code
                       (incf idx 2)
                       (decf noctets 2)
                       nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf idx 2)
                       (decf noctets 2)
                       t)
                      (t #+little-endian-target t)))))
       (do* ((i 0 (1+ i))
           (index idx (+ index 2)))
          ((>= i noctets) index)
       (declare (fixnum i index))
       (let* ((1st-unit (%get-unsigned-word pointer index)))
         (declare (type (unsigned-byte 16) 1st-unit))
         (if swap (setq 1st-unit (%swap-u16 1st-unit)))
         (setf (schar string i) (or (code-char 1st-unit) #\Replacement_Character)))))))
  :octets-in-string-function
  (nfunction
   ucs-2-bom-octets-in-string
   (lambda (string start end)
     (+ 2 (ucs-2-octets-in-string string start end))))
  :length-of-vector-encoding-function
  (nfunction
   ucs-2-length-of-vector-encoding
   (lambda (vector start end)
     (declare (fixnum start end))
     (when (>= end (+ start 2))
       (let* ((maybe-bom (%native-u8-ref-u16 vector start)))
         (declare (type (unsigned-byte 16) maybe-bom))
         (when (or (= maybe-bom byte-order-mark-char-code)
                   (= maybe-bom swapped-byte-order-mark-char-code))
           (incf start 2))))
     (do* ((i start j)
           (j (+ i 2) (+ j 2))
           (nchars 0 (1+ nchars)))
          ((> j end) (values nchars i)))))
  :length-of-memory-encoding-function
  (nfunction
   ucs-2-length-of-memory-encoding
   (lambda (pointer noctets start)
     (let* ((skip 
             (when (> noctets 1)
               (case (%get-unsigned-word pointer start)
                 (#.byte-order-mark-char-code
                  2)
                 (#.swapped-byte-order-mark-char-code
                  2)))))
     (values (ash (- noctets skip) -1) (logandc2 noctets 1)))))
  :decode-literal-code-unit-limit #x10000
  :encode-literal-char-code-limit #x10000  
  :use-byte-order-mark
  #+big-endian-target :ucs-2le
  #+little-endian-target :ucs-2be
  :nul-encoding #(0 0)
  )


(defun ucs-4-stream-encode (char write-function stream)
  (let* ((code (char-code char)))
    (declare (type (mod #x110000) code))
    (funcall write-function stream code)
    1))

(defun ucs-4-stream-decode (1st-unit next-unit-function stream)
  (declare (type (unsigned-byte 16) 1st-unit)
           (ignore next-unit-function stream))
  (code-char 1st-unit))


(defun ucs-4-octets-in-string (string start end)
  (declare (ignore string))
  (if (>= end start)
    (* 4 (- end start))
    0))


(declaim (inline %big-endian-u8-ref-u32 %little-endian-u8-ref-u32))
(defun %big-endian-u8-ref-u32 (u8-vector idx)
  (declare (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (fixnum idx))
  (logior (the (unsigned-byte 32) (ash (the (unsigned-byte 8) (aref u8-vector idx)) 24))
          (the (unsigned-byte 24)
            (logior
             (ash (the (unsigned-byte 8) (aref u8-vector (the fixnum (1+ idx)))) 16)
             (the (unsigned-byte 16)
               (logior
                (ash (the (unsigned-byte 8) (aref u8-vector (the fixnum (+ idx 2)))) 8)
                (the (unsigned-byte 8) (aref u8-vector (the fixnum (+ idx 3))))))))))

(defun %little-endian-u8-ref-u32 (u8-vector idx)
  (declare (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (fixnum idx))
  (logior (the (unsigned-byte 32) (ash (the (unsigned-byte 8) (aref u8-vector (the fixnum (+ idx 3)))) 24))
          (the (unsigned-byte 24)
            (logior
             (ash (the (unsigned-byte 8) (aref u8-vector (the fixnum (+ idx 2)))) 16)
             (the (unsigned-byte 16)
               (logior
                (ash (the (unsigned-byte 8) (aref u8-vector (the fixnum (1+ idx)))) 8)
                (the (unsigned-byte 8) (aref u8-vector (the fixnum idx)))))))))

#+big-endian-target
(progn
(defmacro %native-u8-ref-u32 (vector idx)
  `(%big-endian-u8-ref-u32 ,vector ,idx))

(defmacro %reversed-u8-ref-u32 (vector idx)
  `(%little-endian-u8-ref-u32 ,vector ,idx))
)

#+little-endian-target
(progn
(defmacro %native-u8-ref-u32 (vector idx)
  `(%little-endian-u8-ref-u32 ,vector ,idx))

(defmacro %reversed-u8-ref-u32 (vector idx)
  `(%big-endian-u8-ref-u32 ,vector ,idx))
)


(declaim (inline (setf %big-endian-u8-ref-32) (setf %little-endian-u8-ref-u32)))
(defun (setf %big-endian-u8-ref-u32) (val u8-vector idx)
  (declare (type (unsigned-byte 32) val)
           (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (fixnum idx))
  (setf (aref u8-vector idx) (ldb (byte 8 24) val)
        (aref u8-vector (the fixnum (1+ idx))) (ldb (byte 8 16) val)
        (aref u8-vector (the fixnum (+ idx 2))) (ldb (byte 8 8) val)
        (aref u8-vector (the fixnum (+ idx 3))) (ldb (byte 8 0) val))
  val)

(defun (setf %little-endian-u8-ref-u32) (val u8-vector idx)
  (declare (type (unsigned-byte 32) val)
           (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (fixnum idx))
  (setf (aref u8-vector idx) (ldb (byte 8 0) val)
        (aref u8-vector (the fixnum (1+ idx))) (ldb (byte 8 8) val)
        (aref u8-vector (the fixnum (+ idx 2))) (ldb (byte 8 16) val)
        (aref u8-vector (the fixnum (+ idx 3))) (ldb (byte 8 24) val))
  val)


;;; UTF-32/UCS-4, native byte order
(define-character-encoding #+big-endian-target :utf-32be #-big-endian-target :utf32le
  #+big-endian-target
  "A 32-bit, fixed-length encoding in which all Unicode characters
encoded in a single 32-bit word. The encoded data is implicitly big-endian;
byte-order-mark characters are not interpreted on input or prepended
to output."
  #+little-endian-target
  "A 32-bit, fixed-length encoding in which all Unicode characters can
encoded in a single 32-bit word. The encoded data is implicitly
little-endian; byte-order-mark characters are not interpreted on input
or prepended to output."
  :aliases #+big-endian-target '(:ucs-4be) #+little-endian-target '(:ucs-4le)
  :max-units-per-char 1
  :code-unit-size 32
  :native-endianness t
  :stream-encode-function
  #'ucs-4-stream-encode
  :Stream-decode-function
  #'ucs-4-stream-decode
  :vector-encode-function
  (nfunction
   native-ucs-4-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (setf (%native-u8-ref-u32 vector idx) code)
         (incf idx 4)))))
  :vector-decode-function
  (nfunction
   native-ucs-4-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (type index idx))
     (do* ((i 0 (1+ i))
           (end (+ idx noctets))
           (index idx (+ 4 index)))
          ((>= index end) index)
       (declare (fixnum i end index))
       (let* ((code (%native-u8-ref-u32 vector index)))
         (declare (type (unsigned-byte 32) code))
         (setf (schar string i)
               (or (if (< code char-code-limit)
                      (code-char code))
                   #\Replacement_Character))))))
  :memory-encode-function
  (nfunction
   native-ucs-4-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-long pointer idx) code)
         (incf idx 4)))))
  :memory-decode-function
  (nfunction
   native-ucs-4-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum noctets idx))
     (do* ((i 0 (1+ i))
           (index idx (+ index 4)))
          ((>= i noctets) index)
       (declare (fixnum i index))
       (let* ((1st-unit (%get-unsigned-long pointer index)))
         (declare (type (unsigned-byte 32) 1st-unit))
         (setf (schar string i) (or (if (< 1st-unit char-code-limit)
                                      (code-char 1st-unit))
                                    #\Replacement_Character))))))
  :octets-in-string-function
  #'ucs-4-octets-in-string
  :length-of-vector-encoding-function
  (nfunction
   native-ucs-4-length-of-vector-encoding
   (lambda (vector start end)
     (declare (ignore vector))
     (do* ((i start j)
           (j (+ i 4) (+ j 4))
           (nchars 0 (1+ nchars)))
          ((> j end) (values nchars i)))))
  :length-of-memory-encoding-function
  (nfunction
   native-ucs-4-length-of-memory-encoding
   (lambda (pointer noctets start)
     (declare (ignore pointer))
     (values (ash noctets -2) (+ start (logandc2 noctets 3)))))
  :decode-literal-code-unit-limit #x110000
  :encode-literal-char-code-limit #x110000
  :nul-encoding #(0 0 0 0)
  )

;;; UTF-32/UCS-4, reversed byte order
(define-character-encoding #+big-endian-target :utf-32le #-big-endian-target :utf-32be
  #+little-endian-target
  "A 32-bit, fixed-length encoding in which all Unicode characters
encoded in a single 32-bit word. The encoded data is implicitly big-endian;
byte-order-mark characters are not interpreted on input or prepended
to output."
  #+big-endian-target
  "A 32-bit, fixed-length encoding in which all Unicode characters can
encoded in a single 32-bit word. The encoded data is implicitly
little-endian; byte-order-mark characters are not interpreted on input
or prepended to output."
  :aliases #+big-endian-target '(:ucs-4le) #+little-endian-target '(:ucs-4be)
  :max-units-per-char 1
  :code-unit-size 32
  :native-endianness nil
  :stream-encode-function
  #'ucs-4-stream-encode
  :Stream-decode-function
  #'ucs-4-stream-decode
  :vector-encode-function
  (nfunction
   native-ucs-4-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (setf (%reversed-u8-ref-u32 vector idx) code)
         (incf idx 4)))))
  :vector-decode-function
  (nfunction
   native-ucs-4-vector-decode
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (type index idx))
     (do* ((i 0 (1+ i))
           (end (+ idx noctets))
           (index idx (+ 4 index)))
          ((>= index end) index)
       (declare (fixnum i end index))
       (let* ((code (%reversed-u8-ref-u32 vector index)))
         (declare (type (unsigned-byte 32) code))
         (setf (schar string i)
               (or (if (< code char-code-limit)
                     (code-char code))
                   #\Replacement_Character))))))
  :memory-encode-function
  (nfunction
   native-ucs-4-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-long pointer idx) (%swap-u32 code))
         (incf idx 4)))))
  :memory-decode-function
  (nfunction
   reversed-ucs-4-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum noctets idx))
     (do* ((i 0 (1+ i))
           (index idx (+ index 4)))
          ((>= i noctets) index)
       (declare (fixnum i index))
       (let* ((1st-unit (%swap-u32 (%get-unsigned-long pointer index))))
         (declare (type (unsigned-byte 32) 1st-unit))
         (setf (schar string i) (or (if (< 1st-unit char-code-limit)
                                      (code-char 1st-unit))
                                    #\Replacement_Character))))))

  :octets-in-string-function
  #'ucs-4-octets-in-string
  :length-of-vector-encoding-function
  (nfunction
   reversed-ucs-4-length-of-vector-encoding
   (lambda (vector start end)
     (declare (ignore vector))
     (do* ((i start j)
           (j (+ i 4) (+ j 4))
           (nchars 0 (1+ nchars)))
          ((> j end) (values nchars i)))))
  :length-of-memory-encoding-function
  (nfunction
   reversed-ucs-4-length-of-memory-encoding
   (lambda (pointer noctets start)
     (declare (ignore pointer))
     (values (ash noctets -2) (+ start (logandc2 noctets 3)))))
  :decode-literal-code-unit-limit #x110000
  :encode-literal-char-code-limit #x110000
  :nul-encoding #(0 0 0 0)  
  )

(define-character-encoding :utf-32
    "A 32-bit, fixed-length encoding in which all Unicode characters can be encoded in a single 32-bit word.  The endianness of the encoded data is indicated by the endianness of a byte-order-mark character (#\u+feff) prepended to the data; in the absence of such a character on input, input data is assumed to be in big-endian order.  Output is written in native byte order with a leading byte-order mark."
    
  :aliases '(:ucs-4)
  :max-units-per-char 1
  :code-unit-size 32
  :native-endianness t                  ;not necessarily true.
  :stream-encode-function
  #+ucs-4-stream-encode
  :stream-decode-function
  #'ucs-4-stream-decode
  :vector-encode-function
  (nfunction
   utf-32-vector-encode
   (lambda (string vector idx start end)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (fixnum idx))
     (setf (%native-u8-ref-u32 vector idx) byte-order-mark-char-code)
     (incf idx 4)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (setf (%native-u8-ref-u32 vector idx) code)
         (incf idx 4)))))
  :vector-decode-function
  (nfunction
   utf-32-vector-decode 
   (lambda (vector idx noctets string)
     (declare (type (simple-array (unsigned-byte 8) (*)) vector)
              (type index idx)
              (fixnum noctets))
     (let* ((swap (if (> noctets 3)
                    (case (%native-u8-ref-u32 vector idx)
                      (#.byte-order-mark-char-code
                       (incf idx 4) (decf noctets 4) nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf idx 4) (decf noctets 4) t)
                       (t #+little-endian-target t)))))

       (do* ((i 0 (1+ i))
             (end (+ idx noctets))
             (index idx (1+ index)))
            ((>= index end) index)
         (declare (fixnum i end index))
         (let* ((1st-unit (if swap
                            (%reversed-u8-ref-u32 vector index)
                            (%native-u8-ref-u32 vector index))))
             (declare (type (unsigned-byte 32) 1st-unit))
             (setf (schar string i) (or (if (< 1st-unit char-code-limit)
                                          (code-char 1st-unit))
                                        #\Replacement_Character)))))))
  :memory-encode-function
  (nfunction
   utf-32-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (setf (%get-unsigned-long pointer idx) byte-order-mark-char-code)
     (incf idx 4)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((code (char-code (schar string i))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-long pointer idx) code)
         (incf idx 4)))))
  :memory-decode-function
  (nfunction
   utf-32-memory-decode
   (lambda (pointer noctets idx string)
     (declare (fixnum noctets idx))
     (let* ((swap (when (> noctets 3)
                    (case (%get-unsigned-long pointer idx)
                      (#.byte-order-mark-char-code
                       (incf idx 4)
                       (decf noctets 4)
                       nil)
                      (#.swapped-byte-order-mark-char-code
                       (incf idx 4)
                       (decf noctets 4)
                       t)
                      (t #+little-endian-target t)))))
       (do* ((i 0 (1+ i))
           (index idx (+ index 2)))
          ((>= i noctets) index)
       (declare (fixnum i index))
       (let* ((1st-unit (%get-unsigned-long pointer index)))
         (declare (type (unsigned-byte 32) 1st-unit))
         (if swap (setq 1st-unit (%swap-u32 1st-unit)))
         (setf (schar string i) (or (if (< 1st-unit char-code-limit)
                                      (code-char 1st-unit))
                                    #\Replacement_Character)))))))
  :octets-in-string-function
  (nfunction
   utf-32-bom-octets-in-string
   (lambda (string start end)
     (+ 4 (ucs-4-octets-in-string string start end))))
  :length-of-vector-encoding-function
  (nfunction
   utf-32-length-of-vector-encoding
   (lambda (vector start end)
     (when (>= end (+ start 4))
       (let* ((maybe-bom (%native-u8-ref-u32 vector start)))
         (declare (type (unsigned-byte 32) maybe-bom))
         (when (or (= maybe-bom byte-order-mark-char-code)
                   (= maybe-bom swapped-byte-order-mark-char-code))
           (incf start 4))))
     (do* ((i start j)
           (j (+ i 4) (+ J 4))
           (nchars 0 (1+ nchars)))
          ((> j end) (values nchars i)))))
  :length-of-memory-encoding-function
  (nfunction
   utf-32-length-of-memory-encoding
   (lambda (pointer noctets start)
     (when (> noctets 3)
       (case (%get-unsigned-long pointer )
         (#.byte-order-mark-char-code
          (incf start 4)
          (decf noctets 4))
         (#.swapped-byte-order-mark-char-code
          (incf start 4)
          (decf noctets 4))))
     (values (ash noctets -2) (+ start (logandc2 noctets 3)))))
  :decode-literal-code-unit-limit #x110000
  :encode-literal-char-code-limit #x110000  
  :use-byte-order-mark
  #+big-endian-target :utf-32le
  #+little-endian-target :utf-32be
  :bom-encoding #+big-endian-target #(#x00 #x00 #xfe #xff) #+little-endian-target #(#xff #xfe #x00 #x00)
  :nul-encoding #(0 0 0 0)  
  )

(defun describe-character-encoding (name)
  (let* ((enc (lookup-character-encoding name)))
    (when enc
      (let* ((name (character-encoding-name enc))
             (doc (character-encoding-documentation enc))
             (aliases (character-encoding-aliases enc)))
        (format t "~&~s" name)
        (when (null (car aliases))
          (pop aliases))
        (when aliases
          (format t " [Aliases:~{ ~s~}]" aliases))
        (format t "~&~a~%~%"  doc)
        (values)))))
      
(defun describe-character-encodings ()
  (let* ((names nil))
    (maphash #'(lambda (name enc)
                 (when (eq name (character-encoding-name enc))
                   (push name names)))
             *character-encodings*)
    (dolist (name (sort names #'string<) (values))
      (describe-character-encoding name))))

(defmethod make-load-form ((c character-encoding) &optional environment)
  (declare (ignore environment))
  `(get-character-encoding ,(character-encoding-name c)))

(defvar *native-newline-string* (make-string 1 :initial-element #\Newline))
(defvar *unicode-newline-string* (make-string 1 :initial-element #\Line_Separator))
(defvar *cr-newline-string* (make-string 1 :initial-element #\Return))
(defvar *crlf-newline-string* (make-array 2 :element-type 'character :initial-contents '(#\Return #\Linefeed)))
(defvar *nul-string* (make-string 1 :initial-element #\Nul))

(defun string-size-in-octets (string &key
                                     (start 0)
                                     end
                                     external-format
                                     use-byte-order-mark)
  (setq end (check-sequence-bounds string start end))
  (let* ((ef (normalize-external-format t external-format)))
    (%string-size-in-octets string
                            start
                            end
                            (get-character-encoding
                             (external-format-character-encoding ef))
                            (cdr (assoc (external-format-line-termination ef)
                                        *canonical-line-termination-conventions*))
                            use-byte-order-mark)))
  

(defun %string-size-in-octets (string start end encoding line-termination use-byte-order-mark)  
    (declare (fixnum start end))
    (multiple-value-bind (simple-string offset)
        (array-data-and-offset string)
      (declare (fixnum offset) (simple-string simple-string))
      (incf start offset)
      (incf end offset)
      (let* ((n (if use-byte-order-mark
                  (length (character-encoding-bom-encoding encoding))
                  0))
             (f (character-encoding-octets-in-string-function encoding))
             (nlpos (if line-termination
                      (position #\Newline simple-string :start start :end end))))
        (if (not nlpos)
          (+ n (funcall f simple-string start end))
          (let* ((nlstring (case line-termination
                             (:cr *cr-newline-string*)
                             (:crlf *crlf-newline-string*)
                             (:unicode *unicode-newline-string*)))
                 (nlstring-length (if (eq line-termination :crlf) 2 1)))
            (do* ()
                 ((null nlpos) (+ n (funcall f simple-string start end)))
              (unless (eql nlpos start)
                (incf n (funcall f simple-string start nlpos)))
              (incf n (funcall f nlstring 0 nlstring-length))
              (setq start (1+ nlpos)
                    nlpos (position #\Newline simple-string :start start :end end))))))))

(defun encode-string-to-octets (string &key
                                       (start 0)
                                       end
                                       external-format
                                       use-byte-order-mark
                                       (vector nil vector-p)
                                       (vector-offset 0))
  (setq end (check-sequence-bounds string start end))
  (let* ((ef (normalize-external-format t external-format)) 
         (encoding (get-character-encoding
                    (external-format-character-encoding ef)))
         (line-termination (cdr (assoc (external-format-line-termination ef)
                                       *canonical-line-termination-conventions*)))
         (n (%string-size-in-octets string start end encoding line-termination use-byte-order-mark)))
    (declare (fixnum start end n))
    (unless (and (typep vector-offset 'fixnum)
                 (or (not vector-p)
                     (< vector-offset (length vector))))
      (error "Invalid vector offset ~s" vector-offset))
    (if (not vector-p)
      (setq vector (make-array (+ n vector-offset)
                               :element-type '(unsigned-byte 8)))
      (progn
        (unless (= (typecode vector) target::subtag-u8-vector)
          (report-bad-arg vector '(simple-array (unsigned-byte 8) (*))))
        (unless (>= (length vector) (+ vector-offset n))
          (error "Can't encode ~s into supplied vector ~s; ~&~d octets are needed, but only ~d are available" string vector n (- (length vector) vector-offset)))))
    (when use-byte-order-mark
      (let* ((bom (character-encoding-bom-encoding encoding)))
        (dotimes (i (length bom))
          (setf (aref vector vector-offset)
                (aref bom i))
          (incf vector-offset))))
    (multiple-value-bind (simple-string offset) (array-data-and-offset string)
      (incf start offset)
      (incf end offset)
      (let* ((f (character-encoding-vector-encode-function encoding))
             (nlpos (if line-termination
                      (position #\Newline simple-string :start start :end end))))
        (if (null nlpos)
          (setq vector-offset
                (funcall f simple-string vector vector-offset start end))
          (let* ((nlstring (case line-termination
                             (:cr *cr-newline-string*)
                             (:crlf *crlf-newline-string*)
                             (:unicode *unicode-newline-string*)))
                 (nlstring-length (if (eq line-termination :crlf) 2 1)))
            (do* ()
                 ((null nlpos)
                  (setq vector-offset
                        (funcall f simple-string vector vector-offset start end)))
              (unless (eql nlpos start)
                (setq vector-offset (funcall f simple-string vector vector-offset start nlpos)))
              (setq vector-offset (funcall f nlstring vector vector-offset 0 nlstring-length))
              (setq start (1+ nlpos)
                    nlpos (position #\Newline simple-string :start start :end end)))))
        (values vector vector-offset)))))



(defun count-characters-in-octet-vector (vector &key
                                                (start 0)
                                                end
                                                external-format)
  (setq end (check-sequence-bounds vector start end))
  (%count-characters-in-octet-vector
   vector
   start
   end
   (get-character-encoding (external-format-character-encoding (normalize-external-format t external-format)))))

(defun %count-characters-in-octet-vector (vector start end encoding)
  (unless (= (typecode vector) target::subtag-u8-vector)
    (report-bad-arg vector '(simple-array (unsigned-byte 8) (*))))
  (funcall (character-encoding-length-of-vector-encoding-function encoding)
           vector
           start
           end))
                                         

(defun decode-string-from-octets (vector &key
                                         (start 0)
                                         end
                                         external-format
                                         (string nil string-p))
  (setq end (check-sequence-bounds vector start end))
  (unless (= (typecode vector) target::subtag-u8-vector)
    (multiple-value-bind (array offset)
        (array-data-and-offset vector)
      (unless (= (typecode array) target::subtag-u8-vector)
        (report-bad-arg vector '(array (unsgigned-byte 8) (*))))
      (setq vector array
            start (+ start offset)
            end (+ end offset))))
  (let* ((encoding (get-character-encoding
                    (external-format-character-encoding
                     (normalize-external-format t external-format)))))
    (multiple-value-bind (nchars last-octet)
        (%count-characters-in-octet-vector vector start end encoding)
      (if (not string-p)
        (setq string (make-string nchars))
        (progn
          (unless (= (typecode string) target::subtag-simple-base-string)
            (report-bad-arg string 'simple-string))
          (unless (>= (length string) nchars)
            (error "String ~s is too small; ~d characters are needed."
                   string nchars))))
      (funcall (character-encoding-vector-decode-function encoding)
               vector
               start
               (- last-octet start)
               string)
      (values string last-octet))))
      
                              
(defun string-encoded-length-in-bytes (encoding string start end)
  (if (typep string 'simple-base-string)
    (funcall (character-encoding-octets-in-string-function encoding)
             string
             (or start 0)
             (or end (length string)))
    (let* ((s (string string)))
      (multiple-value-bind (data offset) (array-data-and-offset s)
        (funcall (character-encoding-octets-in-string-function encoding)
                 data
                 (+ offset (or start 0))
                 (+ offset (or end (length s))))))))

;;; Same as above, but add the length of a trailing 0 code-unit.
(defun cstring-encoded-length-in-bytes (encoding string start end)
  (+ (ash (character-encoding-code-unit-size encoding) -3) ; NUL terminator
     (string-encoded-length-in-bytes encoding string start end)))

                   

(defun encode-string-to-memory (encoding pointer offset string start end)
  (if (typep string 'simple-base-string)
    (funcall (character-encoding-memory-encode-function encoding)
             string pointer offset (or start 0) (or end (length string)))
    (let* ((s (string string)))
      (multiple-value-bind (data data-offset)
          (array-data-and-offset s)
        (funcall (character-encoding-memory-encode-function encoding)
                 data pointer offset (+ data-offset (or start 0)) (+ data-offset (or end (length s))))))))

(defun get-encoded-string (encoding-name pointer noctets)
  (let* ((encoding (ensure-character-encoding encoding-name)))
    (multiple-value-bind (nchars nused)
        (funcall (character-encoding-length-of-memory-encoding-function encoding)
                 pointer
                 noctets
                 0)
      (let* ((string (make-string nchars)))
        (funcall (character-encoding-memory-decode-function encoding)
                 pointer
                 nused
                 0
                 string)
        string))))


(defun get-encoded-cstring (encoding-name pointer)
  (let* ((encoding (ensure-character-encoding encoding-name)))
    (get-encoded-string
     encoding
     pointer
     (ecase (character-encoding-code-unit-size encoding)
       (8 (%cstrlen pointer))
       (16 (do* ((i 0 (+ i 2)))
                ((= 0 (%get-unsigned-word pointer i))
                 (return i))
             (declare (fixnum i))))
       (32 (do* ((i 0 (+ i 4)))
                ((= 0 (%get-unsigned-long pointer i))
                 (return i))
             (declare (fixnum i))))))))
    

      




;;; This is an array of 256 integers, that (sparsely) encodes 64K bits.
;;; (There might be as many as 256 significant bits in some of entries
;;; in this table.)
(defstatic *bmp-combining-bitmap*
    #(
	#x00
        #x00
        #x00
        #xFFFF0000FFFFFFFFFFFFFFFFFFFF
        #x37800000000000000000000000000000000
        #x16BBFFFFFBFFFE000000000000000000000000000000000000
        #x3D9FFFC00000000000000000000000010000003FF8000000000000000000
        #x1FFC00000000000000000000007FFFFFF000000020000
        
	#x00
        #xC0080399FD00000000000000E0000000C001E3FFFD00000000000000E
        #x3BBFD00000000000000E0003000000003987D000000000000004
        #x803DC7C0000000000000040000000000C0398FD00000000000000E
        #x603DDFC00000000000000C0000000000603DDFC00000000000000E
        #xC0000FF5F8400000000000000000C0000000000803DCFC00000000000000C
        #x3F001BF20000000000000000000000007F8007F2000000000000
        #x401FFFFFFFFEFF00DFFFFE000000000000C2A0000003000000
        
        #x3C0000003C7F00000000000
        #x7FFFFFF0000000000003FFFFE000000000000000000000000
        #x00
        #x00
        #x00
        #x00
        #x00
        #xFFFFFFFF0000000000000000C0000000C0000001C0000001C0000        
        
        #x2000000000000000000000000000000000000003800
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
        #x7FFFFFF0000000000000000000000000000000000000000000000000000
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
        #x600000000000000000000000000FC0000000000
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        #x00
        
	#x00
        #x00
        #x00
        #x40000000
        #x00
        #x00
        #xF0000FFFF
        #x00))

(defun is-combinable (char)
  (let* ((code (char-code char)))
    (declare (type (mod #x110000) code))
    (when (< code #x1000)
      (logbitp (ldb (byte 8 0) code)
               (svref *bmp-combining-bitmap* (ldb (byte 8 8) code))))))

(defstatic *bmp-combining-chars*
  #(#\Combining_Grave_Accent 
    #\Combining_Acute_Accent 
    #\Combining_Circumflex_Accent 
    #\Combining_Tilde 
    #\Combining_Macron 
    #\Combining_Breve 
    #\Combining_Dot_Above 
    #\Combining_Diaeresis 
    #\Combining_Hook_Above 
    #\Combining_Ring_Above 
    #\Combining_Double_Acute_Accent 
    #\Combining_Caron 
    #\Combining_Double_Grave_Accent 
    #\Combining_Inverted_Breve 
    #\Combining_Comma_Above 
    #\Combining_Reversed_Comma_Above 
    #\Combining_Horn 
    #\Combining_Dot_Below 
    #\Combining_Diaeresis_Below 
    #\Combining_Ring_Below 
    #\Combining_Comma_Below 
    #\Combining_Cedilla 
    #\Combining_Ogonek 
    #\Combining_Circumflex_Accent_Below 
    #\Combining_Breve_Below 
    #\Combining_Tilde_Below 
    #\Combining_Macron_Below 
    #\Combining_Long_Solidus_Overlay 
    #\Combining_Greek_Perispomeni 
    #\Combining_Greek_Ypogegrammeni 
    #\Arabic_Maddah_Above 
    #\Arabic_Hamza_Above 
    #\Arabic_Hamza_Below 
    #\U+093C 
    #\U+09BE 
    #\U+09D7 
    #\U+0B3E 
    #\U+0B56 
    #\U+0B57 
    #\U+0BBE 
    #\U+0BD7 
    #\U+0C56 
    #\U+0CC2 
    #\U+0CD5 
    #\U+0CD6 
    #\U+0D3E 
    #\U+0D57 
    #\U+0DCA 
    #\U+0DCF 
    #\U+0DDF 
    #\U+102E 
    #\U+3099 
    #\U+309A))

(defstatic *bmp-combining-base-chars*
  #(
    ;; #\Combining_Grave_Accent

    #(#\A #\E #\I #\N #\O #\U #\W #\Y #\a #\e #\i #\n #\o #\u #\w #\y
      #\Diaeresis #\Latin_Capital_Letter_A_With_Circumflex
      #\Latin_Capital_Letter_E_With_Circumflex
      #\Latin_Capital_Letter_O_With_Circumflex
      #\Latin_Capital_Letter_U_With_Diaeresis
      #\Latin_Small_Letter_A_With_Circumflex
      #\Latin_Small_Letter_E_With_Circumflex
      #\Latin_Small_Letter_O_With_Circumflex
      #\Latin_Small_Letter_U_With_Diaeresis
      #\Latin_Capital_Letter_A_With_Breve #\Latin_Small_Letter_A_With_Breve
      #\Latin_Capital_Letter_E_With_Macron
      #\Latin_Small_Letter_E_With_Macron
      #\Latin_Capital_Letter_O_With_Macron
      #\Latin_Small_Letter_O_With_Macron #\Latin_Capital_Letter_O_With_Horn
      #\Latin_Small_Letter_O_With_Horn #\Latin_Capital_Letter_U_With_Horn
      #\Latin_Small_Letter_U_With_Horn #\Greek_Capital_Letter_Alpha
      #\Greek_Capital_Letter_Epsilon #\Greek_Capital_Letter_Eta
      #\Greek_Capital_Letter_Iota #\Greek_Capital_Letter_Omicron
      #\Greek_Capital_Letter_Upsilon #\Greek_Capital_Letter_Omega
      #\Greek_Small_Letter_Alpha #\Greek_Small_Letter_Epsilon
      #\Greek_Small_Letter_Eta #\Greek_Small_Letter_Iota
      #\Greek_Small_Letter_Omicron #\Greek_Small_Letter_Upsilon
      #\Greek_Small_Letter_Omega #\Greek_Small_Letter_Iota_With_Dialytika
      #\Greek_Small_Letter_Upsilon_With_Dialytika
      #\Cyrillic_Capital_Letter_Ie #\Cyrillic_Capital_Letter_I
      #\Cyrillic_Small_Letter_Ie #\Cyrillic_Small_Letter_I #\U+1F00 #\U+1F01
      #\U+1F08 #\U+1F09 #\U+1F10 #\U+1F11 #\U+1F18 #\U+1F19 #\U+1F20
      #\U+1F21 #\U+1F28 #\U+1F29 #\U+1F30 #\U+1F31 #\U+1F38 #\U+1F39
      #\U+1F40 #\U+1F41 #\U+1F48 #\U+1F49 #\U+1F50 #\U+1F51 #\U+1F59
      #\U+1F60 #\U+1F61 #\U+1F68 #\U+1F69 #\U+1FBF #\U+1FFE)


    ;; #\Combining_Acute_Accent

    #(#\A #\C #\E #\G #\I #\K #\L #\M #\N #\O #\P #\R #\S #\U #\W #\Y #\Z
      #\a #\c #\e #\g #\i #\k #\l #\m #\n #\o #\p #\r #\s #\u #\w #\y #\z
      #\Diaeresis #\Latin_Capital_Letter_A_With_Circumflex
      #\Latin_Capital_Letter_A_With_Ring_Above #\Latin_Capital_Letter_Ae
      #\Latin_Capital_Letter_C_With_Cedilla
      #\Latin_Capital_Letter_E_With_Circumflex
      #\Latin_Capital_Letter_I_With_Diaeresis
      #\Latin_Capital_Letter_O_With_Circumflex
      #\Latin_Capital_Letter_O_With_Tilde
      #\Latin_Capital_Letter_O_With_Stroke
      #\Latin_Capital_Letter_U_With_Diaeresis
      #\Latin_Small_Letter_A_With_Circumflex
      #\Latin_Small_Letter_A_With_Ring_Above #\Latin_Small_Letter_Ae
      #\Latin_Small_Letter_C_With_Cedilla
      #\Latin_Small_Letter_E_With_Circumflex
      #\Latin_Small_Letter_I_With_Diaeresis
      #\Latin_Small_Letter_O_With_Circumflex
      #\Latin_Small_Letter_O_With_Tilde #\Latin_Small_Letter_O_With_Stroke
      #\Latin_Small_Letter_U_With_Diaeresis
      #\Latin_Capital_Letter_A_With_Breve #\Latin_Small_Letter_A_With_Breve
      #\Latin_Capital_Letter_E_With_Macron
      #\Latin_Small_Letter_E_With_Macron
      #\Latin_Capital_Letter_O_With_Macron
      #\Latin_Small_Letter_O_With_Macron #\Latin_Capital_Letter_U_With_Tilde
      #\Latin_Small_Letter_U_With_Tilde #\Latin_Capital_Letter_O_With_Horn
      #\Latin_Small_Letter_O_With_Horn #\Latin_Capital_Letter_U_With_Horn
      #\Latin_Small_Letter_U_With_Horn #\Greek_Capital_Letter_Alpha
      #\Greek_Capital_Letter_Epsilon #\Greek_Capital_Letter_Eta
      #\Greek_Capital_Letter_Iota #\Greek_Capital_Letter_Omicron
      #\Greek_Capital_Letter_Upsilon #\Greek_Capital_Letter_Omega
      #\Greek_Small_Letter_Alpha #\Greek_Small_Letter_Epsilon
      #\Greek_Small_Letter_Eta #\Greek_Small_Letter_Iota
      #\Greek_Small_Letter_Omicron #\Greek_Small_Letter_Upsilon
      #\Greek_Small_Letter_Omega #\Greek_Small_Letter_Iota_With_Dialytika
      #\Greek_Small_Letter_Upsilon_With_Dialytika
      #\Greek_Upsilon_With_Hook_Symbol #\Cyrillic_Capital_Letter_Ghe
      #\Cyrillic_Capital_Letter_Ka #\Cyrillic_Small_Letter_Ghe
      #\Cyrillic_Small_Letter_Ka #\U+1F00 #\U+1F01 #\U+1F08 #\U+1F09
      #\U+1F10 #\U+1F11 #\U+1F18 #\U+1F19 #\U+1F20 #\U+1F21 #\U+1F28
      #\U+1F29 #\U+1F30 #\U+1F31 #\U+1F38 #\U+1F39 #\U+1F40 #\U+1F41
      #\U+1F48 #\U+1F49 #\U+1F50 #\U+1F51 #\U+1F59 #\U+1F60 #\U+1F61
      #\U+1F68 #\U+1F69 #\U+1FBF #\U+1FFE)


    ;; #\Combining_Circumflex_Accent

    #(#\A #\C #\E #\G #\H #\I #\J #\O #\S #\U #\W #\Y #\Z #\a #\c #\e #\g
      #\h #\i #\j #\o #\s #\u #\w #\y #\z #\U+1EA0 #\U+1EA1 #\U+1EB8
      #\U+1EB9 #\U+1ECC #\U+1ECD)


    ;; #\Combining_Tilde

    #(#\A #\E #\I #\N #\O #\U #\V #\Y #\a #\e #\i #\n #\o #\u #\v #\y
      #\Latin_Capital_Letter_A_With_Circumflex
      #\Latin_Capital_Letter_E_With_Circumflex
      #\Latin_Capital_Letter_O_With_Circumflex
      #\Latin_Small_Letter_A_With_Circumflex
      #\Latin_Small_Letter_E_With_Circumflex
      #\Latin_Small_Letter_O_With_Circumflex
      #\Latin_Capital_Letter_A_With_Breve #\Latin_Small_Letter_A_With_Breve
      #\Latin_Capital_Letter_O_With_Horn #\Latin_Small_Letter_O_With_Horn
      #\Latin_Capital_Letter_U_With_Horn #\Latin_Small_Letter_U_With_Horn)


    ;; #\Combining_Macron

    #(#\A #\E #\G #\I #\O #\U #\Y #\a #\e #\g #\i #\o #\u #\y
      #\Latin_Capital_Letter_A_With_Diaeresis #\Latin_Capital_Letter_Ae
      #\Latin_Capital_Letter_O_With_Tilde
      #\Latin_Capital_Letter_O_With_Diaeresis
      #\Latin_Capital_Letter_U_With_Diaeresis
      #\Latin_Small_Letter_A_With_Diaeresis #\Latin_Small_Letter_Ae
      #\Latin_Small_Letter_O_With_Tilde
      #\Latin_Small_Letter_O_With_Diaeresis
      #\Latin_Small_Letter_U_With_Diaeresis
      #\Latin_Capital_Letter_O_With_Ogonek
      #\Latin_Small_Letter_O_With_Ogonek
      #\Latin_Capital_Letter_A_With_Dot_Above
      #\Latin_Small_Letter_A_With_Dot_Above
      #\Latin_Capital_Letter_O_With_Dot_Above
      #\Latin_Small_Letter_O_With_Dot_Above #\Greek_Capital_Letter_Alpha
      #\Greek_Capital_Letter_Iota #\Greek_Capital_Letter_Upsilon
      #\Greek_Small_Letter_Alpha #\Greek_Small_Letter_Iota
      #\Greek_Small_Letter_Upsilon #\Cyrillic_Capital_Letter_I
      #\Cyrillic_Capital_Letter_U #\Cyrillic_Small_Letter_I
      #\Cyrillic_Small_Letter_U #\U+1E36 #\U+1E37 #\U+1E5A #\U+1E5B)


    ;; #\Combining_Breve

    #(#\A #\E #\G #\I #\O #\U #\a #\e #\g #\i #\o #\u
      #\Latin_Capital_Letter_E_With_Cedilla
      #\Latin_Small_Letter_E_With_Cedilla #\Greek_Capital_Letter_Alpha
      #\Greek_Capital_Letter_Iota #\Greek_Capital_Letter_Upsilon
      #\Greek_Small_Letter_Alpha #\Greek_Small_Letter_Iota
      #\Greek_Small_Letter_Upsilon #\Cyrillic_Capital_Letter_A
      #\Cyrillic_Capital_Letter_Ie #\Cyrillic_Capital_Letter_Zhe
      #\Cyrillic_Capital_Letter_I #\Cyrillic_Capital_Letter_U
      #\Cyrillic_Small_Letter_A #\Cyrillic_Small_Letter_Ie
      #\Cyrillic_Small_Letter_Zhe #\Cyrillic_Small_Letter_I
      #\Cyrillic_Small_Letter_U #\U+1EA0 #\U+1EA1)


    ;; #\Combining_Dot_Above

    #(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\M #\N #\O #\P #\R #\S #\T #\W
      #\X #\Y #\Z #\a #\b #\c #\d #\e #\f #\g #\h #\m #\n #\o #\p #\r #\s
      #\t #\w #\x #\y #\z #\Latin_Capital_Letter_S_With_Acute
      #\Latin_Small_Letter_S_With_Acute #\Latin_Capital_Letter_S_With_Caron
      #\Latin_Small_Letter_S_With_Caron #\Latin_Small_Letter_Long_S #\U+1E62
      #\U+1E63)


    ;; #\Combining_Diaeresis

    #(#\A #\E #\H #\I #\O #\U #\W #\X #\Y #\a #\e #\h #\i #\o #\t #\u #\w
      #\x #\y #\Latin_Capital_Letter_O_With_Tilde
      #\Latin_Small_Letter_O_With_Tilde #\Latin_Capital_Letter_U_With_Macron
      #\Latin_Small_Letter_U_With_Macron #\Greek_Capital_Letter_Iota
      #\Greek_Capital_Letter_Upsilon #\Greek_Small_Letter_Iota
      #\Greek_Small_Letter_Upsilon #\Greek_Upsilon_With_Hook_Symbol
      #\Cyrillic_Capital_Letter_Byelorussian-Ukrainian_I
      #\Cyrillic_Capital_Letter_A #\Cyrillic_Capital_Letter_Ie
      #\Cyrillic_Capital_Letter_Zhe #\Cyrillic_Capital_Letter_Ze
      #\Cyrillic_Capital_Letter_I #\Cyrillic_Capital_Letter_O
      #\Cyrillic_Capital_Letter_U #\Cyrillic_Capital_Letter_Che
      #\Cyrillic_Capital_Letter_Yeru #\Cyrillic_Capital_Letter_E
      #\Cyrillic_Small_Letter_A #\Cyrillic_Small_Letter_Ie
      #\Cyrillic_Small_Letter_Zhe #\Cyrillic_Small_Letter_Ze
      #\Cyrillic_Small_Letter_I #\Cyrillic_Small_Letter_O
      #\Cyrillic_Small_Letter_U #\Cyrillic_Small_Letter_Che
      #\Cyrillic_Small_Letter_Yeru #\Cyrillic_Small_Letter_E
      #\Cyrillic_Small_Letter_Byelorussian-Ukrainian_I
      #\Cyrillic_Capital_Letter_Schwa #\Cyrillic_Small_Letter_Schwa
      #\Cyrillic_Capital_Letter_Barred_O #\Cyrillic_Small_Letter_Barred_O)


    ;; #\Combining_Hook_Above

    #(#\A #\E #\I #\O #\U #\Y #\a #\e #\i #\o #\u #\y
      #\Latin_Capital_Letter_A_With_Circumflex
      #\Latin_Capital_Letter_E_With_Circumflex
      #\Latin_Capital_Letter_O_With_Circumflex
      #\Latin_Small_Letter_A_With_Circumflex
      #\Latin_Small_Letter_E_With_Circumflex
      #\Latin_Small_Letter_O_With_Circumflex
      #\Latin_Capital_Letter_A_With_Breve #\Latin_Small_Letter_A_With_Breve
      #\Latin_Capital_Letter_O_With_Horn #\Latin_Small_Letter_O_With_Horn
      #\Latin_Capital_Letter_U_With_Horn #\Latin_Small_Letter_U_With_Horn)


    ;; #\Combining_Ring_Above

    #(#\A #\U #\a #\u #\w #\y)


    ;; #\Combining_Double_Acute_Accent

    #(#\O #\U #\o #\u #\Cyrillic_Capital_Letter_U
      #\Cyrillic_Small_Letter_U)


    ;; #\Combining_Caron

    #(#\A #\C #\D #\E #\G #\H #\I #\K #\L #\N #\O #\R #\S #\T #\U #\Z #\a
      #\c #\d #\e #\g #\h #\i #\j #\k #\l #\n #\o #\r #\s #\t #\u #\z
      #\Latin_Capital_Letter_U_With_Diaeresis
      #\Latin_Small_Letter_U_With_Diaeresis #\Latin_Capital_Letter_Ezh
      #\Latin_Small_Letter_Ezh)


    ;; #\Combining_Double_Grave_Accent

    #(#\A #\E #\I #\O #\R #\U #\a #\e #\i #\o #\r #\u
      #\Cyrillic_Capital_Letter_Izhitsa #\Cyrillic_Small_Letter_Izhitsa)


    ;; #\Combining_Inverted_Breve

    #(#\A #\E #\I #\O #\R #\U #\a #\e #\i #\o #\r #\u)


    ;; #\Combining_Comma_Above

    #(#\Greek_Capital_Letter_Alpha #\Greek_Capital_Letter_Epsilon
      #\Greek_Capital_Letter_Eta #\Greek_Capital_Letter_Iota
      #\Greek_Capital_Letter_Omicron #\Greek_Capital_Letter_Omega
      #\Greek_Small_Letter_Alpha #\Greek_Small_Letter_Epsilon
      #\Greek_Small_Letter_Eta #\Greek_Small_Letter_Iota
      #\Greek_Small_Letter_Omicron #\Greek_Small_Letter_Rho
      #\Greek_Small_Letter_Upsilon #\Greek_Small_Letter_Omega)


    ;; #\Combining_Reversed_Comma_Above

    #(#\Greek_Capital_Letter_Alpha #\Greek_Capital_Letter_Epsilon
      #\Greek_Capital_Letter_Eta #\Greek_Capital_Letter_Iota
      #\Greek_Capital_Letter_Omicron #\Greek_Capital_Letter_Rho
      #\Greek_Capital_Letter_Upsilon #\Greek_Capital_Letter_Omega
      #\Greek_Small_Letter_Alpha #\Greek_Small_Letter_Epsilon
      #\Greek_Small_Letter_Eta #\Greek_Small_Letter_Iota
      #\Greek_Small_Letter_Omicron #\Greek_Small_Letter_Rho
      #\Greek_Small_Letter_Upsilon #\Greek_Small_Letter_Omega)


    ;; #\Combining_Horn

    #(#\O #\U #\o #\u)


    ;; #\Combining_Dot_Below

    #(#\A #\B #\D #\E #\H #\I #\K #\L #\M #\N #\O #\R #\S #\T #\U #\V #\W
      #\Y #\Z #\a #\b #\d #\e #\h #\i #\k #\l #\m #\n #\o #\r #\s #\t #\u
      #\v #\w #\y #\z #\Latin_Capital_Letter_O_With_Horn
      #\Latin_Small_Letter_O_With_Horn #\Latin_Capital_Letter_U_With_Horn
      #\Latin_Small_Letter_U_With_Horn)


    ;; #\Combining_Diaeresis_Below

    #(#\U #\u)


    ;; #\Combining_Ring_Below

    #(#\A #\a)


    ;; #\Combining_Comma_Below

    #(#\S #\T #\s #\t)


    ;; #\Combining_Cedilla

    #(#\C #\D #\E #\G #\H #\K #\L #\N #\R #\S #\T #\c #\d #\e #\g #\h #\k
      #\l #\n #\r #\s #\t)


    ;; #\Combining_Ogonek

    #(#\A #\E #\I #\O #\U #\a #\e #\i #\o #\u)


    ;; #\Combining_Circumflex_Accent_Below

    #(#\D #\E #\L #\N #\T #\U #\d #\e #\l #\n #\t #\u)


    ;; #\Combining_Breve_Below

    #(#\H #\h)


    ;; #\Combining_Tilde_Below

    #(#\E #\I #\U #\e #\i #\u)


    ;; #\Combining_Macron_Below

    #(#\B #\D #\K #\L #\N #\R #\T #\Z #\b #\d #\h #\k #\l #\n #\r #\t #\z)


    ;; #\Combining_Long_Solidus_Overlay

    #(#\< #\= #\> #\U+2190 #\U+2192 #\U+2194 #\U+21D0 #\U+21D2 #\U+21D4
      #\U+2203 #\U+2208 #\U+220B #\U+2223 #\U+2225 #\U+223C #\U+2243
      #\U+2245 #\U+2248 #\U+224D #\U+2261 #\U+2264 #\U+2265 #\U+2272
      #\U+2273 #\U+2276 #\U+2277 #\U+227A #\U+227B #\U+227C #\U+227D
      #\U+2282 #\U+2283 #\U+2286 #\U+2287 #\U+2291 #\U+2292 #\U+22A2
      #\U+22A8 #\U+22A9 #\U+22AB #\U+22B2 #\U+22B3 #\U+22B4 #\U+22B5)


    ;; #\Combining_Greek_Perispomeni

    #(#\Diaeresis #\Greek_Small_Letter_Alpha #\Greek_Small_Letter_Eta
      #\Greek_Small_Letter_Iota #\Greek_Small_Letter_Upsilon
      #\Greek_Small_Letter_Omega #\Greek_Small_Letter_Iota_With_Dialytika
      #\Greek_Small_Letter_Upsilon_With_Dialytika #\U+1F00 #\U+1F01 #\U+1F08
      #\U+1F09 #\U+1F20 #\U+1F21 #\U+1F28 #\U+1F29 #\U+1F30 #\U+1F31
      #\U+1F38 #\U+1F39 #\U+1F50 #\U+1F51 #\U+1F59 #\U+1F60 #\U+1F61
      #\U+1F68 #\U+1F69 #\U+1FBF #\U+1FFE)


    ;; #\Combining_Greek_Ypogegrammeni

    #(#\Greek_Capital_Letter_Alpha #\Greek_Capital_Letter_Eta
      #\Greek_Capital_Letter_Omega #\Greek_Small_Letter_Alpha_With_Tonos
      #\Greek_Small_Letter_Eta_With_Tonos #\Greek_Small_Letter_Alpha
      #\Greek_Small_Letter_Eta #\Greek_Small_Letter_Omega
      #\Greek_Small_Letter_Omega_With_Tonos #\U+1F00 #\U+1F01 #\U+1F02
      #\U+1F03 #\U+1F04 #\U+1F05 #\U+1F06 #\U+1F07 #\U+1F08 #\U+1F09
      #\U+1F0A #\U+1F0B #\U+1F0C #\U+1F0D #\U+1F0E #\U+1F0F #\U+1F20
      #\U+1F21 #\U+1F22 #\U+1F23 #\U+1F24 #\U+1F25 #\U+1F26 #\U+1F27
      #\U+1F28 #\U+1F29 #\U+1F2A #\U+1F2B #\U+1F2C #\U+1F2D #\U+1F2E
      #\U+1F2F #\U+1F60 #\U+1F61 #\U+1F62 #\U+1F63 #\U+1F64 #\U+1F65
      #\U+1F66 #\U+1F67 #\U+1F68 #\U+1F69 #\U+1F6A #\U+1F6B #\U+1F6C
      #\U+1F6D #\U+1F6E #\U+1F6F #\U+1F70 #\U+1F74 #\U+1F7C #\U+1FB6
      #\U+1FC6 #\U+1FF6)


    ;; #\Arabic_Maddah_Above

    #(#\Arabic_Letter_Alef)


    ;; #\Arabic_Hamza_Above

    #(#\Arabic_Letter_Alef #\Arabic_Letter_Waw #\Arabic_Letter_Yeh
      #\Arabic_Letter_Heh_Goal #\Arabic_Letter_Yeh_Barree
      #\Arabic_Letter_Ae)


    ;; #\Arabic_Hamza_Below

    #(#\Arabic_Letter_Alef)


    ;; #\U+093C

    #(#\U+0928 #\U+0930 #\U+0933)


    ;; #\U+09BE

    #(#\U+09C7)


    ;; #\U+09D7

    #(#\U+09C7)


    ;; #\U+0B3E

    #(#\U+0B47)


    ;; #\U+0B56

    #(#\U+0B47)


    ;; #\U+0B57

    #(#\U+0B47)


    ;; #\U+0BBE

    #(#\U+0BC6 #\U+0BC7)


    ;; #\U+0BD7

    #(#\U+0B92 #\U+0BC6)


    ;; #\U+0C56

    #(#\U+0C46)


    ;; #\U+0CC2

    #(#\U+0CC6)


    ;; #\U+0CD5

    #(#\U+0CBF #\U+0CC6 #\U+0CCA)


    ;; #\U+0CD6

    #(#\U+0CC6)


    ;; #\U+0D3E

    #(#\U+0D46 #\U+0D47)


    ;; #\U+0D57

    #(#\U+0D46)


    ;; #\U+0DCA

    #(#\U+0DD9 #\U+0DDC)


    ;; #\U+0DCF

    #(#\U+0DD9)


    ;; #\U+0DDF

    #(#\U+0DD9)


    ;; #\U+102E

    #(#\U+1025)


    ;; #\U+3099

    #(#\U+3046 #\U+304B #\U+304D #\U+304F #\U+3051 #\U+3053 #\U+3055
      #\U+3057 #\U+3059 #\U+305B #\U+305D #\U+305F #\U+3061 #\U+3064
      #\U+3066 #\U+3068 #\U+306F #\U+3072 #\U+3075 #\U+3078 #\U+307B
      #\U+309D #\U+30A6 #\U+30AB #\U+30AD #\U+30AF #\U+30B1 #\U+30B3
      #\U+30B5 #\U+30B7 #\U+30B9 #\U+30BB #\U+30BD #\U+30BF #\U+30C1
      #\U+30C4 #\U+30C6 #\U+30C8 #\U+30CF #\U+30D2 #\U+30D5 #\U+30D8
      #\U+30DB #\U+30EF #\U+30F0 #\U+30F1 #\U+30F2 #\U+30FD)


    ;; #\U+309A

    #(#\U+306F #\U+3072 #\U+3075 #\U+3078 #\U+307B #\U+30CF #\U+30D2
      #\U+30D5 #\U+30D8 #\U+30DB)
    ))

(defstatic *bmp-precombined-chars*
  #(

    ;; #\Combining_Grave_Accent

    #(#\Latin_Capital_Letter_A_With_Grave
      #\Latin_Capital_Letter_E_With_Grave
      #\Latin_Capital_Letter_I_With_Grave
      #\Latin_Capital_Letter_N_With_Grave
      #\Latin_Capital_Letter_O_With_Grave
      #\Latin_Capital_Letter_U_With_Grave #\U+1E80 #\U+1EF2
      #\Latin_Small_Letter_A_With_Grave #\Latin_Small_Letter_E_With_Grave
      #\Latin_Small_Letter_I_With_Grave #\Latin_Small_Letter_N_With_Grave
      #\Latin_Small_Letter_O_With_Grave #\Latin_Small_Letter_U_With_Grave
      #\U+1E81 #\U+1EF3 #\U+1FED #\U+1EA6 #\U+1EC0 #\U+1ED2
      #\Latin_Capital_Letter_U_With_Diaeresis_And_Grave #\U+1EA7 #\U+1EC1
      #\U+1ED3 #\Latin_Small_Letter_U_With_Diaeresis_And_Grave #\U+1EB0
      #\U+1EB1 #\U+1E14 #\U+1E15 #\U+1E50 #\U+1E51 #\U+1EDC #\U+1EDD
      #\U+1EEA #\U+1EEB #\U+1FBA #\U+1FC8 #\U+1FCA #\U+1FDA #\U+1FF8
      #\U+1FEA #\U+1FFA #\U+1F70 #\U+1F72 #\U+1F74 #\U+1F76 #\U+1F78
      #\U+1F7A #\U+1F7C #\U+1FD2 #\U+1FE2
      #\Cyrillic_Capital_Letter_Ie_With_Grave
      #\Cyrillic_Capital_Letter_I_With_Grave
      #\Cyrillic_Small_Letter_Ie_With_Grave
      #\Cyrillic_Small_Letter_I_With_Grave #\U+1F02 #\U+1F03 #\U+1F0A
      #\U+1F0B #\U+1F12 #\U+1F13 #\U+1F1A #\U+1F1B #\U+1F22 #\U+1F23
      #\U+1F2A #\U+1F2B #\U+1F32 #\U+1F33 #\U+1F3A #\U+1F3B #\U+1F42
      #\U+1F43 #\U+1F4A #\U+1F4B #\U+1F52 #\U+1F53 #\U+1F5B #\U+1F62
      #\U+1F63 #\U+1F6A #\U+1F6B #\U+1FCD #\U+1FDD)


    ;; #\Combining_Acute_Accent

    #(#\Latin_Capital_Letter_A_With_Acute
      #\Latin_Capital_Letter_C_With_Acute
      #\Latin_Capital_Letter_E_With_Acute
      #\Latin_Capital_Letter_G_With_Acute
      #\Latin_Capital_Letter_I_With_Acute #\U+1E30
      #\Latin_Capital_Letter_L_With_Acute #\U+1E3E
      #\Latin_Capital_Letter_N_With_Acute
      #\Latin_Capital_Letter_O_With_Acute #\U+1E54
      #\Latin_Capital_Letter_R_With_Acute
      #\Latin_Capital_Letter_S_With_Acute
      #\Latin_Capital_Letter_U_With_Acute #\U+1E82
      #\Latin_Capital_Letter_Y_With_Acute
      #\Latin_Capital_Letter_Z_With_Acute #\Latin_Small_Letter_A_With_Acute
      #\Latin_Small_Letter_C_With_Acute #\Latin_Small_Letter_E_With_Acute
      #\Latin_Small_Letter_G_With_Acute #\Latin_Small_Letter_I_With_Acute
      #\U+1E31 #\Latin_Small_Letter_L_With_Acute #\U+1E3F
      #\Latin_Small_Letter_N_With_Acute #\Latin_Small_Letter_O_With_Acute
      #\U+1E55 #\Latin_Small_Letter_R_With_Acute
      #\Latin_Small_Letter_S_With_Acute #\Latin_Small_Letter_U_With_Acute
      #\U+1E83 #\Latin_Small_Letter_Y_With_Acute
      #\Latin_Small_Letter_Z_With_Acute #\Greek_Dialytika_Tonos #\U+1EA4
      #\Latin_Capital_Letter_A_With_Ring_Above_And_Acute
      #\Latin_Capital_Letter_Ae_With_Acute #\U+1E08 #\U+1EBE #\U+1E2E
      #\U+1ED0 #\U+1E4C #\Latin_Capital_Letter_O_With_Stroke_And_Acute
      #\Latin_Capital_Letter_U_With_Diaeresis_And_Acute #\U+1EA5
      #\Latin_Small_Letter_A_With_Ring_Above_And_Acute
      #\Latin_Small_Letter_Ae_With_Acute #\U+1E09 #\U+1EBF #\U+1E2F #\U+1ED1
      #\U+1E4D #\Latin_Small_Letter_O_With_Stroke_And_Acute
      #\Latin_Small_Letter_U_With_Diaeresis_And_Acute #\U+1EAE #\U+1EAF
      #\U+1E16 #\U+1E17 #\U+1E52 #\U+1E53 #\U+1E78 #\U+1E79 #\U+1EDA
      #\U+1EDB #\U+1EE8 #\U+1EE9 #\Greek_Capital_Letter_Alpha_With_Tonos
      #\Greek_Capital_Letter_Epsilon_With_Tonos
      #\Greek_Capital_Letter_Eta_With_Tonos
      #\Greek_Capital_Letter_Iota_With_Tonos
      #\Greek_Capital_Letter_Omicron_With_Tonos
      #\Greek_Capital_Letter_Upsilon_With_Tonos
      #\Greek_Capital_Letter_Omega_With_Tonos
      #\Greek_Small_Letter_Alpha_With_Tonos
      #\Greek_Small_Letter_Epsilon_With_Tonos
      #\Greek_Small_Letter_Eta_With_Tonos
      #\Greek_Small_Letter_Iota_With_Tonos
      #\Greek_Small_Letter_Omicron_With_Tonos
      #\Greek_Small_Letter_Upsilon_With_Tonos
      #\Greek_Small_Letter_Omega_With_Tonos
      #\Greek_Small_Letter_Iota_With_Dialytika_And_Tonos
      #\Greek_Small_Letter_Upsilon_With_Dialytika_And_Tonos
      #\Greek_Upsilon_With_Acute_And_Hook_Symbol
      #\Cyrillic_Capital_Letter_Gje #\Cyrillic_Capital_Letter_Kje
      #\Cyrillic_Small_Letter_Gje #\Cyrillic_Small_Letter_Kje #\U+1F04
      #\U+1F05 #\U+1F0C #\U+1F0D #\U+1F14 #\U+1F15 #\U+1F1C #\U+1F1D
      #\U+1F24 #\U+1F25 #\U+1F2C #\U+1F2D #\U+1F34 #\U+1F35 #\U+1F3C
      #\U+1F3D #\U+1F44 #\U+1F45 #\U+1F4C #\U+1F4D #\U+1F54 #\U+1F55
      #\U+1F5D #\U+1F64 #\U+1F65 #\U+1F6C #\U+1F6D #\U+1FCE #\U+1FDE)


    ;; #\Combining_Circumflex_Accent

    #(#\Latin_Capital_Letter_A_With_Circumflex
      #\Latin_Capital_Letter_C_With_Circumflex
      #\Latin_Capital_Letter_E_With_Circumflex
      #\Latin_Capital_Letter_G_With_Circumflex
      #\Latin_Capital_Letter_H_With_Circumflex
      #\Latin_Capital_Letter_I_With_Circumflex
      #\Latin_Capital_Letter_J_With_Circumflex
      #\Latin_Capital_Letter_O_With_Circumflex
      #\Latin_Capital_Letter_S_With_Circumflex
      #\Latin_Capital_Letter_U_With_Circumflex
      #\Latin_Capital_Letter_W_With_Circumflex
      #\Latin_Capital_Letter_Y_With_Circumflex #\U+1E90
      #\Latin_Small_Letter_A_With_Circumflex
      #\Latin_Small_Letter_C_With_Circumflex
      #\Latin_Small_Letter_E_With_Circumflex
      #\Latin_Small_Letter_G_With_Circumflex
      #\Latin_Small_Letter_H_With_Circumflex
      #\Latin_Small_Letter_I_With_Circumflex
      #\Latin_Small_Letter_J_With_Circumflex
      #\Latin_Small_Letter_O_With_Circumflex
      #\Latin_Small_Letter_S_With_Circumflex
      #\Latin_Small_Letter_U_With_Circumflex
      #\Latin_Small_Letter_W_With_Circumflex
      #\Latin_Small_Letter_Y_With_Circumflex #\U+1E91 #\U+1EAC #\U+1EAD
      #\U+1EC6 #\U+1EC7 #\U+1ED8 #\U+1ED9)


    ;; #\Combining_Tilde

    #(#\Latin_Capital_Letter_A_With_Tilde #\U+1EBC
      #\Latin_Capital_Letter_I_With_Tilde
      #\Latin_Capital_Letter_N_With_Tilde
      #\Latin_Capital_Letter_O_With_Tilde
      #\Latin_Capital_Letter_U_With_Tilde #\U+1E7C #\U+1EF8
      #\Latin_Small_Letter_A_With_Tilde #\U+1EBD
      #\Latin_Small_Letter_I_With_Tilde #\Latin_Small_Letter_N_With_Tilde
      #\Latin_Small_Letter_O_With_Tilde #\Latin_Small_Letter_U_With_Tilde
      #\U+1E7D #\U+1EF9 #\U+1EAA #\U+1EC4 #\U+1ED6 #\U+1EAB #\U+1EC5
      #\U+1ED7 #\U+1EB4 #\U+1EB5 #\U+1EE0 #\U+1EE1 #\U+1EEE #\U+1EEF)


    ;; #\Combining_Macron

    #(#\Latin_Capital_Letter_A_With_Macron
      #\Latin_Capital_Letter_E_With_Macron #\U+1E20
      #\Latin_Capital_Letter_I_With_Macron
      #\Latin_Capital_Letter_O_With_Macron
      #\Latin_Capital_Letter_U_With_Macron
      #\Latin_Capital_Letter_Y_With_Macron
      #\Latin_Small_Letter_A_With_Macron #\Latin_Small_Letter_E_With_Macron
      #\U+1E21 #\Latin_Small_Letter_I_With_Macron
      #\Latin_Small_Letter_O_With_Macron #\Latin_Small_Letter_U_With_Macron
      #\Latin_Small_Letter_Y_With_Macron
      #\Latin_Capital_Letter_A_With_Diaeresis_And_Macron
      #\Latin_Capital_Letter_Ae_With_Macron
      #\Latin_Capital_Letter_O_With_Tilde_And_Macron
      #\Latin_Capital_Letter_O_With_Diaeresis_And_Macron
      #\Latin_Capital_Letter_U_With_Diaeresis_And_Macron
      #\Latin_Small_Letter_A_With_Diaeresis_And_Macron
      #\Latin_Small_Letter_Ae_With_Macron
      #\Latin_Small_Letter_O_With_Tilde_And_Macron
      #\Latin_Small_Letter_O_With_Diaeresis_And_Macron
      #\Latin_Small_Letter_U_With_Diaeresis_And_Macron
      #\Latin_Capital_Letter_O_With_Ogonek_And_Macron
      #\Latin_Small_Letter_O_With_Ogonek_And_Macron
      #\Latin_Capital_Letter_A_With_Dot_Above_And_Macron
      #\Latin_Small_Letter_A_With_Dot_Above_And_Macron
      #\Latin_Capital_Letter_O_With_Dot_Above_And_Macron
      #\Latin_Small_Letter_O_With_Dot_Above_And_Macron #\U+1FB9 #\U+1FD9
      #\U+1FE9 #\U+1FB1 #\U+1FD1 #\U+1FE1
      #\Cyrillic_Capital_Letter_I_With_Macron
      #\Cyrillic_Capital_Letter_U_With_Macron
      #\Cyrillic_Small_Letter_I_With_Macron
      #\Cyrillic_Small_Letter_U_With_Macron #\U+1E38 #\U+1E39 #\U+1E5C
      #\U+1E5D)


    ;; #\Combining_Breve

    #(#\Latin_Capital_Letter_A_With_Breve
      #\Latin_Capital_Letter_E_With_Breve
      #\Latin_Capital_Letter_G_With_Breve
      #\Latin_Capital_Letter_I_With_Breve
      #\Latin_Capital_Letter_O_With_Breve
      #\Latin_Capital_Letter_U_With_Breve #\Latin_Small_Letter_A_With_Breve
      #\Latin_Small_Letter_E_With_Breve #\Latin_Small_Letter_G_With_Breve
      #\Latin_Small_Letter_I_With_Breve #\Latin_Small_Letter_O_With_Breve
      #\Latin_Small_Letter_U_With_Breve #\U+1E1C #\U+1E1D #\U+1FB8 #\U+1FD8
      #\U+1FE8 #\U+1FB0 #\U+1FD0 #\U+1FE0
      #\Cyrillic_Capital_Letter_A_With_Breve
      #\Cyrillic_Capital_Letter_Ie_With_Breve
      #\Cyrillic_Capital_Letter_Zhe_With_Breve
      #\Cyrillic_Capital_Letter_Short_I #\Cyrillic_Capital_Letter_Short_U
      #\Cyrillic_Small_Letter_A_With_Breve
      #\Cyrillic_Small_Letter_Ie_With_Breve
      #\Cyrillic_Small_Letter_Zhe_With_Breve #\Cyrillic_Small_Letter_Short_I
      #\Cyrillic_Small_Letter_Short_U #\U+1EB6 #\U+1EB7)


    ;; #\Combining_Dot_Above

    #(#\Latin_Capital_Letter_A_With_Dot_Above #\U+1E02
      #\Latin_Capital_Letter_C_With_Dot_Above #\U+1E0A
      #\Latin_Capital_Letter_E_With_Dot_Above #\U+1E1E
      #\Latin_Capital_Letter_G_With_Dot_Above #\U+1E22
      #\Latin_Capital_Letter_I_With_Dot_Above #\U+1E40 #\U+1E44
      #\Latin_Capital_Letter_O_With_Dot_Above #\U+1E56 #\U+1E58 #\U+1E60
      #\U+1E6A #\U+1E86 #\U+1E8A #\U+1E8E
      #\Latin_Capital_Letter_Z_With_Dot_Above
      #\Latin_Small_Letter_A_With_Dot_Above #\U+1E03
      #\Latin_Small_Letter_C_With_Dot_Above #\U+1E0B
      #\Latin_Small_Letter_E_With_Dot_Above #\U+1E1F
      #\Latin_Small_Letter_G_With_Dot_Above #\U+1E23 #\U+1E41 #\U+1E45
      #\Latin_Small_Letter_O_With_Dot_Above #\U+1E57 #\U+1E59 #\U+1E61
      #\U+1E6B #\U+1E87 #\U+1E8B #\U+1E8F
      #\Latin_Small_Letter_Z_With_Dot_Above #\U+1E64 #\U+1E65 #\U+1E66
      #\U+1E67 #\U+1E9B #\U+1E68 #\U+1E69)


    ;; #\Combining_Diaeresis

    #(#\Latin_Capital_Letter_A_With_Diaeresis
      #\Latin_Capital_Letter_E_With_Diaeresis #\U+1E26
      #\Latin_Capital_Letter_I_With_Diaeresis
      #\Latin_Capital_Letter_O_With_Diaeresis
      #\Latin_Capital_Letter_U_With_Diaeresis #\U+1E84 #\U+1E8C
      #\Latin_Capital_Letter_Y_With_Diaeresis
      #\Latin_Small_Letter_A_With_Diaeresis
      #\Latin_Small_Letter_E_With_Diaeresis #\U+1E27
      #\Latin_Small_Letter_I_With_Diaeresis
      #\Latin_Small_Letter_O_With_Diaeresis #\U+1E97
      #\Latin_Small_Letter_U_With_Diaeresis #\U+1E85 #\U+1E8D
      #\Latin_Small_Letter_Y_With_Diaeresis #\U+1E4E #\U+1E4F #\U+1E7A
      #\U+1E7B #\Greek_Capital_Letter_Iota_With_Dialytika
      #\Greek_Capital_Letter_Upsilon_With_Dialytika
      #\Greek_Small_Letter_Iota_With_Dialytika
      #\Greek_Small_Letter_Upsilon_With_Dialytika
      #\Greek_Upsilon_With_Diaeresis_And_Hook_Symbol
      #\Cyrillic_Capital_Letter_Yi
      #\Cyrillic_Capital_Letter_A_With_Diaeresis
      #\Cyrillic_Capital_Letter_Io
      #\Cyrillic_Capital_Letter_Zhe_With_Diaeresis
      #\Cyrillic_Capital_Letter_Ze_With_Diaeresis
      #\Cyrillic_Capital_Letter_I_With_Diaeresis
      #\Cyrillic_Capital_Letter_O_With_Diaeresis
      #\Cyrillic_Capital_Letter_U_With_Diaeresis
      #\Cyrillic_Capital_Letter_Che_With_Diaeresis
      #\Cyrillic_Capital_Letter_Yeru_With_Diaeresis
      #\Cyrillic_Capital_Letter_E_With_Diaeresis
      #\Cyrillic_Small_Letter_A_With_Diaeresis #\Cyrillic_Small_Letter_Io
      #\Cyrillic_Small_Letter_Zhe_With_Diaeresis
      #\Cyrillic_Small_Letter_Ze_With_Diaeresis
      #\Cyrillic_Small_Letter_I_With_Diaeresis
      #\Cyrillic_Small_Letter_O_With_Diaeresis
      #\Cyrillic_Small_Letter_U_With_Diaeresis
      #\Cyrillic_Small_Letter_Che_With_Diaeresis
      #\Cyrillic_Small_Letter_Yeru_With_Diaeresis
      #\Cyrillic_Small_Letter_E_With_Diaeresis #\Cyrillic_Small_Letter_Yi
      #\Cyrillic_Capital_Letter_Schwa_With_Diaeresis
      #\Cyrillic_Small_Letter_Schwa_With_Diaeresis
      #\Cyrillic_Capital_Letter_Barred_O_With_Diaeresis
      #\Cyrillic_Small_Letter_Barred_O_With_Diaeresis)


    ;; #\Combining_Hook_Above

    #(#\U+1EA2 #\U+1EBA #\U+1EC8 #\U+1ECE #\U+1EE6 #\U+1EF6 #\U+1EA3
      #\U+1EBB #\U+1EC9 #\U+1ECF #\U+1EE7 #\U+1EF7 #\U+1EA8 #\U+1EC2
      #\U+1ED4 #\U+1EA9 #\U+1EC3 #\U+1ED5 #\U+1EB2 #\U+1EB3 #\U+1EDE
      #\U+1EDF #\U+1EEC #\U+1EED)


    ;; #\Combining_Ring_Above

    #(#\Latin_Capital_Letter_A_With_Ring_Above
      #\Latin_Capital_Letter_U_With_Ring_Above
      #\Latin_Small_Letter_A_With_Ring_Above
      #\Latin_Small_Letter_U_With_Ring_Above #\U+1E98 #\U+1E99)


    ;; #\Combining_Double_Acute_Accent

    #(#\Latin_Capital_Letter_O_With_Double_Acute
      #\Latin_Capital_Letter_U_With_Double_Acute
      #\Latin_Small_Letter_O_With_Double_Acute
      #\Latin_Small_Letter_U_With_Double_Acute
      #\Cyrillic_Capital_Letter_U_With_Double_Acute
      #\Cyrillic_Small_Letter_U_With_Double_Acute)


    ;; #\Combining_Caron

    #(#\Latin_Capital_Letter_A_With_Caron
      #\Latin_Capital_Letter_C_With_Caron
      #\Latin_Capital_Letter_D_With_Caron
      #\Latin_Capital_Letter_E_With_Caron
      #\Latin_Capital_Letter_G_With_Caron
      #\Latin_Capital_Letter_H_With_Caron
      #\Latin_Capital_Letter_I_With_Caron
      #\Latin_Capital_Letter_K_With_Caron
      #\Latin_Capital_Letter_L_With_Caron
      #\Latin_Capital_Letter_N_With_Caron
      #\Latin_Capital_Letter_O_With_Caron
      #\Latin_Capital_Letter_R_With_Caron
      #\Latin_Capital_Letter_S_With_Caron
      #\Latin_Capital_Letter_T_With_Caron
      #\Latin_Capital_Letter_U_With_Caron
      #\Latin_Capital_Letter_Z_With_Caron #\Latin_Small_Letter_A_With_Caron
      #\Latin_Small_Letter_C_With_Caron #\Latin_Small_Letter_D_With_Caron
      #\Latin_Small_Letter_E_With_Caron #\Latin_Small_Letter_G_With_Caron
      #\Latin_Small_Letter_H_With_Caron #\Latin_Small_Letter_I_With_Caron
      #\Latin_Small_Letter_J_With_Caron #\Latin_Small_Letter_K_With_Caron
      #\Latin_Small_Letter_L_With_Caron #\Latin_Small_Letter_N_With_Caron
      #\Latin_Small_Letter_O_With_Caron #\Latin_Small_Letter_R_With_Caron
      #\Latin_Small_Letter_S_With_Caron #\Latin_Small_Letter_T_With_Caron
      #\Latin_Small_Letter_U_With_Caron #\Latin_Small_Letter_Z_With_Caron
      #\Latin_Capital_Letter_U_With_Diaeresis_And_Caron
      #\Latin_Small_Letter_U_With_Diaeresis_And_Caron
      #\Latin_Capital_Letter_Ezh_With_Caron
      #\Latin_Small_Letter_Ezh_With_Caron)


    ;; #\Combining_Double_Grave_Accent

    #(#\Latin_Capital_Letter_A_With_Double_Grave
      #\Latin_Capital_Letter_E_With_Double_Grave
      #\Latin_Capital_Letter_I_With_Double_Grave
      #\Latin_Capital_Letter_O_With_Double_Grave
      #\Latin_Capital_Letter_R_With_Double_Grave
      #\Latin_Capital_Letter_U_With_Double_Grave
      #\Latin_Small_Letter_A_With_Double_Grave
      #\Latin_Small_Letter_E_With_Double_Grave
      #\Latin_Small_Letter_I_With_Double_Grave
      #\Latin_Small_Letter_O_With_Double_Grave
      #\Latin_Small_Letter_R_With_Double_Grave
      #\Latin_Small_Letter_U_With_Double_Grave
      #\Cyrillic_Capital_Letter_Izhitsa_With_Double_Grave_Accent
      #\Cyrillic_Small_Letter_Izhitsa_With_Double_Grave_Accent)


    ;; #\Combining_Inverted_Breve

    #(#\Latin_Capital_Letter_A_With_Inverted_Breve
      #\Latin_Capital_Letter_E_With_Inverted_Breve
      #\Latin_Capital_Letter_I_With_Inverted_Breve
      #\Latin_Capital_Letter_O_With_Inverted_Breve
      #\Latin_Capital_Letter_R_With_Inverted_Breve
      #\Latin_Capital_Letter_U_With_Inverted_Breve
      #\Latin_Small_Letter_A_With_Inverted_Breve
      #\Latin_Small_Letter_E_With_Inverted_Breve
      #\Latin_Small_Letter_I_With_Inverted_Breve
      #\Latin_Small_Letter_O_With_Inverted_Breve
      #\Latin_Small_Letter_R_With_Inverted_Breve
      #\Latin_Small_Letter_U_With_Inverted_Breve)


    ;; #\Combining_Comma_Above

    #(#\U+1F08 #\U+1F18 #\U+1F28 #\U+1F38 #\U+1F48 #\U+1F68 #\U+1F00
      #\U+1F10 #\U+1F20 #\U+1F30 #\U+1F40 #\U+1FE4 #\U+1F50 #\U+1F60)


    ;; #\Combining_Reversed_Comma_Above

    #(#\U+1F09 #\U+1F19 #\U+1F29 #\U+1F39 #\U+1F49 #\U+1FEC #\U+1F59
      #\U+1F69 #\U+1F01 #\U+1F11 #\U+1F21 #\U+1F31 #\U+1F41 #\U+1FE5
      #\U+1F51 #\U+1F61)


    ;; #\Combining_Horn

    #(#\Latin_Capital_Letter_O_With_Horn
      #\Latin_Capital_Letter_U_With_Horn #\Latin_Small_Letter_O_With_Horn
      #\Latin_Small_Letter_U_With_Horn)


    ;; #\Combining_Dot_Below

    #(#\U+1EA0 #\U+1E04 #\U+1E0C #\U+1EB8 #\U+1E24 #\U+1ECA #\U+1E32
      #\U+1E36 #\U+1E42 #\U+1E46 #\U+1ECC #\U+1E5A #\U+1E62 #\U+1E6C
      #\U+1EE4 #\U+1E7E #\U+1E88 #\U+1EF4 #\U+1E92 #\U+1EA1 #\U+1E05
      #\U+1E0D #\U+1EB9 #\U+1E25 #\U+1ECB #\U+1E33 #\U+1E37 #\U+1E43
      #\U+1E47 #\U+1ECD #\U+1E5B #\U+1E63 #\U+1E6D #\U+1EE5 #\U+1E7F
      #\U+1E89 #\U+1EF5 #\U+1E93 #\U+1EE2 #\U+1EE3 #\U+1EF0 #\U+1EF1)


    ;; #\Combining_Diaeresis_Below

    #(#\U+1E72 #\U+1E73)


    ;; #\Combining_Ring_Below

    #(#\U+1E00 #\U+1E01)


    ;; #\Combining_Comma_Below

    #(#\Latin_Capital_Letter_S_With_Comma_Below
      #\Latin_Capital_Letter_T_With_Comma_Below
      #\Latin_Small_Letter_S_With_Comma_Below
      #\Latin_Small_Letter_T_With_Comma_Below)


    ;; #\Combining_Cedilla

    #(#\Latin_Capital_Letter_C_With_Cedilla #\U+1E10
      #\Latin_Capital_Letter_E_With_Cedilla
      #\Latin_Capital_Letter_G_With_Cedilla #\U+1E28
      #\Latin_Capital_Letter_K_With_Cedilla
      #\Latin_Capital_Letter_L_With_Cedilla
      #\Latin_Capital_Letter_N_With_Cedilla
      #\Latin_Capital_Letter_R_With_Cedilla
      #\Latin_Capital_Letter_S_With_Cedilla
      #\Latin_Capital_Letter_T_With_Cedilla
      #\Latin_Small_Letter_C_With_Cedilla #\U+1E11
      #\Latin_Small_Letter_E_With_Cedilla
      #\Latin_Small_Letter_G_With_Cedilla #\U+1E29
      #\Latin_Small_Letter_K_With_Cedilla
      #\Latin_Small_Letter_L_With_Cedilla
      #\Latin_Small_Letter_N_With_Cedilla
      #\Latin_Small_Letter_R_With_Cedilla
      #\Latin_Small_Letter_S_With_Cedilla
      #\Latin_Small_Letter_T_With_Cedilla)


    ;; #\Combining_Ogonek

    #(#\Latin_Capital_Letter_A_With_Ogonek
      #\Latin_Capital_Letter_E_With_Ogonek
      #\Latin_Capital_Letter_I_With_Ogonek
      #\Latin_Capital_Letter_O_With_Ogonek
      #\Latin_Capital_Letter_U_With_Ogonek
      #\Latin_Small_Letter_A_With_Ogonek #\Latin_Small_Letter_E_With_Ogonek
      #\Latin_Small_Letter_I_With_Ogonek #\Latin_Small_Letter_O_With_Ogonek
      #\Latin_Small_Letter_U_With_Ogonek)


    ;; #\Combining_Circumflex_Accent_Below

    #(#\U+1E12 #\U+1E18 #\U+1E3C #\U+1E4A #\U+1E70 #\U+1E76 #\U+1E13
      #\U+1E19 #\U+1E3D #\U+1E4B #\U+1E71 #\U+1E77)


    ;; #\Combining_Breve_Below

    #(#\U+1E2A #\U+1E2B)


    ;; #\Combining_Tilde_Below

    #(#\U+1E1A #\U+1E2C #\U+1E74 #\U+1E1B #\U+1E2D #\U+1E75)


    ;; #\Combining_Macron_Below

    #(#\U+1E06 #\U+1E0E #\U+1E34 #\U+1E3A #\U+1E48 #\U+1E5E #\U+1E6E
      #\U+1E94 #\U+1E07 #\U+1E0F #\U+1E96 #\U+1E35 #\U+1E3B #\U+1E49
      #\U+1E5F #\U+1E6F #\U+1E95)


    ;; #\Combining_Long_Solidus_Overlay

    #(#\U+226E #\U+2260 #\U+226F #\U+219A #\U+219B #\U+21AE #\U+21CD
      #\U+21CF #\U+21CE #\U+2204 #\U+2209 #\U+220C #\U+2224 #\U+2226
      #\U+2241 #\U+2244 #\U+2247 #\U+2249 #\U+226D #\U+2262 #\U+2270
      #\U+2271 #\U+2274 #\U+2275 #\U+2278 #\U+2279 #\U+2280 #\U+2281
      #\U+22E0 #\U+22E1 #\U+2284 #\U+2285 #\U+2288 #\U+2289 #\U+22E2
      #\U+22E3 #\U+22AC #\U+22AD #\U+22AE #\U+22AF #\U+22EA #\U+22EB
      #\U+22EC #\U+22ED)


    ;; #\Combining_Greek_Perispomeni

    #(#\U+1FC1 #\U+1FB6 #\U+1FC6 #\U+1FD6 #\U+1FE6 #\U+1FF6 #\U+1FD7
      #\U+1FE7 #\U+1F06 #\U+1F07 #\U+1F0E #\U+1F0F #\U+1F26 #\U+1F27
      #\U+1F2E #\U+1F2F #\U+1F36 #\U+1F37 #\U+1F3E #\U+1F3F #\U+1F56
      #\U+1F57 #\U+1F5F #\U+1F66 #\U+1F67 #\U+1F6E #\U+1F6F #\U+1FCF
      #\U+1FDF)


    ;; #\Combining_Greek_Ypogegrammeni

    #(#\U+1FBC #\U+1FCC #\U+1FFC #\U+1FB4 #\U+1FC4 #\U+1FB3 #\U+1FC3
      #\U+1FF3 #\U+1FF4 #\U+1F80 #\U+1F81 #\U+1F82 #\U+1F83 #\U+1F84
      #\U+1F85 #\U+1F86 #\U+1F87 #\U+1F88 #\U+1F89 #\U+1F8A #\U+1F8B
      #\U+1F8C #\U+1F8D #\U+1F8E #\U+1F8F #\U+1F90 #\U+1F91 #\U+1F92
      #\U+1F93 #\U+1F94 #\U+1F95 #\U+1F96 #\U+1F97 #\U+1F98 #\U+1F99
      #\U+1F9A #\U+1F9B #\U+1F9C #\U+1F9D #\U+1F9E #\U+1F9F #\U+1FA0
      #\U+1FA1 #\U+1FA2 #\U+1FA3 #\U+1FA4 #\U+1FA5 #\U+1FA6 #\U+1FA7
      #\U+1FA8 #\U+1FA9 #\U+1FAA #\U+1FAB #\U+1FAC #\U+1FAD #\U+1FAE
      #\U+1FAF #\U+1FB2 #\U+1FC2 #\U+1FF2 #\U+1FB7 #\U+1FC7 #\U+1FF7)


    ;; #\Arabic_Maddah_Above

    #(#\Arabic_Letter_Alef_With_Madda_Above)


    ;; #\Arabic_Hamza_Above

    #(#\Arabic_Letter_Alef_With_Hamza_Above
      #\Arabic_Letter_Waw_With_Hamza_Above
      #\Arabic_Letter_Yeh_With_Hamza_Above
      #\Arabic_Letter_Heh_Goal_With_Hamza_Above
      #\Arabic_Letter_Yeh_Barree_With_Hamza_Above
      #\Arabic_Letter_Heh_With_Yeh_Above)


    ;; #\Arabic_Hamza_Below

    #(#\Arabic_Letter_Alef_With_Hamza_Below)


    ;; #\U+093C

    #(#\U+0929 #\U+0931 #\U+0934)


    ;; #\U+09BE

    #(#\U+09CB)


    ;; #\U+09D7

    #(#\U+09CC)


    ;; #\U+0B3E

    #(#\U+0B4B)


    ;; #\U+0B56

    #(#\U+0B48)


    ;; #\U+0B57

    #(#\U+0B4C)


    ;; #\U+0BBE

    #(#\U+0BCA #\U+0BCB)


    ;; #\U+0BD7

    #(#\U+0B94 #\U+0BCC)


    ;; #\U+0C56

    #(#\U+0C48)


    ;; #\U+0CC2

    #(#\U+0CCA)


    ;; #\U+0CD5

    #(#\U+0CC0 #\U+0CC7 #\U+0CCB)


    ;; #\U+0CD6

    #(#\U+0CC8)


    ;; #\U+0D3E

    #(#\U+0D4A #\U+0D4B)


    ;; #\U+0D57

    #(#\U+0D4C)


    ;; #\U+0DCA

    #(#\U+0DDA #\U+0DDD)


    ;; #\U+0DCF

    #(#\U+0DDC)


    ;; #\U+0DDF

    #(#\U+0DDE)


    ;; #\U+102E

    #(#\U+1026)


    ;; #\U+3099

    #(#\U+3094 #\U+304C #\U+304E #\U+3050 #\U+3052 #\U+3054 #\U+3056
      #\U+3058 #\U+305A #\U+305C #\U+305E #\U+3060 #\U+3062 #\U+3065
      #\U+3067 #\U+3069 #\U+3070 #\U+3073 #\U+3076 #\U+3079 #\U+307C
      #\U+309E #\U+30F4 #\U+30AC #\U+30AE #\U+30B0 #\U+30B2 #\U+30B4
      #\U+30B6 #\U+30B8 #\U+30BA #\U+30BC #\U+30BE #\U+30C0 #\U+30C2
      #\U+30C5 #\U+30C7 #\U+30C9 #\U+30D0 #\U+30D3 #\U+30D6 #\U+30D9
      #\U+30DC #\U+30F7 #\U+30F8 #\U+30F9 #\U+30FA #\U+30FE)


    ;; #\U+309A

    #(#\U+3071 #\U+3074 #\U+3077 #\U+307A #\U+307D #\U+30D1 #\U+30D4
      #\U+30D7 #\U+30DA #\U+30DD)
    ))

(defun search-char-vector (vector char)
  ;; vector is a SIMPLE-VECTOR of chars sorted by char-code.
  ;; return the index of char in vector or NIL if not found
  (let* ((left 0)
         (right (1- (length vector))))
    (declare (fixnum left right))
    (if (and (char>= char (svref vector left))
             (char<= char (svref vector right)))
      (do* ()
           ((> left right))
        (let* ((mid (ash (the fixnum (+ left right)) -1))
               (midch (svref vector mid)))
          (declare (fixnum mid))
          (if (eql char midch)
            (return mid)
            (if (char< char midch)
              (setq right (1- mid))
              (setq left (1+ mid)))))))))


(defconstant HANGUL-SBASE #xAC00)
(defconstant HANGUL-LBASE #x1100)
(defconstant HANGUL-VBASE #x1161)
(defconstant HANGUL-TBASE #x11A7)

(defconstant HANGUL-SCOUNT 11172)
(defconstant HANGUL-LCOUNT 19)
(defconstant HANGUL-VCOUNT 21)
(defconstant HANGUL-TCOUNT 28)
(defconstant HANGUL-NCOUNT (* HANGUL-VCOUNT HANGUL-TCOUNT))

(defun combine-bmp-chars (base combiner)
  (if (and (char>= combiner (code-char hangul-vbase))
           (char< combiner (code-char (+ hangul-tbase hangul-tcount))))
    (if (and (char< combiner (code-char (+ hangul-vbase hangul-vcount)))
             (char>= base (code-char hangul-lbase))
             (char< base (code-char (+ hangul-lbase hangul-lcount))))
      (return-from combine-bmp-chars
        (code-char (+ hangul-lbase
                      (* hangul-ncount (- (char-code base) hangul-lbase))
                      (* hangul-tcount (- (char-code combiner) hangul-vbase))))))
    (if (and (char> combiner (code-char hangul-tbase))
             (char>= base (code-char hangul-sbase))
             (char< base (code-char (+ hangul-sbase hangul-scount))))
      (if (not (zerop (the fixnum (mod (- (char-code base) hangul-sbase) hangul-tcount))))
        (return-from combine-bmp-chars nil)
        (return-from combine-bmp-chars
          (code-char (+ (char-code base) (- (char-code combiner) hangul-tbase)))))))
    
  (let* ((idx (search-char-vector *bmp-combining-chars* combiner))
         (base-table (if idx (svref *bmp-combining-base-chars* idx))))
    (if base-table
      (let* ((combined-idx (search-char-vector base-table base)))
        (if combined-idx
          (svref (svref *bmp-precombined-chars* idx) combined-idx))))))

(defun precompose-simple-string (s)
  (let* ((n (length s)))
    (or (dotimes (i n s)
          (when (is-combinable (schar s i))
            (return nil)))
        (let* ((new (make-string n)))
          (declare (dynamic-extent new))
          (do* ((i 0 (1+ i))
                (nout -1)
                (lastch nil))
               ((= i n) (subseq new 0 (1+ nout)))
            (declare (fixnum nout i))
            (let* ((ch (schar s i)))
              (if (or (not lastch)
                      (not (is-combinable ch)))
                (setf lastch ch
                      (schar new (incf nout)) ch)
                (let* ((combined (combine-bmp-chars lastch ch)))
                  (if combined
                    (setf (schar new nout) (setq lastch combined))
                    (setf lastch ch
                      (schar new (incf nout)) ch))))))))))
