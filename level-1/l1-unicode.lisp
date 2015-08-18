;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2006-2009 Clozure Associates and contributors.
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
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


(defun character-encoded-in-single-octet (c)
  (declare (ignore c))
  1)

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
  (character-size-in-octets-function 'character-encoded-in-single-octet)
  )

(defconstant byte-order-mark #\u+feff)
(defconstant byte-order-mark-char-code (char-code byte-order-mark))
(defconstant swapped-byte-order-mark-char-code #xfffe)

;;; Map 30-bit integers to 30-bit integers, using binary search.
;;; This is mostly to avoid using hash tables with integer key/value
;;; pairs and to avoid the GC impact of using such hash tables.
(defstruct pv-map
  keys
  values)



(defun %pv-map-lookup (code keys values)
  (declare (type (signed-byte 30) code)
           (type (simple-array (signed-byte 30) (*)) keys values)
           (optimize (speed 3) (safety 0)))
  (let* ((right (length keys))
         (left 0)
         (mid (ash right -1)))
    (declare (fixnum left right mid))
    (loop
      (if (>= left right)
        (return)
        (let* ((val (aref keys mid)))
          (declare (type (signed-byte 30) val))
          (if (= val code)
            (return (aref values mid))
            (progn
              (if (< val code)
                (setq left (1+ mid))
                (setq right mid))
              (setq mid (+ left (the fixnum (ash (the fixnum (- right left)) -1)))))))))))

(defun pv-map-lookup (code map)
  (%pv-map-lookup code (pv-map-keys map) (pv-map-values map)))


(defun pv-map-vectors (alist reverse-mapping)
  #-cross-compiling
  (dolist (pair alist)
    (unless (and (consp pair)
                 (typep (car pair) '(signed-byte 30))
                 (typep (cdr pair) '(signed-byte 30)))
      (error "Alist element ~s is not a CONS of two 30-bit integers" pair)))
  (let* ((n (length alist))
         (cars (make-array n :element-type '(signed-byte 30)))
         (cdrs (make-array n :element-type '(signed-byte 30)))
         (i 0))
    (declare (fixnum i n))
    (dolist (pair (sort-list  (copy-list alist) #'<  (if reverse-mapping (lambda (x) (cdr x)) (lambda (x) (car x)))))
      (setf (aref cars i) (car pair)
            (aref cdrs i) (cdr pair))
      (incf i))
    (if reverse-mapping
      (values cdrs cars)
      (values cars cdrs))))

(defun init-pv-map (alist &key reverse-mapping)
  (multiple-value-bind (keys values) (pv-map-vectors alist reverse-mapping)
    (make-pv-map :keys keys :values values)))


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

(defun note-stream-decoding-problem (stream)
  (let* ((source (if (typep stream 'ioblock)
                   (ioblock-stream stream)
                   stream))
         (position (stream-position source))
         (encoding-name
          (character-encoding-name
           (lookup-character-encoding (external-format-character-encoding (stream-external-format source))))))
    (signal (make-condition 'decoding-problem
                            :source source
                            :position position
                            :encoding-name encoding-name))
    #\Replacement_Character))

(defun note-vector-decoding-problem (vector index encoding)
  (signal (make-condition 'decoding-problem
                          :source vector
                          :position index
                          :encoding-name (let* ((enc (if (typep encoding 'character-encoding)
                                                       encoding
                                                       (lookup-character-encoding encoding))))
                                           (if enc (character-encoding-name enc) encoding))))
  #\Replacement_Character)

(defun note-encoding-problem (char destination encoding code)
  (signal (make-condition 'encoding-problem
                          :character char
                          :destination (if (typep destination 'ioblock)
                                         (ioblock-stream destination)
                                         destination)
                          :encoding-name (let* ((enc (if (typep encoding 'character-encoding)
                                                       encoding
                                                       (lookup-character-encoding encoding))))
                                           (if enc (character-encoding-name enc) encoding))))
  code)


(defun remove-character-encoding-alias (alias)
  "(REMOVE-CHARACTER-ENCODING-ALIAS alias)
alias - a keyword which is an alias for a defined character encoding.
Makes the keyword cease to be an alias for that encoding and returns T."
  (let* ((encoding (get-character-encoding alias))
         (aliases (character-encoding-aliases encoding)))
    (if (not (member alias aliases))
      (error "~S is not an alias for ~s." alias encoding)
      (progn
        (setf (character-encoding-aliases encoding)
              (remove alias aliases))
        (remhash alias *character-encodings*)
        t))))
              
  
(defun define-character-encoding-alias (alias existing)
  "(DEFINE-CHARACTER-ENCODING-ALIAS alias existing)
alias - a keyword
existing - a defined character encoding or a keyword that names one.
Tries to make alias an alias for the existing encoding and returns
that encoding."
  (check-type alias keyword)
  (let* ((canonical-encoding (ensure-character-encoding existing))
         (current (lookup-character-encoding alias)))
    (unless (eq current canonical-encoding)
      (if (and current
               (eq alias (character-encoding-name current)))
        (error "Can't make ~s an alias for ~s, since it already names ~s."
               alias existing current)
        (progn
          (when current
            (setf (character-encoding-aliases current)
                  (remove alias (character-encoding-aliases current))))
          (pushnew alias (character-encoding-aliases canonical-encoding))
          (setf (get-character-encoding alias) canonical-encoding))))
    canonical-encoding))

                          
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
         (setq code (note-encoding-problem char stream :iso-8859-1 (char-code #\Sub))))
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
           (setq code (note-encoding-problem char vector :iso-8859-1 (char-code #\Sub))))
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
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (if (>= code 256)
           (setq code (note-encoding-problem char pointer :iso-8859-1 (char-code #\Sub))))
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
         (setq code (note-encoding-problem char stream :us-ascii (char-code #\Sub))))
       (funcall write-function stream code)
       1)))
  :stream-decode-function
  (nfunction
   ascii-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit 128)
       (code-char 1st-unit)
       (note-stream-decoding-problem stream))))
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
           (setq code (note-encoding-problem char vector :us-ascii (char-code #\Sub))))
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
         (setf (schar string i) (if (< code 128)
                                  (code-char code)
                                  (note-vector-decoding-problem vector index :us-ascii)))))))
  :memory-encode-function
  (nfunction
   ascii-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (if (>= code 128)
           (setq code (note-encoding-problem char pointer :us-ascii (char-code #\Sub))))
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
           (setf (schar string i) (note-vector-decoding-problem pointer index :us-ascii))
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
;;; the break is at #x80 or #xd0 instead of #xa0).

(defmacro define-8-bit-fixed-width-encoding (name doc aliases encode-map decode-string mapping &optional (literal-limit #xa0))
  (unless (= (length mapping) (- 256 literal-limit))
    (error "Mapping data for encoding ~s should contain exactly ~d elements, not ~d" name (- 256 literal-limit) (length mapping)))
  `(progn
    (defstatic ,encode-map (init-pv-map ',(loop
                                             for i from literal-limit to 256
                                             for code across mapping
                                             unless (eql code (char-code #\replacement_character))
                                             collect (cons i code))
                                          :reverse-mapping t))
    (defstatic ,decode-string ,(map 'string #'code-char mapping))
    (define-character-encoding ,name
        ,doc
      :aliases ',aliases
      :stream-encode-function
      (nfunction ,(intern (concatenate 'string (string name) "-STREAM-ENCODE"))
       (lambda (char write-function stream)
         (let* ((code (char-code char))
                (c2 (if (< code ,literal-limit)
                      code
                      (pv-map-lookup code ,encode-map))))
           (declare (type (mod #x110000) code))
           (funcall write-function stream (or c2 (note-encoding-problem char stream ,name (char-code #\Sub))))
           1)))
      :stream-decode-function
      (nfunction
       ,(intern (concatenate 'string (string name) "-STREAM-DECODE"))
       (lambda (1st-unit next-unit-function stream)
         (declare (ignore next-unit-function)
                  (type (unsigned-byte 8) 1st-unit))
         (if (< 1st-unit ,literal-limit)
           (code-char 1st-unit)
           (let* ((char (schar ,decode-string (- 1st-unit ,literal-limit))))
             (when (eql char #\replacement_character)
               (note-stream-decoding-problem stream))
             char))))
      :vector-encode-function
      (nfunction
       ,(intern (concatenate 'string (string name) "-VECTOR-ENCODE"))
       (lambda (string vector idx start end)
         (declare (type (simple-array (unsigned-byte 8) (*)) vector)
                  (fixnum idx))
         (do* ((i start (1+ i)))
              ((>= i end) idx)
           (let* ((char (schar string i))
                  (code (char-code char))
                  (c2 (if (< code ,literal-limit)
                        code
                        (pv-map-lookup code ,encode-map))))
             (declare (type (mod #x110000) code))
             (setf (aref vector idx) (or c2 (note-encoding-problem char vector ,name (char-code #\Sub))))
             (incf idx)))))
      :vector-decode-function
      (nfunction
       ,(intern (concatenate 'string (string name) "-VECTOR-DECODE"))
       (lambda (vector idx noctets string)
         (declare (type (simple-array (unsigned-byte 8) (*)) vector))
         (do* ((i 0 (1+ i))
               (index idx (1+ index)))
              ((>= i noctets) index)
           (let* ((1st-unit (aref vector index)))
             (declare (type (unsigned-byte 8) 1st-unit))
             (setf (schar string i)
                   (if (< 1st-unit ,literal-limit)
                     (code-char 1st-unit)
                     (let* ((char (schar ,decode-string (the fixnum (- 1st-unit ,literal-limit)))))
                       (when (eql char #\replacement_character)
                         (note-vector-decoding-problem vector i ,name ))
                       char)))))))
      :memory-encode-function
      (nfunction
       ,(intern (concatenate 'string (string name) "-MEMORY-ENCODE"))
       (lambda (string pointer idx start end)
         (do* ((i start (1+ i)))
              ((>= i end) idx)
           (let* ((char (schar string i))
                  (code (char-code char))
                  (c2 (if (< code ,literal-limit)
                        code
                        (pv-map-lookup code ,encode-map))))
             (declare (type (mod #x110000) code))
             (setf (%get-unsigned-byte pointer idx) (or c2 (note-encoding-problem char pointer ,name (char-code #\Sub))))
             (incf idx)))))
      :memory-decode-function
      (nfunction
       ,(intern (concatenate 'string (string name) "-MEMORY-DECODE"))
       (lambda (pointer noctets idx string)
         (do* ((i 0 (1+ i))
               (index idx (1+ index)))
              ((>= i noctets) index)
           (let* ((1st-unit (%get-unsigned-byte pointer index)))
             (declare (type (unsigned-byte 8) 1st-unit))
             (setf (schar string i)
                   (if (< 1st-unit ,literal-limit)
                     (code-char 1st-unit)
                     (let* ((char (schar ,decode-string (the fixnum (- 1st-unit ,literal-limit)))))
                       (when (eql char #\replacement_character)
                         (note-vector-decoding-problem pointer index ,name ))
                       char)))))))
      :octets-in-string-function
      #'8-bit-fixed-width-octets-in-string
      :length-of-vector-encoding-function
      #'8-bit-fixed-width-length-of-vector-encoding
      :length-of-memory-encoding-function 
      #'8-bit-fixed-width-length-of-memory-encoding
      :decode-literal-code-unit-limit ,literal-limit
      :encode-literal-char-code-limit ,literal-limit)))

(define-8-bit-fixed-width-encoding :iso-8859-2
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in most
languages used in Central/Eastern Europe."
  (:iso_8859-2 :latin2 :l2 :csISOLatin2)
  *unicode-to-iso-8859-2-map*
  *iso-to-8859-2-to-unicode-string*
  #(#xA0 #x104 #x2D8 #x141 #xA4 #x13D #x15A #xA7 #xA8 #x160 #x15E #x164
    #x179 #xAD #x17D #x17B #xB0 #x105 #x2DB #x142 #xB4 #x13E #x15B #x2C7
    #xB8 #x161 #x15F #x165 #x17A #x2DD #x17E #x17C #x154 #xC1 #xC2 #x102
    #xC4 #x139 #x106 #xC7 #x10C #xC9 #x118 #xCB #x11A #xCD #xCE #x10E
    #x110 #x143 #x147 #xD3 #xD4 #x150 #xD6 #xD7 #x158 #x16E #xDA #x170
    #xDC #xDD #x162 #xDF #x155 #xE1 #xE2 #x103 #xE4 #x13A #x107 #xE7
    #x10D #xE9 #x119 #xEB #x11B #xED #xEE #x10F #x111 #x144 #x148 #xF3
    #xF4 #x151 #xF6 #xF7 #x159 #x16F #xFA #x171 #xFC #xFD #x163 #x2D9))

(define-8-bit-fixed-width-encoding :iso-8859-3
  "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in most
languages used in Southern Europe."
  (:iso_8859-3 :latin3 :l3 :csisolatin3)
  *unicode-to-iso-8859-3-map*
  *iso-to-8859-3-to-unicode-string*
  #(
    ;; #xa0 
    #x00a0 #x0126 #x02d8 #x00a3 #x00a4 #xfffd #x0124 #x00a7
    #x00a8 #x0130 #x015e #x011e #x0134 #x00ad #xfffd #x017b
    ;; #xb0 
    #x00b0 #x0127 #x00b2 #x00b3 #x00b4 #x00b5 #x0125 #x00b7
    #x00b8 #x0131 #x015f #x011f #x0135 #x00bd #xfffd #x017c
    ;; #xc0 
    #x00c0 #x00c1 #x00c2 #xfffd #x00c4 #x010a #x0108 #x00c7
    #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf
    ;; #xd0 
    #xfffd #x00d1 #x00d2 #x00d3 #x00d4 #x0120 #x00d6 #x00d7
    #x011c #x00d9 #x00da #x00db #x00dc #x016c #x015c #x00df
    ;; #xe0 
    #x00e0 #x00e1 #x00e2 #xfffd #x00e4 #x010b #x0109 #x00e7
    #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
    ;; #xf0 
    #xfffd #x00f1 #x00f2 #x00f3 #x00f4 #x0121 #x00f6 #x00f7
    #x011d #x00f9 #x00fa #x00fb #x00fc #x016d #x015d #x02d9
    ))

(define-8-bit-fixed-width-encoding :iso-8859-4
  "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in most
languages used in Northern Europe."
  (:iso_8859-4 :latin4 :l4 :csisolatin4)
  *unicode-to-iso-8859-4-map*
  *iso-to-8859-4-to-unicode-string*
  #(
    ;; #xa0 
    #x00a0 #x0104 #x0138 #x0156 #x00a4 #x0128 #x013b #x00a7
    #x00a8 #x0160 #x0112 #x0122 #x0166 #x00ad #x017d #x00af
    ;; #xb0 
    #x00b0 #x0105 #x02db #x0157 #x00b4 #x0129 #x013c #x02c7
    #x00b8 #x0161 #x0113 #x0123 #x0167 #x014a #x017e #x014b
    ;; #xc0 
    #x0100 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x012e
    #x010c #x00c9 #x0118 #x00cb #x0116 #x00cd #x00ce #x012a
    ;; #xd0 
    #x0110 #x0145 #x014c #x0136 #x00d4 #x00d5 #x00d6 #x00d7
    #x00d8 #x0172 #x00da #x00db #x00dc #x0168 #x016a #x00df
    ;; #xe0 
    #x0101 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x012f
    #x010d #x00e9 #x0119 #x00eb #x0117 #x00ed #x00ee #x012b
    ;; #xf0 
    #x0111 #x0146 #x014d #x0137 #x00f4 #x00f5 #x00f6 #x00f7
    #x00f8 #x0173 #x00fa #x00fb #x00fc #x0169 #x016b #x02d9
    ))

(define-8-bit-fixed-width-encoding :iso-8859-5
  "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in the
Cyrillic alphabet."
  (:iso_8859-5 :cyrillic :csISOLatinCyrillic :iso-ir-144)
  *unicode-to-iso-8859-5-map*
  *iso-to-8859-5-to-unicode-string*
  #(
    ;; #xa0
    #x00a0 #x0401 #x0402 #x0403 #x0404 #x0405 #x0406 #x0407
    #x0408 #x0409 #x040a #x040b #x040c #x00ad #x040e #x040f
    ;; #xb0
    #x0410 #x0411 #x0412 #x0413 #x0414 #x0415 #x0416 #x0417
    #x0418 #x0419 #x041a #x041b #x041c #x041d #x041e #x041f
    ;; #xc0
    #x0420 #x0421 #x0422 #x0423 #x0424 #x0425 #x0426 #x0427
    #x0428 #x0429 #x042a #x042b #x042c #x042d #x042e #x042f
    ;; #xd0
    #x0430 #x0431 #x0432 #x0433 #x0434 #x0435 #x0436 #x0437
    #x0438 #x0439 #x043a #x043b #x043c #x043d #x043e #x043f
    ;; #xe0
    #x0440 #x0441 #x0442 #x0443 #x0444 #x0445 #x0446 #x0447
    #x0448 #x0449 #x044a #x044b #x044c #x044d #x044e #x044f
    ;; #xf0
    #x2116 #x0451 #x0452 #x0453 #x0454 #x0455 #x0456 #x0457
    #x0458 #x0459 #x045a #x045b #x045c #x00a7 #x045e #x045f
    ))

(define-8-bit-fixed-width-encoding :iso-8859-6
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in the
Arabic alphabet."
  (:iso_8859-6 :arabic :csISOLatinArabic :iso-ir-127)
  *unicode-to-iso-8859-6-map*
  *iso-to-8859-6-to-unicode-string*
  #(
    ;; #xa0 
    #x00a0 #xfffd #xfffd #xfffd #x00a4 #xfffd #xfffd #xfffd
    #xfffd #xfffd #xfffd #xfffd #x060c #x00ad #xfffd #xfffd
    ;; #xb0 
    #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
    #xfffd #xfffd #xfffd #x061b #xfffd #xfffd #xfffd #x061f
    ;; #xc0 
    #xfffd #x0621 #x0622 #x0623 #x0624 #x0625 #x0626 #x0627
    #x0628 #x0629 #x062a #x062b #x062c #x062d #x062e #x062f
    ;; #xd0 
    #x0630 #x0631 #x0632 #x0633 #x0634 #x0635 #x0636 #x0637
    #x0638 #x0639 #x063a #xfffd #xfffd #xfffd #xfffd #xfffd
    ;; #xe0 
    #x0640 #x0641 #x0642 #x0643 #x0644 #x0645 #x0646 #x0647
    #x0648 #x0649 #x064a #x064b #x064c #x064d #x064e #x064f
    ;; #xf0 
    #x0650 #x0651 #x0652 #xfffd #xfffd #xfffd #xfffd #xfffd
    #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
    ))

(define-8-bit-fixed-width-encoding :iso-8859-7
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in the
Greek alphabet."
  (:iso_8859-7 :greek  :greek8 :csISOLatinGreek :iso-ir-126 :ELOT_928 :ecma-118)
  *unicode-to-iso-8859-7-map*
  *iso-to-8859-7-to-unicode-string*
  #(
    ;; #xa0
    #x00a0 #x2018 #x2019 #x00a3 #x20ac #x20af #x00a6 #x00a7
    #x00a8 #x00a9 #x037a #x00ab #x00ac #x00ad #xfffd #x2015
    ;; #xb0
    #x00b0 #x00b1 #x00b2 #x00b3 #x0384 #x0385 #x0386 #x00b7
    #x0388 #x0389 #x038a #x00bb #x038c #x00bd #x038e #x038f
    ;; #xc0
    #x0390 #x0391 #x0392 #x0393 #x0394 #x0395 #x0396 #x0397
    #x0398 #x0399 #x039a #x039b #x039c #x039d #x039e #x039f
    ;; #xd0
    #x03a0 #x03a1 #xfffd #x03a3 #x03a4 #x03a5 #x03a6 #x03a7
    #x03a8 #x03a9 #x03aa #x03ab #x03ac #x03ad #x03ae #x03af
    ;; #xe0
    #x03b0 #x03b1 #x03b2 #x03b3 #x03b4 #x03b5 #x03b6 #x03b7
    #x03b8 #x03b9 #x03ba #x03bb #x03bc #x03bd #x03be #x03bf
    ;; #xf0
    #x03c0 #x03c1 #x03c2 #x03c3 #x03c4 #x03c5 #x03c6 #x03c7
    #x03c8 #x03c9 #x03ca #x03cb #x03cc #x03cd #x03ce #xfffd
    ))

(define-8-bit-fixed-width-encoding :iso-8859-8
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in the
Hebrew alphabet."
  (:iso_8859-8 :hebrew :csISOLatinHebrew :iso-ir-138)
  *unicode-to-iso-8859-8-map*
  *iso-to-8859-8-to-unicode-string*
  #(
    ;; #xa0
    #x00a0 #xfffd #x00a2 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
    #x00a8 #x00a9 #x00d7 #x00ab #x00ac #x00ad #x00ae #x00af
    ;; #xb0
    #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
    #x00b8 #x00b9 #x00f7 #x00bb #x00bc #x00bd #x00be #xfffd
    ;; #xc0
    #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
    #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
    ;; #xd0
    #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
    #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #x2017
    ;; #xe0
    #x05d0 #x05d1 #x05d2 #x05d3 #x05d4 #x05d5 #x05d6 #x05d7
    #x05d8 #x05d9 #x05da #x05db #x05dc #x05dd #x05de #x05df
    ;; #xf0
    #x05e0 #x05e1 #x05e2 #x05e3 #x05e4 #x05e5 #x05e6 #x05e7
    #x05e8 #x05e9 #x05ea #xfffd #xfffd #x200e #x200f #xfffd
    ))

(define-8-bit-fixed-width-encoding :iso-8859-9
    "An 8-bit, fixed-width character encoding in which codes #x00-#xcf
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in the
Turkish alphabet."
  (:iso_8859-9 :latin5 :csISOLatin5 :iso-ir-148)
  *unicode-to-iso-8859-9-map*
  *iso-to-8859-9-to-unicode-string*
  #(
    ;; #xd0
    #x011e #x00d1 #x00d2 #x00d3 #x00d4 #x00d5 #x00d6 #x00d7
    #x00d8 #x00d9 #x00da #x00db #x00dc #x0130 #x015e #x00df
    ;; #xe0
    #x00e0 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x00e7
    #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
    ;; #xf0
    #x011f #x00f1 #x00f2 #x00f3 #x00f4 #x00f5 #x00f6 #x00f7
    #x00f8 #x00f9 #x00fa #x00fb #x00fc #x0131 #x015f #x00ff
    )
  #xd0)

(define-8-bit-fixed-width-encoding :iso-8859-10
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in Nordic
alphabets."
  (:iso_8859-10 :latin6 :csISOLatin6 :iso-ir-157)
  *unicode-to-iso-8859-10-map*
  *iso-to-8859-10-to-unicode-string*
  #(
    ;; #xa0
    #x00a0 #x0104 #x0112 #x0122 #x012a #x0128 #x0136 #x00a7
    #x013b #x0110 #x0160 #x0166 #x017d #x00ad #x016a #x014a
    ;; #xb0
    #x00b0 #x0105 #x0113 #x0123 #x012b #x0129 #x0137 #x00b7
    #x013c #x0111 #x0161 #x0167 #x017e #x2015 #x016b #x014b
    ;; #xc0
    #x0100 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x012e
    #x010c #x00c9 #x0118 #x00cb #x0116 #x00cd #x00ce #x00cf
    ;; #xd0
    #x00d0 #x0145 #x014c #x00d3 #x00d4 #x00d5 #x00d6 #x0168
    #x00d8 #x0172 #x00da #x00db #x00dc #x00dd #x00de #x00df
    ;; #xe0
    #x0101 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x012f
    #x010d #x00e9 #x0119 #x00eb #x0117 #x00ed #x00ee #x00ef
    ;; #xf0
    #x00f0 #x0146 #x014d #x00f3 #x00f4 #x00f5 #x00f6 #x0169
    #x00f8 #x0173 #x00fa #x00fb #x00fc #x00fd #x00fe #x0138
    ))

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
       (funcall write-function stream (or c2 (note-encoding-problem char stream :iso-8859-11 (char-code #\Sub))))
       1)))
  :stream-decode-function
  (nfunction
   iso-8859-11-stream-decode
   (lambda (1st-unit next-unit-function stream)
     (declare (ignore next-unit-function)
              (type (unsigned-byte 8) 1st-unit))
     (if (< 1st-unit #xa1)
       (code-char 1st-unit)
       (if (and (>= 1st-unit #xe01)
                (<= 1st-unit #xe5b)
                (not (and (>= 1st-unit #xe3b)
                          (<= 1st-unit #xe3e))))
         (code-char (- 1st-unit #xd60))
         (note-stream-decoding-problem stream)))))
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
         (setf (aref vector idx) (or c2 (note-encoding-problem char vector :iso-8859-11 (char-code #\Sub))))
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
                   (note-vector-decoding-problem vector index :iso-8859-11))))))))
  :memory-encode-function
  (nfunction
   iso-8859-11-memory-encode
   (lambda (string pointer idx start end)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char))
              (c2 (cond ((< code #xa1) code)
                      ((and (<= code #xfb)
                            (not (and (>= code #xdb) (<= code #xde))))
                       (+ code #x0d60)))))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-byte pointer idx) (or c2 (note-encoding-problem char pointer :iso-8859-11 (char-code #\Sub))))
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
                   (note-vector-decoding-problem pointer index :iso-8859-11))))))))
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

(define-8-bit-fixed-width-encoding :iso-8859-13
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in Baltic
alphabets."
  () 
  *unicode-to-iso-8859-13-map*
  *iso-to-8859-13-to-unicode-string*
  #(
    ;; #xa0
    #x00a0 #x201d #x00a2 #x00a3 #x00a4 #x201e #x00a6 #x00a7
    #x00d8 #x00a9 #x0156 #x00ab #x00ac #x00ad #x00ae #x00c6
    ;; #xb0
    #x00b0 #x00b1 #x00b2 #x00b3 #x201c #x00b5 #x00b6 #x00b7
    #x00f8 #x00b9 #x0157 #x00bb #x00bc #x00bd #x00be #x00e6
    ;; #xc0
    #x0104 #x012e #x0100 #x0106 #x00c4 #x00c5 #x0118 #x0112
    #x010c #x00c9 #x0179 #x0116 #x0122 #x0136 #x012a #x013b
    ;; #xd0
    #x0160 #x0143 #x0145 #x00d3 #x014c #x00d5 #x00d6 #x00d7
    #x0172 #x0141 #x015a #x016a #x00dc #x017b #x017d #x00df
    ;; #xe0
    #x0105 #x012f #x0101 #x0107 #x00e4 #x00e5 #x0119 #x0113
    #x010d #x00e9 #x017a #x0117 #x0123 #x0137 #x012b #x013c
    ;; #xf0
    #x0161 #x0144 #x0146 #x00f3 #x014d #x00f5 #x00f6 #x00f7
    #x0173 #x0142 #x015b #x016b #x00fc #x017c #x017e #x2019
    ))

(define-8-bit-fixed-width-encoding :iso-8859-14
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in Celtic
languages."
  (:iso_8859-14 :iso-ir-199 :latin8 :l8 :iso-celtic)
  *unicode-to-iso-8859-14-map*
  *iso-to-8859-14-to-unicode-string*
  #(
    ;; #xa0
    #x00a0 #x1e02 #x1e03 #x00a3 #x010a #x010b #x1e0a #x00a7
    #x1e80 #x00a9 #x1e82 #x1e0b #x1ef2 #x00ad #x00ae #x0178
    ;; #xb0
    #x1e1e #x1e1f #x0120 #x0121 #x1e40 #x1e41 #x00b6 #x1e56
    #x1e81 #x1e57 #x1e83 #x1e60 #x1ef3 #x1e84 #x1e85 #x1e61
    ;; #xc0
    #x00c0 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x00c7
    #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf
    ;; #xd0
    #x0174 #x00d1 #x00d2 #x00d3 #x00d4 #x00d5 #x00d6 #x1e6a
    #x00d8 #x00d9 #x00da #x00db #x00dc #x00dd #x0176 #x00df
    ;; #xe0
    #x00e0 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x00e7
    #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
    ;; #xf0
    #x0175 #x00f1 #x00f2 #x00f3 #x00f4 #x00f5 #x00f6 #x1e6b
    #x00f8 #x00f9 #x00fa #x00fb #x00fc #x00fd #x0177 #x00ff
    ))

(define-8-bit-fixed-width-encoding :iso-8859-15
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in
Western European languages (including the Euro sign and some other
characters missing from ISO-8859-1.)"
  (:iso_8859-15 :latin9)
  *unicode-to-iso-8859-15-map*
  *iso-to-8859-15-to-unicode-string*
  #(
    ;; #xa0
    #x00a0 #x00a1 #x00a2 #x00a3 #x20ac #x00a5 #x0160 #x00a7
    #x0161 #x00a9 #x00aa #x00ab #x00ac #x00ad #x00ae #x00af
    ;; #xb0
    #x00b0 #x00b1 #x00b2 #x00b3 #x017d #x00b5 #x00b6 #x00b7
    #x017e #x00b9 #x00ba #x00bb #x0152 #x0153 #x0178 #x00bf
    ;; #xc0
    #x00c0 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x00c7 
    ;; #xc8
    #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf 
    ;; #xd0
    #x00d0 #x00d1 #x00d2 #x00d3 #x00d4 #x00d5 #x00d6 #x00d7 
    ;; #xd8
    #x00d8 #x00d9 #x00da #x00db #x00dc #x00dd #x00de #x00df 
    ;; #xe0
    #x00e0 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x00e7 
    ;; #xe8
    #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef 
    ;; #xf0
    #x00f0 #x00f1 #x00f2 #x00f3 #x00f4 #x00f5 #x00f6 #x00f7 
    ;; #xf8
    #x00f8 #x00f9 #x00fa #x00fb #x00fc #x00fd #x00fe #x00ff 
    ))

(define-8-bit-fixed-width-encoding :iso-8859-16
    "An 8-bit, fixed-width character encoding in which codes #x00-#x9f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Intended to provide most characters found in Southeast
European languages."
  (:iso_8859-16 :latin10 :l1 :iso-ir-226)
  *unicode-to-iso-8859-16-map*
  *iso-to-8859-16-to-unicode-string*
  #(
    ;; #xa0
    #x00a0 #x0104 #x0105 #x0141 #x20ac #x201e #x0160 #x00a7
    #x0161 #x00a9 #x0218 #x00ab #x0179 #x00ad #x017a #x017b
    ;; #xb0
    #x00b0 #x00b1 #x010c #x0142 #x017d #x201d #x00b6 #x00b7
    #x017e #x010d #x0219 #x00bb #x0152 #x0153 #x0178 #x017c
    ;; #xc0
    #x00c0 #x00c1 #x00c2 #x0102 #x00c4 #x0106 #x00c6 #x00c7
    #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf
    ;; #xd0
    #x0110 #x0143 #x00d2 #x00d3 #x00d4 #x0150 #x00d6 #x015a
    #x0170 #x00d9 #x00da #x00db #x00dc #x0118 #x021a #x00df
    ;; #xe0
    #x00e0 #x00e1 #x00e2 #x0103 #x00e4 #x0107 #x00e6 #x00e7
    #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
    ;; #xf0
    #x0111 #x0144 #x00f2 #x00f3 #x00f4 #x0151 #x00f6 #x015b
    #x0171 #x00f9 #x00fa #x00fb #x00fc #x0119 #x021b #x00ff
    ))

(define-8-bit-fixed-width-encoding :macintosh
    "An 8-bit, fixed-width character encoding in which codes #x00-#x7f
map to their Unicode equivalents and other codes map to other Unicode
character values.  Traditionally used on Classic MacOS to encode characters
used in western languages."
  (:macos-roman :macosroman :mac-roman :macroman)
  *unicode-to-macintosh-map*
  *macintosh-to-unicode-string*
  #(
    ;; #x80 
    #x00c4 #x00c5 #x00c7 #x00c9 #x00d1 #x00d6 #x00dc #x00e1
    #x00e0 #x00e2 #x00e4 #x00e3 #x00e5 #x00e7 #x00e9 #x00e8
    ;; #x90 
    #x00ea #x00eb #x00ed #x00ec #x00ee #x00ef #x00f1 #x00f3
    #x00f2 #x00f4 #x00f6 #x00f5 #x00fa #x00f9 #x00fb #x00fc
    ;; #xa0 
    #x2020 #x00b0 #x00a2 #x00a3 #x00a7 #x2022 #x00b6 #x00df
    #x00ae #x00a9 #x2122 #x00b4 #x00a8 #x2260 #x00c6 #x00d8
    ;; #xb0 
    #x221e #x00b1 #x2264 #x2265 #x00a5 #x00b5 #x2202 #x2211
    #x220f #x03c0 #x222b #x00aa #x00ba #x2126 #x00e6 #x00f8
    ;; #xc0 
    #x00bf #x00a1 #x00ac #x221a #x0192 #x2248 #x2206 #x00ab
    #x00bb #x2026 #x00a0 #x00c0 #x00c3 #x00d5 #x0152 #x0153
    ;; #xd0 
    #x2013 #x2014 #x201c #x201d #x2018 #x2019 #x00f7 #x25ca
    #x00ff #x0178 #x2044 #x00a4 #x2039 #x203a #xfb01 #xfb02
    ;; #xe0 
    #x2021 #x00b7 #x201a #x201e #x2030 #x00c2 #x00ca #x00c1
    #x00cb #x00c8 #x00cd #x00ce #x00cf #x00cc #x00d3 #x00d4
    ;; #xf0 
    #xf8ff #x00d2 #x00da #x00db #x00d9 #x0131 #x02c6 #x02dc
    #x00af #x02d8 #x02d9 #x02da #x00b8 #x02dd #x02db #x02c7
    )
  #x80)



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
                     (note-stream-decoding-problem stream))
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
                                 (note-stream-decoding-problem stream))
                             (note-stream-decoding-problem stream))
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


                                     (note-stream-decoding-problem stream)))))
                             (note-stream-decoding-problem stream))))))))))
           (note-stream-decoding-problem stream)))))
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
               (setf (schar string i) (or char (note-vector-decoding-problem vector index :utf-8))))))))
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
    :character-size-in-octets-function  (lambda (c)
                                          (let* ((code (char-code c)))
                                            (declare (type (mod #x110000) code))
                                            (if (< code #x80)
                                              1
                                              (if (< code #x800)
                                                2
                                                (if (< code #x10000)
                                                  3
                                                  4)))))
      
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
              (note-stream-decoding-problem stream)))))
      (note-stream-decoding-problem stream))))



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

(defun utf-16-character-size-in-octets (c)
  (let* ((code (char-code c)))
    (declare (type (mod #x110000) code))
    (if (< code #x10000)
      2
      4)))

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
             (setf (schar string i) (or char (note-vector-decoding-problem vector index #+big-endian-target :utf-16be #-big-endian-target :utf-16le))))))))
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
            (setf (schar string i) (or char (note-vector-decoding-problem pointer index #+big-endian-target :utf-16be #-big-endian-target :utf-16le))))))))
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
    :character-size-in-octets-function 'utf-16-character-size-in-octets
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
           (setf (schar string i) (or char (note-vector-decoding-problem vector index #+big-endian-target :utf-16le #-big-endian-target :utf-16be))))))))
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
           (setf (schar string i) (or char (note-vector-decoding-problem pointer index #+big-endian-target :utf-16le #-big-endian-target :utf-16be))))))))
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
  :character-size-in-octets-function 'utf-16-character-size-in-octets
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
             (setf (schar string i) (or char (note-vector-decoding-problem vector index :utf-16)))))))))
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
             (setf (schar string i) (or char (note-vector-decoding-problem pointer index :utf-16)))))))))
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
         (declare (fixnum i j end nchars))
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
  :character-size-in-octets-function 'utf-16-character-size-in-octets  
  )


(defun two-octets-per-character (c)
  (declare (ignore c))
  2)

(defun ucs-2-stream-encode (char write-function stream)
  (let* ((code (char-code char)))
    (declare (type (mod #x110000) code))
    (if (>= code #x10000)
      (setq code (note-encoding-problem char stream :ucs-2 (char-code #\Replacement_Character))))
    (funcall write-function stream code)
    1))

(defun ucs-2-stream-decode (1st-unit next-unit-function stream)
  (declare (type (unsigned-byte 16) 1st-unit)
           (ignore next-unit-function))
  ;; CODE-CHAR returns NIL on either half of a surrogate pair.
  (or (code-char 1st-unit)
      (note-stream-decoding-problem stream)))


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
           (setq code (note-encoding-problem char vector #+big-endian-target :ucs-2be #-big-endian-target :ucs-2le (char-code #\Replacement_Character))))
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
                 (note-vector-decoding-problem vector index  #+big-endian-target :ucs-2be #-big-endian-target :ucs-2le))))))
  :memory-encode-function
  (nfunction
   native-ucs-2-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-word pointer idx)
                      (if (>= code #x10000)
                        (note-encoding-problem char pointer #+big-endian-target :ucs-2be #-big-endian-target :ucs-2le (char-code #\Replacement_Character))
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
         (setf (schar string i) (or (char-code 1st-unit) (note-vector-decoding-problem pointer index  #+big-endian-target :ucs-2be #-big-endian-target :ucs-2le)))))))
  :octets-in-string-function
  #'ucs-2-octets-in-string
  :length-of-vector-encoding-function
  #'ucs-2-length-of-vector-encoding
  :length-of-memory-encoding-function
  #'ucs-2-length-of-memory-encoding
  :decode-literal-code-unit-limit #x10000
  :encode-literal-char-code-limit #x10000  
  :nul-encoding #(0 0)
  :character-size-in-octets-function 'two-octets-per-character
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
           (setq code (note-encoding-problem char vector #+big-endian-target :ucs-2le #-big-endian-target :ucs-2be (char-code #\Replacement_Character))))
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
                 (note-vector-decoding-problem vector index #+big-endian-target :ucs-2le #-big-endian-target :ucs-2be))))))
  :memory-encode-function
  (nfunction
   reversed-ucs-2-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-word pointer idx)
               (if (>= code #x10000)
                 (%swap-u16 (note-encoding-problem char pointer #+big-endian-target :ucs-2le #-big-endian-target :ucs-2be (char-code #\Replacement_Character)))
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
         (setf (schar string i) (or (code-char 1st-unit) (note-vector-decoding-problem pointer index #+big-endian-target :ucs-2le #-big-endian-target :ucs-2be)))))))
  :octets-in-string-function
  #'ucs-2-octets-in-string
  :length-of-vector-encoding-function
  #'ucs-2-length-of-vector-encoding
  :length-of-memory-encoding-function
  #'ucs-2-length-of-memory-encoding
  :decode-literal-code-unit-limit #x10000
  :encode-literal-char-code-limit #x10000
  :nul-encoding #(0 0)
  :character-size-in-octets-function 'two-octets-per-character
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
           (setq code (note-encoding-problem char vector :ucs-2 (char-code #\Replacement_Character))))
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
             (setf (schar string i) (or (code-char 1st-unit)
                                        (note-vector-decoding-problem vector index :ucs-2))))))))
  :memory-encode-function
  (nfunction
   ucs-2-memory-encode
   (lambda (string pointer idx start end)
     (declare (fixnum idx))
     (setf (%get-unsigned-word pointer idx) byte-order-mark-char-code)
     (incf idx 2)
     (do* ((i start (1+ i)))
          ((>= i end) idx)
       (let* ((char (schar string i))
              (code (char-code char)))
         (declare (type (mod #x110000) code))
         (setf (%get-unsigned-word pointer idx)
                      (if (>= code #x10000)
                        (note-encoding-problem char pointer :ucs-2 (char-code #\Replacement_Character))
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
         (setf (schar string i) (or (code-char 1st-unit)
                                    (note-vector-decoding-problem pointer index :ucs-2))))))))
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
  :character-size-in-octets-function 'two-octets-per-character
  )


(defun four-octets-per-character (c)
  (declare (ignore c))
  4)

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
(define-character-encoding #+big-endian-target :utf-32be #-big-endian-target :utf-32le
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
                   (note-vector-decoding-problem vector index #+big-endian-target :utf-32be #-big-endian-target :utf-32le)))))))
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
                                    (note-vector-decoding-problem
                                     pointer index #+big-endian-target :utf-32be #-big-endian-target :utf-32le)))))))
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
  :character-size-in-octets-function 'four-octets-per-character
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
                   (note-vector-decoding-problem vector index #+big-endian-target :utf-32le #-big-endian-target :utf-32be)))))))
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
                                    (note-vector-decoding-problem pointer index #+big-endian-target :utf-32le #-big-endian-target :utf-32be)))))))

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
  :character-size-in-octets-function 'four-octets-per-character
  )

(define-character-encoding :utf-32
    "A 32-bit, fixed-length encoding in which all Unicode characters
can be encoded in a single 32-bit word.  The endianness of the encoded
data is indicated by the endianness of a byte-order-mark
character (#\u+feff) prepended to the data; in the absence of such a
character on input, input data is assumed to be in big-endian order.
Output is written in native byte order with a leading byte-order
mark."
    
  :aliases '(:ucs-4)
  :max-units-per-char 1
  :code-unit-size 32
  :native-endianness t                  ;not necessarily true.
  :stream-encode-function
  #'ucs-4-stream-encode
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
                                        (note-vector-decoding-problem
                                         vector index :utf-32))))))))
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
                                    (note-vector-decoding-problem
                                     pointer index :utf-32))))))))
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
  :character-size-in-octets-function 'four-octets-per-character
  )

(defun list-character-encodings (&key include-aliases)
  "Return a list of the names of supported character encodings."
  (let ((names nil))
    (maphash #'(lambda (name enc)
		 (if (eq name (character-encoding-name enc))
		   (push name names)
		   (when include-aliases
		     (push name names))))
	     *character-encodings*)
    names))

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
  (let* ((names (list-character-encodings)))
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
        (report-bad-arg vector '(array (unsigned-byte 8) (*))))
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

;;; Parse the string LINE (an Emacs-style file-attributes line)
;;; into a plist with Emacs variable names as keywords and values
;;; as strings (or list of strings).  Quietly return NIL on error.
(defun parse-file-options-line (line)
  (let* ((start (search "-*-" line))
         (start+3 (when start (+ start 3)))
         (end (and start+3 (search "-*-" line :start2 start+3))))
    (when end
      (setq line (subseq line start+3 end))
      (let* ((plist ()))
        (loop
          ;; The line between -*- pairs should be of the form
          ;; {varname: value;}*.  Emacs and Hemlock both seem
          ;; able to deal with the case where the last pair is
          ;; missing a trailing semicolon.
          (let* ((colon (position #\: line))
                 (semi (and colon (position #\; line :start (1+ colon)))))
            (unless colon
              (return plist))
            (let* ((key (intern (nstring-upcase (string-trim " 	" (subseq line 0 colon))) "KEYWORD"))
                   (val (string-trim '(#\space #\tab) (subseq line (1+ colon) (or semi (length line))))))
              (setq line (if semi (subseq line (1+ semi)) ""))
              (unless (eq key :eval)
                (let* ((already (getf plist key)))
                  (if already
                    (setf (getf plist key) (nconc (if (atom already)
                                                    (list already)
                                                    already)
                                                  (list val)))
                    (setq plist (nconc plist (list key val)))))))))))))

(defun process-file-coding-option (emacs-name line-termination)
  (when emacs-name
      (let* ((len (length emacs-name)))
        (cond ((and (> len 5) (string-equal "-unix" emacs-name :start2 (- len 5)))
               (setq emacs-name (subseq emacs-name 0 (- len 5))))
              ((and (> len 4) (or
                               (when (string-equal "-dos" emacs-name :start2 (- len 4))
                                 (setq line-termination :crlf))
                               (when (string-equal "-mac" emacs-name :start2 (- len 4))
                                 (setq line-termination :cr))))
                               
               (setq emacs-name (subseq emacs-name 0 (- len 4))))))
        (let* ((key (intern (string-upcase emacs-name) "KEYWORD"))
               (encoding (lookup-character-encoding key)))
          (if encoding
            (make-external-format :character-encoding (character-encoding-name encoding)
                                  :line-termination line-termination)
            (warn "file CODING option ~s isn't recognized as the name of a character encoding.~&Consider using ~S to define ~S as an alias for a supported encoding." key 'define-character-encoding-alias key)))))
  
(defun external-format-from-file-options (line)
  (process-file-coding-option (getf (parse-file-options-line line) :coding)
                              :unix))

(defun external-format-from-octet-buffer (buf count)
  (declare (fixnum count))
  (dotimes (i count)
    (let* ((octet (%get-unsigned-byte buf i)))
      (cond ((or (eql octet (char-code #\linefeed))
                 (eql octet (char-code #\return)))
             (return (external-format-from-file-options (%str-from-ptr buf i))))))))

