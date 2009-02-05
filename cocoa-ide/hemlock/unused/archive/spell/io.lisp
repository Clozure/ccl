(in-package "SPELL")

(defparameter default-binary-dictionary #p"HOME:spell.bin")

(defconstant +descriptor-bytes+ 10
  "The number of bytes a descriptor takes up on disk.")

;;; going for ease of writing on this first pass.  later we'll pack things
;;; together a little bit more and document it.
(defun read-descriptor (stream)
  (let ((hash-code (read-byte stream))
        (length (read-byte stream))
        (low-index (read-byte stream))
        (high-index (read-byte stream))
        (flags (read-byte stream)))
    (make-descriptor :hash-code hash-code
                     :length length
                     :char-index (dpb high-index +whole-index-high-byte+
                                      low-index)
                     :flags flags)))

(defun write-descriptor (descriptor stream)
  (write-byte (desc-hash-code descriptor) stream)
  (write-byte (desc-length descriptor) stream)
  (write-byte (ldb +whole-index-low-byte+ (desc-string-index descriptor))
              stream)
  (write-byte (ldb +whole-index-high-byte+ (desc-string-index descriptor))
              stream)
  (write-byte (desc-flags descriptor) stream)
  (values))

(defun write-dictionary (filename dictionary entry-count string-table-length)
  (declare (fixnum string-table-length))
  (with-open-file (s filename
                     :direction :output
                     :element-type '(unsigned-byte 16)
                     :if-exists :overwrite
                     :if-does-not-exist :create)
    (write-byte +magic-file-id+ s)
    (write-byte +new-dictionary-size+ s)
    (write-byte entry-count s)
    (write-byte (ldb +whole-index-low-byte+ string-table-length) s)
    (write-byte (ldb +whole-index-high-byte+ string-table-length) s)
    (dotimes (i +new-dictionary-size+)
      (write-byte (aref (descriptor-table dictionary) i) s))
    (dotimes (i entry-count)
      ;; hack, because the 0th element goes unused.  see if we can
      ;; fix this assumption in the code elsewhere
      (unless (zerop i)
        (write-descriptor (aref (descriptors dictionary) i) s)))
    (with-open-file (s filename
                       :direction :output
                       :element-type 'base-char
                       :if-exists :append)
      (write-string (string-table dictionary)
                    s :end string-table-length))))

(defun read-dictionary (&optional (filename default-binary-dictionary))
  (with-open-file (stream filename
                          :direction :input
                          :if-does-not-exist :error
                          :element-type '(unsigned-byte 16))
    (let* ((header (make-array 5 :element-type '(unsigned-byte 16)))
           (header-len (read-sequence header stream)))
      (unless (= header-len 5)
        (error "File is not a dictionary: ~S." filename))
      (unless (= (aref header 0) +magic-file-id+)
        (error "File is not a dictionary: ~S." filename))
      (let* ((dict-size (read-byte stream))
             (entry-count (read-byte stream))
             (string-table-length-low (read-byte stream))
             (string-table-length-high (read-byte stream))
             (string-table-length (dpb string-table-length-high
                                       +whole-index-high-byte+
                                       string-table-length-low))
             (word-table (make-array dict-size
                                     :element-type '(unsigned-byte 16)))
             (descriptors (make-array (1+ entry-count)
                                      :initial-element nil))
             (string-table (make-array string-table-length
                                       :element-type 'base-char)))
        (read-sequence word-table stream)
        (dotimes (i entry-count)
          (setf (aref descriptors (1+ i)) (read-descriptor stream)))
        (with-open-file (s filename
                           :direction :input
                           :if-does-not-exist :error
                           :element-type 'base-char)
          ;; ??? is this portable?
          (file-position s (file-position stream))
          (read-sequence string-table s))
        (make-instance 'dictionary
                       :string-table string-table
                       :descriptors descriptors
                       :descriptor-table word-table)))))
