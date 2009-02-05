(in-package :spell)

(defclass dictionary ()
  ((string-table :accessor string-table :initarg :string-table)
   (descriptors :accessor descriptors :initarg :descriptors)
   ;; maps from hashes of strings to their corresponding descriptors
   (descriptor-table :accessor descriptor-table
                     :initarg :descriptor-table)
   (free-descriptors :accessor free-descriptors
                     :initarg :free-descriptors
                     :initform 0)
   (free-string-table-bytes :accessor free-string-table-bytes
                            :initarg :free-string-table-bytes
                            :initform 0)))

(defstruct (descriptor
             (:conc-name desc-))
  hash-code
  length
  string-index
  flags)
