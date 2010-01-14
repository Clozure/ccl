;; ns-string-utils.lisp

(defpackage :interface-utilities
  (:nicknames :iu)
  (:export ns-to-lisp-string lisp-str-to-ns-data ns-data-to-lisp-str 
           lisp-object-to-ns-data ns-data-to-lisp-object lisp-to-temp-nsstring))

(in-package :iu)

(defun ns-to-lisp-string (ns-str)
  (if (plusp (#/length ns-str))
    (%get-cstring (#/cStringUsingEncoding: ns-str #$NSUTF8StringEncoding))
    ""))

(defun lisp-str-to-ns-data (lisp-str)
  (with-cstrs ((str lisp-str))
    (#/dataWithBytes:length: ns:ns-data str (1+ (length lisp-str)))))

(defun ns-data-to-lisp-str (nsdata)
  (%get-cstring (#/bytes nsdata)))

(defun lisp-object-to-ns-data (obj)
  (lisp-str-to-ns-data (format nil "~s" obj)))

(defun ns-data-to-lisp-object (nsdata)
  (read-from-string (ns-data-to-lisp-str nsdata)))

(defun lisp-to-temp-nsstring (string)
  ;; creates a string that is not owned by caller
  ;; so no release is necessary
  (with-encoded-cstrs :utf-8 ((s string))
    (#/stringWithUTF8String: ns:ns-string s)))

(provide :ns-string-utils)