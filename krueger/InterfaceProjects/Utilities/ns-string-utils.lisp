;; ns-string-utils.lisp

(defpackage :interface-utilities
  (:nicknames :iu)
  (:export ns-to-lisp-string lisp-str-to-ns-data ns-data-to-lisp-str 
           lisp-object-to-ns-data ns-data-to-lisp-object lisp-to-temp-nsstring
           nsstring-to-class nsstring-to-func nsstring-to-sym find-func))

(in-package :iu)

(defun ns-to-lisp-string (ns-str)
  (if (and (not (eql (%null-ptr) ns-str)) (plusp (#/length ns-str)))
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

(defun nsstring-to-class (ns-str)
  (let ((lisp-obj (read-from-string (ns-to-lisp-string ns-str) nil nil))
        (classes nil))
    (if (consp lisp-obj)
      (dolist (obj lisp-obj (nreverse classes))
        (push (find-class obj nil) classes))
      (find-class lisp-obj nil))))

(defun find-func (func-str)
  (let* ((sym (read-from-string func-str nil nil)))
    (cond ((and (typep sym 'function-name) (fboundp sym))
           (symbol-function sym))
          ((and (consp sym) (eq (first sym) 'function))
           (let ((fsym (second sym)))
             (and (typep fsym 'function-name) 
                  (fboundp fsym)
                  (symbol-function fsym)))))))

(defun nsstring-to-func (ns-str)
  (find-func (ns-to-lisp-string ns-str)))

(defun nsstring-to-sym (ns-str)
  (let ((sym (read-from-string (ns-to-lisp-string ns-str) nil nil)))
    (if (symbolp sym) sym nil)))

(provide :ns-string-utils)