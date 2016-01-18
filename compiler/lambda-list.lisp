;;;-*-Mode: LISP; Package: CCL -*-
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


;;; Compiler functions needed elsewhere

(defun %lfun-info-index (fn)
  (and (compiled-function-p fn)
       (let ((bits (lfun-bits fn)))
         (declare (fixnum bits))
         (and (logbitp $lfbits-info-bit bits)
               (%i- (uvsize (function-to-function-vector fn))
                              (if (logbitp $lfbits-noname-bit bits) 2 3))))))
(defun %lfun-info (fn)
  (let* ((index (%lfun-info-index fn)))
    (if index (%svref (function-to-function-vector fn) index))))

(defun function-source-note (fn)
  (getf (%lfun-info fn) '%function-source-note))

(defun %function-acode-string (fn)
  (getf (%lfun-info fn) '%function-acode-string))

(defun uncompile-function (fn)
  (getf (%lfun-info fn) 'function-lambda-expression ))

;;; used-by: backtrace, arglist
(defun function-symbol-map (fn)
  (getf (%lfun-info fn) 'function-symbol-map))

(defun find-source-note-at-pc (fn pc)
  ;(declare (values source-note start-pc end-pc))
  (let* ((function-note (function-source-note fn))
         (pc-source-map (getf (%lfun-info fn) 'pc-source-map))
         (best-guess -1)
         (best-length 0)
         (len (length pc-source-map)))
    (declare (fixnum best-guess best-length len))
    (when (and function-note pc-source-map)
      (do ((q 0 (+ q 4)))
          ((= q len))
        (declare (fixnum q))
        (let* ((pc-start (aref pc-source-map q))
               (pc-end (aref pc-source-map (%i+ q 1))))
          (declare (fixnum pc-start pc-end))
          (when (and (<= pc-start pc)
		     (< pc pc-end)
                     (or (eql best-guess -1)
                         (< (%i- pc-end pc-start) best-length)))
            (setf best-guess q
                  best-length (- pc-end pc-start)))))
      (unless (eql best-guess -1)
        (values
          (let ((def-pos (source-note-start-pos function-note)))
            (make-source-note :source function-note
                              :filename (source-note-filename function-note)
                              :start-pos (+ def-pos (aref pc-source-map (+ best-guess 2)))
                              :end-pos (+ def-pos (aref pc-source-map (+ best-guess 3)))))
          (aref pc-source-map best-guess)
          (aref pc-source-map (+ best-guess 1)))))))

;;; Lambda-list utilities





;;; Lambda-list verification:

;;; these things MUST be compiled.
(eval-when (:load-toplevel)

(defvar *structured-lambda-list* nil)




(defun parse-body (body env &optional (doc-string-allowed t) &aux
   decls
   doc
   (tail body)
   form)
  (declare (ignore env))
  (loop
   (if (endp tail) (return))  ; otherwise, it has a %car and a %cdr
   (if (and (stringp (setq form (%car tail))) (%cdr tail))
    (if doc-string-allowed
     (setq doc form)
     (return))
    (if (not (and (consp form) (symbolp (%car form)))) 
     (return)
     (if (eq (%car form) 'declare)
      (push form decls)
      (return))))
   (setq tail (%cdr tail)))
  (return-from parse-body (values tail (nreverse decls) doc)))

) ; end of eval-when (load)

;;; End of verify-lambda-list.lisp
