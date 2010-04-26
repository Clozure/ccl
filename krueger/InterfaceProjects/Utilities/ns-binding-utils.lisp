;; ns-binding-utils.lisp

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
  (require :ns-object-utils)
  (require :assoc-array))

(defpackage :interface-utilities
  (:nicknames :iu)
  (:export bound-to linking-views link-path-components lisp-to-objc-keypathname objc-to-lisp-keypathname))

(in-package :iu)

;; Given a window and another object, find all the subviews of the window that link to that object
;; for their value and the key paths that they use (returned as a string).

(defmethod linking-views ((win ns:ns-window) (obj ns:ns-object))
  (let* ((cview (#/contentView win))
         (subviews (ns-to-lisp-list (#/subviews cview) :no-convert t)))
    (mapcan #'(lambda (v)
                (multiple-value-bind (bound-p path) (bound-to v obj #@"value")
                  (when bound-p
                    (list (cons v path)))))
            subviews)))

(defmethod bound-to ((view ns::ns-view) (bound-to-obj ns::ns-object) binding-ns-string)
  (let* ((objc-info (#/infoForBinding: view binding-ns-string))
         (info (and (not (eql objc-info (%null-ptr))) (ns-to-lisp-assoc objc-info :no-convert t)))
         (proxy (cdr (assoc 'CL-USER::NSOBSERVEDOBJECT info)))
         (proxy-desc (and proxy (ns-to-lisp-string (#/description proxy))))
         (bound-to-desc (ns-to-lisp-string (#/description bound-to-obj)))
         (path (and info (ns-to-lisp-string (cdr (assoc 'CL-USER::NSOBSERVEDKEYPATH info))))))
    (if (and info (find-substring bound-to-desc proxy-desc))
      (values t path)
      (values nil nil))))

(defun link-path-components (path-str)
  (if (zerop (length path-str))
    nil
    (do* ((begin 0
                 (1+ end))
          (end (or (position #\. path-str :start (1+ begin))
                   (length path-str))
               (or (position #\. path-str :start (1+ begin))
                   (length path-str)))
          (words (list (subseq path-str begin end))
                 (push (subseq path-str begin end) words)))
         ((>= end (length path-str))
          (mapcar #'objc-to-lisp-keypathname (nreverse words))))))

(let ((path-trans (make-instance 'assoc-array :rank 2)))

  (defun objc-to-lisp-keypathname (name-str)
    ;; translate a name from Objective-C to Lisp
    ;; Use standard translation for function/slot names except the use of 
    ;; underscore (#\_) is used initially to delimit a package specifier
    (let* ((package-end (if (char= (elt name-str 0) #\_)
                          (position #\_ name-str :start 1)))
           (pkg-string (string-upcase (subseq name-str 1 package-end)))
           (path-string (if package-end 
                          (subseq name-str (1+ package-end))
                          name-str))
           (pkg (or (and package-end
                         (find-package pkg-string))
                    (find-package :cl-user))))
      ;; cache the string we are translating so that we can reverse
      ;; translate to exactly the same string used by the developer.
      (setf (assoc-aref path-trans pkg path-string) name-str)
      (ccl::compute-lisp-name path-string pkg)))
  
  (defun lisp-to-objc-keypathname (name)
    ;; translate name (symbol or string) from Lisp to Objective-C
    ;; If we previously cached a string that came from Objective-C and
    ;; was translated to this name, then translate back to it. This
    ;; prevents problems caused by alternative package names/nicknames
    ;; that the developer might have used in Interface Builder.
    ;; Use standard translation for function/slot names except prefix
    ;; package name and underscore if package is not cl-user
   
    (let ((pkg (if (symbolp name)
                 (symbol-package name)
                 (find-package :cl-user)))
          (name-str (string name))
          (name-segments nil))
      (or (assoc-aref path-trans pkg name-str)
          (progn
            (do* ((start 0 (1+ pos))
                  (pos (position #\- name-str)
                       (position #\- name-str :start start)))
                 ((null pos) (setf name-segments (nreverse (cons (subseq name-str start) name-segments))))
              (setf name-segments (cons (subseq name-str start pos) name-segments)))
            (setf name-segments (cons (string-downcase (first name-segments))
                                      (mapcar #'string-capitalize (rest name-segments))))
            (unless (eq pkg (find-package :cl-user))
              (setf name-segments (cons (concatenate 'string
                                                     "_"
                                                     (lisp-to-objc-keypathname (or (first (package-nicknames pkg))
                                                                                   (package-name pkg)))
                                                     "_")
                                        name-segments)))
            (apply #'concatenate 'string name-segments)))))

) ;; end of definitions using path-trans assoc-array
            