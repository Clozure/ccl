;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;;   Copyright (C) 2004 Clozure Associates
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

(in-package "GUI")

(defstruct cocoa-default
  symbol                                ; a lisp special variable
  string                                ; an NSConstantString
  type                                  ; a keyword
  value                                 ; the "standard" initial value
  doc                                   ; a doc string
  change-hook                           ; an optional hook function
  )

(let* ((cocoa-defaults ()))
  (defun %get-cocoa-default (name)
    (find name cocoa-defaults :key #'cocoa-default-symbol))
  (defun %put-cocoa-default (default)
    (push default cocoa-defaults))
  (defun cocoa-defaults () cocoa-defaults)
  (defun %remove-cocoa-default (name)
    (setq cocoa-defaults
          (delete name cocoa-defaults :key #'cocoa-default-symbol)))
  (defun %clear-cocoa-defaults () (setq cocoa-defaults nil)))

(defun set-cocoa-default (name string type value doc &optional change-hook)
  (check-type name symbol)
  (check-type string ccl::objc-constant-string)
  (check-type type keyword)
  (check-type doc (or null string))
  (%remove-cocoa-default name)
  (%put-cocoa-default (make-cocoa-default :symbol name
                                          :string string
                                          :type type
                                          :value value
                                          :doc doc
                                          :change-hook change-hook))
  (if (eq type :color)
    (apply #'color-values-to-nscolor value)
    value))

;;; Names which contain #\* confuse Cocoa Bindings.
(defun objc-default-key (name)
  (ccl::ns-constant-string (ccl::lisp-to-objc-message (list (make-symbol (remove #\* (string name)))))))
  

(defun %define-cocoa-default (name type value doc &optional change-hook)
  (proclaim `(special ,name))
  ;; Make the variable "GLOBAL": its value can be changed, but it can't
  ;; have a per-thread binding.
  (ccl::%symbol-bits name (logior (ash 1 ccl::$sym_vbit_global)
				  (the fixnum (ccl::%symbol-bits name))))
  (record-source-file name 'variable)
  (setf (documentation name 'variable) doc)
  (set name (set-cocoa-default name (objc-default-key name) type value doc change-hook))
  name)
  
  

(defmacro def-cocoa-default (name type value  doc &optional change-hook &environment env)
  `(progn
     (eval-when (:compile-toplevel)
       (ccl::note-variable-info ',name :global ,env))
    (declaim (special ,name))
    (defloadvar ,name nil)
    (%define-cocoa-default ',name  ',type ,value ',doc ,change-hook)))

    
(defun update-cocoa-defaults ()
  (update-cocoa-defaults-list
   (#/standardUserDefaults ns:ns-user-defaults)
   (cocoa-defaults)))

(defun update-cocoa-defaults-list (domain defaults)
  (dolist (d defaults)
    (let* ((name (cocoa-default-symbol d))
           (type (cocoa-default-type d)) 
           (key (ccl::objc-constant-string-nsstringptr (cocoa-default-string d))))
      (let* ((hook (cocoa-default-change-hook d))
             (old-value (symbol-value name)))
        (case type
          (:int
           (set name (#/integerForKey: domain key)))
          (:float
           (set name (#/floatForKey: domain key)))
          (:bool
           (set name (#/boolForKey: domain key)))
          (:string
           (let* ((nsstring (#/stringForKey: domain key)))
             (unless (%null-ptr-p nsstring)
               (set name (lisp-string-from-nsstring nsstring)))))
          ((:color :font)
           (let* ((data (#/dataForKey: domain key)))
             (unless (%null-ptr-p data)
               (set name (#/retain (#/unarchiveObjectWithData: ns:ns-unarchiver data)))))))
        (when hook (funcall hook old-value (symbol-value name)))))))



;;; Return an NSDictionary describing the "default" values of the defaults.
(defun cocoa-defaults-initial-values ()
  (let* ((defaults (cocoa-defaults))
         (dict (make-instance 'ns:ns-mutable-dictionary
                              :with-capacity (length defaults))))
    (dolist (d defaults dict)
      (let* ((value (cocoa-default-value d)))
        (#/setObject:forKey: dict
                             (case (cocoa-default-type d)
                               (:color (#/archivedDataWithRootObject:
                                        ns:ns-archiver
                                        (apply #'color-values-to-nscolor value)))
			       (:font (#/archivedDataWithRootObject:
				       ns:ns-archiver
				       (funcall value)))
                               (:bool (if value #@"YES" #@"NO"))
                               (t
                                (%make-nsstring (format nil "~a" (cocoa-default-value d)))))
                             (ccl::objc-constant-string-nsstringptr (cocoa-default-string d)))))))
