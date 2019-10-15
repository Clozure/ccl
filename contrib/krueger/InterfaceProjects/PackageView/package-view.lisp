;;; package-view.lisp

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
  (require :nib))

(defpackage :package-view
  (:nicknames :pv)
  (:use :ccl :common-lisp)
  (:export test-package))

(in-package :pv)

;;; Sample lisp/Cocoa interface that uses a NIB file defined with interface builder;
;;; A definition is provided for the "SpeechController" class that was specified to interface builder
;;; as the class of the NIB file owner.
;;; We manually create an instance of SpeechController and specify it as the owner for the NIB file.

(defclass package-view-controller (ns:ns-object)
  ((package-table :foreign-type :id :accessor package-table)
   (use-table :foreign-type :id :accessor use-table)
   (used-by-table :foreign-type :id :accessor used-by-table)
   (current-package :accessor current-package :initform nil)
   (current-nicknames :accessor current-nicknames :initform nil)
   (all-packages :accessor all-packages :initform nil)
   (current-package-use-list :accessor current-package-use-list :initform nil)
   (current-package-used-by-list :accessor current-package-used-by-list :initform nil)
   (window-controller :accessor window-controller :initform nil))
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self package-view-controller) 
                                       &key &allow-other-keys)
  (let ((pkgs (list-all-packages)))
    (setf (all-packages self) (make-array (list (list-length pkgs)) 
                                          :initial-contents pkgs)))
  (let ((nib-name (ccl::%make-nsstring 
                   (namestring (truename "ip:PackageView;packageview.nib")))))
    (setf (window-controller self)
          (make-instance ns:ns-window-controller
            :with-window-nib-path nib-name
            :owner self))
    ;; Now make the controller load the nib file and make the window visible
    (#/window (window-controller self))
    (#/release nib-name))
  ;; we do the following so that ccl:terminate will be called before we are garbage
  ;; collected and we can release the window-controller that we created
  (ccl:terminate-when-unreachable self))

(defmethod ccl:terminate ((self package-view-controller))
  (#/release (window-controller self)))

;; Initialization methods called when the nib file is loaded
;; These initialize links from this package-view-controller to the text-views we defined 
;; to display things. Names correspond to the outlet names we defined for the 
;; PackageViewController class as file-owner in IB.

(objc:defmethod (#/setPackageTable: :void) 
                ((self package-view-controller) (tab :id))
  (setf (package-table self) tab)
  ;; Table may already have initialized before this link was set. Tell it to reload 
  ;; just in case.
  (#/reloadData tab))

(objc:defmethod (#/setUseTable: :void) 
                ((self package-view-controller) (tab :id))
  (setf (use-table self) tab)
  ;; Table may already have initialized before this link was set. Tell it to reload 
  ;; just in case
  (#/reloadData tab))

(objc:defmethod (#/setUseByTable: :void) 
                ((self package-view-controller) (tab :id))
  (setf (used-by-table self) tab)
  ;; Table may already have initialized before this link was set. Tell it to reload just in case
  (#/reloadData tab))

;; Methods called because we linked this class (as file owner) via the data-source outlet for 
;; our text views in IB

(objc:defmethod (#/numberOfRowsInTableView: #>NSInteger) 
                ((self package-view-controller) (tab :id))
  (cond ((eql tab (package-table self))
         (array-dimension (all-packages self) 0))
        ((eql tab (use-table self))
         (if (current-package-use-list self)
           (array-dimension (current-package-use-list self) 0)
           0))
        ((eql tab (used-by-table self))
         (if (current-package-used-by-list self)
           (array-dimension (current-package-used-by-list self) 0)
           0))
        (t
         ;; We can get called before the links are initialized. If so, return 0
         0)))
  
(objc:defmethod (#/tableView:objectValueForTableColumn:row: :id) 
                ((self package-view-controller) 
                 (tab :id)
                 (col :id)
                 (row #>NSInteger))
  (let ((ret-str nil))
    (cond ((eql tab (package-table self))
           (let ((col-id (ccl::lisp-string-from-nsstring (#/identifier col))))
             (setf ret-str (ccl::%make-nsstring 
                            (if (string= col-id "1") 
                              (package-name (svref (all-packages self) row))
                              (format nil 
                                      "~{~a~^,~}" 
                                      (package-nicknames 
                                       (svref (all-packages self) row))))))))
          ((eql tab (use-table self))
           (setf ret-str (ccl::%make-nsstring 
                          (if (current-package-use-list self)
                            (package-name (svref (current-package-use-list self) row))
                            ""))))
          ((eql tab (used-by-table self))
           (setf ret-str (ccl::%make-nsstring 
                          (if (current-package-used-by-list self)
                            (package-name (svref (current-package-used-by-list self) row))
                            ""))))
          (t
           (error "~s is not a linked view (~s, ~s, or ~s)" 
                  tab
                  (package-table self)
                  (use-table self)
                  (used-by-table self))))
    (#/autorelease ret-str)
    ret-str))

;; Methods called because we linked this class (as file owner) via the text-view delegate outlets in IB


(objc:defmethod (#/tableViewSelectionDidChange: :void) 
                ((self package-view-controller) (notif :id))
  (let ((tab (#/object notif)))
    (when (eql tab (package-table self))
      ;; change the other two tables to reflect the package selected
      (let* ((pkg (svref (all-packages self) (#/selectedRow (package-table self))))
             (pkgs-used (package-use-list pkg))
             (pkgs-using (package-used-by-list pkg)))
        (setf (current-package-use-list self)
              (make-array (list (list-length pkgs-used)) :initial-contents pkgs-used))
        (setf (current-package-used-by-list self)
              (make-array (list (list-length pkgs-using)) :initial-contents pkgs-using))
        (#/reloadData (use-table self))
        (#/reloadData (used-by-table self))))))
  

;; Methods called because we linked this class (as file owner) via the Window delegate outlet in IB

;; test by
(defun test-package ()
  (make-instance 'package-view-controller))

(provide :package-view)