;; lisp-doc-controller.lisp

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
  (require :menu-utils))

(defpackage :interface-utilities
  (:nicknames :iu)
  (:export lisp-doc-controller))

(in-package :iu)

;; demonstration code for creating a Cocoa document class in lisp that can be required
;; and loaded into a standard running CCL IDE (i.e. does not require a stand-alone program).

;; lisp-doc-controller class
;; This class does some of the same things that the shared NSDocumentController instance
;; does for stand-alone application programs. We use it so that we don't have to mess
;; with CCL's existing interfaces to or its NSApplication delegate objects et. We will
;; create specific menu-items that target an instance of this class to create new documents
;; of a specified type that the CCL IDE knows nothing about. We will tell the shared
;; NSDocumentController about our documents so that it can manage things like saving and
;; closing them. This class will also handle opening files of a specified type.
;; The creator of one of these objects can also specify a name to use for inserting
;; "New <menu-class>" and "Open <menu-class>" menuitems which will target this instance.

(defclass lisp-doc-controller (ns:ns-object)
  ((document-class :accessor document-class :initarg :doc-class)
   (menu-class-name :accessor menu-class-name :initarg :menu-class)
   (file-ext :accessor file-ext :initarg :file-ext)
   (doc-ctrlr :accessor doc-ctrlr)
   (open-panel :accessor open-panel)
   (type-ns-str :accessor type-ns-str)
   (ext-ns-str :accessor ext-ns-str)
   (type-array :accessor type-array)
   (documents :accessor documents :initform nil))
  (:default-initargs 
    :doc-class nil
    :menu-class nil
    :file-ext nil)
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self lisp-doc-controller) 
                                       &key menu-class file-ext &allow-other-keys)
  (ccl:terminate-when-unreachable self)
  (when menu-class
    (setf (type-ns-str self) (ccl::%make-nsstring menu-class))
    (setf (ext-ns-str self) (ccl::%make-nsstring file-ext))
    (setf (doc-ctrlr self) (#/sharedDocumentController ns:ns-document-controller))
    (setf (open-panel self) (make-instance ns:ns-open-panel))
    (#/retain (open-panel self))
    (setf (type-array self)
          (#/arrayByAddingObject: (make-instance ns:ns-array) (ext-ns-str self)))
    (make-and-install-menuitems-after "File" "New"
                                      (list (concatenate 'string "New " menu-class) 
                                            "newDoc"
                                            nil
                                            self))
    (make-and-install-menuitems-after "File" "Open..."
                                      (list (concatenate 'string "Open " menu-class "...") 
                                            "openDoc"
                                            nil
                                            self))
    (make-and-install-menuitems-after "File" "Print..."
                                      (list (concatenate 'string "Print " menu-class "...") 
                                            (concatenate 'string "print" menu-class ":")
                                            nil
                                            nil))))

(defmethod ccl:terminate ((self lisp-doc-controller))
  (#/release (type-ns-str self))
  (#/release (ext-ns-str self))
  (#/release (type-array self))
  (#/release (open-panel self)))

(objc:defmethod (#/newDoc :void)
                ((self lisp-doc-controller))
  (let ((new-doc (make-instance (document-class self))))
    (push new-doc (documents self))
    ;; register the document with the shared controller so that things like
    ;; "save" and "close" will work properly
    (#/addDocument: (doc-ctrlr self) new-doc)
    (#/makeWindowControllers new-doc)
    (#/showWindows new-doc)))

(objc:defmethod (#/openDoc :void)
                ((self lisp-doc-controller))
  (let ((result (#/runModalForTypes: (open-panel self) (type-array self))))
    (when (eql result 1)
      (let ((urls (#/URLs (open-panel self))))
        (dotimes (i (#/count urls))
          (let ((doc (make-instance (document-class self))))
            (setf doc (#/initWithContentsOfURL:ofType:error: 
                       doc
                       (#/objectAtIndex: urls i)
                       (type-ns-str self)
                       (%null-ptr)))
            (if doc
              (progn
                (pushnew doc (documents self))
                (#/addDocument: (doc-ctrlr self) doc)
                (#/makeWindowControllers doc)
                (#/showWindows doc))
              (#_NSRunAlertPanel #@"ALERT" 
                                 #@"Could not open specified file ... ignoring it."
                                 #@"OK"  
                                 (%null-ptr)
                                 (%null-ptr)))))))))

(provide :lisp-doc-controller)
