;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          nib-loading.lisp
;;;; Version:       0.1
;;;; Project:       cocoa examples
;;;; Purpose:       examples of loading nibs dynamically in the IDE
;;;;
;;;; ***********************************************************************

(in-package :ccl)

(defun load-nibfile (nib-path)
  (let* ((app-zone (#/zone *NSApp*))
         (nib-name (%make-nsstring (namestring nib-path)))
         (objects-array (#/arrayWithCapacity: ns:ns-mutable-array 16))
         (dict (#/dictionaryWithObjectsAndKeys: ns:ns-mutable-dictionary
                    *NSApp* #&NSNibOwner
                    objects-array #&NSNibTopLevelObjects
		    +null-ptr+))
         (toplevel-objects (list))
         (result (#/loadNibFile:externalNameTable:withZone: ns:ns-bundle
                                                            nib-name
                                                            dict
                                                            app-zone)))
    (dotimes (i (#/count objects-array))
      (setf toplevel-objects 
            (cons (#/objectAtIndex: objects-array i)
                  toplevel-objects)))
    (#/release nib-name)
    (values toplevel-objects result)))

#|
(ccl::load-nibfile "/usr/local/openmcl/trunk/source/examples/cocoa/nib-loading/hello.nib")
|#

