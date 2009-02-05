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
  (let* ((app-class-name (%make-nsstring "NSApplication"))
         (app-class (#_NSClassFromString app-class-name))
         (app (#/sharedApplication app-class))
         (app-zone (#/zone app))
         (nib-name (%make-nsstring (namestring nib-path)))
         (objects-array (#/arrayWithCapacity: (@class ns-mutable-array) 16))
         (dict (#/dictionaryWithObjectsAndKeys: (@class ns-mutable-dictionary)
                    app #@"NSNibOwner"
                    objects-array #&NSNibTopLevelObjects))
         (toplevel-objects (list))
         (result (#/loadNibFile:externalNameTable:withZone: (@class ns-bundle)
                                                            nib-name
                                                            dict
                                                            app-zone)))
    (dotimes (i (#/count objects-array))
      (setf toplevel-objects 
            (cons (#/objectAtIndex: objects-array i)
                  toplevel-objects)))
    (#/release app-class-name)
    (#/release nib-name)
    (#/release dict)
    (#/release objects-array)
    (values toplevel-objects result)))

#|
(ccl::load-nibfile "/usr/local/openmcl/trunk/source/examples/cocoa/nib-loading/hello.nib")
|#

