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
  (let* ((appclass (#_NSClassFromString (%make-nsstring "NSApplication")))
         (app (#/sharedApplication appclass))
         (app-zone (#/zone app))
         (nib-name (%make-nsstring (namestring nib-path)))
         (toplevel-objects-array (#/arrayWithCapacity: (@class ns-mutable-array) 8))
         (context (#/dictionaryWithObjectsAndKeys: (@class ns-mutable-dictionary)
                                                   app #@"NSNibOwner" 
                                                   toplevel-objects-array #@"NSNibTopLevelObjects"))
         (load-succeeded-p (#/loadNibFile:externalNameTable:withZone: (@class ns-bundle)
                                                                      nib-name context app-zone)))
    (values load-succeeded-p context)))

(setf  *my-app*
       (let* ((class-name (%make-nsstring "NSApplication"))
              (appclass (#_NSClassFromString class-name)))
         (#/release class-name)
         (#/sharedApplication appclass)))


#|
(ccl::load-nibfile "/usr/local/openmcl/trunk/source/examples/cocoa/nib-loading/hello.nib")
|#