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
         (app-class (#_NSClassFromString class-name))
         (app (#/sharedApplication appclass))
         (app-zone (#/zone app))
         (nib-name (%make-nsstring (namestring nib-path)))
         (dict (#/dictionaryWithObject:forKey: 
                (@class ns-mutable-dictionary) app #@"NSNibOwner"))
         (result (#/loadNibFile:externalNameTable:withZone: (@class ns-bundle)
                                                            nib-name
                                                            dict
                                                            app-zone)))
    (#/release app-class-name)
    (#/release nib-name)
    (#/release dict)
    result))
#|
(ccl::load-nibfile "/usr/local/openmcl/trunk/source/examples/cocoa/nib-loading/hello.nib")
|#