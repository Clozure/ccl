;;; -*- lisp -*-

#+openmcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

#-openmcl
(error "Requires OpenMCL")


(cl:defpackage :easygui-system (:use :cl :asdf))

(in-package :easygui-system)

(defsystem cocoa.asd)

(defmethod perform :after ((o compile-op) (sys (eql (find-system :cocoa.asd))))
  (require :cocoa))

(defmethod operation-done-p ((o compile-op) (sys (eql (find-system :cocoa.asd))))
  nil)

(defsystem easygui
  :depends-on (cocoa.asd)
  :components ((:file "package")
               (:file "new-cocoa-bindings" :depends-on ("package"))
               (:file "events" :depends-on ("new-cocoa-bindings"))
               (:file "views" :depends-on ("events"))
               (:file "action-targets" :depends-on ("views"))
               (:file "dialogs" :depends-on ("new-cocoa-bindings"))
               (:module "example"
                        :depends-on ("action-targets")
                        :components
                        ((:file "tiny")
                         (:file "currency-converter")
                         (:file "view-hierarchy")))))