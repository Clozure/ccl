;;;; -*-Mode: Lisp; Package: CCL -*-
;;;;
;;;; SPDX-License-Identifier: Apache-2.0

;;; Build a standalone IDE in ccl:cocoa-ide;build;Clozure CL.app;.
;;; Unlike the IDE built by (require :cocoa-application), it has its
;;; own copies of CCL's sources and interfaces, so it can be moved
;;; anywhere.
;;;
;;; cocoa-ide/Makefile loads this into each Darwin lisp in turn; each
;;; one adds its lisp kernel and image to the same bundle, making a
;;; universal application.

(in-package "CCL")

(pushnew :standalone-ide *features*)

(defparameter *cocoa-ide-path* "ccl:cocoa-ide;build;Clozure CL.app;")
(defparameter *cocoa-ide-copy-sources-p* t)
(defparameter *cocoa-ide-copy-headers-p* t)
(defparameter *cocoa-ide-fasl-directory* "ccl:cocoa-ide;build;fasls;")

(require :cocoa-application)
