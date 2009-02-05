;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2001 Clozure Associates
;;;   This file is part of OpenMCL.  
;;;
;;;   Opensourced MCL is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Lesser General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2.1 of the License, or (at your option) any later version.
;;;
;;;   Opensourced MCL is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;   Lesser General Public License for more details.
;;;
;;;   You should have received a copy of the GNU Lesser General Public
;;;   License along with this library; if not, write to the Free Software
;;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :GTK))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; I don't know why it's necessary to explicitly open
  ;; libgdk.so (which transitively opens half a dozen
  ;; other libraries), while opening libgtk.so by itself
  ;; would complain about unresolved symbols from libgdk.
  (dolist (lib '("libgdk.so" "libgtk.so"))
    (open-shared-library lib)))


;;; All arguments (including the first, required one) should
;;; be strings.  This is supposed to be called from a C main
;;; function; it picks off gtk+-specific arguments from the
;;; caller's argv and deletes them from that C string vector.
;;; I don't know how to suppress any messages that this call
;;; might generate.
(defun gtk-init (arg &rest args)
  (declare (dynamic-extent args))
  (push arg args)
  (with-string-vector (argv args)
    (rlet ((argvp (* t))
           (argcp :signed))
     (setf (%get-ptr argvp) argv
           (%get-long argcp) (length args))
       (#_gtk_init argcp argvp))))

;;; Run this every 10 ticks.  (There seem to be about 100 ticks
;;; per second.)
#-openmcl-native-threads
(def-load-pointers gtk-task ()
  (%install-periodic-task 'gtk-task
			  #'(lambda ()
			      (do* ()
				   ((eql (#_gtk_events_pending) 0))
                              (#_gtk_main_iteration)))
                        10))

;;; Ensure that GTK's initialized whenever this file's loaded
;;; and whenever a saved image starts up.  (If an application
;;; needs to defer GTK initialization, *GTK-AUTO-INITIALIZE*
;;; can be set to nil to suppress this behavior.)

;;; Used in error reporting and to provide default window titles
(defvar *gtk-init-application-name* "OpenMCL")

(defvar *gtk-init-arguments* ())
(defvar *gtk-auto-initialize* t)

(def-load-pointers initialize-gtk ()
  (when *gtk-auto-initialize*
    (apply #'gtk-init *gtk-init-application-name* *gtk-init-arguments*)))

