;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 2001-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :GTK2))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (open-shared-library "libgnomeui-2.so"))


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
(defvar *gtk-init-application-name* "Clozure CL")

(defvar *gtk-init-arguments* ())
(defvar *gtk-auto-initialize* t)

(def-load-pointers initialize-gtk ()
  (when *gtk-auto-initialize*
    (apply #'gtk-init *gtk-init-application-name* *gtk-init-arguments*)))

