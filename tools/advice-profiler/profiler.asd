;;; -*- Lisp -*-

;;;   Copyright (c) 2008, Hans Huebner.
;;;   This file is part of Clozure CL.  

;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CL-USER")

(defpackage "PROFILER-SYSTEM"
  (:use "CL" "ASDF"))

(in-package "PROFILER-SYSTEM")

(defsystem :profiler
  :name "Clozure CL deterministic multithread-profiler"
  :author "Hans Huebner <hans@clozure.com>"
  :components ((:file "package")
               (:file "profiler" :depends-on ("package"))
               (:file "overhead" :depends-on ("profiler"))))