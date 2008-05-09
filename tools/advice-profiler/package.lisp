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

(defpackage "PROFILER"
  (:nicknames "PROF")
  (:use "COMMON-LISP" "CCL")
  (:export "PROFILE" "UNPROFILE"
           "UNPROFILE-ALL"
           "PROFILE-PACKAGE" "UNPROFILE-PACKAGE"
           "ENABLE-PROFILING" "DISABLE-PROFILING"
           "PROCESS-ENABLE-PROFILING" "PROCESS-DISABLE-PROFILING"
	   "RESET"
	   "REPORT"))