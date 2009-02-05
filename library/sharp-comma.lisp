;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc.
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html


(in-package "CCL")

;;; #, was removed from CL in 1998 or so, but there may be some legacy
;;; code that still uses it.

(set-dispatch-macro-character
 #\#
 #\,
 #'(lambda (stream subchar numarg)
     (let* ((sharp-comma-token *reading-for-cfasl*))
       (if (or *read-suppress* (not *compiling-file*) (not sharp-comma-token))
         (read-eval stream subchar numarg)
         (progn
           (require-no-numarg subchar numarg)
           (list sharp-comma-token (read stream t nil t)))))))