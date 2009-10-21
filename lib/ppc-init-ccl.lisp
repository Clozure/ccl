;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
a;;;   Opensourced MCL is distributed in the hope that it will be useful,
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

;(setq *save-local-symbols* t)

#+ppc-target
(progn
(breaker)
(format t "~&Initializing Macintosh Common Lisp ...")
(setq *load-verbose* t)
(setq *warn-if-redefine* nil)
(setq *.fasl-pathname* (pathname ".pfsl")) ; leave it?
(setq *.pfsl-pathname* (pathname ".pfsl"))
(setq *fasl-target* :ppc)
(setq *save-exit-functions* nil)

(require 'compile-ccl)
(ppc-load-ccl)

(setq *warn-if-redefine* t)
(setq *load-verbose* nil)
(format t "~&Macintosh Common Lisp Loaded")

(defun save-mcl-libraries (&optional (suffix ""))
  (save-library (concatenate 'string "ccl:ccl;pmcl-compiler" suffix)
                "pmcl-compiler" *nx-start* *nx-end*)
  ; More here ?
  ; Pick up the leftovers ...
  (save-library (concatenate 'string "ccl:ccl;pmcl-library" suffix)
                "pmcl-library" nil nil))

(defun save-it (&optional (suffix ""))
  (save-mcl-libraries (and suffix (concatenate 'string "-" suffix)))
  (let ((prefix "ccl:ccl;PPCCL"))
    (save-application (if suffix
                        (concatenate 'string prefix " " suffix)
                        prefix))))

;(save-application "ccl;CCL")
)
; End of init-ccl.lisp
