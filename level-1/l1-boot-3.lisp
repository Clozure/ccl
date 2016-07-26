;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
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

;;; l1-boot-3.lisp
;;; Third part of l1-boot

(in-package "CCL")

;;; Register Emacs-friendly aliases for some character encodings.
;;; This could go on forever; try to recognize at least some common
;;; cases.  (The precise set of encoding/coding-system names supported
;;; by Emacs likely depends on Emacs version, loaded Emacs packages, etc.)

(dotimes (i 16)
  (let* ((key (find-symbol (format nil "LATIN~d" i) :keyword))
         (existing (and key (lookup-character-encoding key))))
    (when existing
      (define-character-encoding-alias (intern (format nil "LATIN-~d" i) :keyword) existing)
      (define-character-encoding-alias (intern (format nil "ISO-LATIN-~d" i) :keyword) existing))))

(define-character-encoding-alias :mule-utf-8 :utf-8)

(set-pathname-encoding-name :utf-8)

(catch :toplevel
    (or (find-package "COMMON-LISP-USER")
        (make-package "COMMON-LISP-USER" :use '("COMMON-LISP" "CCL") :NICKNAMES '("CL-USER")))
)

(set-periodic-task-interval .33)
(setq cmain xcmain)
(setq %err-disp %xerr-disp)

;;;end of l1-boot-3.lisp

