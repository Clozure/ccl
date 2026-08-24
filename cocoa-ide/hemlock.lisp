;;;-*- Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 2016 Clozure Associates
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

(require "COMPILE-HEMLOCK")

;;; Historically this always passed T (force-recompile every IDE load).
;;; On Darwin/arm64 that recompiles ~50 Hemlock files under W^X on every
;;; (require "COCOA"), thrashing the heap (BOGUS semaphore macptrs, corrupt
;;; uvector headers, CLASS-CELL-TYPEP FAR faults) before the event loop is
;;; usable.  Only force when *cocoa-ide-force-compile* is true; otherwise
;;; load existing fasls (recompile when sources are newer).
(let* ((force (and (boundp '*cocoa-ide-force-compile*)
                   *cocoa-ide-force-compile*)))
  (format t "~&;;; ~:[Loading~;Compiling~] Hemlock ...~%" force)
  (compile-hemlock force))
