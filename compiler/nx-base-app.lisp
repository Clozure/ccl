;;;-*-Mode: LISP; Package: CCL -*-
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


; Loaded instead of compiler for standalone applications.

(in-package "CCL")

;(require 'numbers)
(require 'sort)
(require 'hash)

; this file is now equiv to nx-basic
(%include "ccl:compiler;nx-basic.lisp")  ; get cons-var, augment-environment
; nx-basic includes lambda-list

; End of nx-base-app.lisp
