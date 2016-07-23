;;; -*- Mode: Lisp; Package: CCL; -*-
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



(in-package "CCL")

;;; not very smart yet

(defppclapfunction %get-errno ()
  (ldr imm1 target::tcr.errno-loc target::rcontext)
  (lwz imm0 0 imm1)
  (stw rzero 0 imm1)
  (neg imm0 imm0)
  (box-fixnum arg_z imm0)
  (blr))

; end
