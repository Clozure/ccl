;;; -*- Mode: Lisp; Package: CCL; -*-
;;;
;;; Copyright 2010 Clozure Associates
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

(defarmlapfunction %get-errno ()
  (mov temp0 (:$ 0))
  (ldr imm1 (:@ rcontext (:$ arm::tcr.errno-loc)))
  (ldr imm0 (:@ imm1 (:$ 0)))
  (str temp0 (:@ imm1 (:$ 0)))
  (rsb imm0 imm0 (:$ 0))
  (box-fixnum arg_z imm0)
  (bx lr))

; end
