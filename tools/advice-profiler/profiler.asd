;;; -*- Lisp -*-
;;;
;;; Copyright 2008 Hans Huebner
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
