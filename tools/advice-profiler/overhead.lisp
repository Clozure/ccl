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

;;; Profiling overhead calculation

;;; This is in a separate file so that the profiler.lisp file can be
;;; compiled and loaded beforehand, making the macrology available.

(in-package "PROFILER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun stub-function (x)
    (declare (ignore x))
    nil)
  (proclaim '(notinline stub-function)))

(defconstant +overhead-iterations+ 10000
  "Number of iterations to make through the empty stub function to
   determine profiling overhead.")

(defvar *determining-overhead* nil
  "Set while determining overhead in order to prevent recursion")

(defun determine-overhead ()
  (format *trace-output* "; Calculating profiling overhead...")
  (force-output *trace-output*)
  (reset)
  (process-enable-profiling *current-process*)
  (unprofile stub-function)
  ;; Determine loop and function call overhead
  (with-real/cpu/cons (bare-real bare-cpu bare-cons)
      (dotimes (i +overhead-iterations+)
        (stub-function nil))
    (profile stub-function)
    (with-real/cpu/cons (alloc-real alloc-cpu alloc-cons)
        (stub-function nil)             ; call once in order to allocate call record structure
      ;; Determine profiling overhead
      (with-real/cpu/cons (profiled-real profiled-cpu profiled-cons)
          (dotimes (i +overhead-iterations+)
            (stub-function nil))
        (unprofile stub-function)
        (setf *real-overhead* (round (/ (- profiled-real bare-real) +overhead-iterations+))
              *cpu-overhead* (round (/ (- profiled-cpu bare-cpu) +overhead-iterations+))
              *cons-overhead* (round (/ (- profiled-cons bare-cons alloc-cons) +overhead-iterations+)))))
    (reset)
    (format *trace-output* "~&; per call overheads: cpu time ~A, real time ~A, cons ~A bytes~%"
            (format-time *cpu-overhead*) (format-time *real-overhead*) *cons-overhead*)))

(determine-overhead)
