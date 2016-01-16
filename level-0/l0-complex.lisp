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

(in-package "CCL")

(eval-when (:compile-toplevel)
  (require "NUMBER-MACROS"))

(defun coerce-to-complex-type (num type)
  (cond ((complexp num)
         (let ((real (%realpart num))
               (imag (%imagpart num)))
           (if (and (typep real type)
                    (typep imag type))
             num
             (complex (coerce real type)
                      (coerce imag type)))))
        (t (complex (coerce num type)))))

;;; end of l0-complex.lisp
