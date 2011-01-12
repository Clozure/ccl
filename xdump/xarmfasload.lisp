;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Portions copyright (C) 2010 Clozure Associates
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "FASLENV" "ccl:xdump;faslenv")
  (require "ARM-LAP"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "XFASLOAD" "ccl:xdump;xfasload"))


(defun xload-arm-lap-word (instruction-form)
  (if (listp instruction-form)
    (uvref (uvref (compile nil
                           `(lambda (&lap 0)
                             (arm-lap-function () ((?? 0))
                              ,instruction-form)))
                  1)
           0)
    instruction-form))

(defparameter *arm-macro-apply-code*
  (let* ((code-vector (uvref (compile nil
          '(lambda (&lap 0)
            (arm-lap-function () ()
             (build-lisp-frame imm0)
             (bla .SPheap-rest-arg)
             (vpop1 arg_z)
             (mov arg_y fname)
             (mov arg_x '#.$xnotfun)
             (set-nargs 3)
             (ba .SPksignalerr))))
                             1))
         (n (uvsize code-vector))
         (u32-vector (make-array n
                                 :element-type '(unsigned-byte 32))))
    (declare (fixnum n))
    (dotimes (i n u32-vector)
      (setf (uvref u32-vector i)
            (uvref code-vector i)))))


(defun arm-fixup-macro-apply-code ()
  *arm-macro-apply-code*)


(defparameter *arm-closure-trampoline-code*
  (let* ((code0 (xload-arm-lap-word '(ldr pc (:@ pc (:$ 4))))))
    (make-array 4
                :element-type '(unsigned-byte 32)
                :initial-contents
                (list code0 0 3 (arm::arm-subprimitive-address '.SPcall-closure)))))

(defun adjust-closure-trampoline-for-subprims-bias (backend-name)
  (let* ((backend (find-backend backend-name))
         (bias (if backend (backend-real-subprims-bias backend) 0))
         (code *arm-closure-trampoline-code*))
    (if (eql bias 0)
      code
      (let* ((new (copy-seq code)))
        (incf (aref new (1- (length new))) bias)
        new))))

;;; For now, do this with a UUO so that the kernel can catch it.
(defparameter *arm-udf-code*
  (let* ((code '((uuo-error-udf-call (:? al) fname))))
    (make-array (length code)
                :element-type '(unsigned-byte 32)
                :initial-contents
                (mapcar #'xload-arm-lap-word code))))


(defun arm-initialize-static-space ()
  (xload-make-word-ivector arm::subtag-u32-vector 1021 *xload-static-space*)
  ;; Make NIL.  Note that NIL is sort of a misaligned cons (it
  ;; straddles two doublewords.)
  (xload-make-cons *xload-target-nil* 0 *xload-static-space*)
  (xload-make-cons 0 *xload-target-nil* *xload-static-space*))



(defparameter *linuxarm-xload-backend*
  (make-backend-xload-info
   :name :linuxarm
   :macro-apply-code-function 'arm-fixup-macro-apply-code
   :closure-trampoline-code *arm-closure-trampoline-code*
   :udf-code *arm-udf-code*
   :default-image-name "ccl:ccl;arm-boot"
   :default-startup-file-name "level-1.lafsl"
   :subdirs '("ccl:level-0;ARM;")
   :compiler-target-name :linuxarm
   :image-base-address #x50000000
   :nil-relative-symbols arm::*arm-nil-relative-symbols*
   :static-space-init-function 'arm-initialize-static-space
   :purespace-reserve (ash 64 20)
   :static-space-address (- (- arm::nil-value arm::fulltag-nil) (ash 1 12))
))

(add-xload-backend *linuxarm-xload-backend*)

(defparameter *darwinarm-xload-backend*
  (make-backend-xload-info
   :name :darwinarm
   :macro-apply-code-function 'arm-fixup-macro-apply-code
   :closure-trampoline-code *arm-closure-trampoline-code*
   :udf-code *arm-udf-code*
   :default-image-name "ccl:ccl;arm-boot.image"
   :default-startup-file-name "level-1.dafsl"
   :subdirs '("ccl:level-0;ARM;")
   :compiler-target-name :darwinarm
   :image-base-address (+ (- arm::nil-value arm::fulltag-nil) (ash 1 12))
   :nil-relative-symbols arm::*arm-nil-relative-symbols*
   :static-space-init-function 'arm-initialize-static-space
   :purespace-reserve (ash 64 20)
   :static-space-address (- (- arm::nil-value arm::fulltag-nil) (ash 1 12))
))

(add-xload-backend *darwinarm-xload-backend*)

(defparameter *androidarm-xload-backend*
  (make-backend-xload-info
   :name :androidarm
   :macro-apply-code-function 'arm-fixup-macro-apply-code
   :closure-trampoline-code (adjust-closure-trampoline-for-subprims-bias :androidarm)
   :udf-code *arm-udf-code*
   :default-image-name "ccl:aarm-boot"
   :default-startup-file-name "level-1.aafsl"
   :subdirs '("ccl:level-0;ARM;")
   :compiler-target-name :androidarm
   :image-base-address #x50000000
   :nil-relative-symbols arm::*arm-nil-relative-symbols*
   :static-space-init-function 'arm-initialize-static-space
   :purespace-reserve (ash 64 20)
   :static-space-address (- (- arm::nil-value arm::fulltag-nil) (ash 1 12))
))

(add-xload-backend *androidarm-xload-backend*)


#+linuxarm-target
(progn
(setq *xload-default-backend* *linuxarm-xload-backend*)
)






