(in-package :GUI)

(defparameter *default-gui-swank-port* 4564)
(defparameter *ccl-swank-active-p* nil)

(load #P"ccl:cocoa-ide;slime;swank-loader.lisp")
(swank-loader::load-swank)

(provide :swank)