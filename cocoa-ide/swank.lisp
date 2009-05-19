(in-package :GUI)

(defparameter *ccl-gui-swank-port* 4564)

(load #P"ccl:cocoa-ide;slime;swank-loader.lisp")
(swank-loader::load-swank)

(provide :swank)