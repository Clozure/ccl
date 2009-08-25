(in-package :cl-user)

(require "COCOA")

(let* ((containing-dir (make-pathname :directory (pathname-directory *load-truename*) :defaults nil)))
  (flet ((load-relative (path)
           (load (merge-pathnames path containing-dir))))
    (load-relative "opengl.lisp")
    (load-relative "vectors.lisp")
    (load-relative "lights.lisp")
    (load-relative "blocks.lisp")
    (load-relative "rubix.lisp")))


; (gui::execute-in-gui #'run-rubix-demo)
