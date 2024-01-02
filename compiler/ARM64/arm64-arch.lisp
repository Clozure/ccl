(defpackage "ARM64"
  (:use "CL")
  #+arm64-target
  (:nicknames "TARGET"))

(require "ARCH")

(in-package "ARM64")



(provide "ARM64-ARCH")
