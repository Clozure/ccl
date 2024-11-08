(in-package "CCL")

(defconstant $numarm64saveregs 4)
(defconstant $numarm64argregs 3)

;;; I'm not sure we reference the following information from anywhere...

(defconstant arm64-nonvolatile-registers-mask
  (logior (ash 1 arm64::save0)
          (ash 1 arm64::save1)
          (ash 1 arm64::save2)
          (ash 1 arm64::save3)))

(defconstant arm64-arg-registers-mask
  (logior (ash 1 arm64::arg_x)
          (ash 1 arm64::arg_y)
          (ash 1 arm64::arg_z)))

(defconstant arm64-temp-registers-mask
  (logior (ash 1 arm64::temp0)
          (ash 1 arm64::temp1)
          (ash 1 arm64::temp2)
          (ash 1 arm64::temp3)))

(defconstant arm64-tagged-registers-mask
  (logior arm64-temp-registers-mask
          arm64-arg-registers-mask
          arm64-nonvolatile-registers-mask))

(provide "ARM64ENV")



