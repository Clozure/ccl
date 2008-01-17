;;; File to fix Irritating Impedance Mismatch between
;;; CMU CL Hemlock and PortableHemlock.

#+cmu
(unless (find-package :hemlock-ext)
  #-hemlock
  (progn
    (load "/usr/share/common-lisp/systems/cmucl-hemlock.system")
    (mk:oos :cmucl-hemlock :load))

  ;; OK, here comes the nasty. CMUCLHemlock stuffs things in the "EXT"
  ;; package (system-dependent stuff, basically). We expect things to be
  ;; orderly and live in a Hemlock package. Thus:
  (common-lisp::enter-new-nicknames (find-package "EXTENSIONS") '("HEMLOCK-EXT")))

