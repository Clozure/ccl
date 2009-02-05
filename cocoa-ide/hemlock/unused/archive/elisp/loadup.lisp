;; Files to load
(load "packages")
(load "read-table")
(load "base")
(load "codewalker")
(load "internals")
(load "hemlock-shims")

;; Functions to call
(let ((*package* (find-package :elisp)))
  (elisp-internals:generate-cl-package))
