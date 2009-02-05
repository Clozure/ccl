(in-package "HI")

(defun %sp-byte-blt (src start dest dstart end)
  (%primitive byte-blt src start dest dstart end))

(defun lisp::sap-to-fixnum (x) (sap-int x))
(defun lisp::fixnum-to-sap (x) (int-sap x))
(defun lisp::%sp-make-fixnum (x) (%primitive make-fixnum x))
(defun lisp::fast-char-upcase (x) (char-upcase x))

;;; prepare-window-for-redisplay  --  Internal
;;;
;;;    Called by make-window to do whatever redisplay wants to set up
;;; a new window.
;;;
(defun prepare-window-for-redisplay (window)
  (setf (window-old-lines window) 0))

(defparameter hunk-width-limit 256)

(defun reverse-video-hook-fun (&rest foo)
  (declare (ignore foo)))
