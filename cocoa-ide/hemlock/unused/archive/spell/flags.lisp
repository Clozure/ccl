(in-package "SPELL")

;;; FIXME: show where these things are documented
(defconstant +V-mask+ (ash 1 13))
(defconstant +N-mask+ (ash 1 12))
(defconstant +X-mask+ (ash 1 11))
(defconstant +H-mask+ (ash 1 10))
(defconstant +Y-mask+ (ash 1 9))
(defconstant +G-mask+ (ash 1 8))
(defconstant +J-mask+ (ash 1 7))
(defconstant +D-mask+ (ash 1 6))
(defconstant +T-mask+ (ash 1 5))
(defconstant +R-mask+ (ash 1 4))
(defconstant +Z-mask+ (ash 1 3))
(defconstant +S-mask+ (ash 1 2))
(defconstant +P-mask+ (ash 1 1))
(defconstant +M-mask+ 1)

(defconstant flag-names-to-masks
  `((#\V . ,+V-mask+) (#\N . ,+N-mask+) (#\X . ,+X-mask+)
    (#\H . ,+H-mask+) (#\Y . ,+Y-mask+) (#\G . ,+G-mask+)
    (#\J . ,+J-mask+) (#\D . ,+D-mask+) (#\T . ,+T-mask+)
    (#\R . ,+R-mask+) (#\Z . ,+Z-mask+) (#\S . ,+S-mask+)
    (#\P . ,+P-mask+) (#\M . ,+M-mask+)))

(defvar *flag-masks*
  (make-array 128 :element-type '(unsigned-byte 16) :initial-element 0)
  "This holds the masks for character flags, which is used when reading
   a text file of dictionary words.  Illegal character flags hold zero.")

(declaim (inline flag-mask))
(defun flag-mask (char)
  (aref *flag-masks* (char-code char)))
(defun %set-flag-mask (char value)
  (setf (aref *flag-masks* (char-code char)) value))

(defsetf flag-mask %set-flag-mask)

(dolist (e flag-names-to-masks)
  (let ((char (car e))
	(mask (cdr e)))
    (setf (flag-mask char) mask)
    (setf (flag-mask (char-downcase char)) mask)))