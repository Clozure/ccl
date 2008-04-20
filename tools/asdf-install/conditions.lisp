(in-package #:asdf-install)

(define-condition download-error (error)
  ((url :initarg :url :reader download-url)
   (response :initarg :response :reader download-response))
  (:report (lambda (c s)
	     (format s "Server responded ~A for GET ~A"
		     (download-response c) (download-url c)))))

(define-condition signature-error (error)
  ((cause :initarg :cause :reader signature-error-cause))
  (:report (lambda (c s)
	     (format s "Cannot verify package signature:  ~A"
		     (signature-error-cause c)))))

(define-condition gpg-error (error)
  ((message :initarg :message :reader gpg-error-message))
  (:report (lambda (c s)
	     (format s "GPG failed with error status:~%~S"
		     (gpg-error-message c)))))

(define-condition gpg-shell-error (gpg-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Call to GPG failed. Perhaps GPG is not installed or not ~
in the path."))))

(define-condition no-signature (gpg-error) ())

(define-condition key-not-found (gpg-error)
  ((key-id :initarg :key-id :reader key-id))
  (:report (lambda (c s)
	     (let* ((*print-circle* nil)
		    (key-id (key-id c))
		    (key-id (if (and (consp key-id) 
				     (> (length key-id) 1))
				(car key-id) key-id)))
	       (format s "~&No key found for key id 0x~A.~%" key-id)
	       (format s "~&Try some command like ~%  gpg  --recv-keys 0x~A"
		       (format nil "~a" key-id))))))

(define-condition key-not-trusted (gpg-error)
  ((key-id :initarg :key-id :reader key-id)
   (key-user-name :initarg :key-user-name :reader key-user-name))
  (:report (lambda (c s)
	     (format s "GPG warns that the key id 0x~A (~A) is not fully trusted"
		     (key-id c) (key-user-name c)))))

(define-condition author-not-trusted (gpg-error)
  ((key-id :initarg :key-id :reader key-id)
   (key-user-name :initarg :key-user-name :reader key-user-name))
  (:report (lambda (c s)
	     (format s "~A (key id ~A) is not on your package supplier list"
		     (key-user-name c) (key-id c)))))
  
(define-condition installation-abort (condition)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (installer-msg s "Installation aborted."))))

(defun report-valid-preferred-locations (stream &optional attempted-location)
  (when attempted-location
    (installer-msg stream "~s is not a valid value for *preferred-location*"
		   attempted-location))
  (installer-msg stream "*preferred-location* may either be nil, a number between 1 and ~d \(the length of *locations*\) or the name of one of the *locations* \(~{~s~^, ~}\). If using a name, then it can be a symbol tested with #'eq or a string tested with #'string-equal."
		 (length *locations*)
		 (mapcar #'third *locations*)))

(define-condition invalid-preferred-location-error (error)
  ((preferred-location :initarg :preferred-location))
  (:report (lambda (c s)
	     (report-valid-preferred-locations 
	      s (slot-value c 'preferred-location)))))

(define-condition invalid-preferred-location-number-error 
    (invalid-preferred-location-error) ())

(define-condition invalid-preferred-location-name-error 
    (invalid-preferred-location-error) ())

