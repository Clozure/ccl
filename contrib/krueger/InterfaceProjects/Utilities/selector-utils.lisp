;; selector-utils.lisp

(defpackage :interface-utilities
  (:nicknames :iu)
  (:export get-selector find-selector-match))

(in-package :iu)

(defun get-selector (name)
  ;; we can't use the @selector macro for some uses because it requires a literal name
  ;; argument and sometimes we want to take a name passed in as a parameter and cause it
  ;; to be registered as an objective C selector name. We could do this directly with
  ;; a call to the C function #_sel_get_uid and let lisp figure it out for itself later
  ;; but we'll play nice with the objective C bridge instead which hashes these things.
  (ccl::%get-selector (ccl::ensure-objc-selector name)))

(defun find-selector-match (sel)
  (maphash #'(lambda (key val)
               (when (eql (ccl::objc-selector-%sel val) sel) 
                 (return-from find-selector-match key)))
           ccl::*objc-selectors*))

(provide :selector-utils)