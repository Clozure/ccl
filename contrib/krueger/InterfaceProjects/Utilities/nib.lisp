;; nib.lisp
;; Start with some corrected functions from .../ccl/examples/cocoa/nib-loading/HOWTO.html

(defpackage :interface-utilities
  (:nicknames :iu)
  (:export load-nibfile))

(in-package :iu)

;; Note that callers of this function are responsible for retaining top-level objects if
;; they're going to be around for a while and them releasing them when no longer needed.

(defun load-nibfile (nib-path &key (nib-owner #&NSApp) (retain-top-objs nil))
  (let* ((app-zone (#/zone nib-owner))
         (nib-name (ccl::%make-nsstring (namestring nib-path)))
         (objects-array (#/arrayWithCapacity: ns:ns-mutable-array 16))
         (toplevel-objects (list))
         (dict (#/dictionaryWithObjectsAndKeys: ns:ns-mutable-dictionary
                nib-owner #&NSNibOwner
                objects-array #&NSNibTopLevelObjects
                (%null-ptr)))
         (result (#/loadNibFile:externalNameTable:withZone: ns:ns-bundle
                                                            nib-name
                                                            dict
                                                            app-zone)))
    (when result
      (dotimes (i (#/count objects-array))
        (setf toplevel-objects 
              (cons (#/objectAtIndex: objects-array i)
                    toplevel-objects)))
      (when retain-top-objs
        (dolist (obj toplevel-objects)
          (#/retain obj))))
    (#/release nib-name)
    ;; Note that both dict and objects-array are temporary (i.e. autoreleased objects)
    ;; so don't need to be released by us
    (values toplevel-objects result)))

(provide :NIB)