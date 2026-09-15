;;;; Inject YES/NO + deprecated NS*KeyMask / NS*State aliases into cocoa
;;;; constants.cdb.
;;;;
;;;; Modern objc.h maps YES/NO to __objc_yes/__objc_no (not numeric), so
;;;; regenerated CDBs lack the historical YES=1 NO=0 entries objc-bridge
;;;; needs.  Deprecated NS*KeyMask and NSControlStateValue / NSOffState
;;;; names are recorded as unlinkable `(static)` foreign vars — inject
;;;; numeric enum-idents matching NSEventModifierFlag* / NSCell.h.
;;;;
;;;;   ./darm64cl --no-init --batch < tools/darwin-arm64-cdb/inject-objc-bool-constants.lisp
(in-package :ccl)

(defparameter *objc-bool-and-mask-constants*
  `(("YES" . 1)
    ("NO" . 0)
    ("NSAlphaShiftKeyMask" . 65536)
    ("NSShiftKeyMask" . 131072)
    ("NSControlKeyMask" . 262144)
    ("NSAlternateKeyMask" . 524288)
    ("NSCommandKeyMask" . 1048576)
    ("NSNumericPadKeyMask" . 2097152)
    ("NSHelpKeyMask" . 4194304)
    ("NSFunctionKeyMask" . 8388608)
    ("NSDeviceIndependentModifierFlagsMask" . 4294901760)
    ;; NSCell.h `static const` — ffigen emits (var … (static)), no value
    ("NSControlStateValueMixed" . -1)
    ("NSControlStateValueOff" . 0)
    ("NSControlStateValueOn" . 1)
    ("NSMixedState" . -1)
    ("NSOffState" . 0)
    ("NSOnState" . 1)
    ;; float.h macros — ffigen records as unlinkable statics / omits them
    ("FLT_MAX" . ,most-positive-single-float)
    ("FLT_MIN" . ,least-positive-normalized-single-float)
    ("DBL_MAX" . ,most-positive-double-float)))

(defun %inject-objc-bool-constants (&optional (dirname "cocoa"))
  (use-interface-dir (intern (string-upcase dirname) :keyword))
  (let* ((d (require-interface-dir (intern (string-upcase dirname) :keyword)))
         (old (db-constants d))
         (dir (merge-pathnames (interface-dir-subdir d)
                               (ftd-interface-db-directory *target-ftd*)))
         (newpath (merge-pathnames "new-constants.cdb" dir))
         (pkg (find-package (ftd-interface-package-name *target-ftd*)))
         (n 0)
         (missing '()))
    (dolist (pair *objc-bool-and-mask-constants*)
      (unless (db-lookup-constant old (intern (car pair) pkg))
        (push pair missing)))
    (setq missing (nreverse missing))
    (format t "~&;; cocoa constants missing ~d of ~d shim entries~%"
            (length missing) (length *objc-bool-and-mask-constants*))
    (when (null missing)
      (format t "~&;; already present; nothing to do~%")
      (return-from %inject-objc-bool-constants nil))
    (format t "~&;; rewriting constants.cdb; adding ~s ...~%"
            (mapcar #'car missing))
    (with-new-db-file (cdbm newpath)
      (dolist (k (cdb-enumerate-keys old))
        (let* ((sym (intern k pkg))
               (v (db-lookup-constant old sym)))
          (when v
            (db-define-constant cdbm k v)
            (incf n))))
      (dolist (pair missing)
        (db-define-constant cdbm (car pair) (cdr pair))))
    (cdb-close old)
    (setf (interface-dir-constants-interface-db-file d) nil)
    (let* ((path (merge-pathnames "constants.cdb" dir)))
      (when (probe-file path)
        (rename-file path
                     (concatenate 'string (namestring (truename path)) "-pre-yesno")
                     :if-exists :supersede))
      (rename-file newpath path))
    (format t "~&;; INJECT-OBJC-BOOL-OK copied=~d added=~d~%" n (length missing))
    t))

(%inject-objc-bool-constants)
(quit 0)
