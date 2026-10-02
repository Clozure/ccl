;;;; Inject objc-bridge shim types into cocoa types.cdb / records.cdb.
;;;;
;;;; Covers: complete `id` struct (for struct-ref), instancetype, ObjC
;;;; generics (ObjectType/…), va_list, __uint128_t, NSConstantString.
;;;; Bare `:id` uses the typedef via `%foreign-type-or-record` (not the
;;;; struct).  Full cocoa reparse also picks these up via cocoa-populate.sh
;;;; installing zzz-objc-bridge-types.ffi.
;;;;
;;;;   ./darm64cl --no-init --batch \
;;;;     < tools/darwin-arm64-cdb/inject-objc-bridge-types.lisp
(in-package :ccl)

(setq *warn-if-redefine-kernel* nil)
(load "library/parse-ffi.lisp")
(use-interface-dir :cocoa)

(let* ((*parse-ffi-target-ftd* *target-ftd*)
       (*ffi-global-typedefs* (make-hash-table :test 'string= :hash-function 'sxhash))
       (*ffi-global-structs* (make-hash-table :test 'string= :hash-function 'sxhash))
       (*ffi-global-unions* (make-hash-table :test 'string= :hash-function 'sxhash))
       (*ffi-global-transparent-unions* (make-hash-table :test 'string= :hash-function 'sxhash))
       (*ffi-global-objc-classes* (make-hash-table :test 'string= :hash-function 'sxhash))
       (*ffi-global-constants* (make-hash-table :test 'string= :hash-function 'sxhash))
       (*ffi-global-vars* (make-hash-table :test 'string= :hash-function 'sxhash))
       (*ffi-global-functions* (make-hash-table :test 'string= :hash-function 'sxhash))
       (*ffi-global-objc-messages* (make-hash-table :test 'string= :hash-function 'sxhash)))
  (parse-ffi "tools/darwin-arm64-cdb/objc-bridge-types.ffi")
  (format t "~&;; typedefs=~s structs=~s~%"
          (let (a) (maphash (lambda (k v) (declare (ignore v)) (push k a))
                            *ffi-global-typedefs*) a)
          (let (a) (maphash (lambda (k v) (declare (ignore v)) (push k a))
                            *ffi-global-structs*) a))
  (let* ((d (require-interface-dir :cocoa))
         (dir (merge-pathnames (interface-dir-subdir d)
                               (ftd-interface-db-directory *target-ftd*)))
         (old-t (db-types d))
         (old-r (db-records d))
         (new-t (merge-pathnames "new-types.cdb" dir))
         (new-r (merge-pathnames "new-records.cdb" dir)))
    (with-new-db-file (cdbm new-t)
      (dolist (k (cdb-enumerate-keys old-t))
        (unless (gethash k *ffi-global-typedefs*)
          (rletZ ((value :cdb-datum) (key :cdb-datum))
            (with-cstrs ((keyname k))
              (setf (pref key :cdb-datum.data) keyname
                    (pref key :cdb-datum.size) (length k)
                    (pref value :cdb-datum.data) (%null-ptr)
                    (pref value :cdb-datum.size) 0)
              (cdb-get old-t key value)
              (unless (%null-ptr-p (pref value :cdb-datum.data))
                (cdbm-put cdbm key value)
                (cdb-free (pref value :cdb-datum.data)))))))
      (maphash (lambda (name def)
                 (declare (ignore name))
                 (save-ffi-typedef cdbm def))
               *ffi-global-typedefs*))
    (with-new-db-file (cdbm new-r)
      (dolist (k (cdb-enumerate-keys old-r))
        (unless (gethash k *ffi-global-structs*)
          (rletZ ((value :cdb-datum) (key :cdb-datum))
            (with-cstrs ((keyname k))
              (setf (pref key :cdb-datum.data) keyname
                    (pref key :cdb-datum.size) (length k)
                    (pref value :cdb-datum.data) (%null-ptr)
                    (pref value :cdb-datum.size) 0)
              (cdb-get old-r key value)
              (unless (%null-ptr-p (pref value :cdb-datum.data))
                (cdbm-put cdbm key value)
                (cdb-free (pref value :cdb-datum.data)))))))
      (maphash (lambda (name def)
                 (declare (ignore name))
                 (save-ffi-struct cdbm def))
               *ffi-global-structs*))
    (cdb-close old-t)
    (cdb-close old-r)
    (setf (interface-dir-types-interface-db-file d) nil
          (interface-dir-records-interface-db-file d) nil)
    (flet ((install (base new)
             (let ((path (merge-pathnames base dir)))
               (when (probe-file path)
                 (rename-file path
                              (concatenate 'string (namestring (truename path))
                                           "-pre-bridge-types")
                              :if-exists :supersede))
               (rename-file new path))))
      (install "types.cdb" new-t)
      (install "records.cdb" new-r)))
  (format t "~&;; INJECT-BRIDGE-TYPES-OK~%"))
(quit 0)
