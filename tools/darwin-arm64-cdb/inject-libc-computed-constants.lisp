;;;; Inject computed libc macros into libc constants.cdb.
;;;;
;;;; These macros expand through casts and sizeof (e.g. FIONBIO =
;;;; _IOW('f', 126, int), HOST_BASIC_INFO_COUNT =
;;;; sizeof(host_basic_info_data_t)/sizeof(integer_t)), which the
;;;; interface translator's macro evaluator cannot reduce, so the
;;;; regenerated CDBs omit them.  Their values are fixed Darwin ABI.
;;;;
;;;;   ./darm64cl --no-init --batch < tools/darwin-arm64-cdb/inject-libc-computed-constants.lisp
(in-package :ccl)

(defparameter *libc-computed-constants*
  `(;; sys/filio.h: _IOW('f', 126, int)
    ("FIONBIO" . #x8004667E)
    ;; mach/host_info.h: sizeof(host_basic_info_data_t)/sizeof(integer_t)
    ("HOST_BASIC_INFO_COUNT" . 12)
    ;; mach/machine.h: ((cpu_type_t) N) cast macros
    ("CPU_TYPE_X86" . 7)
    ("CPU_TYPE_POWERPC" . 18)
    ("CPU_SUBTYPE_X86_ALL" . 3)
    ("CPU_SUBTYPE_POWERPC_ALL" . 0)))

(defun %inject-libc-computed-constants (&optional (dirname "libc"))
  (use-interface-dir (intern (string-upcase dirname) :keyword))
  (let* ((d (require-interface-dir (intern (string-upcase dirname) :keyword)))
         (old (db-constants d))
         (dir (merge-pathnames (interface-dir-subdir d)
                               (ftd-interface-db-directory *target-ftd*)))
         (newpath (merge-pathnames "new-constants.cdb" dir))
         (pkg (find-package (ftd-interface-package-name *target-ftd*)))
         (n 0)
         (missing '()))
    (dolist (pair *libc-computed-constants*)
      (unless (db-lookup-constant old (intern (car pair) pkg))
        (push pair missing)))
    (setq missing (nreverse missing))
    (format t "~&;; libc constants missing ~d of ~d shim entries~%"
            (length missing) (length *libc-computed-constants*))
    (when (null missing)
      (format t "~&;; already present; nothing to do~%")
      (return-from %inject-libc-computed-constants nil))
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
                     (concatenate 'string (namestring (truename path)) "-pre-computed")
                     :if-exists :supersede))
      (rename-file newpath path))
    (format t "~&;; INJECT-LIBC-COMPUTED-OK copied=~d added=~d~%" n (length missing))
    t))

(%inject-libc-computed-constants)
(quit 0)
