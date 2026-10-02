;;;; Surgical reload of tip open-issue fixes, then objc smoke.
;;;; For a purified tip image use tools/darwin-clean-build-smoke.lisp instead.
;;;; Poll — do not block forever.
(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)

;; %throw LAP
(let* ((src (merge-pathnames "level-0/ARM64/arm64-def.lisp" (ccl-directory)))
       (form nil))
  (with-open-file (s src)
    (loop for f = (read s nil s)
          until (eq f s)
          when (and (consp f) (eq (car f) 'defarm64lapfunction)
                    (eq (cadr f) '%throw))
          do (setq form f)))
  (unless form (error "%throw form not found"))
  (eval form)
  (format t "~&%throw ok~%"))

;; %throwing-through-cleanup-p
(let* ((src (merge-pathnames "level-0/l0-def.lisp" (ccl-directory)))
       (form nil))
  (with-open-file (s src)
    (loop for f = (read s nil s)
          until (eq f s)
          when (and (consp f) (eq (car f) 'defun)
                    (eq (cadr f) '%throwing-through-cleanup-p))
          do (setq form f)))
  ;; Prefer the arm64 definition: last matching defun wins if both
  ;; appear under reader conditionals — READ already filtered.
  (when form (eval form) (format t "~&%throwing ok~%")))

(format t "~&fbound %throw=~s throwing=~s~%"
        (fboundp '%throw) (fboundp '%throwing-through-cleanup-p))

;; Reload objc-runtime varargs + objc-support trampoline bits via require path
(require "OBJC-SUPPORT")

;; Force Protocol mapping if image loaded support before the ensure.
(%ensure-class-declaration "Protocol" "NSObject")
(reset-objc-class-count)
(maybe-map-objc-classes t)

;; Reload varargs compilers from tip source
(let* ((src (merge-pathnames "objc-bridge/objc-runtime.lisp" (ccl-directory)))
       (wanted '(%process-varargs-list
                 %compile-varargs-send-function-for-signature))
       (forms ()))
  (with-open-file (s src)
    (loop for f = (read s nil s)
          until (eq f s)
          when (and (consp f) (eq (car f) 'defun) (member (cadr f) wanted))
          do (push f forms)))
  (dolist (f (nreverse forms)) (eval f))
  (format t "~&varargs reload (~d)~%" (length forms)))

;; Clear any cached compiled send for stringWithFormat:
(when (boundp '*objc-message-info*)
  (maphash (lambda (k v)
             (declare (ignore v))
             (when (and (stringp k) (search "stringWithFormat" k))
               (remhash k *objc-message-info*)))
           *objc-message-info*))

(let* ((s (%make-nsstring "abcdef"))
       (r (ns:make-ns-range 1 3))
       (sub (#/substringWithRange: s r))
       (cstr (%get-cstring (#/UTF8String sub))))
  (format t "~&substring=~s~%" cstr)
  (unless (equal cstr "bcd") (error "substring => ~s" cstr)))

(let* ((fmt (%make-nsstring "%d-%@"))
       (arg (%make-nsstring "ok"))
       (formatted (#/stringWithFormat: ns:ns-string fmt 1 arg))
       (cstr (%get-cstring (#/UTF8String formatted))))
  (format t "~&format=~s~%" cstr)
  (unless (equal cstr "1-ok") (error "format => ~s" cstr)))

(unless (find-class 'ns:protocol nil)
  (error "ns:protocol missing"))
(format t "~&protocol=~s~%" (find-class 'ns:protocol nil))

;; %throw + %throwing-through-cleanup-p
(let ((got (catch 'tg (apply #'%throw 'tg '(77)))))
  (format t "~&%throw => ~s~%" got)
  (unless (eql got 77) (error "%throw => ~s" got)))

(defun %open-issues-uwp-probe (expected-throwing)
  (let* ((ctx (%throwing-through-cleanup-p))
         (throwing (not (null ctx))))
    (format t "~&uwp throwing=~s ctx=~s (expect ~s)~%" throwing ctx expected-throwing)
    (unless (eq throwing expected-throwing)
      (error "throwing-through: got ~s expected ~s ctx=~s"
             throwing expected-throwing ctx))
    (when throwing
      (unless (and (consp ctx) (eq (car ctx) 'tg))
        (error "bad throw ctx ~s" ctx)))
    ctx))

(catch 'tg
  (unwind-protect (throw 'tg (values 1 2))
    (%open-issues-uwp-probe t)))

(catch 'tg
  (unwind-protect 'normal
    (%open-issues-uwp-probe nil)))

(block b
  (unwind-protect (return-from b 42)
    (let ((ctx (%throwing-through-cleanup-p)))
      (format t "~&return-from ctx=~s~%" ctx)
      (when ctx
        (error "return-from ctx ~s (expected nil)" ctx)))))

(format t "~&DARWIN-OPEN-ISSUES-SMOKE-OK~%")
(quit 0)
