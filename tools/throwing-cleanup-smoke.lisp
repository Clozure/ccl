(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)

;; Load tip %throwing-through-cleanup-p (arm64 defun only under feature).
(let* ((src (merge-pathnames "level-0/l0-def.lisp" (ccl-directory)))
       (form nil))
  (with-open-file (s src)
    (loop for f = (read s nil s)
          until (eq f s)
          when (and (consp f) (eq (car f) 'defun)
                    (eq (cadr f) '%throwing-through-cleanup-p))
          do (setq form f)))
  (unless form (error "no %throwing-through-cleanup-p"))
  (eval form)
  (format t "~&reloaded %throwing-through-cleanup-p~%"))

(defun check (label expected-throw &key tag values)
  (let ((ctx (%throwing-through-cleanup-p)))
    (format t "~&~a => ~s~%" label ctx)
    (let ((throwing (not (null ctx))))
      (unless (eq throwing expected-throw)
        (error "~a: throwing=~s expected ~s" label throwing expected-throw)))
    (when expected-throw
      (unless (consp ctx)
        (error "~a: expected cons ctx, got ~s" label ctx))
      (when tag
        (unless (eq (car ctx) tag)
          (error "~a: bad tag in ~s (want ~s)" label ctx tag)))
      (when values
        (unless (equal (cdr ctx) values)
          (error "~a: bad values in ~s (want ~s)" label ctx values))))
    ctx))

(catch 'tg
  (unwind-protect (throw 'tg 99)
    (check "throw-sv" t :tag 'tg :values '(99))))

(catch 'tg
  (unwind-protect (throw 'tg (values 1 2 3))
    (check "throw-mv" t :tag 'tg :values '(1 2 3))))

(catch 'tg
  (unwind-protect 'ok
    (check "normal" nil)))

;; return-from uses nthrow1value (same tsp shape as normal uwp exit);
;; match x86: not reported as throwing-through-cleanup.
(block b
  (unwind-protect (return-from b 'RB)
    (check "return-from" nil)))

(catch 'a
  (catch 'b
    (unwind-protect (throw 'a 7)
      (check "throw-skip" t :tag 'a :values '(7)))))

(format t "~&THROWING-CLEANUP-SMOKE-OK~%")
(quit 0)
