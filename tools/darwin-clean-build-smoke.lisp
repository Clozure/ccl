;;;; Post-rebuild smoke: tip must already be baked into darm64cl.image.
;;;; Fails if level-0 / objc tip is stale.
;;;;
;;;;   ./darm64cl --no-init --batch < tools/darwin-clean-build-smoke.lisp
;;;;   ./tools/run-darwin-smoke.sh 120 tools/darwin-clean-build-smoke.lisp
(in-package :ccl)

(unless (and (fboundp '%throw) (fboundp '%throwing-through-cleanup-p))
  (error "stale image: %throw=~s %throwing-through-cleanup-p=~s (rebuild required)"
         (fboundp '%throw) (fboundp '%throwing-through-cleanup-p)))
(format t "~&baked %throw / %throwing-through-cleanup-p~%")

(require "OBJC-SUPPORT")
(format t "~&OBJC-SUPPORT ok~%")

(unless (find-class 'ns:protocol nil)
  (error "ns:protocol missing after require"))
(format t "~&protocol=~s~%" (find-class 'ns:protocol nil))

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

(let ((got (catch 'tg (apply #'%throw 'tg '(77)))))
  (unless (eql got 77) (error "%throw => ~s" got))
  (format t "~&%throw => ~s~%" got))

(catch 'tg
  (unwind-protect (throw 'tg (values 1 2))
    (let ((ctx (%throwing-through-cleanup-p)))
      (unless (and (consp ctx) (eq (car ctx) 'tg) (equal (cdr ctx) '(1 2)))
        (error "throw uwp ctx ~s" ctx))
      (format t "~&throw-uwp=~s~%" ctx))))

(catch 'tg
  (unwind-protect 'normal
    (when (%throwing-through-cleanup-p)
      (error "normal uwp expected nil, got ~s" (%throwing-through-cleanup-p)))
    (format t "~&normal-uwp=NIL~%")))

;; return-from shares nthrow1value with normal uwp exit — not throwing
;; for :propagate-throw (x86 parity).
(block b
  (unwind-protect (return-from b 42)
    (when (%throwing-through-cleanup-p)
      (error "return-from uwp expected nil, got ~s" (%throwing-through-cleanup-p)))
    (format t "~&return-from=NIL~%")))

(format t "~&DARWIN-CLEAN-BUILD-SMOKE-OK~%")
(quit 0)
