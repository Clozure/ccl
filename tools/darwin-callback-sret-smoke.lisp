;;;; Smoke: AAPCS64 :memory (x8 sret) callback return + :gpr still works.
;;;; No Cocoa.
;;;;   ./darm64cl --no-init --batch --eval '(load "tools/darwin-callback-sret-smoke.lisp")'
(in-package :ccl)

(defun %sret-smoke-quit (code)
  (finish-output)
  (finish-output *error-output*)
  (ff-call (foreign-symbol-address "exit") :signed-fullword code :void))

(format t "~&;; darwin-callback-sret-smoke~%")
(finish-output)

;; Flat field list (CCL struct translator takes &rest fields).
(def-foreign-type :sret_triple
  (:struct :sret_triple
    (:a :unsigned-doubleword)
    (:b :unsigned-doubleword)
    (:c :unsigned-doubleword)))

(def-foreign-type :gpr_pair
  (:struct :gpr_pair
    (:a :unsigned-doubleword)
    (:b :unsigned-doubleword)))

(flet ((class-of-spec (spec)
         (let* ((ftype (parse-foreign-type spec)))
           (cond ((arm64::hfa-type-info ftype) :hfa)
                 ((arm64::record-type-returns-structure-as-first-arg
                   ftype)
                  :memory)
                 (t :gpr)))))
  (assert (eq (class-of-spec :sret_triple) :memory))
  (assert (eq (class-of-spec :gpr_pair) :gpr)))
(format t "~&;; classify ok~%")
(finish-output)

(defcallback %smoke-make-triple
    (out :unsigned-doubleword a :unsigned-doubleword b :sret_triple)
  (progn
    (setf (pref out :sret_triple.a) a
          (pref out :sret_triple.b) b
          (pref out :sret_triple.c) (+ a b))
    out))

(rlet ((got :sret_triple))
  (ff-call %smoke-make-triple
           got
           :unsigned-doubleword 10
           :unsigned-doubleword 20
           :sret_triple)
  (assert (= (pref got :sret_triple.a) 10))
  (assert (= (pref got :sret_triple.b) 20))
  (assert (= (pref got :sret_triple.c) 30))
  (format t "~&;; :memory callback round-trip ok (~d,~d,~d)~%"
          (pref got :sret_triple.a)
          (pref got :sret_triple.b)
          (pref got :sret_triple.c))
  (finish-output))

(defcallback %smoke-make-pair
    (out :unsigned-doubleword a :unsigned-doubleword b :gpr_pair)
  (progn
    (setf (pref out :gpr_pair.a) a
          (pref out :gpr_pair.b) b)
    out))

(rlet ((got :gpr_pair))
  (ff-call %smoke-make-pair
           got
           :unsigned-doubleword 42
           :unsigned-doubleword 7
           :gpr_pair)
  (assert (= (pref got :gpr_pair.a) 42))
  (assert (= (pref got :gpr_pair.b) 7))
  (format t "~&;; :gpr pair still ok (~d,~d)~%"
          (pref got :gpr_pair.a) (pref got :gpr_pair.b))
  (finish-output))

(format t "~&;; PASS darwin-callback-sret-smoke~%")
(%sret-smoke-quit 0)
