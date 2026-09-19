;;;; Darwin/arm64 inbound callback composite returns.
;;;;
;;;; Bug class guarded against: treating EVERY foreign-record return as
;;;; a hidden stret pointer in x0.  For :gpr (NSRange) / :hfa (NSRect)
;;;; that steals `self` on ObjC IMPs → memmove into the object →
;;;; object_getClass EXC_BREAKPOINT (mouse select /
;;;; #/selectionRangeForProposedRange:granularity:).
;;;;
;;;;   ./darm64cl --no-init --batch < tools/darwin-callback-struct-return-smoke.lisp
(in-package :ccl)

(format t "~&;; darwin-callback-struct-return-smoke~%")
(finish-output)

(use-interface-dir :cocoa)

(flet ((class-of-spec (spec)
         (let* ((ftype (parse-foreign-type spec)))
           (cond ((arm64::hfa-type-info ftype) :hfa)
                 ((arm64::record-type-returns-structure-as-first-arg
                   ftype)
                  :memory)
                 (t :gpr)))))
  (assert (eq (class-of-spec :<NSR>ange) :gpr))
  (assert (eq (class-of-spec :<NSR>ect) :hfa)))
(format t "~&;; classify ok~%")
(finish-output)

;; Live round-trip: Lisp→C ABI→callback→registers→Lisp.
;; defcallback struct-return path does (let* ((result ,@body)) — one form.
(defcallback %smoke-make-nsrange
    (out :unsigned-long loc :unsigned-long len :<NSR>ange)
  (progn
    (setf (pref out :<NSR>ange.location) loc
          (pref out :<NSR>ange.length) len)
    out))

(rlet ((got :<NSR>ange))
  ;; Struct-return ff-call args: RESULT-BUF …args… RESULT-TYPE
  (ff-call %smoke-make-nsrange
           got
           :unsigned-long 42
           :unsigned-long 7
           :<NSR>ange)
  (assert (= (pref got :<NSR>ange.location) 42))
  (assert (= (pref got :<NSR>ange.length) 7))
  (format t "~&;; NSRange callback round-trip ok (~d,~d)~%"
          (pref got :<NSR>ange.location)
          (pref got :<NSR>ange.length))
  (finish-output))

(defcallback %smoke-make-nsrect
    (out :double x :double y :double w :double h :<NSR>ect)
  (progn
    (setf (pref out :<NSR>ect.origin.x) x
          (pref out :<NSR>ect.origin.y) y
          (pref out :<NSR>ect.size.width) w
          (pref out :<NSR>ect.size.height) h)
    out))

(rlet ((got :<NSR>ect))
  (ff-call %smoke-make-nsrect
           got
           :double 1.0d0 :double 2.0d0
           :double 3.0d0 :double 4.0d0
           :<NSR>ect)
  (assert (= (pref got :<NSR>ect.origin.x) 1.0d0))
  (assert (= (pref got :<NSR>ect.origin.y) 2.0d0))
  (assert (= (pref got :<NSR>ect.size.width) 3.0d0))
  (assert (= (pref got :<NSR>ect.size.height) 4.0d0))
  (format t "~&;; NSRect HFA callback round-trip ok~%")
  (finish-output))

(format t "~&;; PASS darwin-callback-struct-return-smoke~%")
(finish-output)
(ff-call (foreign-symbol-address "exit") :signed-fullword 0 :void)
