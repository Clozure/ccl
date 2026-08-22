;;;; Darwin/arm64 AAPCS64 composite return smoke.
;;;; Poll — CCL smokes hang or die quickly; do not block indefinitely.
;;;;
;;;; Covers HFA (NSSize/NSPoint/NSRect), GPR≤16B (NSRange), and that
;;;; expand-ff-call never stuffs the result buffer into x0.
(in-package :ccl)

(format t "~&;; darwin-struct-return-smoke~%")
(finish-output)

(use-interface-dir :cocoa)

(defun flat-arglist (form)
  "Walk LET/%stack-block wrappers to the inner %ff-call."
  (cond ((atom form) nil)
        ((eq (car form) '%ff-call) form)
        (t (or (flat-arglist (car form))
               (flat-arglist (cdr form))))))

(flet ((class-of-spec (spec)
         (let* ((ftype (parse-foreign-type spec)))
           (cond ((arm64::hfa-type-info ftype) :hfa)
                 ((arm64::record-type-returns-structure-as-first-arg
                   ftype)
                  :memory)
                 (t :gpr))))
       (implicit (spec)
         (funcall (ftd-ff-call-struct-return-by-implicit-arg-function *target-ftd*)
                  spec)))
  (assert (eq (class-of-spec :<NSS>ize) :hfa))
  (assert (eq (class-of-spec :<NSP>oint) :hfa))
  (assert (eq (class-of-spec :<NSR>ect) :hfa))
  (assert (eq (class-of-spec :<NSR>ange) :gpr))
  (assert (null (implicit :<NSS>ize)))
  (assert (null (implicit :<NSR>ect)))
  (assert (null (implicit :<NSR>ange)))
  (format t "~&;; classify ok~%")
  (finish-output))

(flet ((expanded (result-spec)
         (funcall (ftd-ff-call-expand-function *target-ftd*)
                  '(%ff-call ENTRY)
                  `(S :address FONT :address SEL ,result-spec)
                  :arg-coerce #'null-coerce-foreign-arg
                  :result-coerce #'null-coerce-foreign-result))
       (has-registers (ex)
         (member :registers (flat-arglist ex)))
       (result-as-x0-p (ex)
         ;; Broken shape: first :address value is the result buffer S.
         (let ((ff (flat-arglist ex)))
           (and (eq (third ff) :address)
                (eq (fourth ff) 'S)))))
  (let ((ex (expanded :<NSS>ize)))
    (format t "~&;; NSSize expand=~%~S~%" ex)
    (finish-output)
    (assert (has-registers ex) () "NSSize needs :registers: ~s" ex)
    (assert (not (result-as-x0-p ex)) () "NSSize still passes result as x0: ~s" ex))
  (let ((ex (expanded :<NSR>ange)))
    (assert (has-registers ex) () "NSRange needs :registers: ~s" ex)
    (assert (not (result-as-x0-p ex)) () "NSRange still passes result as x0: ~s" ex))
  (let ((ex (expanded :<NSR>ect)))
    (assert (has-registers ex) () "NSRect HFA needs :registers: ~s" ex)
    (assert (not (result-as-x0-p ex)) () "NSRect still passes result as x0: ~s" ex))
  (format t "~&;; expand shape ok~%")
  (finish-output))

(require "OBJC-SUPPORT")
(format t "~&;; objc-support loaded~%")
(finish-output)

(objc:with-autorelease-pool
  (let* ((font (#/systemFontOfSize: ns:ns-font 12.0d0))
         (g (#/glyphWithName: font #@"i"))
         (adv (#/advancementForGlyph: font g))
         (bb (#/boundingRectForGlyph: font g)))
    (format t "~&;; font=~s glyph=~s~%" font g)
    (format t "~&;; adv width=~s height=~s~%"
            (ns:ns-size-width adv) (ns:ns-size-height adv))
    (format t "~&;; bbox origin=(~s,~s) size=(~s,~s)~%"
            (ns:ns-rect-x bb) (ns:ns-rect-y bb)
            (ns:ns-rect-width bb) (ns:ns-rect-height bb))
    (finish-output)
    (assert (and (floatp (ns:ns-size-width adv))
                 (plusp (ns:ns-size-width adv))))
    (let* ((s (%make-nsstring "abcdef"))
           (r (ns:make-ns-range 1 3))
           (sub (#/substringWithRange: s r)))
      (assert (equal (%get-cstring (#/UTF8String sub)) "bcd")))))

(format t "~&DARWIN-STRUCT-RETURN-SMOKE-OK~%")
(quit)
