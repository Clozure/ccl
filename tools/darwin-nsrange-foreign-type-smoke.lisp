;;;; NSRange/NSRect typedef vs empty (:struct …) stub resolution.
;;;;
;;;;   ./darm64cl --no-init --batch < tools/darwin-nsrange-foreign-type-smoke.lisp
(in-package :ccl)
(require "OBJC-SUPPORT")
(use-interface-dir :cocoa)

(assert (= 128 (ensure-foreign-type-bits (parse-foreign-type :<NSR>ange))))
(assert (= 128 (ensure-foreign-type-bits (parse-foreign-type '(:struct :<NSR>ange)))))
(assert (= 16 (%foreign-type-or-record-size :<NSR>ange :bytes)))
(assert (= 16 (%foreign-type-or-record-size '(:struct :<NSR>ange) :bytes)))
(assert (make-gcable-record :<NSR>ange))
;; Macro arg must be the type form, not a quoted list (same as send compile).
(assert (funcall (compile nil '(lambda () (make-gcable-record (:struct :<NSR>ange))))))
(assert (ns:ns-range-p (funcall (compile nil '(lambda () (make-gcable-record (:struct :<NSR>ange)))))))
(assert (ns:ns-range-p (make-gcable-record :<NSR>ange)))
(assert (= (foreign-type-ordinal (%foreign-type-or-record '(:struct :<NSR>ange)))
           (foreign-type-ordinal (parse-foreign-type :<NSR>ange))))
(let* ((ftype (parse-foreign-type '(:struct :<NSR>ange))))
  ;; NSRange returns in registers: not an HFA, not a memory (x8) return.
  (assert (null (arm64::hfa-type-info ftype)))
  (assert (null (arm64::record-type-returns-structure-as-first-arg ftype))))
(assert (= 256 (ensure-foreign-type-bits (parse-foreign-type '(:struct :<NSR>ect)))))

(dolist (sig '((:void :<NSUI>nteger (:struct :<NSR>ange) :<NSI>nteger)
               (:<NSR>ange :<NSR>ect (:* (:struct :<NST>ext<C>ontainer)))
               ((:struct :<NSR>ange) :<NSR>ect (:* (:struct :<NST>ext<C>ontainer)))
               ((:struct :<NSR>ange) (:struct :<NSR>ange) (:* (:struct :<NSR>ange)))
               (:<NSR>ange (:* (:struct :<NSS>tring)))))
  (assert (compile-send-function-for-signature sig) () "send compile failed: ~s" sig))

;; Install freshly compiled NSRange return and exercise via #/.
(let* ((sig '(:<NSR>ange (:* (:struct :<NSS>tring))))
       (f (compile-send-function-for-signature sig)))
  (setf (objc-method-signature-info-function (objc-method-signature-info sig)) f)
  (maphash (lambda (name msg)
             (declare (ignore name))
             (dolist (m (append (objc-message-info-methods msg)
                                (objc-message-info-protocol-methods msg)))
               (let ((si (objc-method-info-signature-info m)))
                 (when (and si (equal sig (objc-method-signature-info-type-signature si)))
                   (setf (objc-method-signature-info-function si) f)))))
           *objc-message-info*)
  (let ((r (#/rangeOfString: #@"hello world" #@"world")))
    (assert (ns:ns-range-p r) () "rangeOfString not ns-range: ~s" r)
    (assert (= 6 (ns:ns-range-location r)))
    (assert (= 5 (ns:ns-range-length r)))))

;; :id must still prefer the pointer typedef over the id struct.
(let ((id-type (%foreign-type-or-record :id)))
  (assert (typep id-type 'foreign-pointer-type) () "id typedef broken: ~s" id-type))

(format t "~&DARWIN-NSRANGE-FOREIGN-TYPE-OK~%")
(quit 0)
