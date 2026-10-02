;;;; 02 — catch / throw / unwind-protect / PROGV / specials integrity.
;;;; After throw through UWP+PROGV, every catch-frame.db-link must still be
;;;; reachable from tcr.db-link (otherwise unbind_to walks off the chain and
;;;; FARs at binding.sym = #x8 — the Cocoa menu death signature).
;;;; Marker: 02-THROW-BIND-INTEGRITY-OK
(in-package :ccl)

(defvar *tba-a* :unbound-a)
(defvar *tba-b* :unbound-b)
(defvar *tba-c* :unbound-c)
(defvar *tba-cleanups* 0)

(defun %tba-db-link ()
  (db-link))

(defun %tba-db-chain (&optional (max 64))
  (let ((p (%tba-db-link)) (out ()) (n 0))
    (loop while (and (not (eql p 0)) (< n max))
          do (push p out)
             (incf n)
             (setq p (%fixnum-ref p 0)))
    (nreverse out)))

(defun %tba-db-reachable-p (target)
  (or (eql target 0)
      (eql target (%tba-db-link))
      (member target (%tba-db-chain) :test #'eql)))

(defun %tba-assert-catch-db-links (tag)
  (do ((cf (%catch-top (%current-tcr))
           (%fixnum-ref cf target::catch-frame.link))
       (i 0 (1+ i)))
      ((or (null cf) (eql cf 0)))
    (let ((db (%fixnum-ref cf target::catch-frame.db-link)))
      (unless (%tba-db-reachable-p db)
        (error "~a: catch[~d] db-link #x~x not on chain (head #x~x)~%  chain=~s"
               tag i db (%tba-db-link)
               (mapcar (lambda (x) (format nil "#x~x" x))
                       (%tba-db-chain 8)))))))

(defun tba-catch-throw ()
  (let ((*tba-a* 1))
    (catch 'tba
      (let ((*tba-a* 2))
        (throw 'tba :ok)))
    (unless (eql *tba-a* 1)
      (error "catch/throw special restore => ~s" *tba-a*))))

(defun tba-uwp-throw ()
  (setq *tba-cleanups* 0)
  (let ((*tba-a* 1))
    (catch 'tba
      (unwind-protect
           (let ((*tba-a* 2))
             (throw 'tba :ok))
        (incf *tba-cleanups*)
        (unless (eql *tba-a* 1)
          (error "uwp cleanup saw *tba-a*=~s" *tba-a*))))
    (unless (eql *tba-cleanups* 1)
      (error "uwp cleanup count ~s" *tba-cleanups*))
    (unless (eql *tba-a* 1)
      (error "after uwp throw *tba-a*=~s" *tba-a*))))

(defun tba-progv-throw ()
  (setq *tba-cleanups* 0)
  (let ((*tba-a* 1) (*tba-b* 10))
    (catch 'tba
      (unwind-protect
           (progv '(*tba-a* *tba-b*) '(2 20)
             (unless (and (eql *tba-a* 2) (eql *tba-b* 20))
               (error "progv bind failed"))
             (%tba-assert-catch-db-links "inside-progv")
             (throw 'tba :ok))
        (incf *tba-cleanups*)))
    (unless (and (eql *tba-a* 1) (eql *tba-b* 10))
      (error "progv restore after throw a=~s b=~s" *tba-a* *tba-b*))
    (unless (eql *tba-cleanups* 1)
      (error "progv uwp cleanup count ~s" *tba-cleanups*))))

(defun tba-nested-uwp-progv ()
  (setq *tba-cleanups* 0)
  (let ((*tba-a* 0) (*tba-b* 0) (*tba-c* 0))
    (catch 'outer
      (unwind-protect
           (let ((*tba-a* 1))
             (unwind-protect
                  (progv '(*tba-b* *tba-c*) '(2 3)
                    (let ((*tba-a* 11))
                      (%tba-assert-catch-db-links "nested-inner")
                      (throw 'outer :ok)))
               (incf *tba-cleanups*)))
        (incf *tba-cleanups*)))
    (unless (eql *tba-cleanups* 2)
      (error "nested cleanups ~s" *tba-cleanups*))
    (unless (and (eql *tba-a* 0) (eql *tba-b* 0) (eql *tba-c* 0))
      (error "nested restore a=~s b=~s c=~s" *tba-a* *tba-b* *tba-c*))))

(defun tba-stress (n)
  (dotimes (i n)
    (tba-catch-throw)
    (tba-uwp-throw)
    (tba-progv-throw)
    (tba-nested-uwp-progv)
    (%tba-assert-catch-db-links (format nil "stress-~d" i))))

(tba-stress 200)
(format t "~&02-THROW-BIND-INTEGRITY-OK~%")
(quit 0)
