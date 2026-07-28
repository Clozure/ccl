;;; -*- Mode: Lisp; Package: CCL -*-
;;; PPC64 LINE-PORT (source: vendor/ccl/lib/ppc-backtrace.lisp, cited
;;; "; ppc:NNN"; scope per lib/arm-backtrace.lisp — see below).
;;;
;;; arm64-backtrace.lisp — arch-side backtrace support (the CFP-LFUN
;;; cluster) for Matt Emerson's upstream ARM64 (low-tag) design.
;;;
;;; Model: PPC fake-stack-frames (heap istructs chained through
;;; *fake-stack-frames*, built by arm64-trap-support's
;;; funcall-with-xp-stack-frames) walking ARM32-style MARKER lisp
;;; frames (kernel ground truth: arm64-gc.c mark_cstack_area — frames
;;; are {marker@0, savevsp@8, savefn@16, savelr@24} identified by
;;; lisp-frame-marker, NOT chained through a stored backlink as on
;;; PPC).
;;;
;;; Scope follows the shipped ARM32 port (lib/arm-backtrace.lisp): the
;;; saved-register machinery (registers-used-by encodings, srv vectors,
;;; frame-restartable-p, %apply-in-frame and its PPC-instruction
;;; branch-tree parser, ppc:379-736) has no analog because this design
;;; keeps NO non-volatile lisp registers (lib/arm64env.lisp R1:
;;; empty save pool) — the compiler can never record a saved-register
;;; variable location.  APPLY-IN-FRAME is therefore unimplemented, as
;;; on ARM32.  LATENT: revisit if save0-3 ever enter the register
;;; pool.
;;;
;;; %frame-backlink and lisp-frame-p live HERE, not in
;;; arm64-threads-utils.lisp as on PPC: they need the
;;; fake-stack-frame accessor macros at compile time, and the cross
;;; gate compiles each file in an x86 host image that only has this
;;; file's macros once they are defined in the same compile unit.

(in-package "CCL")

;;; ppc:20-29.  Slot order is pinned by %cons-fake-stack-frame
;;; (library/lispequ.lisp:211).
(def-accessors (fake-stack-frame) %svref
  nil                           ; 'fake-stack-frame
  %fake-stack-frame.sp          ; fixnum. The stack pointer where this frame "should" be
  %fake-stack-frame.next-sp     ; Either sp or another fake-stack-frame
  %fake-stack-frame.fn          ; The current function
  %fake-stack-frame.lr          ; fixnum offset from fn (nil if fn is not functionp)
  %fake-stack-frame.vsp         ; The value stack pointer
  %fake-stack-frame.xp          ; Exception frame.
  %fake-stack-frame.link        ; next in *fake-stack-frames* list
  )

;;; Linked list of fake stack frames.
;;; %frame-backlink looks here (ppc:31-33).
(def-standard-initial-binding *fake-stack-frames* nil)

(defun fake-stack-frame-p (x)   ; ppc:36
  (istruct-typep x 'fake-stack-frame))

;;; ppc-threads-utils.lisp:67-79 (see placement note in the header).
(defun %frame-backlink (p &optional context)
  (cond ((fake-stack-frame-p p)
         (%fake-stack-frame.next-sp p))
        ((fixnump p)
         (let ((backlink (%%frame-backlink p))
               (fake-frame
                (if context (bt.fake-frames context) *fake-stack-frames*)))
           (loop
             (when (null fake-frame) (return backlink))
             (when (eq backlink (%fake-stack-frame.sp fake-frame))
               (return fake-frame))
             (setq fake-frame (%fake-stack-frame.link fake-frame)))))
        (t (error "~s is not a valid stack frame" p))))

;;; arm-threads-utils.lisp:61-65 shape (marker frames); PPC instead
;;; validates frame size + savefn because its frames carry no marker.
(defun lisp-frame-p (p context)
  (if (fake-stack-frame-p p)
    (values t nil)
    (if (bottom-of-stack-p p context)
      (values nil t)
      (values (eql (%fixnum-ref-natural p) arm64::lisp-frame-marker)
              nil))))

(defun cfp-lfun (p)             ; ppc:39-47
  (if (fake-stack-frame-p p)
    (let* ((fn (%fake-stack-frame.fn p))
           (lr (%fake-stack-frame.lr p)))
      (if (and (typep fn 'function)
               (typep lr 'fixnum))
        (values fn lr)
        (values nil nil)))
    (%cfp-lfun p)))

(defun %stack< (index1 index2 &optional context) ; ppc:50-64
  (cond ((fake-stack-frame-p index1)
         (let ((sp1 (%fake-stack-frame.sp index1)))
           (declare (fixnum sp1))
           (if (fake-stack-frame-p index2)
             (or (%stack< sp1 (%fake-stack-frame.sp index2) context)
                 (eq index2 (%fake-stack-frame.next-sp index1)))
             (%stack< sp1 (%i+ index2 1) context))))
        ((fake-stack-frame-p index2)
         (%stack< index1 (%fake-stack-frame.sp index2) context))
        (t (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
                  (cs-area (%fixnum-ref tcr target::tcr.cs-area)))
             (and (%ptr-in-area-p index1 cs-area)
                  (%ptr-in-area-p index2 cs-area)
                  (< (the fixnum index1) (the fixnum index2)))))))

;;; No non-volatile lisp registers in this design (header note), so no
;;; register-save encoding exists to decode — the ARM32 answer
;;; (arm-backtrace.lisp:49-51), not PPC's LWZ-trailer scheme
;;; (ppc:101-134).
(defun registers-used-by (lfun &optional at-pc)
  (declare (ignore lfun at-pc))
  (values nil nil))

(defun %frame-savefn (p)        ; ppc:153-156
  (if (fake-stack-frame-p p)
    (%fake-stack-frame.fn p)
    (%%frame-savefn p)))

(defun %frame-savevsp (p)       ; ppc:158-161
  (if (fake-stack-frame-p p)
    (%fake-stack-frame.vsp p)
    (%%frame-savevsp p)))

(defun frame-vsp (frame)        ; ppc:163-164
  (%frame-savevsp frame))

;;; Return two values: the vsp of p and the vsp of p's "parent" frame.
;;; The "parent" frame vsp might actually be the end of p's segment,
;;; if the real "parent" frame vsp is in another segment.
(defun vsp-limits (p context)   ; ppc:169-200
  (let* ((vsp (%frame-savevsp p))
         parent)
    (when (eql vsp 0)
      ;; This frame is where the code continues after an unwind-protect cleanup form
      (setq vsp (%frame-savevsp (child-frame p context))))
    (flet ((grand-parent (frame)
             (let ((parent (parent-frame frame context)))
               (when (and parent (eq parent (%frame-backlink frame context)))
                 (let ((grand-parent (parent-frame parent context)))
                   (when (and grand-parent (eq grand-parent (%frame-backlink parent context)))
                     grand-parent))))))
      (declare (dynamic-extent #'grand-parent))
      (let* ((frame p)
             grand-parent)
        (loop
          (setq grand-parent (grand-parent frame))
          (when (or (null grand-parent) (not (eql 0 (%frame-savevsp grand-parent))))
            (return))
          (setq frame grand-parent))
        (setq parent (parent-frame frame context)))
      (let* ((parent-vsp (if parent (%frame-savevsp parent) vsp))
             (tcr (if context (bt.tcr context) (%current-tcr)))
             (vsp-area (%fixnum-ref tcr target::tcr.vs-area)))
        (if (eql 0 parent-vsp)
          (values vsp vsp)              ; p is the kernel frame pushed by an unwind-protect cleanup form
          (progn
            (unless vsp-area
              (error "~s is not a stack frame pointer for context ~s" p tcr))
            (unless (%ptr-in-area-p parent-vsp vsp-area)
              (setq parent-vsp (%fixnum-ref vsp-area target::area.high)))
            (values vsp parent-vsp)))))))

(defun catch-csp-p (p context)  ; ppc:203-212
  (let ((catch (if context
                 (bt.top-catch context)
                 (%catch-top (%current-tcr)))))
    (loop
      (when (null catch) (return nil))
      (let ((sp (catch-frame-sp catch)))
        (when (eql sp p)
          (return t)))
      (setq catch (next-catch catch)))))

(defun last-catch-since (sp context) ; ppc:214-223
  (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
         (catch (%catch-top tcr))
         (last-catch nil))
    (loop
      (unless catch (return last-catch))
      (let ((csp (uvref catch target::catch-frame.csp-cell)))
        (when (%stack< sp csp context) (return last-catch))
        (setq last-catch catch
              catch (next-catch catch))))))

;;; With no saved registers (see registers-used-by) the compiler never
;;; records a saved-register variable location, so these are
;;; unreachable — the ARM32 answer (arm-backtrace.lisp:131-139).
(defun %find-register-argument-value (context cfp regval bad)
  (declare (ignore context cfp regval))
  bad)

(defun %set-register-argument-value (context cfp regval new)
  (declare (ignore context cfp regval))
  new)

(defun %raw-frame-ref (cfp context idx bad) ; ppc:276-297
  (declare (fixnum idx))
  (multiple-value-bind (frame base)
      (vsp-limits cfp context)
    (let* ((raw-size (- base frame)))
      (declare (fixnum frame base raw-size))
      (if (and (>= idx 0)
               (< idx raw-size))
        (let* ((addr (- (the fixnum (1- base))
                        idx)))
          (multiple-value-bind (db-count first-db last-db)
              (count-db-links-in-frame frame base context)
            (let* ((is-db-link
                    (unless (zerop db-count)
                      (do* ((last last-db (previous-db-link last first-db)))
                           ((null last))
                        (when (= addr last)
                          (return t))))))
              (if is-db-link
                (oldest-binding-frame-value context addr)
                (%fixnum-ref addr)))))
        bad))))

(defun %raw-frame-set (cfp context idx new) ; ppc:299-320
  (declare (fixnum idx))
  (multiple-value-bind (frame base)
      (vsp-limits cfp context)
    (let* ((raw-size (- base frame)))
      (declare (fixnum frame base raw-size))
      (if (and (>= idx 0)
               (< idx raw-size))
        (let* ((addr (- (the fixnum (1- base))
                        idx)))
          (multiple-value-bind (db-count first-db last-db)
              (count-db-links-in-frame frame base context)
            (let* ((is-db-link
                    (unless (zerop db-count)
                      (do* ((last last-db (previous-db-link last first-db)))
                           ((null last))
                        (when (= addr last)
                          (return t))))))
              (if is-db-link
                (setf (oldest-binding-frame-value context addr) new)
                (setf (%fixnum-ref addr) new))))
          t)))))

;;; Used for printing only (ppc:322-326).
(defun index->address (p)
  (when (fake-stack-frame-p p)
    (setq p (%fake-stack-frame.sp p)))
  (ldb (byte 64 0) (ash p target::fixnumshift)))

(defun match-local-name (cellno info pc) ; ppc:329-338
  (when info
    (let* ((syms (%car info))
           (ptrs (%cdr info)))
      (dotimes (i (length syms))
        (let ((j (%i+ i (%i+ i i))))
          (and (eq (uvref ptrs j) (%ilogior (%ilsl (+ 6 target::word-shift) cellno) #o77))
               (%i>= pc (uvref ptrs (%i+ j 1)))
               (%i< pc (uvref ptrs (%i+ j 2)))
               (return (aref syms i))))))))

(defun exception-frame-p (frame) ; ppc:877
  (fake-stack-frame-p frame))

(defun arg-check-call-arguments (frame function) ; ppc:880-882
  (declare (ignore function))
  (xp-argument-list (%fake-stack-frame.xp frame)))

;;; arm-backtrace.lisp:173-178 (see the scope note in the header).
(defun apply-in-frame (frame function arglist &optional context)
  (declare (ignore frame function arglist context))
  (error "APPLY-IN-FRAME isn't implemented on ARM64."))

(defun return-from-frame (frame &rest values)
  (apply-in-frame frame #'values values nil))
