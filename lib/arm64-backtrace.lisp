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
;;; Scope: save0..save3 are IN the register pool (*arm642-nvrs*,
;;; arm642.lisp), so the saved-register recovery set is real here:
;;; the compiler homes variables in save0-3, records those locations
;;; in the symbol map (arm642-digest-symbols), and records the
;;; register-save frame layout as a REGISTER-SAVE-INFO entry in the
;;; function's %LFUN-INFO plist (arm642-encode-regsave-info, format
;;; documented there) that REGISTERS-USED-BY below reads back -- PPC's
;;; LWZ-trailer scheme (ppc:101-134) with the metadata kept in the
;;; function object rather than the code vector.  Still absent, as on ARM32
;;; (lib/arm-backtrace.lisp): the srv vectors, frame-restartable-p and
;;; %apply-in-frame's PPC-instruction branch-tree parser (ppc:379-736).
;;; APPLY-IN-FRAME therefore remains unimplemented.
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

;;; Give the istruct a class so that class-cell-typep has a real
;;; class wrapper to look at.
(make-istruct-class 'fake-stack-frame *istruct-class*)

;;; Linked list of fake stack frames.
;;; %frame-backlink looks here (ppc:31-33).
(def-standard-initial-binding *fake-stack-frames* nil)

(defun fake-stack-frame-p (x)   ; ppc:36
  (istruct-typep x 'fake-stack-frame))

;;; The control stack on arm64 contains only 32-byte lisp frames (word 0
;;; being lisp-frame-marker) and stack-allocated u64-vectors that cover
;;; foreign data.
(defun %cstack-next-object (p)
  "Return the next (older, higher-addressed) object on the control stack."
  (declare (fixnum p))
  (let ((header (%fixnum-ref-natural p)))
    (cond
      ((eql header arm64::lisp-frame-marker)
       (+ p (ash arm64::lisp-frame.size (- arm64::word-shift))))
      ;; A foreign-data cover: header word + elements, padded to a dnode.
      ((eql (logand header (1- (ash 1 arm64::num-subtag-bits)))
            arm64::subtag-u64-vector)
       (let ((words (1+ (ash header (- arm64::num-subtag-bits)))))
         (+ p (if (logtest words 1) (1+ words) words))))
      ;; Neither.  Something is wrong.  Maybe if we close our eyes, it
      ;; will go away.  lisp-frame-p will treat nil as the bottom of
      ;; the stack.
      (t nil))))

;;; ppc-threads-utils.lisp:67-79 (see placement note in the header).
(defun %frame-backlink (p &optional context)
  (cond ((fake-stack-frame-p p)
         (%fake-stack-frame.next-sp p))
        ((fixnump p)
         (let ((backlink (%cstack-next-object p))
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
  (cond ((null p) (values nil t))
        ((fake-stack-frame-p p) (values t nil))
        ((bottom-of-stack-p p context) (values nil t))
        (t (values (eql (%fixnum-ref-natural p) arm64::lisp-frame-marker)
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

;;; ppc:101-134 (the ppc64 arm), with the register-save record read
;;; from the function's %LFUN-INFO plist (REGISTER-SAVE-INFO, a packed
;;; fixnum emitted by arm642-encode-regsave-info) rather than from a
;;; code-vector trailer.  Fields (keep in sync with the emitter):
;;;   bits 2..0 nregs   bits 28..3 pc (words)   bits 54..29 ea (cells)
;;; Returns (mask where) once AT-PC has passed the save sequence,
;;; (mask nil) with no AT-PC ("saved somewhere, can't locate" -- the
;;; walkers then answer their BAD value), (nil nil) otherwise.
;;; Index space, shared by the mask, *saved-register-names* and the
;;; catch-frame fallback: save0=3 .. save3=0.  The save-nvrs vinsn
;;; (arm64-vinsns.lisp:7741) pushes save0 FIRST, so save0 sits at the
;;; HIGHEST address = raw-frame index WHERE+0 = the highest mask bit,
;;; PPC's exact geometry (stmw leaves save0=r31 highest, index 7).
;;; ARM64-DEVIATION: the recorded pc is the save COMPLETION point, so
;;; the AT-PC comparison is exact even for an exception pc inside the
;;; save sequence; PPC (>= the sequence START) and x86-64 (<= rpc) are
;;; both off by up to the sequence length there.
(defun registers-used-by (lfun &optional at-pc)
  (let* ((info (getf (%lfun-info lfun) 'register-save-info)))
    (if info
      (let* ((nregs (ldb (byte 3 0) info))
             (pc (ldb (byte 26 3) info))
             (ea-cells (ldb (byte 26 29) info))
             (mask (ash (1- (ash 1 nregs)) (- 4 nregs))))
        (declare (fixnum nregs pc ea-cells mask))
        (if at-pc
          (if (>= (ash at-pc -2) pc)
            (values mask (- ea-cells nregs))
            (values nil nil))
          (values mask nil)))
      (values nil nil))))

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

;;; ppc:224-225.  ARM64-DEVIATION: this pool ASCENDS from save0=x19
;;; where PPC's DESCENDS to save0=r31, and save0 must get the highest
;;; index (see registers-used-by), so the sense flips: save3 - regno.
(defun register-number->saved-register-index (regno)
  (- arm64::save3 regno))

;;; ppc:340-351.  ARM64-DEVIATION: our catch frame stores save0 FIRST,
;;; ascending (arm64-arch.lisp catch-frame: "regs[] holds save0..save3
;;; in ascending order"); PPC stores save7 first.  With index save0=3,
;;; the cell is save-save0-cell + (3 - index).
(defun get-register-value (address last-catch index)
  (if address
    (%fixnum-ref address)
    (uvref last-catch (+ (- 3 index) target::catch-frame.save-save0-cell))))

;;; Inverse of get-register-value
(defun set-register-value (value address last-catch index)
  (if address
    (%fixnum-set address value)
    (setf (uvref last-catch (+ (- 3 index) target::catch-frame.save-save0-cell))
          value)))

;;; ppc:227-249, with x86-backtrace.lisp:176-181's nil-WHERE guard (a
;;; younger frame that saved the register at an unknown location must
;;; answer BAD, not keep walking: an older copy or the catch snapshot
;;; would be a stale value presented as current).  Walk correctness:
;;; going from CFP toward YOUNGER frames, the FIRST frame whose mask
;;; claims the register stored the value it saw at ITS entry, and no
;;; live frame between CFP and it touched the register (one that had
;;; would have saved it and been found first), so that stored value is
;;; CFP's function's value.  A fake frame carries the register live in
;;; its exception context.  If nothing saved it, the innermost catch
;;; frame's snapshot is the donor-sanctioned fallback.
(defun %find-register-argument-value (context cfp regval bad)
  (let* ((last-catch (last-catch-since cfp context))
         (index (register-number->saved-register-index regval)))
    (do* ((frame cfp (child-frame frame context))
          (first t))
         ((null frame))
      (if (fake-stack-frame-p frame)
        (return-from %find-register-argument-value
          (xp-gpr-lisp (%fake-stack-frame.xp frame) regval))
        (if first
          (setq first nil)
          (multiple-value-bind (lfun pc)
              (cfp-lfun frame)
            (when lfun
              (multiple-value-bind (mask where)
                  (registers-used-by lfun pc)
                (when (if mask (logbitp index mask))
                  (return-from %find-register-argument-value
                    (if where
                      (raw-frame-ref frame context
                                     (+ where
                                        (logcount
                                         (logandc2 mask
                                                   (1- (ash 1 (1+ index))))))
                                     bad)
                      bad)))))))))
    (get-register-value nil last-catch index)))

;;; ppc:251-275, same shape and the same nil-WHERE guard (return NIL:
;;; the caller set-map-entry-value treats non-nil as success).
(defun %set-register-argument-value (context cfp regval new)
  (let* ((last-catch (last-catch-since cfp context))
         (index (register-number->saved-register-index regval)))
    (do* ((frame cfp (child-frame frame context))
          (first t))
         ((null frame))
      (if (fake-stack-frame-p frame)
        (return-from %set-register-argument-value
          (setf (xp-gpr-lisp (%fake-stack-frame.xp frame) regval) new))
        (if first
          (setq first nil)
          (multiple-value-bind (lfun pc)
              (cfp-lfun frame)
            (when lfun
              (multiple-value-bind (mask where)
                  (registers-used-by lfun pc)
                (when (if mask (logbitp index mask))
                  (return-from %set-register-argument-value
                    (when where
                      (raw-frame-set frame context
                                     (+ where
                                        (logcount
                                         (logandc2 mask
                                                   (1- (ash 1 (1+ index))))))
                                     new))))))))))
    (set-register-value new nil last-catch index)))

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
