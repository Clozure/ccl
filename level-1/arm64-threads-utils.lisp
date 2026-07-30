;;; -*- Mode: Lisp; Package: CCL -*-
;;; ARM64-SPECIFIC — upstream (Matt Emerson low-tag) lane file; the tag
;;; geometry mirrors x8664, so the donor is level-1/x86-threads-utils.lisp
;;; (#+x8664-target branches), not PPC64.  Declared doctrine exception:
;;; PPC64's file assumes PPC tag/register models with no analog here.
;;;
;;; arm64-threads-utils.lisp — per-arch threads/frames predicates for Matt
;;; Emerson's upstream ARM64 (low-tag) design.
;;;
;;; Donor: level-1/x86-threads-utils.lisp (#+x8664-target branches) — the
;;; arm64 tag geometry mirrors x8664's (5 header classes split across
;;; fulltag-immheader-0/1/2 + fulltag-nodeheader-0/1; dedicated symbol and
;;; function pointer fulltags), cited "; x86:NNN".  Deviations:
;;;  - NO tagged-return-address (TRA) cases: AArch64 return addresses live
;;;    in lr/stack frames, untagged, as on PPC (x8664's fulltag-tra-0/1
;;;    clauses in valid-header-p/bogus-thing-p have no analog).
;;;  - catch-frame-sp is the PPC shape (ppc-threads-utils.lisp:84): catch
;;;    frames are misc-tagged uvectors on the temp stack with an explicit
;;;    csp slot (kernel ground truth: spentry-C-bind-catch-throw.s
;;;    _structf catch_frame + mkcatch), not x8664's stack-consed
;;;    rbp-cell frame.
;;;
;;; The lfun-bits &optional fixups are the PPC file's canonical four
;;; (ppc-threads-utils.lisp:25-55; the x86 file duplicates the first pair).

(in-package "CCL")

;;; %frame-backlink and lisp-frame-p live in lib/arm64-backtrace.lisp
;;; (16m17): they need the fake-stack-frame accessor macros at compile
;;; time, and the cross gate compiles each file in a host image that
;;; only gets those macros inside that compile unit.  (The x86-donor
;;; versions that used to sit here checked the VALUE stack — an x8664
;;; frame model; this design's lisp frames are marker frames on the
;;; CONTROL stack, kernel arm64-gc.c mark_cstack_area.)

;;; %%frame-backlink (one step toward the parent/older frame) is a
;;; LEVEL-0 LAP primitive (level-0/ARM64/arm64-def.lisp:~300, promoted
;;; 16m21).  Under the DECIDED cstack-walk design (Option A,
;;; comms/ARM64-CSTACK-WALK-DECISION.md), the cstack is a HOMOGENEOUS
;;; chain of 32-byte marker frames (nfp + stack-cons live on the TSP),
;;; so the walk is a plain +32 stride — NOT the ARM32-twin heterogeneous
;;; ivector-skip decode (Option B, explicitly not chosen).  It carries a
;;; marker@0 rather than a stored backlink, unlike PPC's one-load
;;; %%frame-backlink (ppc-def.lisp:227-230).

(defun bottom-of-stack-p (p context)            ; ppc-threads-utils:87-92
  (and (fixnump p)
       (locally (declare (fixnum p))
	 (let* ((tcr (if context (bt.tcr context) (%current-tcr)))
                (cs-area (%fixnum-ref tcr target::tcr.cs-area)))
	   (not (%ptr-in-area-p p cs-area))))))

;;; Catch frames are misc-tagged temp-stack uvectors with a csp slot, as
;;; on PPC (kernel: spentry-C-bind-catch-throw.s mkcatch); ppc:84 shape.
(defun catch-frame-sp (catch)
  (uvref catch target::catch-frame.csp-cell))

;;; Sure would be nice to have &optional in defarm64lapfunction arglists
;;; Sure would be nice not to do this at runtime.

(let ((bits (lfun-bits #'(lambda (x &optional y) (declare (ignore x y))))))
  (lfun-bits #'%fixnum-ref
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-ref)))))

(let ((bits (lfun-bits #'(lambda (x &optional y) (declare (ignore x y))))))
  (lfun-bits #'%fixnum-ref-natural
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-ref-natural)))))

(let ((bits (lfun-bits #'(lambda (x y &optional z) (declare (ignore x y z))))))
  (lfun-bits #'%fixnum-set
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-set)))))

(let ((bits (lfun-bits #'(lambda (x y &optional z) (declare (ignore x y z))))))
  (lfun-bits #'%fixnum-set-natural
             (dpb (ldb $lfbits-numreq bits)
                  $lfbits-numreq
                  (dpb (ldb $lfbits-numopt bits)
                       $lfbits-numopt
                       (lfun-bits #'%fixnum-set-natural)))))

(defun valid-subtag-p (subtag)                  ; x86:115 (#+x8664-target)
  (declare (fixnum subtag))
  (let* ((tagval (logand arm64::fulltagmask subtag))
         (high4 (ash subtag (- arm64::ntagbits))))
    (declare (fixnum tagval high4))
    (not (eq 'bogus
             (case tagval
               (#.arm64::fulltag-immheader-0
                (%svref *immheader-0-types* high4))
               (#.arm64::fulltag-immheader-1
                (%svref *immheader-1-types* high4))
               (#.arm64::fulltag-immheader-2
                (%svref *immheader-2-types* high4))
               (#.arm64::fulltag-nodeheader-0
                (%svref *nodeheader-0-types* high4))
               (#.arm64::fulltag-nodeheader-1
                (%svref *nodeheader-1-types* high4))
               (t 'bogus))))))

(defun valid-header-p (thing)                   ; x86:144 (#+x8664-target)
  (let* ((fulltag (fulltag thing)))
    (declare (fixnum fulltag))
    (case fulltag
      ((#.arm64::fulltag-even-fixnum
        #.arm64::fulltag-odd-fixnum
        #.arm64::fulltag-single-float
        #.arm64::fulltag-imm-0
        #.arm64::fulltag-imm-1)
       t)
      ;; (fulltag-function removed, patch 0055: functions are ordinary
      ;;  miscobjs and take the fulltag-misc clause below.)
      (#.arm64::fulltag-symbol
       (= arm64::subtag-symbol (typecode (%symptr->symvector thing))))
      (#.arm64::fulltag-misc
       (valid-subtag-p (typecode thing)))
      ;; x8664's fulltag-tra-0/tra-1 clauses have no arm64 analog.
      (#.arm64::fulltag-cons t)
      (#.arm64::fulltag-nil (null thing))
      (t nil))))

(defun bogus-thing-p (x)                        ; x86:190 (#+x8664-target)
  (when x
    (or (not (valid-header-p x))
        (let* ((tag (lisptag x)))
          (unless (or (eql tag arm64::tag-fixnum)
                      (eql tag arm64::tag-single-float)
                      (eql tag arm64::tag-imm)
                      (in-any-consing-area-p x)
                      (temporary-cons-p x)
                      (and (or (typep x 'function)
                               (typep x 'gvector))
                           (on-any-tsp-stack x))
                      ;; x8664's tag-tra clause has no arm64 analog.
                      (and (typep x 'ivector)
                           (on-any-csp-stack x))
                      (%heap-ivector-p x))
            t)))))
