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

#+darwinarm64-target
(defun %code-vector-in-jit-area-p (x)
  "True if X is a code vector in the lisp kernel's MAP_JIT code area."
  (and (eql (typecode x) arm64::subtag-code-vector)
       (not (eql 0 (external-call "darwin_arm64_in_code_heap"
                                  :unsigned-doubleword (%address-of x)
                                  :signed-fullword)))))

(defun bogus-thing-p (x)
  (when x
    (or (not (valid-header-p x))
        (let* ((tag (lisptag x)))
          (unless (or (eql tag arm64::tag-fixnum)
                      (eql tag arm64::tag-single-float)
                      (eql tag arm64::tag-imm)
                      (in-any-consing-area-p x)
                      ;; Dynamic-extent objects of every type, conses
                      ;; and %stack-block macptrs included, live on a
                      ;; tstack, as on PPC.  Nothing lisp can see lives
                      ;; on the cstack or the vstack.
                      (on-any-tsp-stack x)
                      (%heap-ivector-p x)
                      ;; Code vectors live in jit_area, which isn't on
                      ;; the area list.
                      #+darwinarm64-target
                      (%code-vector-in-jit-area-p x))
            t)))))
