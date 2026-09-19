;;;; Smoke: arm64 inspector disassemble-lines.
;;;;   ./darm64cl --no-init --batch < tools/darwin-disassemble-lines-smoke.lisp
(in-package :ccl)
(setq *warn-if-redefine-kernel* nil)

(format t "~&;; darwin-disassemble-lines-smoke~%")
(finish-output)

;; Tip definition (image may lack it).
(load "ccl:compiler;ARM64;arm64-disassemble.lisp")

(assert (fboundp 'disassemble-lines))

(let* ((fn #'cons)
       (lines (disassemble-lines fn)))
  (assert (typep lines 'simple-vector) () "expected simple-vector, got ~s" (type-of lines))
  (assert (plusp (length lines)) () "empty disassembly")
  (let ((insn-lines (loop for line across lines
                          when (consp line) collect line)))
    (assert (plusp (length insn-lines)) () "no instruction lines")
    (destructuring-bind (object label instr) (car insn-lines)
      (declare (ignore object))
      (assert (or (integerp label) (and (consp label) (eq (car label) :label)))
              () "bad label ~s" label)
      (assert (stringp instr) () "bad instr ~s" instr)
      (format t "~&;; first line label=~s instr=~s~%" label instr)))
  (format t "~&;; ~d lines (~d comments)~%"
          (length lines)
          (count-if #'stringp lines)))

;; Named non-global (type-class style) — what the inspector hit.
(let* ((fn (symbol-function 'typep))
       (lines (disassemble-lines fn)))
  (assert (plusp (length lines)))
  (format t "~&;; typep => ~d lines~%" (length lines)))

(format t "~&;; PASS darwin-disassemble-lines-smoke~%")
(quit 0)
)
