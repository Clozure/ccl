;;;; Smoke: Darwin/arm64 libc CDB struct stat size vs C sizeof.
;;;;
;;;;   cc -arch arm64 -o /tmp/stat_sizeof /tmp/stat_sizeof.c && /tmp/stat_sizeof
;;;;   ./darm64cl --no-init --batch < tools/darwin-cdb-stat-smoke.lisp
(in-package :ccl)
(use-interface-dir :libc)
(defun cdb-stat-size ()
  (ccl::record-length :stat))
(let* ((cdb (cdb-stat-size))
       (c (let ((p (run-program "/tmp/stat_sizeof" () :output :stream :wait t)))
            (prog1 (parse-integer (read-line (external-process-output-stream p)))
              (close (external-process-output-stream p))))))
  (format t "~&cdb :stat =~s  C sizeof(struct stat)=~s~%" cdb c)
  (unless (eql cdb c)
    (error "struct stat size mismatch: cdb=~s c=~s (x86-copy CDB?)" cdb c)))
(format t "~&DARWIN-CDB-STAT-SMOKE-OK~%")
(quit)
