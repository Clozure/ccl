;;;; Mach-on smoke: UUOs, compile, fixed-arity FFI.
(in-package :ccl)

(format t "~&eval ~s~%" (+ 10 20 30))
(finish-output)

(format t "~&compile ~s~%"
        (funcall (compile nil
                          (lambda (n)
                            (labels ((fib (k)
                                       (if (< k 2) k
                                           (+ (fib (1- k)) (fib (- k 2))))))
                              (fib n))))
                 20))
(finish-output)

(let ((a (foreign-symbol-address "getpid")))
  (format t "~&pid ~s~%" (ff-call a :signed-fullword)))
(finish-output)

(let ((a (foreign-symbol-address "strlen")))
  (with-cstrs ((c "mach-ok"))
    (format t "~&strlen ~s~%" (ff-call a :address c :unsigned-fullword))))
(finish-output)

(format t "~&type-err ~s~%" (nth-value 1 (ignore-errors (car 1))))
(finish-output)

(format t "~&div-err ~s~%" (nth-value 1 (ignore-errors (/ 1 0))))
(finish-output)

(format t "~&hash ~s~%"
        (let ((h (make-hash-table)))
          (setf (gethash :a h) 99)
          (gethash :a h)))
(finish-output)

(format t "~&MACH-SMOKE-OK~%")
(quit 0)
