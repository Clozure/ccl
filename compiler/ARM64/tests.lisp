(in-package "ARM64")

;;; functions for testing logical immediate encoding

(defun test-ctz ()
  (let* ((n #xffffffffffffffff))
    (loop for i from 0 to 64 do
          (format t "~16,'0x: " n)
          (format t "~d~%" (count-trailing-zeros-64 n))
          (setq n (ldb (byte 64 0) (ash n 1))))))

(defun test-clz ()
  (let* ((all-ones #xffffffffffffffff)
         (n all-ones))
    (loop for i from 0 to 64 do
          (format t "~16,'0x: " n)
          (format t "~2d~%" (count-leading-zeros-64 n))
          (setq n (ldb (byte 64 0) (ash n -1))))))

(defun all-logical-immediates ()
  "Return a list of all possible encoded logical immediates."
  ;; https://gist.github.com/dinfuehr/9e1c2f28d0f912eae5e595207cb835c2
  (flet ((encode-imms (size length)
           (logior length (ecase size
                            (2  #b111100)
                            (4  #b111000)
                            (8  #b110000)
                            (16 #b100000)
                            ((32 64) #b000000)))))
    (let ((results nil))
      (dolist (size '(2 4 8 16 32 64))
        (loop for length from 0 below (1- size) do
              (loop for rotation from 0 below size do
                    (let ((n (if (= size 64) 1 0))
                          (immr rotation)
                          (imms (encode-imms size length)))
                      (push (logior (ash n 12)
                                    (ash immr 6)
                                    (ldb (byte 6 0) imms))
                            results)))))
      (nreverse results))))

(defun test-logical-immediate-encode-decode (&optional show-values)
  (let ((values (all-logical-immediates)))
    (assert (= (length values) 5334))
    (dolist (val values t)
      (let ((decoded (decode-logical-immediate val)))
        (assert (not (null decoded)))
        (assert (= val (encode-logical-immediate decoded)))
        (when show-values
          (let ((n (ldb (byte 1 12) val))
                (immr (ldb (byte 6 6) val))
                (imms (ldb (byte 6 0) val)))
            (format t "~&~(~16,'0x~) ~64,'0b N=~b immr=~6,'0b imms=~6,'0b" decoded decoded
                    n immr imms)))))))

(defun test-fp-imm8 ()
  (dotimes (i 256 t)
    (let* ((decoded (decode-fp-imm8 i))
           (encoded (encode-fp-imm8 decoded)))
      (assert (= encoded i)))))
