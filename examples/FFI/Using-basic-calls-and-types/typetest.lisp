(defun message (string)
  (format t "~a~%~%" string)
  (force-output))

;; Setup
(message "*** Building the shared library")
(run-program (namestring
	      (translate-logical-pathname #P"ccl:examples;FFI;Using-basic-calls-and-types;typetest-compile.sh"))
	     (list
	      (namestring
	       (translate-logical-pathname #P"ccl:examples;FFI;Using-basic-calls-and-types")))
	     :output t)
(message (format nil "*** Shared libraries (before load): ~a~%~%*** Loading the shared library"ccl::*shared-libraries*))
(open-shared-library (namestring
		      (translate-logical-pathname #P"ccl:examples;FFI;Using-basic-calls-and-types;libtypetest.dylib")))
(message (format nil "*** Shared libraries (after load): ~a"ccl::*shared-libraries*))

;; First set of tutorial function calls
(message "*** Calling first group of test functions")
; test the basics
(message
 (format nil
	 "_void_void_test: ~a"
	 (external "_void_void_test")))
(message
 (format nil
	 "_sc_sc_test: ~a"
	 (external "_sc_sc_test")))
(message
 (format nil
	 "_uc_uc_test: ~a"
	 (external "_uc_uc_test")))
(message
 (format nil
	 "functiondoesnotexist: ~a"
(external "functiondoesnotexist")))
(message
 (format nil
	 "_void_void_test returned: ~a"
	 (external-call "_void_void_test"
			:void)))
(message
 (format nil
	 "_sc_sc_test returned: ~a"
	 (external-call "_sc_sc_test"
			:signed-byte -128
			:signed-byte)))
; value exceeding limit and clips
(message "* The following calls will exceed limits and clip results:")
(message
 (format nil
	 "_sc_sc_test returned: ~a"
	 (external-call "_sc_sc_test"
			:signed-byte -567
			:signed-byte)))
(message
 (format nil
	 "_uc_uc_test returned: ~a"
	 (external-call "_uc_uc_test"
			:unsigned-byte 255
			:unsigned-byte)))
(message
 (format nil
	 "_uc_uc_test returned: ~a"
	 (external-call "_uc_uc_test"
			:unsigned-byte 567
			:unsigned-byte)))
(message
 (format nil
	 "_uc_uc_test returned: ~a"
	 (external-call "_uc_uc_test"
			:unsigned-byte -567
			:unsigned-byte)))

;; Second set of tutorial function calls
(message "*** Calling second group of test functions")
(message
 (format nil
	 "_si_si_test returned: ~a"
	 (external-call "_si_si_test"
			:signed-fullword -178965
			:signed-fullword)))
(message "* Longs are the same size as ints")
(message
 (format nil
	 "_sl_sl_test returned: ~a"
	 (external-call "_sl_sl_test"
			:signed-fullword -178965
			:signed-fullword)))
(message
 (format nil
	 "_sll_sll_test returned: ~a"
	 (external-call "_sll_sll_test"
			:signed-doubleword -973891578912
			:signed-doubleword)))
(message "* Mistakenly calling sl_sl_test() for sll_sll_test(), thinking that a long is actually a doubleword:")
(message
 (format nil
	 "_sl_sl_test returned: ~a"
	 (external-call "_sl_sl_test"
			:signed-doubleword -973891578912
			:signed-doubleword)))

;; Third set of tutuorial function calls
(message "*** Calling the third group of test functions")

(message
 (format nil
	 "_f_f_test returned: ~a"
	 (external-call "_f_f_test"
			:single-float -1.256791e+11
			:single-float)))
(message
 (format nil
	 "_d_d_test returned: ~a"
	 (external-call "_d_d_test"
			:double-float -1.256791d+290
			:double-float)))
