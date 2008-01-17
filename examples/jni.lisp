(eval-when (:compile-toplevel :execute)
  (use-interface-dir :java))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (open-shared-library "/System/Library/Frameworks/JavaVM.framework/JavaVM"))

(defun %init-java-vm (&rest args)
  (declare (dynamic-extent args))
  (let* ((nargs (length args)))
    (rlet ((initargs :<J>ava<VMI>nit<A>rgs)
	   (env (* :<JNIE>nv))
	   (vm (* :<J>ava<VM>)))
      (%stack-block ((options (* nargs (record-length :<J>ava<VMO>ption))))
	(do* ((i 0 (1+ i))
	      (args args (cdr args))
	      (p options (%inc-ptr p (record-length :<J>ava<VMO>ption))))
	     ((= i nargs))
	  (setf (pref p :<J>ava<VMO>ption.option<S>tring)
		(car args)))
	(setf (pref initargs :<J>ava<VMI>nit<A>rgs.version) #$JNI_VERSION_1_2
	      (pref initargs :<J>ava<VMI>nit<A>rgs.n<O>ptions) nargs
	      (pref initargs :<J>ava<VMI>nit<A>rgs.options) options
	      (pref initargs :<J>ava<VMI>nit<A>rgs.ignore<U>nrecognized) #$JNI_TRUE)
	(let* ((result (#_JNI_CreateJavaVM  :monitor-exception-ports vm env initargs)))
	  (if (>= result 0)
	    (values (%get-ptr vm) (%get-ptr env))
	    (error "Can't create Java VM: result = ~d" result)))))))
	      
	      