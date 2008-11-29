;    Copyright (c) Rich Hickey. All rights reserved.
;    The use and distribution terms for this software are covered by the
;    Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;    which can be found in the file CPL.TXT at the root of this distribution.
;    By using this software in any fashion, you are agreeing to be bound by
;    the terms of this license.
;    You must not remove this notice, or any other, from this software.

#|
This is a straight wrapper around the JNI API
Originally I intended to expose this API directly, but it turns out
that JNI is very sensitive to errors, and, given bad args, wrong types etc
causes the JVM (and Lisp) to crash, not very much in the spirit of safe, robust,
interactive development offered by Lisp

So, now this just forms the substrate under jfli, which uses the Reflection API, and is much
more robust and error tolerant, at some cost in speed I guess.

Bottom line is you shouldn't be using this API directly unless you are extending jfli,
and then you must take care not to allow bad end-user data to pass through to JNI. 

Caveat emptor.

I have tried to limit LispWorks FLI code to this file.
|#

(defpackage :jni
  (:export
   :*jni-lib-path*
   :*pvm*
   :*penv*
   :register-invocation-handler
   :create-jvm
   :JNI-VERSION-1-2
   :JNI-VERSION-1-4
   :JNI-OK
   :java-ref
   :jvoid :jboolean :jbyte :jchar :jshort :jint :jlong :jfloat :jdouble :jsize
   :jobject :jclass :jthrowable :jstring :jarray
   :jboolean-array :jbyte-array :jchar-array :jshort-array :jint-array :jlong-array
   :jfloat-array :jdouble-array :jobject-array
   :jfield-id :jmethod-id :jweak
   :pvm :penv
   :jvalue
   :arg-array
   :jni-native-method :jni-env
   :java-vm :java-vm-option :jdk-1-1-init-args
   :jni-get-default-java-vm-init-args :java-vm-inits-args
   :jni-create-java-vm :jni-get-created-java-vms
   :try :try-null :try-neg
   :local-ref-to-global-ref :local-ref-to-string
   :def-jni-function :def-jni-functions :def-jni-constructor :def-jni-field
   :jaref :convert-to-java-string :convert-from-java-string :java-ref-p
   :is-name-of-primitive :split-package-and-class
   ;; Export JNIEnv function names, too
   :get-array-length :is-same-object :jni-find-class :is-assignable-from
   :delete-local-ref :new-object-array :new-int-array
   ))

(in-package :jni)

(defclass java-object (ccl::foreign-standard-object)
    ())

(ccl::defloadvar *java-object-domain*
    (ccl::register-foreign-object-domain :java
                                         :recognize #'ccl::false
                                         :class-of (lambda (x)
                                                     (declare (ignore x))
                                                     (find-class 'java-object))
                                         :classp #'ccl::false
                                         :instance-class-wrapper
                                         (lambda (x)
                                           (declare (ignore x))
                                           (ccl::class-own-wrapper
                                            (find-class 'java-object)))
                                         :class-own-wrapper
                                         #'ccl::false
                                         :slots-vector #'ccl::false
                                         :class-ordinal #'ccl::false
                                         :set-class-ordinal
                                         #'ccl::false))

(deftype java-ref () 'java-object)

(defun java-ref-p (x)
  (and (eql (ccl::typecode x) target::subtag-macptr)
       (eql (ccl::%macptr-domain x) *java-object-domain*)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :jni))

(defun string-append (&rest args)
  (declare (dynamic-extent args))
  (do* ((a args (cdr a)))
     ((null a) (apply #'concatenate 'string args))
    (let* ((arg (car a)))
      (unless (typep arg 'string)
        (setf (car a) (string arg))))))

(defvar *jni-lib-path*
#+:darwin-target "/System/Library/Frameworks/JavaVM.framework/JavaVM"
#+:win32-target "C:/j2sdk1.4.2_01/jre/bin/client/jvm.dll"
"Set this to point to your jvm dll prior to calling create-jvm")

(ccl::defloadvar *pvm* nil)

;;; Map between lisp and Java booleans
(eval-when (:compile-toplevel)
  (declaim (inline jboolean-arg jboolean-result jobject-result)))

(defun jboolean-arg (val)
  (if (and val (not (eql val #$JNI_FALSE)))
    #$JNI_TRUE
    #$JNI_FALSE))

(defun jboolean-result (val)
  (not (eql val #$JNI_FALSE)))

;;; Might also want to register p for termination (finalization).
(defun jobject-result (val)
  (unless (ccl::%null-ptr-p val)
    (ccl::%set-macptr-domain val *java-object-domain*))
  val)



(defconstant JNI-VERSION-1-2 #$JNI_VERSION_1_2)
(defconstant JNI-VERSION-1-4 #$JNI_VERSION_1_4)
(defconstant JNI-OK #$JNI_OK)

(defun load-jni-lib (&optional (libpath *jni-lib-path*))
  (ccl:open-shared-library libpath))

(defun current-env ()
  "return a pointer to the current thread's JNIEnv, creating that environment
if necessary."
  (rlet ((pjnienv :address))
    (let* ((jvm (get-pvm)))
      (unless (eql jni-ok
                   (ff-call (pref jvm #>JavaVM.GetEnv)
                            :address jvm
                            :address pjnienv
                            :jint jni-version-1-4
                            :jint))
        ;; On Darwin, attaching the current thread to a JVM instance
        ;; overwrites the thread's Mach exception ports, which CCL
        ;; happens to be using.  We can work around this by calling
        ;; a function in the CCL kernel and having that function
        ;; call the vm's AttachCurrentThread function and then restore
        ;; the thread's exception ports before returning.  Yes, that
        ;; -is- total nonsense.
        (unless (eql jni-ok
                     (ff-call
                      (ccl::%kernel-import target::kernel-import-jvm-init)
                      :address (pref jvm #>JavaVM.AttachCurrentThread)
                      :address jvm
                      :address pjnienv
                      :address (ccl::%null-ptr)
                      :jint))
          (error "Can't attach thread to JVM ~s" jvm)))
      (let* ((result (pref pjnienv :address)))
        (ccl::%set-macptr-type result (load-time-value (ccl::foreign-type-ordinal (ccl::foreign-pointer-type-to (ccl::parse-foreign-type #>JNIEnv)))))
        result))))


;;; JNIEnv functions.

(defmacro jnienv-call ((slot result-type) &rest specs)
  ;; We might want to special-case some result-types for finalization.
  (let* ((env (gensym))
         (accessor (ccl::escape-foreign-name (concatenate 'string "JNIEnv." slot)))
         (form
          `(let* ((,env (current-env)))
            (ff-call (pref ,env ,accessor) :address ,env ,@specs ,result-type))))
    (case result-type
      (:jboolean `(jboolean-result ,form))
      ((:jobject :jclass :jstring :jthrowable :jarray #>jbooleanArray
                 #>jbyteArray #>jcharArray #>jshortArray #>jintArray
                 #>jlongArray #>jfloatArray #>jdoubleArray #>jobjectArray)
       `(jobject-result ,form))
      (t form))))
                 

(defun get-version ()
  (jnienv-call ("GetVersion" :jint)))

(defun define-class (name loader buf len)
  (ccl::with-utf-8-cstrs ((cname name))
    (jnienv-call ("DefineClass" :jclass) 
                 :address cname
                 :jobject loader
                 (:* :jbyte) buf
                 :jsize len)))

(defun jni-find-class (name)
  (ccl::with-utf-8-cstrs ((cname name))
    (jnienv-call ("FindClass" :jclass) :address cname)))


(defun from-reflected-method (method)
  (jnienv-call ("FromReflectedMethod" #>jmethodID) :jobject method))

(defun from-reflected-field (field)
  (jnienv-call ("FromReflectedField" #>jfieldID) :jobject field))

(defun to-reflected-method (cls method-id is-static)
  
  (jnienv-call ("ToReflectedMethod" :jobject)
               :jclass cls
               #>jmethodID method-id
               :jboolean (jboolean-arg is-static)))

(defun get-superclass (sub)
  (jnienv-call ("GetSuperclass" :jclass) :jclass sub))

(defun is-assignable-from (sub sup)
  
  (jnienv-call ("IsAssignableFrom" :jboolean) :jclass sub :jclass sup))

(defun to-reflected-field (cls field-id is-static)
  
  (jnienv-call ("ToReflectedField" :jobject)
               :jclass cls
               #>jfieldID field-id
               :jboolean (jboolean-arg is-static)))

(defun jni-throw (obj)
  (jnienv-call ("Throw" :jint) :jthrowable obj))

(defun throw-new (clazz msg)
  (ccl::with-utf-8-cstrs ((cmsg msg))
    (jnienv-call ("ThrowNew" :jint) :jclass clazz :address cmsg)))

(defun exception-occurred ()
  (jnienv-call ("ExceptionOccurred" :jthrowable)))

(defun exception-describe ()
  (jnienv-call ("ExceptionDescribe" :void)))

(defun exception-clear ()
  (jnienv-call ("ExceptionClear" :void)))

(defun fatal-error (msg)
  (ccl::with-utf-8-cstrs ((cmsg msg))
    (jnienv-call ("FatalError" :void) :address cmsg)))
  
(defun push-local-frame (capacity)
  (jnienv-call ("PushLocalFrame" :jint) :jint capacity))

(defun pop-local-frame (result)
  
  (jnienv-call ("PopLocalFrame" :jobject) :jobject result))

(defun new-global-ref (lobj)
  (jnienv-call ("NewGlobalRef" :jobject) :jobject lobj))

(defun delete-global-ref (gref)
  (jnienv-call ("DeleteGlobalRef" :void) :jobject gref))
  
(defun delete-local-ref (obj)
  (jnienv-call ("DeleteLocalRef" :void) :jobject obj))

(defun is-same-object (obj1 obj2)
  
  (jnienv-call ("IsSameObject" :jboolean) :jobject obj1 :jobject obj2))

(defun new-local-ref (ref)
  
  (jnienv-call ("NewLocalRef" :jobject) :jobject ref))

(defun ensure-local-capacity (capacity)
  (jnienv-call ("EnsureLocalCapacity" :jint) :jint capacity))

(defun alloc-object (clazz)
  (jnienv-call ("AllocObject" :jobject) :jclass clazz))

;;; We probably can't get very far with NewObject or NewObjectV, which
;;; depend on the underlying varargs mechanism.  NewObjectA is more
;;; tractable.

(defun new-object-a (clazz method-id args)
  
  (jnienv-call ("NewObjectA" :jobject) :jclass clazz #>jmethodID method-id (:* :jvalue) args))

(defun get-object-class (obj)
  (jnienv-call ("GetObjectClass" :jclass) :jobject obj))

(defun is-instance-of (obj clazz)
  
  (jnienv-call ("IsInstanceOf" :jboolean) :jobject obj :jclass clazz))

(defun get-method-id (clazz name sig)
  (ccl::with-utf-8-cstrs ((cname name)
                          (csig sig))
    (jnienv-call ("GetMethodID" #>jmethodID)
                 :jclass clazz :address cname :address csig)))

;;; Likewise for Call*Method and Call*MethodV vs Call*MethodA.

(defun call-object-method-a (obj method-id args)
  (jnienv-call ("CallObjectMethodA" :jobject)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-boolean-method-a (obj method-id args)
  
  (jnienv-call ("CallBooleanMethodA" :jboolean)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-byte-method-a (obj method-id args)
  (jnienv-call ("CallByteMethodA" :jbyte)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-byte-method-a (obj method-id args)
  (jnienv-call ("CallCharMethodA" :jchar)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-short-method-a (obj method-id args)
  (jnienv-call ("CallShortMethodA" :jshort)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-int-method-a (obj method-id args)
  (jnienv-call ("CallIntMethodA" :jint)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-long-method-a (obj method-id args)
  (jnienv-call ("CallLongMethodA" :jlong)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-float-method-a (obj method-id args)
  (jnienv-call ("CallFloatMethodA" :jfloat)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-double-method-a (obj method-id args)
  (jnienv-call ("CallDoubleMethodA" :jdouble)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-void-method-a (obj method-id args)
  (jnienv-call ("CallVoidMethodA" :void)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

;;; Nonvirtual method calls.
(defun call-nonvirtual-object-method-a (obj method-id args)
  (jnienv-call ("CallNonvirtualObjectMethodA" :jobject)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-nonvirtual-boolean-method-a (obj method-id args)
  
  (jnienv-call ("CallNonvirtualBooleanMethodA" :jboolean)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-nonvirtual-byte-method-a (obj method-id args)
  (jnienv-call ("CallNonvirtualByteMethodA" :jbyte)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-nonvirtual-char-method-a (obj method-id args)
  (jnienv-call ("CallNonvirtualCharMethodA" :jchar)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-nonvirtual-short-method-a (obj method-id args)
  (jnienv-call ("CallNonvirtualShortMethodA" :jshort)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))


(defun call-nonvirtual-int-method-a (obj method-id args)
  (jnienv-call ("CallNonvirtualIntMethodA" :jint)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-nonvirtual-long-method-a (obj method-id args)
  (jnienv-call ("CallNonvirtualLongMethodA" :jlong)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-nonvirtual-float-method-a (obj method-id args)
  (jnienv-call ("CallNonvirtualFloatMethodA" :jfloat)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-nonvirtual-double-method-a (obj method-id args)
  (jnienv-call ("CallNonvirtualDoubleMethodA" :jdouble)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-nonvirtual-void-method-a (obj method-id args)
  (jnienv-call ("CallNonvirtualVoidMethodA" :void)
               :jobject obj
               #>jmethodID method-id
               (:* :jvalue) args))

(defun get-field-id (clazz name sig)
  (ccl::with-utf-8-cstrs ((cname name)
                          (csig sig))
    (jnienv-call ("GetFieldID" #>jfieldID)
                 :jclass clazz
                 :address cname
                 :address csig)))

(defun get-object-field (obj field-id)
  
  (jnienv-call ("GetObjectField" :jobject)
               :jobject obj
               #>jfieldID field-id))

(defun get-boolean-field (obj field-id)
  
  (jnienv-call ("GetBooleanField" :jboolean)
               :jobject obj
               #>jfieldID field-id))

(defun get-byte-field (obj field-id)
  (jnienv-call ("GetByteField" :jbyte)
               :jobject obj
               #>jfieldID field-id))

(defun get-char-field (obj field-id)
  (jnienv-call ("GetCharField" :jchar)
               :jobject obj
               #>jfieldID field-id))

(defun get-short-field (obj field-id)
  (jnienv-call ("GetShortField" :jshort)
               :jobject obj
               #>jfieldID field-id))


(defun get-int-field (obj field-id)
  (jnienv-call ("GetIntField" :jint)
               :jobject obj
               #>jfieldID field-id))

(defun get-long-field (obj field-id)
  (jnienv-call ("GetLongField" :jlong)
               :jobject obj
               #>jfieldID field-id))

(defun get-float-field (obj field-id)
  (jnienv-call ("GetFloatField" :jfloat)
               :jobject obj
               #>jfieldID field-id))

(defun get-double-field (obj field-id)
  (jnienv-call ("GetDoubleField" :jdouble)
               :jobject obj
               #>jfieldID field-id))

(defun set-object-field (obj field-id val)
  (jnienv-call ("SetObjectField" :void)
               :jobject obj
               #>jfieldID field-id
               :jobject val))

(defun set-boolean-field (obj field-id val)
  (jnienv-call ("SetBooleanField" :void)
               :jobject obj
               #>jfieldID field-id
               :jboolean (jboolean-arg val)))

(defun set-byte-field (obj field-id val)
  (jnienv-call ("SetByteField" :void)
               :jobject obj
               #>jfieldID field-id
               :jbyte val))

(defun set-char-field (obj field-id val)
  (jnienv-call ("SetCharField" :void)
               :jobject obj
               #>jfieldID field-id
               :jchar val))

(defun set-short-field (obj field-id val)
  (jnienv-call ("SetShortField" :void)
               :jobject obj
               #>jfieldID field-id
               :jshort val))

(defun set-int-field (obj field-id val)
  (jnienv-call ("SetIntField" :void)
               :jobject obj
               #>jfieldID field-id
               :jint val))

(defun set-long-field (obj field-id val)
  (jnienv-call ("SetLongField" :void)
               :jobject obj
               #>jfieldID field-id
               :jlong val))

(defun set-float-field (obj field-id val)
  (jnienv-call ("SetFloatField" :void)
               :jobject obj
               #>jfieldID field-id
               :jfloat val))

(defun set-double-field (obj field-id val)
  (jnienv-call ("SetDoubleField" :void)
               :jobject obj
               #>jfieldID field-id
               :jdouble val))

(defun get-static-method-id (clazz name sig)
  (ccl::with-utf-8-cstrs ((cname name)
                          (csig sig))
    (jnienv-call ("GetStaticMethodID" #>jmethodID)
                 :jclass clazz
                 :address cname
                 :address csig)))

(defun call-static-object-method-a (clazz method-id args)
  
  (jnienv-call ("CallStaticObjectMethodA" :jobject)
               :jclass clazz
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-static-boolean-method-a (clazz method-id args)
  
  (jnienv-call ("CallStaticBooleanMethodA" :jboolean)
               :jclass clazz
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-static-byte-method-a (clazz method-id args)
  (jnienv-call ("CallStaticByteMethodA" :jbyte)
               :jclass clazz
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-static-char-method-a (clazz method-id args)
  (jnienv-call ("CallStaticCharMethodA" :jchar)
               :jclass clazz
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-static-short-method-a (clazz method-id args)
  (jnienv-call ("CallStaticShortMethodA" :jshort)
               :jclass clazz
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-static-int-method-a (clazz method-id args)
  (jnienv-call ("CallStaticIntMethodA" :jint)
               :jclass clazz
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-static-long-method-a (clazz method-id args)
  (jnienv-call ("CallStaticLongMethodA" :jlong)
               :jclass clazz
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-static-float-method-a (clazz method-id args)
  (jnienv-call ("CallStaticFloatMethodA" :jfloat)
               :jclass clazz
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-static-double-method-a (clazz method-id args)
  (jnienv-call ("CallStaticDoubleMethodA" :jdouble)
               :jclass clazz
               #>jmethodID method-id
               (:* :jvalue) args))

(defun call-static-void-method-a (clazz method-id args)
  (jnienv-call ("CallStaticVoidMethodA" :void)
               :jclass clazz
               #>jmethodID method-id
               (:* :jvalue) args))

(defun get-static-field-id (clazz name sig)
  (ccl::with-utf-8-cstrs ((cname name)
                          (csig sig))
    (jnienv-call ("GetStaticFieldID" #>jfieldID)
                 :jclass clazz
                 :address cname
                 :address csig)))

(defun get-static-object-field (clazz field-id)
  (jnienv-call ("GetStaticObjectField" :jobject)
               :jclass clazz
               #>jfieldID field-id))

(defun get-static-boolean-field (clazz field-id)
  
  (jnienv-call ("GetStaticBooleanField" :jboolean)
               :jclass clazz
               #>jfieldID field-id))

(defun get-static-byte-field (clazz field-id)
  (jnienv-call ("GetStaticByteField" :jbyte)
               :jclass clazz
               #>jfieldID field-id))

(defun get-static-char-field (clazz field-id)
  (jnienv-call ("GetStaticCharField" :jchar)
               :jclass clazz
               #>jfieldID field-id))

(defun get-static-short-field (clazz field-id)
  (jnienv-call ("GetStaticShortField" :jshort)
               :jclass clazz
               #>jfieldID field-id))

(defun get-static-int-field (clazz field-id)
  (jnienv-call ("GetStaticIntField" :jint)
               :jclass clazz
               #>jfieldID field-id))

(defun get-static-long-field (clazz field-id)
  (jnienv-call ("GetStaticLongField" :jlong)
               :jclass clazz
               #>jfieldID field-id))

(defun get-static-float-field (clazz field-id)
  (jnienv-call ("GetStaticFloatField" :jfloat)
               :jclass clazz
               #>jfieldID field-id))

(defun get-static-double-field (clazz field-id)
  (jnienv-call ("GetStaticDoubleField" :jdouble)
               :jclass clazz
               #>jfieldID field-id))


(defun set-static-object-field (clazz field-id value)
  (jnienv-call ("SetStaticObjectField" :void)
               :jclass clazz
               #>jfieldID field-id
               :jobject value))

(defun set-static-boolean-field (clazz field-id value)
  (jnienv-call ("SetStaticBooleanField" :void)
               :jclass clazz
               #>jfieldID field-id
               :jboolean (jboolean-arg value)))

(defun set-static-byte-field (clazz field-id value)
  (jnienv-call ("SetStaticByteField" :void)
               :jclass clazz
               #>jfieldID field-id
               :jbyte value))

(defun set-static-char-field (clazz field-id value)
  (jnienv-call ("SetStaticCharField" :void)
               :jclass clazz
               #>jfieldID field-id
               :jchar value))

(defun set-static-short-field (clazz field-id value)
  (jnienv-call ("SetStaticShortField" :void)
               :jclass clazz
               #>jfieldID field-id
               :jshort value))

(defun set-static-int-field (clazz field-id value)
  (jnienv-call ("SetStaticIntField" :void)
               :jclass clazz
               #>jfieldID field-id
               :jint value))

(defun set-static-long-field (clazz field-id value)
  (jnienv-call ("SetStaticLongField" :void)
               :jclass clazz
               #>jfieldID field-id
               :jlong value))

(defun set-static-float-field (clazz field-id value)
  (jnienv-call ("SetStaticFloatField" :void)
               :jclass clazz
               #>jfieldID field-id
               :jfloat value))

(defun set-static-double-field (clazz field-id value)
  (jnienv-call ("SetStaticDoubleField" :void)
               :jclass clazz
               #>jfieldID field-id
               :jdouble value))

(defun new-string (unicode len)
  (ccl::with-native-utf-16-cstrs ((cstring unicode))
    (jnienv-call ("NewString" :jstring)
                 (:* :jchar) cstring
                 :jsize len)))

(defun get-string-length (str)
  (jnienv-call ("GetStringLength" :jsize)
               :jstring str))

(defun get-string-chars (str is-copy)
  (jnienv-call ("GetStringChars" (:* :jchar))
               :jstring str
               (:* :jboolean) is-copy))

(defun release-string-chars (str chars)
  (jnienv-call ("ReleaseStringChars" :void)
               :jstring str
               (:* :jchar) chars))

(defun new-string-utf (string)
  (ccl::with-utf-8-cstrs ((cstring string))
    (jnienv-call ("NewStringUTF" :jstring)
                 :address cstring)))

(defun get-string-utf-chars (str)
  (rlet ((is-copy :jboolean))
    (let* ((chars (jnienv-call ("GetStringUTFChars" (:* :char))
                               :jstring str
                               (:* :jboolean) is-copy)))
      (values chars  (pref is-copy :jboolean)))))

(defun release-string-utf-chars (str chars)
  (jnienv-call ("ReleaseStringUTFChars" :void)
               :jstring str
               (:* :char) chars))

(defun get-array-length (array)
  (jnienv-call ("GetArrayLength" :jsize)
               :jArray array))

(defun new-object-array (len clazz init)
  (jnienv-call ("NewObjectArray" #>jobjectArray)
               :jsize len
               :jclass clazz
               :jobject init))

(defun get-object-array-element (array index)
  (jnienv-call ("GetObjectArrayElement" :jobject)
               #>jobjectArray array
               :jsize index))

(defun set-object-array-element (array index val)
  (jnienv-call ("SetObjectArrayElement" :void)
               #>jobjectArray array
               :jsize index
               :jobject val))

(defun new-boolean-array (len)
  (jnienv-call ("NewBooleanArray" #>jbooleanArray)
               :jsize len))

(defun new-byte-array (len)
  (jnienv-call ("NewByteArray" #>jbyteArray)
               :jsize len))

(defun new-char-array (len)
  (jnienv-call ("NewCharArray" #>jcharArray)
               :jsize len))

(defun new-short-array (len)
  (jnienv-call ("NewShortArray" #>jshortArray)
               :jsize len))

(defun new-int-array (len)
  (jnienv-call ("NewIntArray" #>jintArray)
               :jsize len))

(defun new-long-array (len)
  (jnienv-call ("NewLongArray" #>jlongArray)
               :jsize len))

(defun new-float-array (len)
  (jnienv-call ("NewFloatArray" #>jfloatArray)
               :jsize len))

(defun new-double-array (len)
  (jnienv-call ("NewDoubleArray" #>jdoubleArray)
               :jsize len))


(defun get-boolean-array-elements (array is-copy)
  (jnienv-call ("GetBooleanArrayElements" (:* :jboolean))
               #>jbooleanArray array
               (:* :jboolean) is-copy))

(defun get-byte-array-elements (array is-copy)
  (jnienv-call ("GetByteArrayElements" (:* :jbyte))
               #>jbyteArray array
               (:* :jboolean) is-copy))

(defun get-char-array-elements (array is-copy)
  (jnienv-call ("GetCharArrayElements" (:* :jchar))
               #>jcharArray array
               (:* :jboolean) is-copy))

(defun get-short-array-elements (array is-copy)
  (jnienv-call ("GetShortArrayElements" (:* :jshort))
               #>jshortArray array
               (:* :jboolean) is-copy))

(defun get-int-array-elements (array is-copy)
  (jnienv-call ("GetIntArrayElements" (:* :jint))
               #>jintArray array
               (:* :jboolean) is-copy))

(defun get-long-array-elements (array is-copy)
  (jnienv-call ("GetLongArrayElements" (:* :jlong))
               #>jlongArray array
               (:* :jboolean) is-copy))

(defun get-float-array-elements (array is-copy)
  (jnienv-call ("GetFloatArrayElements" (:* :jfloat))
               #>jfloatArray array
               (:* :jboolean) is-copy))

(defun get-double-array-elements (array is-copy)
  (jnienv-call ("GetDoubleArrayElements" (:* :jdouble))
               #>jdoubleArray array
               (:* :jboolean) is-copy))

(defun release-boolean-array-elements (array elems mode)
  (jnienv-call ("ReleaseBooleanArrayElements" :void)
               #>jbooleanArray array
               (:* jboolean) elems
               :jint mode))

(defun release-byte-array-elements (array elems mode)
  (jnienv-call ("ReleaseByteArrayElements" :void)
               #>jbyteArray array
               (:* jbyte) elems
               :jint mode))

(defun release-char-array-elements (array elems mode)
  (jnienv-call ("ReleaseCharArrayElements" :void)
               #>jcharArray array
               (:* jchar) elems
               :jint mode))

(defun release-short-array-elements (array elems mode)
  (jnienv-call ("ReleaseShortArrayElements" :void)
               #>jshortArray array
               (:* jshort) elems
               :jint mode))

(defun release-int-array-elements (array elems mode)
  (jnienv-call ("ReleaseIntArrayElements" :void)
               #>jintArray array
               (:* jint) elems
               :jint mode))

(defun release-long-array-elements (array elems mode)
  (jnienv-call ("ReleaseLongArrayElements" :void)
               #>jlongArray array
               (:* jlong) elems
               :jint mode))

(defun release-float-array-elements (array elems mode)
  (jnienv-call ("ReleaseFloatArrayElements" :void)
               #>jfloatArray array
               (:* jfloat) elems
               :jint mode))

(defun release-double-array-elements (array elems mode)
  (jnienv-call ("ReleaseDoubleArrayElements" :void)
               #>jdoubleArray array
               (:* jdouble) elems
               :jint mode))


(defun get-boolean-array-region (array start len buf)
  (jnienv-call ("GetBooleanArrayRegion" :void)
               #>jbooleanArray array
               :jsize start
               :jsize len
               (:* :jboolean) buf))

(defun get-byte-array-region (array start len buf)
  (jnienv-call ("GetByteArrayRegion" :void)
               #>jbyteArray array
               :jsize start
               :jsize len
               (:* :jbyte) buf))

(defun get-char-array-region (array start len buf)
  (jnienv-call ("GetCharArrayRegion" :void)
               #>jcharArray array
               :jsize start
               :jsize len
               (:* :jchar) buf))

(defun get-short-array-region (array start len buf)
  (jnienv-call ("GetShortArrayRegion" :void)
               #>jshortArray array
               :jsize start
               :jsize len
               (:* :jshort) buf))

(defun get-int-array-region (array start len buf)
  (jnienv-call ("GetIntArrayRegion" :void)
               #>jintArray array
               :jsize start
               :jsize len
               (:* :jint) buf))

(defun get-long-array-region (array start len buf)
  (jnienv-call ("GetLongArrayRegion" :void)
               #>jlongArray array
               :jsize start
               :jsize len
               (:* :jlong) buf))

(defun get-float-array-region (array start len buf)
  (jnienv-call ("GetFloatArrayRegion" :void)
               #>jfloatArray array
               :jsize start
               :jsize len
               (:* :jfloat) buf))

(defun get-double-array-region (array start len buf)
  (jnienv-call ("GetDoubleArrayRegion" :void)
               #>jdoubleArray array
               :jsize start
               :jsize len
               (:* :jdouble) buf))

(defun set-boolean-array-region (array start len buf)
  (jnienv-call ("SetBooleanArrayRegion" :void)
               #>jbooleanArray array
               :jsize start
               :jsize len
               (:* :jboolean) buf))

(defun set-byte-array-region (array start len buf)
  (jnienv-call ("SetByteArrayRegion" :void)
               #>jbyteArray array
               :jsize start
               :jsize len
               (:* :jbyte) buf))

(defun set-char-array-region (array start len buf)
  (jnienv-call ("SetCharArrayRegion" :void)
               #>jcharArray array
               :jsize start
               :jsize len
               (:* :jchar) buf))

(defun set-short-array-region (array start len buf)
  (jnienv-call ("SetShortArrayRegion" :void)
               #>jshortArray array
               :jsize start
               :jsize len
               (:* :jshort) buf))

(defun set-int-array-region (array start len buf)
  (jnienv-call ("SetIntArrayRegion" :void)
               #>jintArray array
               :jsize start
               :jsize len
               (:* :jint) buf))

(defun set-long-array-region (array start len buf)
  (jnienv-call ("SetLongArrayRegion" :void)
               #>jlongArray array
               :jsize start
               :jsize len
               (:* :jlong) buf))

(defun set-float-array-region (array start len buf)
  (jnienv-call ("SetFloatArrayRegion" :void)
               #>jfloatArray array
               :jsize start
               :jsize len
               (:* :jfloat) buf))

(defun set-double-array-region (array start len buf)
  (jnienv-call ("SetDoubleArrayRegion" :void)
               #>jdoubleArray array
               :jsize start
               :jsize len
               (:* :jdouble) buf))


(defun register-natives (clazz methods nmethods)
  (jnienv-call ("RegisterNatives":jint)
               :jclass clazz
               (:* #>JNINativeMethod) methods
               :jint nmethods))


(defun unregister-natives (clazz)
  (jnienv-call ("UnregisterNatives" :jint)
               :jclass clazz))

(defun monitor-enter (obj)
  (jnienv-call ("MonitorEnter" :jint)
               :jobject obj))

(defun monitor-exit (obj)
  (jnienv-call ("MonitorExit" :jint)
               :jobject obj))

(defun get-java-vm (vm)
  (jnienv-call ("GetJavaVM" :jint)
               (:* (:* #>JavaVM)) vm))

(defun get-string-region (str start len buf)
  (jnienv-call ("GetStringRegion" :void)
               :jstring str
               :jsize start
               :jsize len
               (:* :jchar) buf))

(defun get-string-utf-region (str start len buf)
  (jnienv-call ("GetStringUTFRegion" :void)
               :jstring str
               :jsize start
               :jsize len
               (:* :char) buf))

(defun get-primitive-array-critical (array is-copy)
  (jnienv-call ("GetPrimitiveArrayCritical" (:* :void))
               :jarray array
               (:* :jboolean) is-copy))

(defun release-primitive-array-critical(jarray carray mode)
  (jnienv-call ("ReleasePrimitiveArrayCritical" :void)
               :jarray jarray
               (:* :void) carray
               :jint mode))

(defun get-string-critical (string is-copy)
  (jnienv-call ("GetStringCritical" (:* :jchar))
               :jstring string
               (:* :jboolean) is-copy))

(defun release-string-critical (string cstring)
  (jnienv-call ("ReleaseStringCritical" :void)
               :jstring string
               (:* :jchar) cstring))

(defun new-weak-global-ref (obj)
  (jnienv-call ("NewWeakGlobalRef" :jweak)
               :jobject obj))

(defun delete-weak-global-ref (ref)
  (jnienv-call ("DeleteWeakGlobalRef" :void)
               :jweak ref))

(defun exception-check ()
  (jnienv-call ("ExceptionCheck" :jboolean)))
               

(defun new-direct-byte-buffer (address capacity)
  (jnienv-call ("NewDirectByteBuffer" :jobject)
               :address address
               :jlong capacity))

(defun get-direct-buffer-address (buf)
  (jnienv-call ("GetDirectBufferAddress" :address)
               :jobject buf))

(defun get-direct-buffer-capacity (buf)
  (jnienv-call ("GetDirectBufferCapacity" :jlong)
               :jobject buf))

;;; End of jnienv functions.  (Finally.)

(defun get-pvm ()
  (or *pvm*
      (error "JVM not loaded")))

#+later
(defun cleanup-jni-gref (gref)
  "set as a special free action to free java classes when no longer used by Lisp"
  (when (java-ref-p gref)
    (delete-global-ref gref)))

(defun create-jvm (&rest args)
  (declare (dynamic-extent args))
  "Creates the JVM, this can only be done once.
The option strings can be used to control the JVM, esp. the classpath:
\"-Djava.class.path=/Users/rich/Lisp/jfli.jar\""
  (when *pvm*
    (error "JVM already created, can only be started once"))
  (load-jni-lib)
  (ccl::call-with-string-vector
   (lambda (argv)
     (let* ((nargs (length args)))
       (rlet ((initargs :<J>ava<VMI>nit<A>rgs)
              (env (:* :<JNIE>nv))
              (vm (:* :<J>ava<VM>)))
         (%stack-block ((options (* nargs (ccl::record-length :<J>ava<VMO>ption))))
           (do* ((i 0 (1+ i))
                 (p options (%inc-ptr p (ccl::record-length :<J>ava<VMO>ption))))
                ((= i nargs))
             (setf (pref p :<J>ava<VMO>ption.option<S>tring)
                   (paref argv (:* (:* :char)) i)))
           (setf (pref initargs :<J>ava<VMI>nit<A>rgs.version) #$JNI_VERSION_1_4
                 (pref initargs :<J>ava<VMI>nit<A>rgs.n<O>ptions) nargs
                 (pref initargs :<J>ava<VMI>nit<A>rgs.options) options
                 (pref initargs :<J>ava<VMI>nit<A>rgs.ignore<U>nrecognized) #$JNI_TRUE)
           ;; In Darwin, JNI_CreateJavaVM will clobber the calling thread's
           ;; Mach exception ports, despite the fact that CCL is using them.
           ;; To work around this, call a function in the lisp kernel which
           ;; restores the thread's exception ports after calling
           ;; JNI_CreateJavaVM for us.
           (let* ((result
                   (ff-call (ccl::%kernel-import target::kernel-import-jvm-init)
                            :address (foreign-symbol-address "JNI_CreateJavaVM")
                            :address vm
                            :address env
                            :address initargs
                            :int)))
             (if (>= result 0)
               (progn
                 (setq *pvm* (%get-ptr vm))
                 (values result (%get-ptr vm) (%get-ptr env)))
               (error "Can't create Java VM: result = ~d" result)))))))
   args))


;;;this is the FLI side of proxy support

(defvar *invocation-handler* nil
  "this will be set by jfli:enable-java-proxies to a function of 3 args")



;;;this will be set as the implementation of a native java function

(defcallback |LispInvocationHandler_invoke|
    (:address env :jobject obj :jobject proxy :jobject method :jobject args :jobject)
  (do-invoke env obj proxy method args))


(defun do-invoke (env obj proxy method args)
  (declare (ignore env))                ;it's not like we're on another thread
  (when *invocation-handler*
    (prog1
        (funcall *invocation-handler* proxy method args)
      ;;(jfli::invocation-handler proxy method args)
      (delete-local-ref obj))))

(defun try (result)
  (if (exception-check)
      (handle-exception)
    result))

;JNI will sometimes indicate theere is an exception via a return value
;so take advantage of that when possible vs. the call back to exception-check
(defun try-null (result)
  (if (ccl:%null-ptr-p result)
      (handle-exception)
    result))

(defun register-invocation-handler (invocation-handler)
  "sets up the Lisp handler and binds the native function - jfli.jar must be in the classpath"
  (setf *invocation-handler* invocation-handler)
  (rlet ((method #>JNINativeMethod))
    (let ((lih (try-null (jni-find-class "com/richhickey/jfli/LispInvocationHandler"))))
      (with-cstrs ((name "invoke")
                   (signature "(Ljava/lang/Object;Ljava/lang/reflect/Method;[Ljava/lang/Object;)Ljava/lang/Object;"))
        (setf (pref method #>JNINativeMethod.name) name
              (pref method #>JNINativeMethod.signature) signature
              (pref method #>JNINativeMethod.fnPtr) |LispInvocationHandler_invoke|)
      (register-natives lih method 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;the code below provides for the generation of wrapper functions that use JNI to access
;methods and fields. This low-level interface is unsafe, in that JNI will not 
;check arg types etc on calls, and therefore should only be used to build safer high-level interfaces
;i.e. use jfli!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;found on c.l.l
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun replace-substrings (string substring replacement)
  (declare (optimize (speed 3))
           (type simple-string string substring replacement))
  (assert (> (length substring) 0) (substring)
    "Substring ~A must be of length ~D > 0"
    substring (length substring))
  (with-output-to-string (stream)
    (loop with substring-length = (length substring)
          for index = 0 then (+ match-index substring-length)
          for match-index = (search substring string :start2 index)
          do
          (write-string string stream :start index :end match-index)
          (when match-index
            (write-string replacement stream))
          while match-index)))


(defun local-ref-to-global-ref (lref)
  (when lref
    (let ((gref (new-global-ref lref)))
      #+laster
      (flag-special-free-action gref)
      (delete-local-ref lref)
      gref)))

(defun local-ref-to-string (lref)
  (prog1
      (convert-from-java-string lref)
    (delete-local-ref lref)))

(defun convert-to-java-string (s)
  (when s
    (try-null (new-string-utf (string s)))))

(defun convert-from-java-string (s)
  (when s
    (let ((chars (try-null (get-string-utf-chars s))))
      (prog1
          (ccl::%get-utf-8-cstring chars)
        (release-string-utf-chars s chars)))))

(defun jaref (array index)
  (try (get-object-array-element array index)))

(defun (setf jaref) (val array index)
  (try (set-object-array-element array index val)))

(defun convert-string-arg (s)
  "if s is stringp, make into java string, else presume it is a java string and return it"
  ;presumably faster than checking if s is a foreign pointer?
  (if (or (stringp s) (symbolp s))
      (convert-to-java-string s)
    s))

(defun process-arg (val type)
  (if (string-equal "java.lang.String" type)
                 `(convert-string-arg ,val)
                 val))

(defmacro set-arg (args i val type)
  `(setf (pref (paref ,args (:* :jvalue) ,i)
          ,(jvalue-accessor-from-typename type))
    ,(process-arg val type)))

(defmacro with-arg-array (arg-array-name args &body body)
  (let ((i -1))
  `(%stack-block ((,arg-array-name (*  ,(length args) (ccl::record-length :jvalue))))
       ,@(mapcar #'(lambda (arg)
                     (list 'set-arg arg-array-name (incf i) (first arg) (second arg))) 
                 args)

       ,@body)))

(defun build-descriptor (params return-type)
  (string-append
   "("
   (apply #'string-append (mapcar #'(lambda (p)
                                      (type-descriptor-from-typename (second p)))
                                  params))
   ")"
   (type-descriptor-from-typename return-type)))

(defun get-class-and-method-id (class-name method-name descriptor is-static)
  (let ((class (local-ref-to-global-ref
                (try-null (jni-find-class class-name)))))
    (values class
            (if is-static
                (try-null (get-static-method-id class method-name descriptor))
              (try-null (get-method-id class method-name descriptor))))))


(defun get-class-and-field-id (class-name field-name descriptor is-static)
  (let ((class (local-ref-to-global-ref
                (try-null (jni-find-class class-name)))))
    (values class
            (if is-static
                (try-null (get-static-field-id class field-name descriptor))
              (try-null (get-field-id class field-name descriptor))))))

(defun is-name-of-primitive (s)
  (member s '("boolean" "byte" "char" "short" "int" "long" "float" "double" "void")
          :test #'string-equal))

(defun package-qualified-name (classname packagename)
  (cond
   ((is-name-of-primitive (subseq classname 0 (position #\< classname))) classname)
   ((find #\. classname) classname)     ;already qualified, presumably by another package
   (t (string-append packagename "." classname)))) 

(defun split-package-and-class (name)
    (let ((p (position #\. name :from-end t)))
      (unless p (error "must supply package-qualified classname"))
      (values (subseq name 0 p)
              (subseq name (1+ p)))))

(defun slot-from-typename (tn)
  (let ((prim (assoc tn
                     '(("boolean" . :z)
                       ("byte" . :b)
                       ("char" . :c)
                       ("short" . :s)
                       ("int" . :i)
                       ("long" . :j)
                       ("float" . :f)
                       ("double" . :d))
                     :test #'string-equal)))
    (if prim
        (rest prim)
      :l)))

(defun jvalue-accessor-from-typename (tn)
  (let ((prim (assoc tn
                     '(("boolean" . :jvalue.z)
                       ("byte" . :jvalue.b)
                       ("char" . :jvalue.c)
                       ("short" . :jvalue.s)
                       ("int" . :jvalue.i)
                       ("long" . :jvalue.j)
                       ("float" . :jvalue.f)
                       ("double" . :jvalue.d))
                     :test #'string-equal)))
    (if prim
        (rest prim)
      :jvalue.l)))

(defun name-component-from-typename (tn)
  (if (is-name-of-primitive tn)
      tn
    "object"))

(defun type-descriptor-from-typename (tn)
  (let ((prim (assoc tn
                     '(("boolean" . "Z")
                       ("byte" . "B")
                       ("char" . "C")
                       ("short" . "S")
                       ("int" . "I")
                       ("long" . "J")
                       ("float" . "F")
                       ("double" . "D")
                       ("void" . "V"))
                     :test #'string-equal)))
    (if prim
        (rest prim)
      (let ((array-depth (count #\< tn))
            (tn-with-slashes (replace-substrings tn "." "/")))
        (if (= 0 array-depth)
            (string-append "L" tn-with-slashes ";")
          (with-output-to-string (s)
            (dotimes (x array-depth)
              (write-string "[" s))
            (write-string (type-descriptor-from-typename
                           (subseq tn-with-slashes 0 (position #\< tn-with-slashes))) s)))))))

;not an exact reciprocal of type-descriptor-from-typename since reflection uses . not / as separator
(defun typename-from-reflection-type-descriptor (tn)
  (let ((prim (assoc tn
                     '(("Z" . "boolean")
                       ("B" . "byte")
                       ("C" . "char")
                       ("S" . "short")
                       ("I" . "int")
                       ("J" . "long")
                       ("F" . "float")
                       ("D" . "double")
                       ("V" . "void"))
                     :test #'string-equal)))
    (if prim
        (rest prim)
      (let ((array-depth (count #\[ tn)))
        (if (= 0 array-depth)
            (subseq tn 1 (1- (length tn))) ;strip leading L and trailing ;
          (with-output-to-string (s)
            (write-string (typename-from-reflection-type-descriptor (subseq tn array-depth)) s)
            (dotimes (x array-depth)
              (write-string "<>" s))))))))

(defun method-name-from-typename (tn static)
    (find-symbol (string-upcase (string-append "call-"
                                               (if static "static-" "")
                                             (name-component-from-typename tn)
                                             "-method-a")) :jni))

(defun field-get-name-from-typename (tn static)
    (find-symbol (string-upcase (string-append "get-"
                                               (if static "static-" "")
                                             (name-component-from-typename tn)
                                             "-field")) :jni))

(defun field-set-name-from-typename (tn static)
    (find-symbol (string-upcase (string-append "set-"
                                               (if static "static-" "")
                                             (name-component-from-typename tn)
                                             "-field")) :jni))
(defun process-return (return-type f &key raw-return)
  (cond
   ((or raw-return (is-name-of-primitive return-type)) f)
   ((string-equal "java.lang.String" return-type) `(local-ref-to-string ,f))
   (t `(local-ref-to-global-ref ,f))))

;JNI wrapper generators - will create functions in current package
;this needs more docs
(defmacro define-java-function (fname class-name return-type method-name params &key static raw-return)
  (let ((this (gensym))
        (class (gensym))
        (id (gensym))
        (args (gensym)))
    `(let (,class ,id)
       (defun ,fname ,(if static (mapcar #'first params)
                        (cons this (mapcar #'first params)))
         (when (null ,class)
           (multiple-value-setq (,class ,id)
               (get-class-and-method-id ,(replace-substrings class-name "." "/")
                                        ,method-name ,(build-descriptor params return-type) ,static)))
         (with-arg-array ,args ,(mapcar #'(lambda (param)
                                           (list (first param) (second param)))
                                       params)
           ,(process-return return-type
                            `(try (,(method-name-from-typename return-type static)
                                   ,(if static class this) ,id ,args))
                            :raw-return raw-return))))))

(defmacro define-java-field (getname class-name field-type field-name &key static)
  (let ((this (gensym))
        (class (gensym))
        (id (gensym))
        (val (gensym)))
    `(let (,class ,id)
       (flet ((load-ids ()
                (when (null ,class)
                  (multiple-value-setq (,class ,id)
                      (get-class-and-field-id ,(replace-substrings class-name "." "/")
                                              ,field-name ,(type-descriptor-from-typename field-type)
                                              ,static)))))
         (defun ,getname ,(if static () (list this))
           (load-ids)
           ,(process-return field-type
                            `(try (,(field-get-name-from-typename field-type static)
                                   ,(if static class this) ,id))))
         (defun (setf ,getname) ,(if static (list val) (list this val))
           (load-ids)
           (try (,(field-set-name-from-typename field-type static)
                 ,(if static class this) ,id ,(process-arg val field-type)))
           ,val)))))

(defmacro define-java-constructor (fname class-name params)
  (let ((class (gensym))
        (id (gensym))
        (args (gensym)))
    `(let (,class ,id)
       (defun ,fname ,(mapcar #'first params)
         (when (null ,class)
           (multiple-value-setq (,class ,id)
               (get-class-and-method-id ,(replace-substrings class-name "." "/")
                                        "<init>" ,(build-descriptor params "void") nil)))
         (with-arg-array ,args ,(mapcar #'(lambda (param)
                                           (list (first param) (second param)))
                                       params)
           (local-ref-to-global-ref (try-null (new-object-a ,class ,id ,args))))))))

(defun make-func-name (class method params append-param-types)
  ;probably a format one-liner that can do this
    (let ((base (string-append class "." method)))
      (if append-param-types
          (string-append base
                         (let ((param-types (mapcar #'second params)))
                           (if param-types
                               (string-append "<"
                                              (reduce #'(lambda (x y)
                                                          (string-append x "-" y)) param-types)
                                              ">")
                             "<>")))
        base)))

;these just do some name twiddling before calling define-java-xxx above
(defmacro def-jni-function (package-and-class method params return-typename
                                               &key static overloaded raw-return)
  (multiple-value-bind (package class) (split-package-and-class package-and-class)
    (let* ((fname (make-func-name class method params overloaded))
           (fsym (read-from-string fname)))
      `(locally ,(list 'define-java-function
                     fsym
                     package-and-class
                     (package-qualified-name return-typename package)
                     method
                     (mapcar #'(lambda (p)
                                 (list (first p) (package-qualified-name (second p) package)))
                             params)
                     :static static :raw-return raw-return)))))

(defmacro def-jni-functions (package-and-class &rest decls)
  `(locally ,@(mapcar #'(lambda (decl)
                          (list* 'def-jni-function package-and-class decl))
                      decls)))

(defmacro def-jni-constructor (package-and-class params &key overloaded)
  (multiple-value-bind (package class) (split-package-and-class package-and-class)
    (let* ((fname (make-func-name class "new" params overloaded))
           (fsym (read-from-string fname)))
      `(locally ,(list 'define-java-constructor
                     fsym 
                     package-and-class 
                     (mapcar #'(lambda (p)
                                 (list (first p) (package-qualified-name (second p) package)))
                             params))))))

(defmacro def-jni-field (package-and-class field typename &key static)
  (multiple-value-bind (package class) (split-package-and-class package-and-class)
    (let ((getsym (read-from-string (string-append class "." field
                                                   (if static "-accessor" ""))))
          (macsym (read-from-string (string-append class "." field))))
      `(locally 
         ,(list 'define-java-field getsym package-and-class
                (package-qualified-name typename package) field :static static)
         ,(when static
            `(define-symbol-macro ,macsym (,getsym)))))))

;we're going to use a little Java to do exception handling below
(def-jni-function "java.lang.Object"
                   "toString" () "String")

(def-jni-function "java.lang.reflect.InvocationTargetException"
                  "getTargetException" () "java.lang.Throwable")

(def-jni-functions "java.lang.Throwable"
                   ("getMessage" () "String")
                   ("getStackTrace" () "StackTraceElement<>"))

(defmacro do-jarray ((x array) &body body)
  (let ((gcount (gensym))
        (gi (gensym))
        (garray (gensym)))
    `(let* ((,garray ,array)
            (,gcount (get-array-length ,garray)))
       (dotimes (,gi ,gcount)
         (let ((,x (jaref ,garray ,gi)))
           ,@body)))))

#||
It is critical that if you call a JNI function that might throw an exception that you clear it,
otherwise the next Java call you make will cause a crash
||#

(defun handle-exception ()
  (let ((e (exception-occurred)))
    (when (not (ccl:%null-ptr-p e)) ;allow for safe calling in non-exceptional state
      (exception-clear)
      ;if the exception occurs in the reflection target, we really want that
      (when (is-instance-of e (jni-find-class "java/lang/reflect/InvocationTargetException"))
        (setf e (invocationtargetexception.gettargetexception e)))
      (error "~A" (with-output-to-string (s)
                    (format s "~A~%" (object.tostring e))
                    (do-jarray (x (throwable.getstacktrace e))
                      (format s "~A~%" (object.tostring x))))))))





(defun try-neg (result)
  (if (minusp result)
      (handle-exception)
    result))


)


