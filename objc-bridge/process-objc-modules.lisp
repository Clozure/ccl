;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2003 Clozure Associates
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

(#-(or apple-objc gnu-objc)
   (eval-when (:compile-toplevel :load-toplevel :execute)
     #+darwinppc-target (pushnew :apple-objc *features*)
     #+linuxppc-target (pushnew :gnu-objc *features*)
     #-(or darwinppc-target linuxppc-target)
     (error "Not sure what ObjC runtime system to use.")))

#+apple-objc
(progn
(defvar *objc-module-verbose* nil)


(defun process-section-in-all-libraries (segname sectionname function)
  "For every loaded shared library, find the section named SECTIONNAME
in the segment named SEGNAME.  If this section exists, call FUNCTION with
a pointer to the section data and the section's size in bytes as arguments."
  (with-cstrs ((seg segname)
	       (sect sectionname))
    (rlet ((size :unsigned))
      (with-macptrs (mach-header sectdata)
	(dotimes (i (#_ _dyld_image_count))
	  (%setf-macptr mach-header (#_ _dyld_get_image_header i))
	  ;; Paranoia: this should never be null
	  (unless (%null-ptr-p mach-header)
            ;; The one instance of an MH_BUNDLE I've encountered
            ;; hasn't had its section data relocated.  I'm not sure
            ;; if that's generally true of MH_BUNDLEs; for the time
            ;; being, ignore them and concentrate on MH_DYLIBs.
            (when (eql (pref mach-header :mach_header.filetype) #$MH_DYLIB)
              (%setf-macptr sectdata (#_getsectdatafromheader
                                      mach-header
                                      seg
                                      sect
                                      size))
              ;; This pointer may be null, unless the shared object
              ;; file denoted by "mach_header" contains a segment and
              ;; section matching those we're looking for.
              (unless (%null-ptr-p sectdata)
                (funcall function sectdata (pref size :unsigned))))))))))

(defun process-objc-modules (f)
  (process-section-in-all-libraries #$SEG_OBJC #$SECT_OBJC_MODULES f))

;;; A not-too-interesting test of the mechanism.
(defun show-objc-module-sections ()
  (process-objc-modules #'(lambda (sect size)
			    (format t "~& module section @~s, size = ~d"
				    sect size))))

(defun process-module-classes (module classfn)
  (when *objc-module-verbose*
    (format t "~& processing classes in module ~s" module)
    (force-output t))  
  (with-macptrs ((symtab (pref module :objc_module.symtab)))
    (with-macptrs ((defsptr (pref symtab :objc_symtab.defs))
		   (classptr))
      (dotimes (i (pref symtab :objc_symtab.cls_def_cnt))
	(%setf-macptr classptr (%get-ptr defsptr (* i (record-length :address))))
	(when *objc-module-verbose*
	  (format t "~& processing class ~a, info = #x~8,'0x"
		  (%get-cstring (pref classptr :objc_class.name))
		  (pref classptr :objc_class.info))
          (force-output t))
	;; process the class
	(funcall classfn classptr)
	;; process the metaclass
	(funcall classfn (pref classptr :objc_class.isa))))))

(defun process-module-categories (module catfn)
  (with-macptrs ((symtab (pref module :objc_module.symtab)))
    (with-macptrs ((catptr
		    (%inc-ptr (pref symtab :objc_symtab.defs)
			      (* (pref symtab :objc_symtab.cls_def_cnt)
				 (record-length :address)))))
      (dotimes (i (pref symtab :objc_symtab.cat_def_cnt))
	(when *objc-module-verbose*
	  (format t "~& processing category ~s "
		  (%get-cstring (pref (%get-ptr catptr)
				      :objc_category.category_name))))
	(funcall catfn (%get-ptr catptr))
	(%incf-ptr catptr (record-length :address))))))


;;; This is roughly equivalent to the inner loop in DO-OBJC-METHODS.
(defun process-methods-in-method-list (mlist class  mfun)
  (unless (%null-ptr-p mlist)
    (with-macptrs ((method (pref mlist :objc_method_list.method_list)))
      (dotimes (i (pref mlist :objc_method_list.method_count))
	(funcall mfun method class)
	(%incf-ptr method (record-length :objc_method))))))

;;; Categories push method lists onto the "front" of the class.
;;; The methods that belong to the class are in the last method list,
;;; so we skip everything else here.
(defun process-class-methods (class methodfun)
  (%stack-block ((iter 4))
    (setf (%get-ptr iter) (%null-ptr))
    (with-macptrs ((next)
		   (mlist ))
      (loop
	  (%setf-macptr next (#_class_nextMethodList class iter))
	  (when (%null-ptr-p next)
	    (process-methods-in-method-list mlist class  methodfun)
	    (return))
	(%setf-macptr mlist next)))))

(defun process-category-methods (category methodfun)
  (with-macptrs ((classname (pref category :objc_category.class_name))
		 (class (#_objc_lookUpClass classname))
		 (metaclass (pref class :objc_class.isa))
		 (instance-methods
		  (pref category :objc_category.instance_methods))
		 (class-methods
		  (pref category :objc_category.class_methods)))
    (process-methods-in-method-list instance-methods class methodfun)
    (process-methods-in-method-list class-methods metaclass methodfun)))

(defun process-module-methods (sectptr size methodfun)
  "Process all modules in the ObjC module section SECTPTR, whose size
in bytes is SIZE.  For each class and each category in each module,
call METHODFUN on each method defined in a class or category.  The
METHODFUN will be called with a stack-allocated/mutable pointer to the
method, and a stack-allocated/mutable pointer to the method receiver's
class or metaclass object."
  (when *objc-module-verbose*
    (format t "~& processing classes in section ~s" sectptr)
    (force-output t))
  (with-macptrs ((module sectptr))
    (let* ((nmodules (/ size (record-length :objc_module))))
      (dotimes (i nmodules)
	(process-module-classes
	 module
	 #'(lambda (class)
	     (when *objc-module-verbose*
	       (format t "~& == processing class #x~8,'0x ~a, (#x~8,'0x) info = #x~8,'0x"
		       (%ptr-to-int class)
		       (%get-cstring (pref class :objc_class.name))
		       (%ptr-to-int (pref class :objc_class.name))
		       (pref class :objc_class.info)))
	     #+nope
	     (unless (logtest #$CLS_META (pref class :objc_class.info))
	       (map-objc-class class))
	     (process-class-methods class methodfun)))
	(process-module-categories	 
	 module
	 #'(lambda (category)
	     (process-category-methods category methodfun)))
	(%incf-ptr module (record-length :objc_module))))))
	   
(defun iterate-over-module-classes (sectptr size classfn)
  (when *objc-module-verbose*
    (format t "~& processing classes in section ~s" sectptr)
    (force-output t))
  (with-macptrs ((module sectptr))
    (let* ((nmodules (/ size (record-length :objc_module))))
      (dotimes (i nmodules)
	(process-module-classes module classfn)
	(%incf-ptr module (record-length :objc_module))))))

	  
(defun process-section-methods (sectptr size methodfun &optional
					(section-check-fun #'true))
  "If SECTION-CHECK-FUN returns true when called with the (stack-allocated,
mutable) Objc modules section SECTPTR, process all methods defined
in all classes/categories in all modules in the section."
  (when (funcall section-check-fun sectptr)
    (process-module-methods sectptr size methodfun)))

(defloadvar *sections-already-scanned-for-methods* ())

(defun check-if-section-already-scanned (sectptr)
  (unless (member sectptr *sections-already-scanned-for-methods*
		  :test #'eql)
    (push (%inc-ptr sectptr 0)		;make a heap-allocated copy!
	  *sections-already-scanned-for-methods*)
    t))

(defun note-all-library-methods (method-function)
  "For all methods defined in all classes and categories defined in all
ObjC module sections in all loaded shared libraries, call METHOD-FUNCTION
with the method and defining class as arguments.  (Both of these arguments
may have been stack-allocated by the caller, and may be destructively
modified by the caller after the METHOD-FUNCTION returns.)
  Sections that have already been scanned in the current lisp session are
ignored."
  (process-objc-modules
   #'(lambda (sectptr size)
       (process-section-methods
	sectptr
	size
	method-function
	#'check-if-section-already-scanned))))


                        

)
(provide "PROCESS-OBJC-MODULES") 

