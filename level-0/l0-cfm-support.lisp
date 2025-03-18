;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.


; l0-cfm-support.lisp

(in-package "CCL")

#+windows-target
(progn
  (defvar *windows-invalid-handle* nil)
  (setq *windows-invalid-handle* (%int-to-ptr #+64-bit-target #xffffffffffffffff #+32-bit-target #xffffffff)))


;;; We have several different conventions for representing an
;;; "entry" (a foreign symbol address, possibly represented as
;;; something cheaper than a MACPTR.)  Destructively modify
;;; ADDR so that it points to where ENTRY points.
(defun entry->addr (entry addr)
  #+ppc32-target
  ;; On PPC32, all function addresses have their low 2 bits clear;
  ;; so do fixnums.
  (%setf-macptr-to-object addr entry)
  #+ppc64-target
  ;; On PPC64, some addresses can use the fixnum trick.  In other
  ;; cases, an "entry" is just a MACPTR.
  (if (typep entry 'fixnum)
    (%setf-macptr-to-object addr entry)
    (%setf-macptr addr entry))
  ;; On x86, an "entry" is just an integer.  There might elswehere be
  ;; some advantage in treating those integers as signed (they might
  ;; be more likely to be fixnums, for instance), so ensure that they
  ;; aren't.
  #+(or x86-target arm-target)
  (%setf-macptr addr (%int-to-ptr
                      (if (< entry 0)
                        (logand entry (1- (ash 1 target::nbits-in-word)))
                        entry)))
  #-(or ppc-target x86-target arm-target) (dbg "Fix entry->addr"))




;;; Bootstrapping. Real version is in l1-aprims.
;;; Called by expansion of with-pstrs

(defun byte-length (string &optional script start end)
    (declare (ignore script))
    (when (or start end)
      (error "Don't support start or end args yet"))
    (if (base-string-p string)
      (length string)
      (error "Don't support non base-string yet.")))




(defun external-entry-point-p (x)
  (istruct-typep x 'external-entry-point))

;;; On both Linux and FreeBSD, RTLD_NEXT and RTLD_DEFAULT behave
;;; the same way wrt symbols defined somewhere other than the lisp
;;; kernel.  On Solaris, RTLD_DEFAULT will return the address of
;;; an imported symbol's procedure linkage table entry if the symbol
;;; has a plt entry (e.g., if it happens to be referenced by the
;;; lisp kernel.)  *RTLD-NEXT* is therefore a slightly better
;;; default; we've traditionaly used *RTLD-DEFAULT*.  
(defvar *rtld-next*)
(defvar *rtld-default*)
(defvar *rtld-use*)
(setq *rtld-next* (%incf-ptr (%null-ptr) -1)
      *rtld-default* (%int-to-ptr #+(or linux-target darwin-target windows-target)  0
				  #-(or linux-target darwin-target windows-target)  -2)
      *rtld-use* #+solaris-target *rtld-next* #-solaris-target *rtld-default*)

#+(or linux-target freebsd-target solaris-target)
(progn

(defvar *dladdr-entry*)
  
;;; I can't think of a reason to change this.
(defvar *dlopen-flags* nil)
(setq *dlopen-flags* (logior #$RTLD_GLOBAL #$RTLD_NOW))
)

(defvar *eeps* nil)

(defvar *fvs* nil)

(defun eeps ()
  (or *eeps*
      (setq *eeps* (make-hash-table :test #'equal))))

(defun fvs ()
  (or *fvs*
      (setq *fvs* (make-hash-table :test #'equal))))

(defun unload-foreign-variables (lib)
  (let* ((fvs (fvs)))
    (when fvs
      (maphash #'(lambda (k fv)
                   (declare (ignore k))
                   (when (fv.addr fv)
                     (when (or (null lib) (eq (fv.container fv) lib))
                       (setf (fv.addr fv) nil)
                       (resolve-foreign-variable fv nil))))
               fvs))))

;;; Walk over all registered entrypoints, invalidating any whose container
;;; is the specified library.  Return true if any such entrypoints were
;;; found.
(defun unload-library-entrypoints (lib)
  (let* ((count 0))
    (declare (fixnum count))
    (maphash #'(lambda (k eep)
		 (declare (ignore k))
                 (when (eep.address eep)
                   (when (or (null lib) (eq (eep.container eep) lib))
                     (setf (eep.address eep) nil)
                     (resolve-eep eep nil)
                     (incf count))))
	     (eeps))
    
    (not (zerop count))))

(defun shared-library-with-name (name)
  (let* ((namelen (length name)))
    (dolist (lib *shared-libraries*)
      (let* ((libname (shlib.soname lib)))
	(when (%simple-string= name libname 0 0 namelen (length libname))
	  (return lib))))))

(defun generate-external-functions (path)
  (let* ((names ()))
    (maphash #'(lambda (k ignore)
		 (declare (ignore ignore))
		 (push k names)) (eeps))
    (with-open-file (stream path
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (dolist (k names) (format stream "~&extern void * ~a();" k))
     
      (format stream "~&external_function external_functions[] = {")
      (dolist (k names) (format stream "~&~t{~s,~a}," k k))
      (format stream "~&~t{0,0}~&};"))))

    
(defvar *shared-libraries* nil)

#+(or linux-target freebsd-target solaris-target)
(progn

;; (pref ptr :link_map.l_addr) is an integer on Linux and a Pointer on FreeBSD
;; This macro returns a pointer on all platforms
(defmacro link_map.l_addr (ptr)
  (let* ((record (%find-foreign-record :link_map))
         (field (%find-foreign-record-type-field record :l_addr))
         (offset (/ (foreign-record-field-offset field) 8)))
    `(%get-ptr ,ptr ,offset)))

(defmacro link_map.l_ld (ptr)
  (let* ((record (%find-foreign-record :link_map))
         (field (%find-foreign-record-type-field record :l_ld))
         (offset (/ (foreign-record-field-offset field) 8)))
    `(%get-ptr ,ptr ,offset)))

(defun soname-ptr-from-link-map (map)
  (let* ((path (pref map :link_map.l_name)))
    (if (or (%null-ptr-p path)
            (not (eql (%get-unsigned-byte path 0) (char-code #\/))))
      (let* ((p (malloc 1)))
        (setf (%get-unsigned-byte p 0) 0)
        p)
      (if (eql (%get-unsigned-byte path 0) 0)
        path
        (with-macptrs ((dyn-strings)
                       (dynamic-entries (link_map.l_ld map)))
          (if (%null-ptr-p dynamic-entries)
            (%null-ptr)
            (let* ((soname-offset nil))
              ;; Walk over the entries in the file's dynamic segment; the
              ;; last such entry will have a tag of #$DT_NULL.  Note the
              ;; (loaded,on Linux; relative to link_map.l_addr on FreeBSD)
              ;; address of the dynamic string table and the offset of the
              ;; #$DT_SONAME string in that string table.
              ;; Actually, the above isn't quite right; there seem to
              ;; be cases (involving vDSO) where the address of a library's
              ;; dynamic string table is expressed as an offset relative
              ;; to link_map.l_addr as well.
              (loop
                (case #+32-bit-target (pref dynamic-entries :<E>lf32_<D>yn.d_tag)
                      #+64-bit-target (pref dynamic-entries :<E>lf64_<D>yn.d_tag)
                      (#. #$DT_NULL (return))
                      (#. #$DT_SONAME
                          (setq soname-offset
                                #+32-bit-target (pref dynamic-entries
                                                      :<E>lf32_<D>yn.d_un.d_val)
                                #+64-bit-target (pref dynamic-entries
                                                      :<E>lf64_<D>yn.d_un.d_val)))
                      (#. #$DT_STRTAB
                          ;; On some architectures, glibc mangles the DT_STRTAB
                          ;; entry into an absolute address, and there appears
                          ;; to be no portable way to detect when that happens.
                          ;; We assume that no displacement can be larger than
                          ;; the base address; this should hold unless a huge
                          ;; object is loaded at an extremely low address.
                          (%setf-macptr dyn-strings
                                        (let* ((disp (%get-natural
                                                      dynamic-entries
                                                      target::node-size))
                                               (addr (link_map.l_addr map)))
                                          ;; Don't risk anything if we don't have to.
                                          #+(or freebsd-target solaris-target android-target)
                                          (%inc-ptr addr disp)
                                          #-(or freebsd-target solaris-target android-target)
                                          (if (> disp (%ptr-to-int addr))
                                              (%int-to-ptr disp)
                                              (%inc-ptr addr disp))))))
                (%setf-macptr dynamic-entries
                              (%inc-ptr dynamic-entries
                                        #+32-bit-target
                                        (record-length :<E>lf32_<D>yn)
                                        #+64-bit-target
                                        (record-length :<E>lf64_<D>yn))))
              (if (and soname-offset
                       (not (%null-ptr-p dyn-strings)))
                (%inc-ptr dyn-strings soname-offset)
                ;; Use the full pathname of the library.
                (pref map :link_map.l_name)))))))))

(defun shared-library-at (base)
  (dolist (lib *shared-libraries*)
    (when (eql (shlib.base lib) base)
      (return lib))))



(defun shlib-from-map-entry (m)
  (let* ((base (link_map.l_addr m)))
    ;; On relatively modern Linux systems, this is often NULL.
    ;; I'm not sure what (SELinux ?  Pre-binding ?  Something else ?)
    ;; counts as being "relatively modern" in this case.
    ;; The link-map's l_ld field is a pointer to the .so's dynamic
    ;; section, and #_dladdr seems to recognize that as being an
    ;; address within the library and returns a reasonable "base address".
    (when (%null-ptr-p base)
      (let* ((addr (%library-base-containing-address (link_map.l_ld m))))
        (if addr (setq base addr))))
    (unless (%null-ptr-p base)
      (or (let* ((existing-lib (shared-library-at base)))
            (when (and existing-lib (null (shlib.map existing-lib)))
              (setf (shlib.map existing-lib) m
                    (shlib.pathname existing-lib)
                    (%get-cstring (pref m :link_map.l_name))
                    (shlib.base existing-lib) base))
            existing-lib)
          (let* ((soname-ptr (soname-ptr-from-link-map m))
                 (soname (unless (%null-ptr-p soname-ptr) (%get-cstring soname-ptr)))
                 (pathname (%get-cstring (pref m :link_map.l_name)))
                 (shlib (shared-library-with-name soname)))
            (if shlib
              (setf (shlib.map shlib) m
                    (shlib.base shlib) base
                    (shlib.pathname shlib) pathname)
              (push (setq shlib (%cons-shlib soname pathname m base))
                    *shared-libraries*))
            shlib)))))


(defun %get-r-debug ()
  (let* ((addr (ff-call (%kernel-import target::kernel-import-get-r-debug)
			address)))
    (unless (%null-ptr-p addr)
      addr)))

(defun %link-map-address ()
  (let* ((r_debug (%get-r-debug)))
    (if r_debug
      (pref r_debug :r_debug.r_map)
      (let* ((p (or (foreign-symbol-address "_dl_loaded")
		    (foreign-symbol-address "_rtld_global"))))
	(if p
	  (%get-ptr p))))))

(defun %walk-shared-libraries (f)
  (let* ((loaded (%link-map-address)))
    (do* ((map (pref loaded :link_map.l_next) (pref map :link_map.l_next)))
         ((%null-ptr-p map))
      (funcall f map))))


(defun %dlopen-shlib (l)
  (with-cstrs ((n (shlib.soname l)))
    (ff-call (%kernel-import target::kernel-import-GetSharedLibrary)
	     :address n
	     :unsigned-fullword *dlopen-flags*
	     :void)))
  
(defun init-shared-libraries ()
  (setq *dladdr-entry* (foreign-symbol-entry "dladdr"))
  (when (null *shared-libraries*)
    (%walk-shared-libraries #'shlib-from-map-entry)
      ;; On Linux, it seems to be necessary to open each of these
      ;; libraries yet again, specifying the RTLD_GLOBAL flag.
      ;; On FreeBSD, it seems desirable -not- to do that.
    #+linux-target
    (progn
      ;; The "program interpreter" (aka the dynamic linker) is itself
      ;; on *shared-libraries*; it seems to be the thing most recently
      ;; pushed on that list.  Remove it: there's little reason for it
      ;; to be there, and on some platforms (Linux ARM during the
      ;; transition to hard float) the dynamic linker name/pathname
      ;; depend on how the kernel was compiled and linked.  We -don't-
      ;; want to later open the "other" dynamic linker.
      (setq *shared-libraries* (cdr *shared-libraries*)) ; find a better way.
      (dolist (l *shared-libraries*)
        (%dlopen-shlib l)))))

(init-shared-libraries)




                     
                     

(defun open-shared-library-internal (name)
  (let* ((handle (with-cstrs ((name name))
                   (ff-call
                    (%kernel-import target::kernel-import-GetSharedLibrary)
                    :address name
                    :unsigned-fullword *dlopen-flags*
                    :address)))
         (link-map #+(and linux-target (not android-target)) handle
                   #+(or freebsd-target solaris-target)
                   (if (%null-ptr-p handle)
                     handle
                     (rlet ((p :address))
                       (if (eql 0 (ff-call
                                   (foreign-symbol-entry "dlinfo")
                                   :address handle
                                   :int #$RTLD_DI_LINKMAP
                                   :address p
                                   :int))
                         (pref p :address)
                         (%null-ptr))))
                   #+android-target (if (%null-ptr-p handle)
                                      handle
                                      (pref handle :soinfo.linkmap))))
    (if (%null-ptr-p link-map)
      (values nil (dlerror))
      (prog1 (let* ((lib (shlib-from-map-entry link-map)))
	       (incf (shlib.opencount lib))
               (setf (shlib.handle lib) handle)
	       lib)
	(%walk-shared-libraries
	 #'(lambda (map)
             (let* ((addr (link_map.l_addr map)))
               (unless (or (%null-ptr-p addr)
                           (shared-library-at addr))
                 (let* ((new (shlib-from-map-entry map)))
                   (%dlopen-shlib new))))))))))

)


#+darwin-target
(progn

(defun shared-library-with-handle (handle)
  (dolist (lib *shared-libraries*)
    (when (eql (shlib.handle lib) handle)
      (return lib))))









;;; end darwin-target
  )  

#+windows-target
(progn
  (defvar *current-process-handle*)
  (defvar *enum-process-modules-addr*)
  (defvar *get-module-file-name-addr*)
  (defvar *get-module-base-name-addr*)
  (defvar *get-module-handle-ex-addr*)

  (defun nbackslash-to-forward-slash (namestring)
    (dotimes (i (length namestring) namestring)
      (when (eql (schar namestring i) #\\)
        (setf (schar namestring i) #\/))))

  (defun init-windows-ffi ()
    (%revive-macptr *windows-invalid-handle*)
    (setq *current-process-handle* (ff-call (foreign-symbol-entry "GetCurrentProcess") :address)) 
    (setq *enum-process-modules-addr* (foreign-symbol-entry "EnumProcessModules"))   
    (setq *get-module-file-name-addr* (foreign-symbol-entry "GetModuleFileNameA"))
    (setq *get-module-base-name-addr* (foreign-symbol-entry "GetModuleBaseNameA"))
    (setq *get-module-handle-ex-addr* (foreign-symbol-entry "GetModuleHandleExA")))

  (init-windows-ffi)
  
  (defun hmodule-pathname (hmodule)
    (do* ((bufsize 128))
         ()
      (%stack-block ((name bufsize))
        (let* ((needed (ff-call *get-module-file-name-addr*
                                :address hmodule
                                :address name
                                :signed-fullword bufsize
                                :signed-fullword)))
          (if (eql 0 needed)
            (return nil)
            (if (<= bufsize needed)
              (setq bufsize (+ bufsize bufsize))
              (return (nbackslash-to-forward-slash (%str-from-ptr name needed)))))))))

  (defun hmodule-basename (hmodule)
    (do* ((bufsize 64))
         ()
      (%stack-block ((name bufsize))
        (let* ((needed (ff-call *get-module-base-name-addr*
                                :address *current-process-handle*
                                :address hmodule
                                :address name
                                :signed-fullword bufsize
                                :signed-fullword)))
          (if (eql 0 needed)
            (return nil)
            (if (< bufsize needed)
              (setq bufsize needed)
              (return (%str-from-ptr name needed))))))))

  (defun existing-shlib-for-hmodule (hmodule)
    (dolist (shlib *shared-libraries*)
      (when (eql hmodule (shlib.map shlib)) (return shlib))))
      
  
  (defun shared-library-from-hmodule (hmodule)
    (or (existing-shlib-for-hmodule hmodule)
        (let* ((shlib (%cons-shlib (hmodule-basename hmodule)
                                   (hmodule-pathname hmodule)
                                   hmodule
                                   hmodule)))
          (push shlib *shared-libraries*)
          shlib)))

  (defun for-each-loaded-module (f)
    (let* ((have (* 16 (record-length #>HMODULE))))
      (rlet ((pneed #>DWORD))
        (loop
          (%stack-block ((modules have))
            (ff-call *enum-process-modules-addr*
                     :address *current-process-handle*
                     :address modules
                     #>DWORD have
                     :address pneed)
            (let* ((need (pref pneed #>DWORD)))
              (if (> need have)
                (setq have need)
                (return
                  (do* ((i 0 (+ i (record-length #>HMODULE))))
                       ((= i need))
                    (funcall f (%get-ptr modules i)))))))))))

  (defun init-shared-libraries ()
    (for-each-loaded-module #'shared-library-from-hmodule))
  
  (defun shlib-containing-entry (addr &optional name)
    (with-macptrs ((p (%int-to-ptr addr)))
      (shlib-containing-address p name)))

  (defun shlib-containing-address (addr &optional name)
    (declare (ignore name))
    (rlet ((phmodule :address (%null-ptr)))
      (let* ((found (ff-call *get-module-handle-ex-addr*
                             #>DWORD (logior
                                      #$GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS
                                      #$GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT)
                             :address addr
                             :address phmodule
                             #>BOOL)))
        (unless (eql 0 found)
          (let* ((hmodule (pref phmodule :address)))
            (dolist (lib *shared-libraries*)
              (when (eql (shlib.map lib)  hmodule)
                (return lib))))))))


  (defun open-shared-library-internal (name)
    (let* ((hmodule (with-cstrs ((name name))
                      (ff-call
                       (%kernel-import target::kernel-import-GetSharedLibrary)
                       :address name
                       :unsigned-fullword 0
                       :address)))
           (shlib (unless (%null-ptr-p hmodule)
                    (shared-library-from-hmodule hmodule))))
      (if shlib
        (progn
          (incf (shlib.opencount shlib))
          (setf (shlib.handle shlib) hmodule)
          shlib)
        (values nil (%windows-error-string (get-last-windows-error))))))

  (init-shared-libraries)

  (defun revive-shared-libraries ()
    (dolist (lib *shared-libraries*)
      (setf (shlib.map lib) nil
            (shlib.handle lib) nil
            (shlib.pathname lib) nil
            (shlib.base lib) nil)
      (let* ((soname (shlib.soname lib))
             (soname-len (length soname)))
        (block found
          (for-each-loaded-module
           (lambda (m)
             (let* ((module-soname (hmodule-basename m)))
               (when (%simple-string= soname module-soname 0 0 soname-len (length module-soname))
                 (let* ((m (%inc-ptr m 0)))
                   (setf (shlib.base lib) m
                         (shlib.map lib) m
                         (shlib.pathname lib) (hmodule-pathname m)))
                 (return-from found)))))))))

  (defun reopen-user-libraries ()
    (dolist (lib *shared-libraries*)
      (unless (shlib.map lib)
        (let* ((handle (with-cstrs ((name (shlib.soname lib)))
                         (ff-call
                          (%kernel-import target::kernel-import-GetSharedLibrary)
                          :address name
                          :unsigned-fullword 0
                          :address))))
          (unless (%null-ptr-p handle)
            (setf (shlib.handle lib) handle
                  (shlib.base lib) handle
                  (shlib.map lib) handle
                  (shlib.pathname lib) (hmodule-pathname handle)
                  (shlib.opencount lib) 1))))))
           
              

;;; end windows-target
  )  


(defun ensure-open-shlib (c force)
  (if (or (shlib.handle c) (not force))
    *rtld-use*
    (error "Shared library not open: ~s" (shlib.soname c))))

(defun resolve-container (c force)
  (if c
    (ensure-open-shlib c force)
    *rtld-use*
    ))




;;; An "entry" can be fixnum (the low 2 bits are clear) which represents
;;; a (32-bit word)-aligned address.  That convention covers all
;;; function addresses on ppc32 and works for addresses that are
;;; 0 mod 8 on PPC64, but can't work for things that're byte-aligned
;;; (x8664 and other non-RISC platforms.)
;;; For PPC64, we may have to cons up a macptr if people use broken
;;; linkers.  (There are usually cache advantages to aligning ppc
;;; function addresses on at least a 16-byte boundary, but some
;;; linkers don't quite get the concept ...)

(defun foreign-symbol-entry (name &optional (handle *rtld-use*))
  "Try to resolve the address of the foreign symbol name. If successful,
return a fixnum representation of that address, else return NIL."
  (with-cstrs ((n name))
    #+ppc-target
    (with-macptrs (addr)      
      (%setf-macptr addr
		    (ff-call (%kernel-import target::kernel-import-FindSymbol)
			     :address handle
			     :address n
			     :address))
      (unless (%null-ptr-p addr)	; No function can have address 0
	(or (macptr->fixnum addr) (%inc-ptr addr 0))))
    #+(or x8632-target arm-target)
    (let* ((addr (ff-call (%kernel-import target::kernel-import-FindSymbol)
			  :address handle
			  :address n
			  :unsigned-fullword)))
      (unless (eql 0 addr) addr))
    #+x8664-target
    (let* ((addr (ff-call (%kernel-import target::kernel-import-FindSymbol)
                          :address handle
                          :address n
                          :unsigned-doubleword)))
      (unless (eql 0 addr) addr))))

(defvar *statically-linked* nil)

#+(or linux-target freebsd-target solaris-target)
(progn

(defun %library-base-containing-address (address)
  (rletZ ((info :<D>l_info))
    (let* ((status (ff-call *dladdr-entry*
                            :address address
                            :address info :signed-fullword)))
      (declare (integer status))
      (unless (zerop status)
        (pref info :<D>l_info.dli_fbase)))))
  
(defun shlib-containing-address (address &optional name)
  (declare (ignore name))
  (let* ((base (%library-base-containing-address address)))
    (if base
      (shared-library-at base))))


(defun shlib-containing-entry (entry &optional name)
  (unless *statically-linked*
    (with-macptrs (p)
      (entry->addr entry p)
      (shlib-containing-address p name))))
)

#+darwin-target
(progn
(defvar *dyld-image-count*)
(defvar *dyld-get-image-header*)
(defvar *dyld-get-image-name*)
(defvar *nslookup-symbol-in-image*)
(defvar *nsaddress-of-symbol*)
(defvar *nsmodule-for-symbol*)
(defvar *ns-is-symbol-name-defined-in-image*)
(defvar *dladdr-entry* 0)
(defvar *dlopen-entry* 0)
(defvar *dlerror-entry* 0)

(defun setup-lookup-calls ()
  (setq *dladdr-entry* (foreign-symbol-entry "dladdr"))
  (setq *dlopen-entry* (foreign-symbol-entry "dlopen"))
  (setq *dlerror-entry* (foreign-symbol-entry "dlerror")) 
  (setq *dyld-image-count* (foreign-symbol-entry "_dyld_image_count"))
  (setq *dyld-get-image-header* (foreign-symbol-entry "_dyld_get_image_header"))
  (setq *dyld-get-image-name* (foreign-symbol-entry "_dyld_get_image_name"))
)

(setup-lookup-calls)

(defun open-shared-library-internal (name)
  (with-cstrs ((cname name))
    (let* ((handle (ff-call *dlopen-entry*
                            :address cname
                            :int (logior #$RTLD_GLOBAL #$RTLD_NOW)
                            :address)))
      (if (%null-ptr-p handle)
        (values nil (%get-cstring (ff-call *dlerror-entry* :address)))
        (let* ((lib (shared-library-with-handle handle)))
          (unless lib
            (setq lib (%cons-shlib name name nil nil))
            (setf (shlib.handle lib) handle)
	    (push lib *shared-libraries*))
          (incf (shlib.opencount lib))
          (values lib nil))))))

;;;
;;; When restarting from a saved image
;;;
(defun reopen-user-libraries ()
  (dolist (lib *shared-libraries*)
    (setf (shlib.handle lib) nil
	  (shlib.base lib) nil))
  (dolist (lib *shared-libraries*)
    (with-cstrs ((cname (shlib.soname lib)))
      (let* ((handle (ff-call *dlopen-entry*
                              :address cname
                              :int (logior #$RTLD_GLOBAL #$RTLD_NOW)
                              :address)))
        (unless (%null-ptr-p handle)
          (setf (shlib.handle lib) handle))))))

(defun shlib-containing-address (address &optional name)
  (declare (ignore name))
  (%stack-block ((info (record-length #>Dl_info) :clear t))
    (unless (zerop (ff-call *dladdr-entry*
                            :address address
                            :address info
                            :signed-fullword))
      (let* ((addr (pref info #>Dl_info.dli_fbase))
             (name (%get-cstring (pref info #>Dl_info.dli_fname)))
             (namelen (length name)))
        (dolist (lib *shared-libraries*)
          (let* ((shlibname  (shlib.pathname lib))
                 (shlibnamelen (length shlibname)))
          (when (%simple-string= name shlibname 0 0 namelen shlibnamelen)
            (unless (shlib.base lib)
              (setf (shlib.base lib) addr)
              #+no; don't change soname of existing library
              (let* ((soname  (soname-from-mach-header addr)))
                (when soname
                  (setf (shlib.soname lib) soname))))
            (return lib))))))))

(defun shlib-containing-entry (entry &optional name)
  (unless name
    (error "foreign name must be non-NIL."))
  (with-macptrs (addr)
    (entry->addr entry addr)
    (shlib-containing-address addr name)))

(defun soname-from-mach-header (header)
  (do* ((p (%inc-ptr header
                     #+64-bit-target (record-length :mach_header_64)
                     #-64-bit-target (record-length :mach_header))
           (%inc-ptr p (pref p :load_command.cmdsize)))
        (i 0 (1+ i))
        (n (pref header
                 #+64-bit-target :mach_header_64.ncmds
                 #-64-bit-target :mach_header.ncmds)))
       ((= i n))
    (when (= #$LC_ID_DYLIB (pref p :load_command.cmd))
      (return (%get-cstring (%inc-ptr p (record-length :dylib_command)))))))

                 
                     
                                                           
(defun init-shared-libraries ()
  (do* ((count (ff-call *dyld-image-count* :unsigned-fullword))
        (i 1 (1+ i)))
       ((= i count))
    (declare (fixnum i count))
    (let* ((addr (ff-call *dyld-get-image-header* :unsigned-fullword i :address))
           (nameptr (ff-call *dyld-get-image-name* :unsigned-fullword i :address))
           (name (%get-cstring nameptr ))
           (lib (%cons-shlib (or (soname-from-mach-header addr) name) name nil addr)))
      (setf (shlib.handle lib)
            (ff-call *dlopen-entry* :address nameptr :unsigned-fullword (logior #$RTLD_GLOBAL #$RTLD_NOLOAD)))
      (push lib *shared-libraries*))))

(init-shared-libraries)

;; end Darwin progn
)

#-(or linux-target darwin-target freebsd-target solaris-target windows-target)
(defun shlib-containing-entry (entry &optional name)
  (declare (ignore entry name))
  *rtld-default*)


(defun resolve-eep (e &optional (require-resolution t))
  (or (eep.address e)
      (let* ((name (eep.name e))
	     (container (eep.container e))
             (handle (resolve-container container require-resolution))
	     (addr (foreign-symbol-entry name handle)))
	(if addr
	  (progn
	    (unless container
	      (setf (eep.container e) (shlib-containing-entry addr name)))
	    (setf (eep.address e) addr))
	  (if require-resolution
	    (error "Can't resolve foreign symbol ~s" name))))))



(defun foreign-symbol-address (name &optional (map *rtld-use*))
  "Try to resolve the address of the foreign symbol name. If successful,
return that address encapsulated in a MACPTR, else returns NIL."
  (with-cstrs ((n name))
    (let* ((addr (ff-call (%kernel-import target::kernel-import-FindSymbol) :address map :address n :address)))
      (unless (%null-ptr-p addr)
        addr))))

(defun resolve-foreign-variable (fv &optional (require-resolution t))
  (or (fv.addr fv)
      (let* ((name (fv.name fv))
	     (container (fv.container fv))
             (handle (resolve-container container require-resolution))
	     (addr (foreign-symbol-address name handle)))
	(if addr
	  (progn
	    (unless container
	      (setf (fv.container fv) (shlib-containing-address addr name)))
	    (setf (fv.addr fv) addr))
	  (if require-resolution
	    (error "Can't resolve foreign symbol ~s" name))))))

(defun load-eep (name)
  (let* ((eep (or (gethash name (eeps)) (setf (gethash name *eeps*) (%cons-external-entry-point name)))))
    (resolve-eep eep nil)
    eep))

(defun load-fv (name type)
  (let* ((fv (or (gethash name (fvs)) (setf (gethash name *fvs*) (%cons-foreign-variable name type)))))
    (resolve-foreign-variable fv nil)
    fv))

         




#+(or linux-target freebsd-target solaris-target)
(progn

;;; Return the position of the last dot character in name, if that
;;; character is followed by one or more decimal digits (e.g., the
;;; start of a numeric suffix on a library name.)  Return NIL if
;;; there's no such suffix.
(defun last-dot-pos (name)
  (do* ((i (1- (length name)) (1- i))
        (default i)
        (trailing-digits nil))
       ((<= i 0) default)
    (declare (fixnum i))
    (let* ((code (%scharcode name i)))
      (declare (type (mod #x110000) code))
      (if (and (>= code (char-code #\0))
               (<= code (char-code #\9)))
        (setq trailing-digits t)
        (if (= code (char-code #\.))
          (return (if trailing-digits i))
          (return default))))))
  
;;; It's assumed that the set of libraries that the OS has open
;;; (accessible via the _dl_loaded global variable) is a subset of
;;; the libraries on *shared-libraries*.

(defun revive-shared-libraries ()
  (dolist (lib *shared-libraries*)
    (setf (shlib.map lib) nil
	  (shlib.pathname lib) nil
	  (shlib.base lib) nil)
    (let* ((soname (shlib.soname lib))
           (last-dot (if soname (last-dot-pos soname))))
      (when soname
	(with-cstrs ((soname soname))
	  (let* ((map (block found
			(%walk-shared-libraries
			 #'(lambda (m)
			     (with-macptrs (libname)
			       (%setf-macptr libname
					     (soname-ptr-from-link-map m))
			       (unless (%null-ptr-p libname)
				 (when (or (%cstrcmp soname libname)
                                           (and last-dot
                                                (%cnstrcmp soname libname (1+ last-dot))))
				   (return-from found  m)))))))))
	    (when map
	      ;;; Sigh.  We can't reliably lookup symbols in the library
	      ;;; unless we open the library (which is, of course,
	      ;;; already open ...)  ourselves, passing in the
	      ;;; #$RTLD_GLOBAL flag.
              #+linux-target
	      (ff-call (%kernel-import target::kernel-import-GetSharedLibrary)
		       :address soname
		       :unsigned-fullword *dlopen-flags*
		       :void)
	      (setf (shlib.base lib) (link_map.l_addr map)
		    (shlib.pathname lib) (%get-cstring
					  (pref map :link_map.l_name))
                    (shlib.soname lib) (%get-cstring (soname-ptr-from-link-map map))
		    (shlib.map lib) map))))))))

;;; Repeatedly iterate over shared libraries, trying to open those
;;; that weren't already opened by the kernel.  Keep doing this until
;;; we reach stasis (no failures or no successes.)

(defun %reopen-user-libraries ()
  (loop
      (let* ((win nil)
	     (lose nil))
	(dolist (lib *shared-libraries*)
	  (let* ((map (shlib.map lib))
                 (handle (shlib.handle lib)))
	    (unless map
	      (with-cstrs ((soname (shlib.soname lib)))
		(setq handle
                      (ff-call
                       (%kernel-import target::kernel-import-GetSharedLibrary)
                       :address soname
                       :unsigned-fullword *dlopen-flags*
                       :address))
                #-(or freebsd-target solaris-target android-target) (setq map handle)
                #+android-target (setq map
                                       (if (%null-ptr-p handle)
                                         handle
                                         (pref handle :soinfo.linkmap)))
                #+(or freebsd-target solaris-target)
                (setq map
                      (if (%null-ptr-p handle)
                        handle
                        (rlet ((p :address))
                          (if (eql 0 (ff-call
                                      (foreign-symbol-entry "dlinfo")
                                      :address handle
                                      :int #$RTLD_DI_LINKMAP
                                      :address p
                                      :int))
                            (pref p :address)
                            (%null-ptr)))))
		(if (%null-ptr-p map)
		  (setq lose t)
		  (setf (shlib.pathname lib)
			(%get-cstring (pref map :link_map.l_name))
			(shlib.base lib)
			(link_map.l_addr map)
			(shlib.map lib) map
                        (shlib.handle lib) handle
			win t))))))
	(when (or (not lose) (not win)) (return)))))
)


(defun refresh-external-entrypoints ()
  #+linux-target
  (setq *statically-linked* (not (eql 0 (%get-kernel-global 'statically-linked))))
  (%revive-macptr *rtld-next*)
  (%revive-macptr *rtld-default*)
  #+(or linux-target freebsd-target solaris-target)
  (unless *statically-linked*
    (setq *dladdr-entry* (foreign-symbol-entry "dladdr"))
    (revive-shared-libraries)
    (%reopen-user-libraries))
  #+darwin-target
  (progn
    (setup-lookup-calls)
    (reopen-user-libraries))
  #+windows-target
  (progn
    (init-windows-ffi)
    (revive-shared-libraries)
    (reopen-user-libraries))
  (when *eeps*
    (without-interrupts 
     (maphash #'(lambda (k v) 
                  (declare (ignore k)) 
                  (setf (eep.address v) nil) 
                  (resolve-eep v nil))
              *eeps*)))
  (when *fvs*
    (without-interrupts
     (maphash #'(lambda (k v)
                  (declare (ignore k))
                  (setf (fv.addr v) nil)
                  (resolve-foreign-variable v nil))
              *fvs*))))

(defun open-shared-library (name &optional (process #+darwin-target :initial
                                                    #-darwin-target :current))
  "If the library denoted by name can be loaded by the operating system,
return an object of type SHLIB that describes the library; if the library
is already open, increment a reference count. If the library can't be
loaded, signal a SIMPLE-ERROR which contains an often-cryptic message from
the operating system."
    (multiple-value-bind (lib error-string)
        (if (or (eq process :current)
                (eq process *current-process*)
                (and (eq process :initial)
                     (eq *current-process* *initial-process*)))
          (open-shared-library-internal name)
          
          (call-in-process (lambda ()
                             (handler-case (open-shared-library-internal  name)
                               (error (condition) (values nil (format nil "~a" condition)))))
                                                                     
                             
                           (if (eq process :initial)
                             *initial-process*
                             process)))
      (or lib
          (error "Error opening shared library ~a : ~a." name error-string))))


