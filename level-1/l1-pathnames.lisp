;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   Portions copyright (c) 2001 Clozure Associates.
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html



;; L1-pathnames.lisp
;-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;ANSI CL logical pathnames

(in-package "CCL")

(defun heap-image-name ()
  (let* ((p (%null-ptr)))
    (declare (dynamic-extent p))
    #-windows-target
    (%get-cstring (%get-kernel-global-ptr 'image-name p))
    #+windows-target
    (strip-drive-for-now
     (nbackslash-to-forward-slash
      (%get-cstring (%get-kernel-global-ptr 'image-name p))))))

(defloadvar *heap-image-name* (heap-image-name))

(defloadvar *command-line-argument-list*
  (let* ((argv (%null-ptr))
	 (res ()))
    (declare (dynamic-extent argv))
    (%get-kernel-global-ptr 'argv argv)
    (do* ((i 0 (+ i target::node-size))
	  (arg (%get-ptr argv i) (%get-ptr argv i)))
	 ((%null-ptr-p arg) (nreverse res))
      (declare (fixnum i))
      (push (%get-cstring arg) res))))

;These are used by make-pathname
(defun %verify-logical-component (name type)
  (when (and name (neq name :unspecific))
    (setq name (ensure-simple-string name))
    (when (or (eql 0 (length name))
              (%str-member *pathname-escape-character* name) ;; Hmm, why?
              (%path-mem "/;" name))
      (error "Illegal logical pathname ~A component ~S" type name)))
  name)


(defun verify-logical-host-name (host)
  (or (and host
	   (%verify-logical-component host "host")
	   (%str-assoc host %logical-host-translations%)
	   host)
      (host-error host)))

(defun %logical-version-component (version)
  (if (or (fixnump version)
          (stringp version)
          (memq version '(nil :wild :newest :unspecific)))
    version
    (require-type version '(or fixnum string (member nil :wild :newest :unspecific)))))

(defun logical-pathname-translations (host)
  "Return the (logical) host object argument's list of translations."
  (setq host (verify-logical-host-name host))
  (let ((translations (%str-assoc host %logical-host-translations%)))
    (unless translations (host-error host))
    (%cdr translations)))

(defun logical-host-p (host)
  (%str-assoc host %logical-host-translations%))

(defun host-error (host) ; supposed to be a type-error
  (signal-type-error host  '(satisfies logical-host-p) "~S is not a defined logical host"))

(defun set-logical-pathname-translations (host list)
  (setq host (%verify-logical-component  host "host"))
  (let ((old (%str-assoc host %logical-host-translations%))
	(new (let ((%logical-host-translations% (cons (list host) %logical-host-translations%)))
	       ;; Do this in the context when host is defined, so no errors.
	       (mapcar #'(lambda (trans)
			   (destructuring-bind (from to &rest ignored) trans
			     (declare (ignore ignored))
			     (let ((from-path (parse-namestring from host))
				   (to-path (pathname to)))
			       (list (require-type from-path 'logical-pathname) to-path))))
		       list))))
    (if old
      (progn (%rplaca old host) (%rplacd old new))
      (push (cons host new) %logical-host-translations%)))
  list)

(defsetf logical-pathname-translations set-logical-pathname-translations)

;;; doesnt check if already there - adds at front 
(defun add-logical-pathname-translation (host translation)
  (let ((trans (%str-assoc host  %logical-host-translations%)))
    (if (not trans)
      (set-logical-pathname-translations host (list translation))
      (let ((new (destructuring-bind (from to &rest ignored) translation
		   (declare (ignore ignored))
		   (list (parse-namestring from host) (pathname to)))))
        (rplacd trans (cons new (cdr trans)))
        (cdr trans)))))

(defun %component-match-p (name wild) 
  (if (or (eq name :unspecific)(eq name :wild)(eq name :wild-inferiors)(and (stringp name) (or  (string= name "*")(string= name "**"))))
    (setq name nil))  
  (if (or (eq wild :unspecific)(eq wild :wild)(eq wild :wild-inferiors)(and (stringp wild) (or (string= wild "*")(string= wild "**"))))
    (setq wild nil))
  (cond ((null name) 
         (null wild))
        ((null wild)
         t)
        ((not (and (stringp name) (stringp wild)))
         (eq name wild))
        (t (%path-str*= name wild))))

(defun translate-directory (source from to reversible &optional thost)
  (declare (ignore thost)) ;; leftover from a mac kludge.
  (let* ((result (translate-directory2 (cdr source)(cdr from)(cdr to) reversible))
	 (relative-p (eq (car source) :relative)))
    (cond ((and (not relative-p)(eq result (cdr source))) (or source (list :absolute)))
	  ((and (not relative-p)(eq result (cdr to))) to)
	  (t (cons (car (or to source from)) result)))))



(defun translate-directory2 (source from to reversible)
  ; we already know it matches
  (let (result srest match tfirst trest twild)
    (multiple-value-setq (tfirst trest twild)
			 (%split-ccdirectory to))
    (when (and to (not twild))
      (return-from translate-directory2 to))
    (multiple-value-bind (ffirst frest fwild)
			 (%split-ccdirectory from)
      (setq srest (nthcdr (length ffirst) source))
      (cond ((eq fwild '**)
	     (setq match (nth-value 1 (%pathname-match-dir1 srest frest t)))               
	     (cond ((eq twild '**)
		    (setq result (nconc tfirst match))
		    (setq srest (nthcdr (length match) srest)))
		   (t (return-from translate-directory2
			(translate-directory2 source (nconc ffirst match frest)
					      to reversible)))))
	    ((eq twild '**)
	     (let ((length (length tfirst)))
	       (setq srest (nthcdr length source))
	       (setq frest (nthcdr length from))
	       (setq  match (nth-value 1 (%pathname-match-dir1 srest trest t)))
	       (cond ((null  match)
		      (setq result tfirst))
		     (t (setq srest (nthcdr (setq length (length match)) srest))
			(setq frest (nthcdr length frest))
			(setq result (nconc tfirst match))))))
	    (t
	     (cond ((null fwild)
		    ; to has a wild component e.g. *abc, from is not wild
		    ; by defintion source is also not wild
		    ; which random source component gets plugged in here??
		    (setq srest (nthcdr (length tfirst) source))
		    (setq frest (nthcdr (length tfirst) source))))
	     (let ((part (translate-component
				(car srest) (car frest)(car trest) reversible)))
	       (if (null part)(setq result tfirst)
		   (progn
		     (setq part (list part))
		     (setq result (nconc tfirst part)))))
	     (setq srest (cdr srest) frest (cdr frest) trest (cdr trest))))
      (when trest 
	(let ((foo (translate-directory2 srest frest trest reversible)))
	  (when foo (setq result (nconc result foo))))))
    result))

; cc stands for cdr canonical
; ("abc" "**" "def" => ("abc") ("def")
; ("abc" "*de") => ("abc") ("*de")
(defun %split-ccdirectory (dir)
  (let ((pos 0) (wildp nil)(rest dir))
    (dolist (e dir)
      (case e
        (:wild (setq wildp '*))
        (:wild-inferiors 
         (setq wildp '**)
         (setq rest (cdr rest)))
	(:up nil)
        (t 
         (when (%path-mem "*" e)
           (cond ((string= e "**")
                  (setq rest (cdr rest))
                  (setq wildp '**))
                 ((eql 1 (length (the string e)))
                  (setq wildp '*))
                 (t (setq wildp t))))))
      (when wildp (return))
      (setq rest (cdr rest))
      (setq pos (%i+ 1 pos)))
    (cond ((not wildp)
           (values dir))
          (t (let (first)
               (when rest (setq rest (copy-list rest)))
               (dotimes (i pos)
                 (declare (fixnum i))
                 (push (car dir) first)
                 (setq dir (cdr dir)))
               (values (nreverse first) rest wildp))))))

; could avoid calling component-match-p by checking here maybe
; if "gazonk" "gaz*" "h*" => "honk"
; then "gazonk" "gaz*" "*" => "onk" or is it "gazonk" (per pg 625)
; I believe in symbolics land "gazonk" is a regular translation
; and "onk" is a reversible translation (achieved by not doing pg 625) AHH
; similarly is "a:" "a:**:" "**"  Nil or "a:" 
(defun translate-component (source from to &optional reversible)                   
  (let ((orig-to to))
    (cond 
     ((and (consp source)(consp from)) ; source and from both logical 
      (setq source (cadr source) from (cadr from)))
     ((or (consp source)(consp from)) ;  or neither
      #-bccl (error "Something non-kosher in translate pathname")
      ))
    (when (memq from '(:wild :wild-inferiors)) (setq from "*"))
    (when (memq source '(:wild :wild-inferiors))(setq source "*"))
    (when (memq to '(:wild :wild-inferiors))(setq to "*"))
    (cond ((consp to)(setq to (cadr to))))  ;??
    (cond ((and (stringp to)(not (%path-mem "*" to)))
           to)
          ((and (or (not reversible)(not (stringp source))) ; <<
                (or (null to)
                    (and (stringp to)(or (string= to "**")(string= to "*")))))
           source)
          ((eq to :unspecific) to)  ; here we interpret :unspecific to mean don't want it
          ((not (stringp source)) to)
          (t 
           (let ((slen (length source)) srest match spos result (f2 nil) snextpos)
             (multiple-value-bind (tfirst trest twild)
                                  (%split-component to)
               (cond ((and to (not twild))(return-from translate-component to)))
               (multiple-value-bind (ffirst frest fwild)
                                    (%split-component from)          
                 (cond (fwild
                        (setq spos (if ffirst (length ffirst) 0))       ; start of source hunk
                        (if frest (setq f2 (%split-component frest)))
                        (setq snextpos (if f2 (%path-member f2 source spos) slen))
                        (setq match (%substr source spos snextpos))
                        (if frest (setq srest (%substr source snextpos slen)))
                        (setq result (if tfirst (%str-cat tfirst match) match))
                        (when frest 
                          (let ((foo (translate-component srest frest trest reversible)))
                            (when foo (setq result (%str-cat result foo))))))
                       (t  ; to is wild, from and source are not
                        (setq result (if tfirst (%str-cat tfirst source) source))
                        (when trest (setq result (%str-cat result trest))))))
               (if (consp orig-to)(progn (error "shouldnt")(list :logical result)) result) ; 7/96
               ))))))


(defun %path-member (small big &optional (start 0))
  (let* ((end (length big))
         (s-end (length small))
         (s-start 1)
         (c1 (%schar small 0))
         (pstart start))
    (if (%i> s-end end)(return-from %path-member nil))
    (when (eql c1 *pathname-escape-character*)
      (setq c1 (%schar small 1))
      (setq s-start 2))      
    (while (and (progn (if (eql (%schar big pstart) *pathname-escape-character*)
                         (setq pstart (%i+ pstart 1)))
                       T)
                (%i< pstart end)
                (neq (%schar big pstart) c1))
      (setq pstart (%i+ pstart 1)))
    (if (neq c1 (%schar big pstart))(return-from %path-member nil))
    (setq start (%i+ pstart 1))
    (while (and (progn (if (eql (%schar big start) *pathname-escape-character*)
                         (setq start (%i+ 1 start)))
                       (if (eql (%schar small s-start) *pathname-escape-character*)
                         (setq s-start (%i+ 1 s-start)))
                       T)
                (%i< start end)
                (%i< s-start s-end)
                (eql (%schar big start)(%schar small s-start)))
      (setq start (%i+ start 1) s-start (%i+ s-start 1)))
    (cond ((= (the fixnum s-start) (the fixnum s-end))
            pstart)
          ((%i< start end)
            (%path-member small big (%i+ 1 pstart)))
          (T nil))))

(defun %split-component (thing &aux pos)
  ;"ab*cd*"  ->  "ab" "cd*"  
  (if (or (not (typep thing 'string))(null (setq pos (%path-mem "*" thing))))
    (values thing nil nil)
    (let* ((len (length thing)))
      (declare (fixnum len))
      (values (if (%izerop pos) nil (%substr thing 0 pos))
              (cond ((eql len (%i+ pos 1)) nil)
                    (t 
                     (when (eq (%schar thing (+ pos 1)) #\*)
                       (setq pos (+ pos 1)))
                     (cond ((eql len (%i+ pos 1)) nil)
                           (t (%substr thing (%i+ pos 1) len)))))
              T))))

(defun translate-pathname (source from-wildname to-wildname &key reversible)
  "Use the source pathname to translate the from-wildname's wild and
   unspecified elements into a completed to-pathname based on the to-wildname."
  (when (not (pathnamep source)) (setq source (pathname source)))
  (flet ((translate-pathname-component-mismatch (component-name source from)
	   (error "~S components of source ~S and from-wildname ~S do not match" component-name source from)))
    (let (r-host  r-directory r-name r-type r-version s-host f-host t-host t-device)
      (setq s-host (pathname-host source))
      (setq f-host (pathname-host from-wildname))
      (setq t-host (pathname-host to-wildname))
      (setq t-device (pathname-device to-wildname))
      (if (not (%host-component-match-p s-host f-host)) (translate-pathname-component-mismatch 'pathname-host source from-wildname))
      (setq r-host (translate-component s-host f-host t-host reversible))
      (let ((s-dir (%std-directory-component (pathname-directory source) s-host))
            (f-dir (%std-directory-component (pathname-directory from-wildname) f-host))
            (t-dir (%std-directory-component (pathname-directory to-wildname) t-host)))
        (let ((match (%pathname-match-directory s-dir f-dir)))
          (if (not match)(translate-pathname-component-mismatch 'pathname-directory source from-wildname))
          (setq r-directory  (translate-directory s-dir f-dir t-dir reversible t-host))))
      (let ((s-name (pathname-name source))
            (f-name (pathname-name from-wildname))
            (t-name (pathname-name to-wildname)))
        (if (not (%component-match-p s-name f-name))(translate-pathname-component-mismatch 'pathname-name  source from-wildname))        
        (setq r-name (translate-component s-name f-name t-name reversible)))
      (let ((s-type (pathname-type source))
            (f-type (pathname-type from-wildname))
            (t-type (pathname-type to-wildname)))
        (if (not (%component-match-p s-type f-type))(translate-pathname-component-mismatch 'pathname-component source from-wildname))
        (setq r-type (translate-component s-type f-type t-type reversible)))
      (let ((s-version (pathname-version source))
            (f-version (pathname-version from-wildname))
            (t-version (pathname-version to-wildname)))
        (if (not (%component-match-p s-version f-version)) (translate-pathname-component-mismatch 'pathname-version source from-wildname))
        (setq r-version (translate-component s-version f-version t-version reversible))
        ;(if (eq r-version :unspecific)(setq r-version nil))
        )
      (make-pathname :device t-device :host r-host :directory r-directory
                     :name r-name :type r-type :version r-version :defaults nil)
      )))



(defvar %empty-logical-pathname% (%cons-logical-pathname nil nil nil nil nil))

(defun logical-pathname-namestring-p (string)
  (multiple-value-bind (sstr start end) (get-sstring string)
    (let ((host (pathname-host-sstr sstr start end t)))
      (and host (not (eq host :unspecific))))))

  
;; This extends CL in that it allows a host-less pathname, like "foo;bar;baz".
(defun logical-pathname (thing &aux (path thing))
  "Converts the pathspec argument to a logical-pathname and returns it."
  (when (typep path 'stream) (setq path (%path-from-stream path)))
  (etypecase path
    (logical-pathname path)
    (pathname (report-bad-arg thing 'logical-pathname))
    (string
     (multiple-value-bind (sstr start end) (get-sstring path)
       ;; Prescan the host, to avoid unknown host errors.
       (let ((host (pathname-host-sstr sstr start end t)))
         (when (or (null host) (eq host :unspecific))
           (report-bad-arg path '(satisfies logical-pathname-namestring-p)))
	 (let ((%logical-host-translations% (cons (list host) %logical-host-translations%)))
	   (declare (special %logical-host-translations%))
	   ;; By calling string-to-pathname with a logical pathname as default, we force
	   ;; parsing as a logical pathname.
	   (string-to-pathname sstr start end nil %empty-logical-pathname%)))))))

(defun %host-component-match-p (path-host wild-host)
  ;; Note that %component-match-p is case sensitive.  Need a
  ;; case-insensitive version for hosts. 
  ;; In addition, host components do not support wildcards.
  (or (eq path-host wild-host)
      (and (stringp path-host)
	   (stringp wild-host)
	   (string-equal path-host wild-host))))

(defun pathname-match-p (pathname wildname)
  "Pathname matches the wildname template?"
  (let ((path-host (pathname-host pathname))
        (wild-host (pathname-host wildname)))
    (and
     (%host-component-match-p path-host wild-host)
     (%component-match-p (pathname-device pathname)(pathname-device wildname))
     (%pathname-match-directory
      (%std-directory-component (pathname-directory pathname) path-host)
      (%std-directory-component (pathname-directory wildname) wild-host))
     (%component-match-p (pathname-name pathname)(pathname-name wildname))
     (%component-match-p (pathname-type pathname)(pathname-type wildname))
     (%component-match-p (pathname-version pathname)(pathname-version wildname)))))


; expects canonicalized directory - how bout absolute vs. relative?
(defun %pathname-match-directory (path wild)
  (cond ((equal path wild) t)
	 ; Don't allow matching absolute and relative, so that can have distinct
	 ; absolute and wild translations in logical-pathname-translations for
	 ; a host, and have them match separately.
	((and (consp path)(consp wild)(neq (car path) (car wild)))
	 nil)  ; one absolute & one relative ??
        ((or ;(and (null wild)
             ;     (let ((dir (cadr path)))
             ;       (if (stringp dir)(string= dir "**")(eq dir :wild-inferiors))))
             (and (null (cddr wild))
                  (let ((dir (cadr wild)))
                    (if (stringp dir)(string= dir "**")(eq dir :wild-inferiors))))))
	((null path)
	 ;; Make missing dir match (:absolute) or (:relative) - is that right?
	 (null (cdr wild)))
	((null wild)
	 nil)
        (t (%pathname-match-dir0 (cdr path)(cdr wild)))))

; munch on tails path and wild 
(defun %pathname-match-dir0 (path wild)
  (flet ((only-wild (dir)
                    (when (null (cdr dir))
                      (setq dir (car dir))
                      (if (stringp dir)(string= dir "**")(eq dir :wild-inferiors)))))
    (cond ((eq path wild) t)
          ((only-wild wild)
           t)
          (t (let ((result t))
               (block nil 
                 (while (and path wild)
                   (let ((pathstr (car path))
                         (wildstr (car wild)))
                     (case wildstr
                       (:wild (setq wildstr "*"))
                       (:wild-inferiors (setq wildstr "**")))
                     (case pathstr
                       (:wild (setq pathstr "*"))
                       (:wild-inferiors (setq pathstr "**")))
                     (when (not 
                            (cond ((string= wildstr "**")
                                   (setq result (%pathname-match-dir1 path (cdr wild)))
                                   (return-from nil))
                                  ((%path-str*= pathstr wildstr))))
                       (setq result nil)
                       (return-from nil))
                     (setq wild (cdr wild) path (cdr path))))
                 (when (and (or path wild)(not (only-wild wild)))
                   (setq result nil)))
               result)))))

(defun %pathname-match-dir0 (path wild)
  (flet ((only-wild (dir)
                    (when (null (cdr dir))
                      (setq dir (car dir))
                      (when (consp dir) (setq dir (cadr dir)))
                      (if (stringp dir)(string= dir "**")(eq dir :wild-inferiors)))))
    (cond ((eq path wild) t)
          ((only-wild wild)
           t)
          (t (let ((result t))
               (block nil 
                 (while (and path wild)
                   (let ((pathstr (car path))
                         (wildstr (car wild)))                     
                     ; allow logical to match physical today
                     ; because one of these days these logical things will disappear!
                     (when (consp pathstr)(setq pathstr (cadr pathstr)))
                     (when (consp wildstr)(setq wildstr (cadr wildstr)))
                     (case wildstr
                       (:wild (setq wildstr "*"))
                       (:wild-inferiors (setq wildstr "**")))
                     (case pathstr
                       (:wild (setq pathstr "*"))
                       (:wild-inferiors (setq pathstr "**")))
                     (if (or (memq wildstr '(:up :back))(memq pathstr '(:up :back))) ;; ????? <<<<
                       (when (neq pathstr wildstr)(setq result nil) (return-from nil))
                       (when (not 
                              (cond ((string= wildstr "**")
                                     (setq result (%pathname-match-dir1 path (cdr wild)))
                                     (return-from nil))
                                    ((%path-str*= pathstr wildstr))))
                         (setq result nil)
                         (return-from nil)))
                     (setq wild (cdr wild) path (cdr path))))
                 (when (and (or path wild)(not (only-wild wild)))
                   (setq result nil)))
               result)))))



; wild is stuff after a "**" - looking for what matches the **  in (path)
(defun %pathname-match-dir1 (path wild &optional cons-result)
  (let ((match nil) pathstr wildstr)
    (cond ((null wild)
           (values T (if cons-result (mapcar #'(lambda (e)
                                            (if (consp e)(cadr e) e))
                                        path))))
          ((%pathname-match-dir0 path wild))   ; ie ** matches nothing
          (t 
           (prog nil
             AGN
               (setq pathstr (car path) wildstr (car wild))
               (when (consp pathstr)(setq pathstr (cadr pathstr)))
               (when (consp wildstr)(setq wildstr (cadr wildstr)))
               (case wildstr
                 (:wild (setq wildstr "*"))
                 (:wild-inferiors (setq wildstr "**")))
               (case pathstr
                 (:wild (setq pathstr "*"))
                 (:wild-inferiors (setq pathstr "**")))
               (until (or (not (consp path))
                          (%path-str*= pathstr wildstr))
                 (when cons-result (push pathstr match))
                 (setq path (cdr path))
                 (setq pathstr (car path))
                 (when (consp pathstr)(setq pathstr (cadr pathstr))))
               ;; either got a match - w and path both have the thing we looked for
               ;; or path is empty
               (when (null path)(return nil))
               (let ((path1 (cdr path))(wild (cdr wild)))
                 (when (and (null path1)(null wild))
                   (return (values t (when match (nreverse match)))))
                 (cond ((%pathname-match-dir0 path1 wild)  ; is the rest happy too?
                        (return (values t (nreverse match))))
                       (t (when cons-result (push pathstr match)) ; nope, let ** eat more
                          (setq path (cdr path))
                          (go AGN)))))))))

; three times bigger and 3 times slower - does it matter?
(defun %path-str*= (string pattern)
  (multiple-value-bind (string s-start s-end) (get-sstring string)
    (multiple-value-bind (pattern p-start p-end) (get-sstring pattern)
      (path-str-sub pattern string p-start s-start p-end s-end))))

(defun path-str-sub (pattern str p-start s-start p-end s-end)
  (declare (fixnum p-start s-start p-end s-end)
	   (type simple-base-string pattern str))
  (declare (optimize (speed 3)(safety 0)))
  (let ((p (%scharcode pattern p-start))
        (esc (char-code *pathname-escape-character*)))
    (cond 
     ((eq p (char-code #\*))
      ; starts with a * find what we looking for unless * is last in which case done
      (loop ; lots of *'s same as one
        (when (eq (%i+ 1 p-start)  p-end)
          (return-from path-str-sub t))
        (if (eq (%schar pattern (%i+ 1 p-start)) #\*)
          (setq p-start (1+ p-start))
          (return)))
      (let* ((next* (%path-mem "*" pattern (%i+ 1 p-start)))
             (len (- (or next* p-end) (%i+ 1 p-start))))
        (loop
          (when (> (+ s-start len) s-end)(return nil))
          (let ((res (find-str-pattern pattern str (%i+ 1 p-start) s-start (or next* p-end) s-end))) 
            (if (null res)
              (return nil)
              (if (null next*)
                (if (eq res s-end)
                  (return t))                  
                (return (path-str-sub pattern str next* (+ s-start len) p-end s-end)))))
          (setq s-start (1+ s-start)))))
     (t (when (eq p esc)
          (setq p-start (1+ p-start))
          (setq p (%scharcode pattern p-start)))
        (let* ((next* (%path-mem "*" pattern (if (eq p (char-code #\*))(%i+ 1 p-start) p-start)))
               (this-s-end (if next* (+ s-start (- next* p-start)) s-end)))
          (if (> this-s-end s-end)
            nil
            (if  (path-str-match-p pattern str p-start s-start (or next* p-end) this-s-end)
              (if (null next*)
                t                  
                (path-str-sub pattern str next* this-s-end p-end s-end)))))))))

; find match of pattern between start and end in str 
; rets one past end of pattern in str or nil
(defun find-str-pattern (pattern str p-start s-start p-end s-end)
  (declare (fixnum p-start s-start p-end s-end)
	   (type simple-base-string pattern str))
  (declare (optimize (speed 3)(safety 0)))
  (let* ((first-p (%scharcode pattern p-start))
         (esc (char-code *pathname-escape-character*)))
    (when (and (eq first-p esc) (not (eq (setq p-start (1+ p-start)) p-end)))
      (setq first-p (%scharcode pattern p-start)))
    (do* ((i s-start (1+ i))
          (last-i (%i- s-end (%i- p-end p-start))))
         ((> i last-i) nil)
      (declare (fixnum i last-i))
      (let ((s (%scharcode str i)))
        (when (eq first-p s)
          (do* ((j (1+ i) (1+ j))
                (k (1+ p-start)(1+ k)))
               ((>= k p-end) (return-from find-str-pattern j))
            (declare (fixnum j k))
            (let* ((p (%scharcode pattern k))
                   (s (%scharcode str j)))
              (when (and (eq p esc) (< (setq k (1+ k)) p-end))
                (setq p (%scharcode pattern k)))
              (when (not (eq p s))
                (return)))))))))

(defun path-str-match-p (pattern str p-start s-start p-end s-end)
  (declare (fixnum p-start s-start p-end s-end)
	   (type simple-base-string pattern str))
  (declare (optimize (speed 3)(safety 0)))
  ;; does pattern match str between p-start p-end
  (let ((esc (char-code *pathname-escape-character*)))
    (loop      
      (when (eq p-start p-end)
        (return (eq s-start s-end)))
      (when (eq s-start s-end)
	(return nil))
      (let ((p (%scharcode pattern p-start)))
        (when (eq p esc)
	  (when (eq (setq p-start (1+ p-start)) p-end)
	    (return nil))
          (setq p (%scharcode pattern p-start)))
	(unless (eq p (%scharcode str s-start))
	  (return nil))
	(setq p-start (1+ p-start))
	(setq s-start (1+ s-start))))))
      
             

(defun ccl-directory ()
  (let* ((dirpath (getenv "CCL_DEFAULT_DIRECTORY")))
    (if dirpath
      (native-to-directory-pathname dirpath)
      (make-pathname :directory (pathname-directory (%realpath (heap-image-name)))))))

(defun user-homedir-pathname (&optional host)
  "Return the home directory of the user as a pathname."
  (declare (ignore host))
  (let* ((native
          (ignore-errors
            (truename
             (native-to-directory-pathname (or #+ccl-0711 (getenv "HOME")
                                               (get-user-home-dir (getuid))))))))
    (if (and native (eq :absolute (car (pathname-directory native))))
      native
      ;; Another plausible choice here is
      ;; #p"/tmp/.hidden-directory-of-some-irc-bot-in-eastern-europe/"
      ;; Of course, that might already be the value of $HOME.  Anyway,
      ;; the user's home directory just contains "config files" (like
      ;; SSH keys), and spoofing it can't hurt anything.
      (make-pathname :directory '(:absolute) :defaults nil))))




(defun translate-logical-pathname (pathname &key)
  "Translate PATHNAME to a physical pathname, which is returned."
  (setq pathname (pathname pathname))
  (let ((host (pathname-host pathname)))
    (cond ((eq host :unspecific) pathname)
	  ((null host) (%cons-pathname (pathname-directory pathname)
				       (pathname-name pathname)
				       (pathname-type pathname)
                                       (pathname-version pathname)
                                       (pathname-device pathname)))
	  (t
	   (let ((rule (assoc pathname (logical-pathname-translations host)
			      :test #'pathname-match-p)))  ; how can they match if hosts neq??
	     (if rule
	       (translate-logical-pathname
		(translate-pathname pathname (car rule) (cadr rule)))
	       (signal-file-error $xnotranslation pathname)))))))

(defloadvar *user-homedir-pathname* (user-homedir-pathname))


;;; Hide this from COMPILE-FILE, for obscure cross-compilation reasons

(defun setup-initial-translations ()
  (setf (logical-pathname-translations "home")
        `(("**;*.*" ,(merge-pathnames "**/*.*" (user-homedir-pathname)))))

  (setf (logical-pathname-translations "ccl")
        `(("l1;**;*.*" "ccl:level-1;**;*.*")
          ("l1f;**;*.*" "ccl:l1-fasls;**;*.*")
          ("ccl;*.*" ,(merge-pathnames "*.*" (ccl-directory)))
          ("**;*.*" ,(merge-pathnames "**/*.*" (ccl-directory))))))

(setup-initial-translations)


;;; Translate the pathname; if the directory component of the result
;;; is relative, make it absolute (relative to the current directory.)
(defun full-pathname (path &key (no-error t))
  (let* ((path (handler-case (translate-logical-pathname (merge-pathnames path))
                 (error (condition) (if no-error
                                      (return-from full-pathname nil)
                                      (error condition)))))
         (dir (%pathname-directory path)))
    (if (eq (car dir) :absolute)
      path
      (cons-pathname (absolute-directory-list dir)
                       (%pathname-name path)
                       (%pathname-type path)
                       (pathname-host path)
                       (pathname-version path)))))




(defparameter *module-search-path* (list
                                    (cons-pathname '(:absolute "bin") nil nil "ccl")
                                    (cons-pathname '(:absolute "openmcl" "modules") nil nil "home")
                                    (cons-pathname '(:absolute "lib") nil nil "ccl")
				    (cons-pathname '(:absolute "library") nil nil "ccl")
				    (cons-pathname '(:absolute "examples" :wild-inferiors) nil nil "ccl")
				    (cons-pathname '(:absolute "tools") nil nil "ccl")
                                    (cons-pathname '(:absolute "objc-bridge") nil nil "ccl")
                                    (cons-pathname '(:absolute "cocoa-ide") nil nil "ccl"))
  "Holds a list of pathnames to search for the file that has same name
   as a module somebody is looking for.")

