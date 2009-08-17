(require 'trivial-ldap)

; see http://www.openldap.org/doc/admin23/quickstart.html for some
; openldap quickstart documentation.  

;; create a new ldap object.  the port number is the default 389 here.
(defparameter l
  (ldap:new-ldap :host "redbaron.local" 
		 :user "cn=directory manager, dc=example, dc=com"
		 :sslflag t
		 :debug t
		 :pass "secret"
		 :base "dc=example,dc=com"
		 :reuse-connection 'ldap:rebind))


;; create some entry objects.  
(defparameter entry-one
  (ldap:new-entry "dc=example,dc=com" 
		  :attrs '((objectclass . (dcobject organization))
			   (o . "example organization"))))

(defparameter entry-two
  (ldap:new-entry "cn=manager,dc=example,dc=com" 
		  :attrs '((objectclass . organizationalrole))))

(defparameter entry-three
  (ldap:new-entry "cn=test user,dc=example,dc=com" 
		  :attrs '((objectclass . organizationalrole))))

(defparameter entry-four
  (ldap:new-entry "cn=quuxor,dc=example,dc=com" 
		  :attrs '((objectclass . (organizationalrole))
			   (description . "another test entry")
			   (l . ("Boston" "Cambridge" "Jamaica Plain"))
			   (st . "Massachusetts")
			   (postalcode . "02115")
			   (street . "Commonwealth Avenue"))))


; a printed representation:
(format t "~A" (ldap:ldif entry-four))

; turn on debugging.
(setf (ldap:debugflag l) t)

; bind to the server.
(when (ldap:bind l)
  (write-line "bound to ldap."))

; turn off debugging.
(setf (ldap:debugflag l) nil)

; add a couple entries.
(ldap:add entry-one l)

; or use the lower-level add specified on ldap first:
(multiple-value-bind (res code msg) (ldap:add l entry-two)
  (format t "res: ~A~%code: ~A~%msg: ~A" res code msg))

; search (and print results in ldif) 
(ldap:ldif-search l "(cn=*)")

; add another entry.
(ldap:add entry-three l)

; search for that.
(if (ldap:search l (ldap:rdn entry-three))
    (describe (ldap:next-search-result l))
    (format t "Search Failed."))

; delete an entry.  
(ldap:delete entry-three l)

; ldap:search will return nil.
(ldap:search l (ldap:rdn entry-three))

; a fourth entry.
(ldap:add entry-four l)

; this should be true.
(ldap:compare entry-four l 'st "Massachusetts")

; as should this, because the st attribute 
; compares case insensitively.
(ldap:compare entry-four l 'st 'massachusetts)

; this is false, so it returns nil.
(ldap:compare entry-four l 'st 'foobarbaz)

; compare (and delete) take strings as well as entry objects.
(ldap:compare (ldap:dn entry-four) l 'l 'boston)
(ldap:delete (ldap:dn entry-four) l)

; put entry four back:
(ldap:add entry-four l)

(ldap:attr-value entry-four 'st)
(ldap:attr-list entry-four)
(ldap:attrs entry-four)

(ldap:modify entry-four l '((ldap:delete l "Boston")
			    (ldap:replace st "Vermont")
			    (ldap:add st "New Hampshire")
			    (ldap:add street ("Massachusetts Avenue"
					      "Newbury Street"
					      "Boylston Street"))))

(format t "~A~%" (ldap:ldif entry-four))


(ldap:moddn entry-four l "cn=buzzer")

; simple ldap filters work more or less as expected.  extended filters
; however have not been implemented yet.
(ldap:search l "(cn=buzz*)")
(ldap:search l "(| (cn=baz*) (cn=*ager))")

; the outside parens are optional:
(ldap:search l "| (cn=baz*) (cn=*ager)")

; clean up.  with one ldap object fetching the results of the search,
; a second LDAP object is required for the delete.

(defparameter j (ldap:new-ldap :host "great-pumpkin.local"
			       :base "dc=example,dc=com"))

; (ldap:dosearch (ent (ldap:search j "cn=*"))
;   (ldap:delete ent l))
;  
; (ldap:delete entry-one l)
