;;;-*-Mode: LISP; Package: CCL -*-
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

(in-package "CCL")

(defparameter *openmcl-major-version* 1)
(defparameter *openmcl-minor-version* 12)
(defparameter *openmcl-revision* nil)
;;; May be set by xload-level-0
(defvar *openmcl-svn-revision* nil)
(defparameter *openmcl-dev-level* nil)

(defparameter *openmcl-version* (format nil "~d.~d~@[-~a~] ~@[(~a)~] ~~A"
					*openmcl-major-version*
					*openmcl-minor-version*
					(unless (null *openmcl-revision*)
					  *openmcl-revision*)
					(if (and (typep *openmcl-svn-revision* 'string)
                                                 (> (length *openmcl-svn-revision*) 0))
                                          *openmcl-svn-revision*)))

;;; end
