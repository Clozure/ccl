;;;
;;; Copyright 2016 Clozure Associates
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

(in-package :gui)

(defclass ide-application (ccl::ccl-application)
  ((console :foreign-type :id :accessor console))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/stringToPasteBoard:  :void) ((self ide-application) string)
  (let* ((pb (#/generalPasteboard ns:ns-pasteboard)))
    (#/declareTypes:owner: pb (#/arrayWithObject: ns:ns-array						  #&NSStringPboardType) nil)
    (#/setString:forType: pb string #&NSStringPboardType)))
    
