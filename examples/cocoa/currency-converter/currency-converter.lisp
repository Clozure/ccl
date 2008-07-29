(in-package "CCL")

;;; define the classes referenced in the nibfile

(defclass converter (ns:ns-object)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/convertCurrency:atRate: :float) 
    ((self converter) (currency :float) (rate :float))
  (* currency rate))

(defclass converter-controller (ns:ns-object)
  ((amount-field :foreign-type :id :accessor amount-field)
   (converter :foreign-type :id :accessor converter)
   (dollar-field :foreign-type :id :accessor dollar-field)
   (rate-field :foreign-type :id :accessor rate-field))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/convert: :void) ((self converter-controller) sender)
  (declare (ignore sender))
  (let* ((conv (converter self))
         (dollar-field (dollar-field self))
         (rate-field (rate-field self))
         (amount-field (amount-field self))
         (dollars (#/floatValue dollar-field))
         (rate (#/floatValue rate-field))
         (amount (#/convertCurrency:atRate: conv dollars rate)))
    (#/setFloatValue: amount-field amount)
    (#/selectText: rate-field self)))




#|
"/Users/mikel/Valise/clozure/openmcl/example-code/currency-converter/CurrencyConverter.nib"

building the app:

(progn
  (setf (current-directory) "/Users/mikel/Valise/clozure/openmcl/example-code/currency-converter/")
  (load "currency-converter")
  (require "build-application")
  (ccl::build-application :name "CurrencyConverter"
                          :main-nib-name "CurrencyConverter"
			  :directory "/Users/mikel/Desktop/"
                          :nibfiles '(#P"/usr/local/openmcl/trunk/ccl/examples/cocoa/currency-converter/CurrencyConverter.xib")))

TODO NOTES:

The name of the app in the main menu title is determined by the CFBundleName field in the
InfoPlist.strings file in the English.lproj resources folder.

|#