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

(in-package "GUI")

;;; action menu item tags
(defconstant $inspect-item-tag 0)
(defconstant $source-item-tag 1)

;;; Modern SDKs define the pre-10.12 AppKit names used by the IDE
;;; (NSOffState, NSCommandKeyMask, NSOKButton, ...) as `static const'
;;; aliases rather than macros or enum members, so interface
;;; translation records them as foreign variables and #$ fails to
;;; resolve them at load time.  Their values are fixed ABI; use these
;;; instead.

;;; NSControlStateValue (NSCell.h)
(defconstant $control-state-value-mixed -1)
(defconstant $control-state-value-off 0)
(defconstant $control-state-value-on 1)

;;; NSEventModifierFlags (NSEvent.h)
(defconstant $event-modifier-flag-caps-lock (ash 1 16))
(defconstant $event-modifier-flag-shift (ash 1 17))
(defconstant $event-modifier-flag-control (ash 1 18))
(defconstant $event-modifier-flag-option (ash 1 19))
(defconstant $event-modifier-flag-command (ash 1 20))
(defconstant $event-modifier-flag-function (ash 1 23))

;;; NSModalResponse (NSPanel.h)
(defconstant $modal-response-ok 1)
(defconstant $modal-response-cancel 0)

;;; NSRunAlertPanel return values (NSPanel.h, deprecated API)
(defconstant $alert-default-return 1)
(defconstant $alert-alternate-return 0)
(defconstant $alert-other-return -1)
(defconstant $alert-error-return -2)

;;; NSProgressIndicatorStyle (NSProgressIndicator.h)
(defconstant $progress-indicator-style-bar 0)
(defconstant $progress-indicator-style-spinning 1)

;;; NSNotFound (NSObjCRuntime.h) = NSIntegerMax
(defconstant $ns-not-found (1- (expt 2 63)))

;;; NSEventType (NSEvent.h)
(defconstant $event-type-left-mouse-down 1)
(defconstant $event-type-left-mouse-up 2)

;;; NSWindowStyleMask (NSWindow.h)
(defconstant $window-style-mask-borderless 0)
(defconstant $window-style-mask-titled 1)
(defconstant $window-style-mask-closable 2)
(defconstant $window-style-mask-miniaturizable 4)
(defconstant $window-style-mask-resizable 8)
(defconstant $window-style-mask-textured-background 256)

;;; NSBezelStyle (NSButtonCell.h)
(defconstant $bezel-style-rounded 1)
(defconstant $bezel-style-regular-square 2)
(defconstant $bezel-style-disclosure 5)
(defconstant $bezel-style-shadowless-square 6)
(defconstant $bezel-style-circular 7)
(defconstant $bezel-style-textured-square 8)
(defconstant $bezel-style-help-button 9)
(defconstant $bezel-style-small-square 10)
(defconstant $bezel-style-textured-rounded 11)
(defconstant $bezel-style-round-rect 12)
(defconstant $bezel-style-recessed 13)
(defconstant $bezel-style-rounded-disclosure 14)

;;; NSControlSize (NSCell.h)
(defconstant $control-size-regular 0)
(defconstant $control-size-small 1)

;;; NSButtonType (NSButtonCell.h)
(defconstant $button-type-switch 3)
(defconstant $button-type-radio 4)

;;; NSColorPanelMode (NSColorPanel.h)
(defconstant $color-panel-mode-wheel 6)
