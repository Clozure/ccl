# Build, sign, and notarize a standalone, universal Clozure CL.app.
#
# The IDE built by (require :cocoa-application) stays in the CCL
# directory and uses the sources and interfaces there.  This one has
# its own copies of them, so it can be moved anywhere (for example, to
# /Applications).
#
#   make app          build build/Clozure CL.app with each lisp in LISPS
#   make sign         sign it (IDENTITY=- signs ad hoc, for testing)
#   make notarize     notarize and staple it (needs NOTARY_PROFILE)
#   make dmg          make build/ClozureCL.dmg from it
#
# Signing needs ENTITLEMENTS (at least com.apple.security.cs.allow-jit).
# NOTARY_PROFILE names credentials saved with
# "xcrun notarytool store-credentials".

CCL = ..
BUILD = build
APP = $(BUILD)/Clozure CL.app
LISPS = darm64cl dx86cl64

IDENTITY =
ENTITLEMENTS =
NOTARY_PROFILE =

# An ad hoc signature can't have a secure timestamp.
TIMESTAMP = $(if $(filter -,$(IDENTITY)),--timestamp=none,--timestamp)

.PHONY: app sign notarize dmg clean

app:
	rm -rf "$(APP)"
	for lisp in $(LISPS); do \
	  (cd $(CCL) && ./$$lisp -n -b -l cocoa-ide/build-standalone-ide.lisp \
	     < /dev/null) || exit 1; \
	done
	xcrun lipo -archs "$(APP)/Contents/MacOS/Clozure CL"

sign:
	@test -n "$(IDENTITY)" || { echo "Set IDENTITY"; exit 1; }
	@test -n "$(ENTITLEMENTS)" || { echo "Set ENTITLEMENTS"; exit 1; }
	codesign --force --options runtime $(TIMESTAMP) -s "$(IDENTITY)" \
	  "$(APP)/Contents/Resources/AltConsole.app"
	codesign --force --options runtime $(TIMESTAMP) -s "$(IDENTITY)" \
	  --entitlements "$(ENTITLEMENTS)" "$(APP)"
	codesign --verify --deep --strict --verbose=2 "$(APP)"

notarize:
	@test -n "$(NOTARY_PROFILE)" || { echo "Set NOTARY_PROFILE"; exit 1; }
	rm -f "$(BUILD)/ClozureCL.zip"
	ditto -c -k --keepParent "$(APP)" "$(BUILD)/ClozureCL.zip"
	xcrun notarytool submit "$(BUILD)/ClozureCL.zip" \
	  --keychain-profile "$(NOTARY_PROFILE)" --wait
	xcrun stapler staple "$(APP)"
	rm -f "$(BUILD)/ClozureCL.zip"

dmg:
	rm -f "$(BUILD)/ClozureCL.dmg"
	hdiutil create -volname "Clozure CL" -srcfolder "$(APP)" \
	  -format UDZO "$(BUILD)/ClozureCL.dmg"

clean:
	rm -rf "$(BUILD)"
