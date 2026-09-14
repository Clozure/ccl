#!/bin/sh
# Layered Darwin/arm64 Cocoa mini-apps — no IDE, no survival hacks.
# Fail-fast. Each app prints <BASENAME-UPPER>-OK and exits 0.
#
# Usage: ./tools/darwin-cocoa-apps/run-darwin-cocoa-apps.sh
# Requires: ./darm64cl + darm64cl.image in the CCL root.

set -e
CCL_DIR=$(cd "$(dirname "$0")/../.." && pwd)
cd "$CCL_DIR"
SMOKE="$CCL_DIR/tools/run-darwin-smoke.sh"
DIR="$CCL_DIR/tools/darwin-cocoa-apps"
TIMEOUT="${CCL_COCOA_APP_TIMEOUT:-120}"

echo ";; darwinarm64 cocoa mini-apps (timeout=${TIMEOUT}s)"

"$SMOKE" "$TIMEOUT" "$DIR/01-objc-support.lisp"
"$SMOKE" "$TIMEOUT" "$DIR/02-throw-bind-integrity.lisp"
"$SMOKE" "$TIMEOUT" "$DIR/03-appkit-shared-application.lisp"
"$SMOKE" "$TIMEOUT" "$DIR/04-window-frame.lisp"
"$SMOKE" "$TIMEOUT" "$DIR/05-objc-subclass-callback.lisp"
"$SMOKE" "$TIMEOUT" "$DIR/06-menu-validate.lisp"
"$SMOKE" "$TIMEOUT" "$DIR/07-event-loop-idle.lisp"
"$SMOKE" "$TIMEOUT" "$DIR/08-menu-tracking.lisp"
"$SMOKE" "$TIMEOUT" "$DIR/09-objc-cnm.lisp" 09-OBJC-CNM-OK
"$SMOKE" "$TIMEOUT" "$DIR/10-ide-shaped-validate.lisp" 10-IDE-SHAPED-VALIDATE-OK
"$SMOKE" "$TIMEOUT" "$DIR/18-callback-error-return.lisp"

echo "DARWIN-COCOA-APPS-OK"
