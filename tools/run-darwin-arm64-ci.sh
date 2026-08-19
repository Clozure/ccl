#!/bin/sh
# Unified Darwin/arm64 smoke gate.  Requires ./darm64cl + darm64cl.image.
# Exit nonzero on first failure.  Timeouts via tools/with-timeout.

set -e
CCL_DIR=$(cd "$(dirname "$0")/.." && pwd)
cd "$CCL_DIR"
SMOKE="$CCL_DIR/tools/run-darwin-smoke.sh"
TIMEOUT="${CCL_SMOKE_TIMEOUT:-90}"

echo ";; darwinarm64 CI smokes (timeout=${TIMEOUT}s)"
"$SMOKE" "$TIMEOUT" tools/darwin-math-smoke.lisp
./tools/run-darwin-purify-smoke.sh
"$SMOKE" "$TIMEOUT" tools/darwin-cocoa-smoke.lisp
"$SMOKE" "$TIMEOUT" tools/darwin-nsoffstate-smoke.lisp
CCL_FORCE_STATIC_RELOC=1 "$SMOKE" "$TIMEOUT" tools/darwin-static-reloc-smoke.lisp
"$SMOKE" "$TIMEOUT" tools/darwin-interp-ff-call-smoke.lisp
"$SMOKE" "$TIMEOUT" tools/darwin-clean-build-smoke.lisp
"$SMOKE" "${CCL_COCOA_REQUIRE_TIMEOUT:-300}" tools/darwin-require-cocoa-smoke.lisp
# Layered AppKit mini-apps (no IDE).  Fail-fast before IDE bring-up.
./tools/darwin-cocoa-apps/run-darwin-cocoa-apps.sh

echo "DARWIN-ARM64-CI-OK"
