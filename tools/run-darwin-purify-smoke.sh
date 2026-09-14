#!/bin/sh
# Experimental Darwin/arm64 :purify t save + reload smoke.
#
#   ./tools/run-darwin-purify-smoke.sh
#
# Leaves production darm64cl.image alone.  Exit 0 only if child prints
# DARWIN-PURIFY-SMOKE-OK.  Hard wall-clock timeouts so unattended runs
# never hang (exit 124 on timeout).

set -e
CCL_DIR=$(cd "$(dirname "$0")/.." && pwd)
cd "$CCL_DIR"
IMG=/tmp/darm64cl-purify-test.image
LOG=/tmp/darwin-purify-smoke.log
TIMEOUT="${CCL_SMOKE_TIMEOUT:-60}"
WT="$CCL_DIR/tools/with-timeout"
# The child image lives in /tmp; without this, "ccl:" resolves to /tmp and
# interface databases (#_getpid) are not found.
CCL_DEFAULT_DIRECTORY="$CCL_DIR"
export CCL_DEFAULT_DIRECTORY

rm -f "$IMG"
"$WT" "$TIMEOUT" ./darm64cl --no-init --batch \
  < tools/darwin-purify-smoke.lisp > "$LOG" 2>&1 || {
  ec=$?
  echo "parent save failed (exit $ec):" >&2
  tail -40 "$LOG" >&2
  exit "$ec"
}

"$WT" "$TIMEOUT" ./darm64cl --image-name "$IMG" --no-init --batch \
  < tools/darwin-purify-smoke-child.lisp >> "$LOG" 2>&1 || {
  ec=$?
  echo "purified child failed (exit $ec):" >&2
  tail -50 "$LOG" >&2
  exit "$ec"
}

grep -q 'DARWIN-PURIFY-SMOKE-OK' "$LOG"
echo "DARWIN-PURIFY-SMOKE-OK"
