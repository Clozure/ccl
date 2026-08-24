#!/bin/sh
# Run a Darwin/arm64 lisp smoke file with a hard timeout.
# Usage: run-darwin-smoke.sh [TIMEOUT] tools/foo-smoke.lisp [EXPECTED_MARKER]
# Default TIMEOUT=60.  Default marker = basename uppercased with -OK.
# Exit 124 on timeout; otherwise lisp exit status / missing marker → 1.

set -e
CCL_DIR=$(cd "$(dirname "$0")/.." && pwd)
cd "$CCL_DIR"
WT="$CCL_DIR/tools/with-timeout"

TIMEOUT=60
if [ $# -ge 1 ] && expr "$1" : '[0-9][0-9]*$' >/dev/null 2>&1; then
  TIMEOUT=$1
  shift
fi
if [ $# -lt 1 ]; then
  echo "usage: $0 [TIMEOUT] smoke.lisp [MARKER]" >&2
  exit 2
fi
SMOKE=$1
shift
MARK=${1:-}
if [ -z "$MARK" ]; then
  base=$(basename "$SMOKE" .lisp)
  # darwin-math-smoke → DARWIN-MATH-SMOKE-OK (keep hyphens)
  MARK=$(printf '%s' "$base" | tr '[:lower:]' '[:upper:]')-OK
fi
LOG=/tmp/$(basename "$SMOKE" .lisp).log

"$WT" "$TIMEOUT" ./darm64cl --no-init --batch < "$SMOKE" > "$LOG" 2>&1 || {
  ec=$?
  echo "SMOKE FAIL: $SMOKE exit=$ec (timeout=124). Tail:" >&2
  tail -40 "$LOG" >&2
  exit "$ec"
}
if ! grep -q "$MARK" "$LOG"; then
  echo "SMOKE FAIL: $SMOKE missing marker $MARK. Tail:" >&2
  tail -40 "$LOG" >&2
  exit 1
fi
echo "$MARK"
