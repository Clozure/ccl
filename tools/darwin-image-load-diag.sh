#!/bin/sh
# Sanitized diagnostic for "Couldn't load lisp heap image" on Darwin/arm64
# (Clozure/ccl#599).  Prints hardware / OS / VM-policy facts and the image
# loader's own failure lines.  No usernames, hostnames, serials, or
# absolute paths appear in the output — safe to paste into a GitHub thread.
#
#   ./tools/darwin-image-load-diag.sh [image]     (default darm64cl.image)
set -e
CCL_DIR=$(cd "$(dirname "$0")/.." && pwd)
cd "$CCL_DIR"
IMAGE=${1:-darm64cl.image}
TMP=$(mktemp -d /tmp/ccl-diag.XXXXXX)
trap 'rm -rf "$TMP"' EXIT

# Strip anything that could identify the machine or user.
sanitize() {
  sed -e "s|$CCL_DIR|<ccl>|g" -e "s|$HOME|<home>|g" -e "s|$(whoami)|<user>|g"
}

echo "== system =="
echo "macos: $(sw_vers -productVersion) ($(sw_vers -buildVersion))"
echo "kernel: $(uname -r) $(uname -m)"
echo "chip: $(sysctl -n machdep.cpu.brand_string)"
echo "pagesize: $(sysctl -n hw.pagesize)"
csrutil status 2>/dev/null | head -1 || true

echo ""
echo "== kernel binary hardening =="
# Only the hardened-process entitlement keys matter; everything else
# (identifiers, team IDs) stays out of the report.
ENTS=$(codesign -d --entitlements - ./darm64cl 2>/dev/null \
         | grep -o 'com\.apple\.security\.hardened-process[a-z.-]*' || true)
if [ -n "$ENTS" ]; then
  echo "$ENTS"
else
  echo "no hardened-process entitlements"
fi

echo ""
echo "== fixed-VA mmap probe =="
if cc -o "$TMP/probe" tools/darwin-fixed-mmap-probe.c 2>"$TMP/cc.err"; then
  "$TMP/probe"
else
  echo "probe build failed:"
  sanitize < "$TMP/cc.err"
fi

echo ""
echo "== image load: $IMAGE =="
if [ ! -x ./darm64cl ]; then
  echo "no ./darm64cl kernel here; build lisp-kernel/darwinarm64 first"
  exit 0
fi
if [ ! -f "$IMAGE" ]; then
  echo "image not found"
  exit 0
fi
# 20s guard: a healthy image prints its banner immediately.
(./tools/with-timeout 20 ./darm64cl -I "$IMAGE" --no-init --batch \
   < /dev/null > "$TMP/load.out" 2>&1) || true
if grep -q "Clozure Common Lisp" "$TMP/load.out"; then
  echo "image loads OK:"
  grep "Clozure Common Lisp" "$TMP/load.out" | head -1
else
  echo "image load FAILED; loader diagnostics:"
  grep -E "CommitMemory|MapFile|load_image_section|Couldn't load|Heap image|another platform" \
    "$TMP/load.out" | sanitize
fi
echo ""
echo "== end of report =="
