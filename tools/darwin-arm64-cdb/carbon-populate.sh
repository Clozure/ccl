#!/bin/sh
# Darwin/arm64 Carbon interface populate (C umbrella Carbon.h).
#
# Carbon is deprecated but still present in MacOSX.sdk. Uses
# -Wno-deprecated-declarations like the historical x86 populate.
#
#   cd $CCL/darwin-arm64-headers/carbon/C
#   $CCL/tools/darwin-arm64-cdb/carbon-populate.sh
#   cd $CCL
#   CCL_INTERFACE=carbon ./darm64cl --stack-size 16M --thread-stack-size 16M \
#     --no-init --batch < tools/darwin-arm64-cdb/parse-interface-dir.lisp
#
# BACK UP carbon/*.cdb first.

set -e
HERE=$(cd "$(dirname "$0")" && pwd)
PATH="${HERE}:$PATH"
export PATH

if [ -z "${SDK}" ]; then
  SDK=$(xcrun --show-sdk-path)
fi
if [ $# -eq 1 ]; then
  SDK=$1
fi
if [ ! -d "$SDK" ]; then
  echo "SDK not found: $SDK" >&2
  exit 1
fi

rm -rf Applications Library System usr

export FFIGEN_LANG=c
unset FILTER_FFI_MACROS

CFLAGS="-arch arm64 -isysroot ${SDK} -F${SDK}/System/Library/Frameworks -Wno-deprecated-declarations"
CLANG_BIN=$(xcrun --find clang)
CLANG_ROOT=$(dirname "$(dirname "$CLANG_BIN")")
CLANG_INC="$CLANG_ROOT/lib/clang"
if [ -d "$CLANG_INC" ]; then
  VER=$(ls "$CLANG_INC" | tail -1)
  if [ -n "$VER" ] && [ -d "$CLANG_INC/$VER/include" ]; then
    CFLAGS="$CFLAGS -isystem $CLANG_INC/$VER/include"
  fi
fi
export CFLAGS SDK

h-to-ffi.sh "${SDK}/System/Library/Frameworks/Carbon.framework/Headers/Carbon.h"

echo ";; carbon-populate done under $(pwd)"
echo ";; function count:" "$(grep -h '^(function ' $(find . -name '*.ffi') 2>/dev/null | wc -l)"
