#!/bin/sh
# Darwin/arm64 Quartz interface populate (ObjC umbrella Quartz.h).
#
# Quartz pulls QuartzCore, PDFKit, ImageKit, QuickLookUI, etc. — must use
# -x objective-c. FILTER_FFI_MACROS=frameworks keeps parse tractable.
#
#   cd $CCL/darwin-arm64-headers/quartz/C
#   $CCL/tools/darwin-arm64-cdb/quartz-populate.sh
#   cd $CCL
#   CCL_INTERFACE=quartz ./darm64cl --stack-size 16M --thread-stack-size 16M \
#     --no-init --batch < tools/darwin-arm64-cdb/parse-interface-dir.lisp
#
# BACK UP quartz/*.cdb first.

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

export FFIGEN_LANG=objective-c
export FILTER_FFI_MACROS=frameworks

CFLAGS="-arch arm64 -isysroot ${SDK} -F${SDK}/System/Library/Frameworks"
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

h-to-ffi.sh "${SDK}/System/Library/Frameworks/Quartz.framework/Headers/Quartz.h"

echo ";; quartz-populate done under $(pwd)"
echo ";; objc-class count:" "$(grep -h '^(objc-class ' $(find . -name '*.ffi') 2>/dev/null | wc -l)"
echo ";; objc-instance-method count:" "$(grep -h '^(objc-instance-method ' $(find . -name '*.ffi') 2>/dev/null | wc -l)"
