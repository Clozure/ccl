#!/bin/sh
# Darwin/arm64 OpenGL interface populate (C: OpenGL + GLU + GLUT).
#
# AGL is gone from modern SDKs; h-to-ffi.sh skips missing headers.
#
#   cd $CCL/darwin-arm64-headers/gl/C
#   $CCL/tools/darwin-arm64-cdb/gl-populate.sh
#   cd $CCL
#   CCL_INTERFACE=gl ./darm64cl --stack-size 16M --thread-stack-size 16M \
#     --no-init --batch < tools/darwin-arm64-cdb/parse-interface-dir.lisp
#
# BACK UP gl/*.cdb first.

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

for h in \
  "${SDK}/System/Library/Frameworks/OpenGL.framework/Headers/OpenGL.h" \
  "${SDK}/System/Library/Frameworks/OpenGL.framework/Headers/glu.h" \
  "${SDK}/System/Library/Frameworks/GLUT.framework/Headers/glut.h" \
  "${SDK}/System/Library/Frameworks/AGL.framework/Headers/agl.h"
do
  h-to-ffi.sh "$h"
done

echo ";; gl-populate done under $(pwd)"
echo ";; function count:" "$(grep -h '^(function ' $(find . -name '*.ffi') 2>/dev/null | wc -l)"
