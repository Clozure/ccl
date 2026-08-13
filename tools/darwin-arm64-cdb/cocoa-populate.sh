#!/bin/sh
# Darwin/arm64 Cocoa interface populate (ObjC runtime + Foundation + AppKit).
#
# Uses -x objective-c (not -x c) so ffigen5 emits objc-class / methods.
# FILTER_FFI_MACROS=frameworks keeps only Frameworks/ macros (fast parse).
#
#   cd $CCL/darwin-arm64-headers/cocoa/C
#   $CCL/tools/darwin-arm64-cdb/cocoa-populate.sh
#   cd $CCL
#   ./darm64cl --stack-size 16M --thread-stack-size 16M --no-init --batch \
#     < tools/darwin-arm64-cdb/parse-cocoa.lisp
#
# BACK UP cocoa/*.cdb first.

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

# ObjC runtime (C headers still useful under objective-c mode)
for h in \
  "${SDK}/usr/include/objc/objc.h" \
  "${SDK}/usr/include/objc/runtime.h" \
  "${SDK}/usr/include/objc/objc-runtime.h" \
  "${SDK}/usr/include/objc/objc-exception.h" \
  "${SDK}/usr/include/objc/NSObject.h" \
  "${SDK}/System/Library/Frameworks/Foundation.framework/Headers/Foundation.h" \
  "${SDK}/System/Library/Frameworks/AppKit.framework/Headers/AppKit.h"
do
  h-to-ffi.sh "$h"
done

# Modern objc.h: `#define YES __objc_yes` / `#define NO __objc_no` — not
# numeric, so macros never become constants. FILTER_FFI_MACROS=frameworks
# also drops usr/include/objc. Historical Clozure CDBs had YES=1 NO=0.
# Ship synthetic enum-idents so parse-ffi records them (objc-bridge).
cp "${HERE}/objc-bool-constants.ffi" ./objc-bool-constants.ffi

# Modern message.h forces OBJC_OLD_DISPATCH_PROTOTYPES=0 →
# `void objc_msgSend(void)`.  Clozure needs the historical
# `id objc_msgSend(id, SEL, ...)` shape (trailing void = kwargs).
# zzz- name so parse-standard-ffi-files overwrites the empty prototypes.
cp "${HERE}/objc-msgsend-prototypes.ffi" ./zzz-objc-msgsend-prototypes.ffi

# instancetype, va_list, NSConstantString layout for objc-bridge.
cp "${HERE}/objc-bridge-types.ffi" ./zzz-objc-bridge-types.ffi

# Protocol class (missing from modern SDK cocoa CDB).
cp "${HERE}/objc-protocol-class.ffi" ./zzz-objc-protocol-class.ffi

echo ";; cocoa-populate done under $(pwd)"
echo ";; objc-class count:" "$(grep -h '^(objc-class ' $(find . -name '*.ffi') 2>/dev/null | wc -l)"
echo ";; objc-instance-method count:" "$(grep -h '^(objc-instance-method ' $(find . -name '*.ffi') 2>/dev/null | wc -l)"
echo ";; installed objc-bool-constants.ffi (YES/NO)"
echo ";; installed zzz-objc-msgsend-prototypes.ffi"
echo ";; installed zzz-objc-bridge-types.ffi"
echo ";; installed zzz-objc-protocol-class.ffi"
