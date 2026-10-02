#!/bin/sh
# After parse-standard-ffi-files, promote new-*.cdb → *.cdb in an interface dir.
set -e
DIR=${1:-.}
cd "$DIR"
for f in new-*.cdb; do
  [ -f "$f" ] || continue
  base=${f#new-}
  mv -f "$f" "$base"
  echo "installed $base"
done
