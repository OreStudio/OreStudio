#!/usr/bin/env bash
# Stages only what the service runtime needs — the ores.*.service binaries
# and the closure of their shared-library dependencies (excludes Qt libs,
# test binaries, and everything else under publish/) — into
# build/docker-stage/{bin,lib} for the Dockerfile to COPY.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
publish="$root/build/output/linux-clang-debug-make/publish"
stage="$root/build/docker-stage"

if [[ ! -d "$publish/bin" ]]; then
    echo "Error: $publish/bin not found — build first" >&2
    exit 1
fi

rm -rf "$stage"
mkdir -p "$stage/bin" "$stage/lib"

cp -a "$publish"/bin/ores.*.service "$stage/bin/"

needed_libs=$(for b in "$stage"/bin/ores.*.service; do
    ldd "$b" 2>/dev/null | awk -v p="$publish/" 'index($3, p) == 1 {print $1}'
done | sort -u)

for lib in $needed_libs; do
    # Follow the unversioned SONAME symlink to its real file, copy both.
    real="$(readlink -f "$publish/lib/$lib")"
    cp -a "$publish/lib/$lib" "$stage/lib/"
    cp -a "$real" "$stage/lib/"
done

echo "Staged $(ls "$stage/bin" | wc -l) service binaries and $(ls "$stage/lib" | wc -l) library files."
du -sh "$stage/bin" "$stage/lib"
