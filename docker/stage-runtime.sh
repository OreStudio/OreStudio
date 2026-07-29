#!/usr/bin/env bash
# Stages only what a service-runtime image needs into build/docker-stage/
# for the Dockerfile to COPY: either every ores.*.service binary plus the
# closure of their shared-library dependencies (default — used by the
# all-in-one controller image), or, with --service <binary-name>, just
# that one binary plus its own dependency closure (used to build a
# minimal per-service image). Either way this excludes Qt libs, test
# binaries, and everything else under publish/ that isn't actually needed
# at runtime.
set -euo pipefail

service=""
if [[ "${1:-}" == "--service" ]]; then
    service="$2"
fi

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
publish="$root/build/output/linux-clang-debug-make/publish"
stage="$root/build/docker-stage"

if [[ ! -d "$publish/bin" ]]; then
    echo "Error: $publish/bin not found — build first" >&2
    exit 1
fi

if [[ -n "$service" && ! -f "$publish/bin/$service" ]]; then
    echo "Error: $publish/bin/$service not found" >&2
    exit 1
fi

rm -rf "$stage"
mkdir -p "$stage/bin" "$stage/lib"

if [[ -n "$service" ]]; then
    cp -a "$publish/bin/$service" "$stage/bin/"
else
    cp -a "$publish"/bin/ores.*.service "$stage/bin/"
fi

# A single `ldd` call on a binary resolves its FULL transitive closure (the
# dynamic linker's own view), not just direct links — no recursion needed
# even though our libraries depend on one another.
needed_libs=$(for b in "$stage"/bin/*; do
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
