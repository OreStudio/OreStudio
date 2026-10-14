#!/usr/bin/env bash
# Stages only what a service-runtime image needs into build/docker-stage/
# for the Dockerfile to COPY: either every supervised service binary plus
# the closure of their shared-library dependencies (default -- unused now
# that per-service images via --service <binary-name> are the only
# supported deployment mode, kept for ad-hoc local use), or, with
# --service <binary-name>, just that one binary plus its own dependency
# closure (used to build a minimal per-service image). Either way this
# excludes Qt libs, test binaries, and everything else under publish/
# that isn't actually needed at runtime.
#
# The default set is the ores.*.service glob plus an explicit allow-list
# for the two supervised binaries that don't follow that naming convention
# (ores.http.server, ores.compute.wrapper) -- not derived from
# ores_controller_service_definitions_tbl itself, so a future supervised
# binary named outside both patterns can still go silently missing.
set -euo pipefail

service=""
if [[ "${1:-}" == "--service" ]]; then
    service="${2:?--service requires a binary name}"
fi

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# The environment's own preset, as .env sets it and docker/remote-run.sh
# reads it: the staged tree has to come from the build the images are made
# from, so a fixed preset name stages the wrong build or none at all.
publish="$root/build/output/${ORES_PRESET:-linux-clang-debug-make}/publish"
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
    for extra in ores.http.server ores.compute.wrapper; do
        if [[ -f "$publish/bin/$extra" ]]; then
            cp -a "$publish/bin/$extra" "$stage/bin/"
        else
            echo "Warning: $publish/bin/$extra not found -- skipping" >&2
        fi
    done
fi

# A single `ldd` call on a binary resolves its FULL transitive closure (the
# dynamic linker's own view), not just direct links — no recursion needed
# even though our libraries depend on one another. The `|| true` matters:
# `ldd` fails on anything in bin/ that is not an ELF binary, and this script
# runs under `set -euo pipefail`, so a single failing ldd aborts the staging
# in silence and ships an image carrying no libraries at all.
needed_libs=$(for b in "$stage"/bin/*; do
    ldd "$b" 2>/dev/null | awk -v p="$publish/" 'index($3, p) == 1 {print $1}' || true
done | sort -u)

for lib in $needed_libs; do
    # Follow the unversioned SONAME symlink to its real file, copy both.
    real="$(readlink -f "$publish/lib/$lib")"
    cp -a "$publish/lib/$lib" "$stage/lib/"
    cp -a "$real" "$stage/lib/"
done

# The build manifest travels beside the binaries, and is copied only now, so
# that it is not in bin/ while the ldd walk above runs. A service resolves it
# relative to its own executable, so leaving it behind makes every deployed
# container report an unknown commit.
if [[ -f "$publish/bin/version.manifest" ]]; then
    cp -a "$publish/bin/version.manifest" "$stage/bin/"
else
    echo "Warning: $publish/bin/version.manifest not found -- deployed" \
         "containers will report an unknown commit" >&2
fi

# Strip debug symbols on the host BEFORE podman ever sees the files.
# This eliminates the dedicated strip Dockerfile stage: the build context
# itself carries pre-stripped binaries and libraries, so the base-image
# Dockerfile just COPYs them straight into the final image.
strip --strip-debug "$stage"/bin/* "$stage"/lib/libores.*.so* 2>/dev/null || \
    echo "Warning: strip not available or no matching files — continuing"

# Pre-create the runtime directories that used to be mkdir'd inside the
# (now-removed) strip Dockerfile stage.  The base-image Dockerfile COPYs
# these from the host build context instead.
for d in log run storage; do
    mkdir -p "$stage/$d"
    chmod 777 "$stage/$d"
done

# Pre-create the entrypoint symlink in the staging bin/ directory.
# When the Dockerfile COPYs bin/ into /app/bin/, the symlink is preserved.
# For the full staging path (all services), the per-service stage_for_build
# creates individual entrypoints under services/<svc>/bin/ instead.
if [[ -n "$service" ]]; then
    ln -sf "./$service" "$stage/bin/entrypoint"
fi

echo "Staged $(ls "$stage/bin" | wc -l) service binaries and $(ls "$stage/lib" | wc -l) library files."
du -sh "$stage/bin" "$stage/lib"
