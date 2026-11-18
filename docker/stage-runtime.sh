#!/usr/bin/env bash
# Stages only what a service-runtime image needs into build/docker-stage/
# for the Dockerfile to COPY: either the binaries named by the caller plus
# the closure of their shared-library dependencies, or, with
# --service <binary-name>, just that one binary plus its own dependency
# closure (used to build a minimal per-service image). Either way this
# excludes Qt libs, test binaries, and everything else under publish/
# that isn't actually needed at runtime.
#
# --services takes the list the service registry reports as runtime
# services, so a binary whose name does not match the ores.*.service
# pattern is still staged. Without either flag the script falls back to
# the ores.*.service glob plus a hard-coded allow-list, kept for ad-hoc
# local use only: that list goes stale as soon as a supervised binary is
# named differently.
set -euo pipefail

service=""
services=""
if [[ "${1:-}" == "--service" ]]; then
    service="${2:?--service requires a binary name}"
elif [[ "${1:-}" == "--services" ]]; then
    services="${2:?--services requires a space-separated list of binary names}"
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
elif [[ -n "$services" ]]; then
    for name in $services; do
        if [[ -f "$publish/bin/$name" ]]; then
            cp -a "$publish/bin/$name" "$stage/bin/"
        else
            echo "Error: $publish/bin/$name not found -- the service" \
                 "registry lists it but this build did not produce it" >&2
            exit 1
        fi
    done
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
# A library the binary needs and the build did not produce is left out of the
# image by the filter below and only shows up when the container starts, so the
# staging stops here instead.
missing_libs=$(for b in "$stage"/bin/*; do
    ldd "$b" 2>/dev/null | awk '$2 == "=>" && $3 == "not" && $4 == "found" {print $1}'
done | sort -u)
if [[ -n "$missing_libs" ]]; then
    echo "Error: a staged binary needs a library the build did not produce:" >&2
    printf '  %s\n' $missing_libs >&2
    exit 1
fi

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

# Strip on the host BEFORE podman ever sees the files, so the build
# context itself carries stripped files and the Dockerfiles only COPY
# them. --strip-all drops .symtab and .strtab, which are about a third of
# a large library; the dynamic symbol table and the relocations stay, so
# the libraries still load. The build manifest sits in bin/ too and is not
# an object file, so each candidate is checked before it is stripped. A
# strip that fails aborts the staging: shipping unstripped files silently
# would hide a missing binutils.
for f in "$stage"/bin/* "$stage"/lib/libores.*.so*; do
    [[ -L "$f" ]] && continue
    if readelf -h "$f" >/dev/null 2>&1; then
        strip --strip-all "$f"
    fi
done

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
