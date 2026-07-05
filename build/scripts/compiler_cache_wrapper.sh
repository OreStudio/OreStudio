#!/bin/sh
# compiler_cache_wrapper.sh — compiler launcher wrapper for sccache/ccache.
#
# Wired in unconditionally via CMakePresets.json (so it wins the race against
# vcpkg's own compiler-detection bootstrap, which otherwise invokes a bare
# `sccache`/`ccache` before this project's CMakeLists.txt gets a chance to
# guard on find_program()). Because it's unconditional, this script itself
# must gate activation explicitly — it must NOT activate a compiler cache
# just because one happens to be in PATH for unrelated reasons on the
# machine. Activation requires an explicit opt-in:
#
#   - CI: always sccache (CI sets SCCACHE_DIR directly; never runs
#     `compass env configure`, so ORES_COMPILER_CACHE is never set there).
#   - Local dev: only if ORES_COMPILER_CACHE is set in .env, written by
#     `compass env configure --compiler-cache {sccache,ccache}`.
#   - Anything else (no .env, or .env without ORES_COMPILER_CACHE): fall
#     through to the real compiler untouched.
#
# For sccache: sets SCCACHE_BASEDIR to the project root before invoking it,
# so that absolute paths in compiler invocations are normalised to relative
# paths before the cache key is computed. This makes all git worktrees of the
# same commit share a single sccache entry per translation unit, eliminating
# redundant full rebuilds across environments. SCCACHE_DIR/SCCACHE_CACHE_SIZE
# are not set here — sccache picks them up from its own global config
# (~/.config/sccache/config).
#
# For ccache: cache location and tuning (direct_mode, hard_link, max_size)
# come from ccache's own global config (~/.config/ccache/ccache.conf).
#
# Used as CMAKE_C_COMPILER_LAUNCHER / CMAKE_CXX_COMPILER_LAUNCHER in
# CMakeLists.txt.  The project root is resolved at runtime from this script's
# own location (build/scripts/ is two levels below the root), so the path is
# always correct regardless of which worktree or machine runs the build.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

if [ -n "${CI:-}" ]; then
    TOOL="sccache"
elif [ -f "${PROJECT_ROOT}/.env" ]; then
    TOOL="$(sed -n 's/^ORES_COMPILER_CACHE=//p' "${PROJECT_ROOT}/.env" | tail -n1)"
fi

if [ -z "${TOOL:-}" ] || ! command -v "${TOOL}" >/dev/null 2>&1; then
    exec "$@"
fi

if [ "${TOOL}" = "sccache" ]; then
    export SCCACHE_BASEDIR="${SCCACHE_BASEDIR:-${PROJECT_ROOT}}"
elif [ "${TOOL}" = "ccache" ]; then
    export CCACHE_BASEDIR="${CCACHE_BASEDIR:-${PROJECT_ROOT}}"
fi

exec "${TOOL}" "$@"
