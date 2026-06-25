#!/bin/sh
# sccache_wrapper.sh — compiler launcher wrapper for sccache.
#
# Sets SCCACHE_BASEDIR to the project root before invoking sccache, so that
# absolute paths in compiler invocations are normalised to relative paths
# before the cache key is computed.  This makes all git worktrees of the same
# commit share a single sccache entry per translation unit, eliminating
# redundant full rebuilds across environments.
#
# Used as CMAKE_C_COMPILER_LAUNCHER / CMAKE_CXX_COMPILER_LAUNCHER in
# CMakeLists.txt.  The project root is resolved at runtime from this script's
# own location (build/scripts/ is two levels below the root), so the path is
# always correct regardless of which worktree or machine runs the build.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
export SCCACHE_BASEDIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
exec sccache "$@"
