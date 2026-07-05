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
#
# sccache is resolved via PATH.  cmake's find_program and the build
# environment share the same PATH, so this is always consistent.
# Set SCCACHE_BASEDIR in the environment before the build to override.
#
# SCCACHE_DIR/SCCACHE_CACHE_SIZE are not exported by the shell that runs
# make/cmake --build, so they are read here from .env — the same file
# `compass env configure` writes them to — rather than duplicating the
# default elsewhere. An already-exported SCCACHE_DIR/SCCACHE_CACHE_SIZE
# (set explicitly by the caller) takes precedence over the .env value.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
export SCCACHE_BASEDIR="${SCCACHE_BASEDIR:-${PROJECT_ROOT}}"

if [ -f "${PROJECT_ROOT}/.env" ]; then
    _env_sccache_dir="$(sed -n 's/^SCCACHE_DIR=//p' "${PROJECT_ROOT}/.env" | tail -n1)"
    _env_sccache_size="$(sed -n 's/^SCCACHE_CACHE_SIZE=//p' "${PROJECT_ROOT}/.env" | tail -n1)"
    [ -n "${_env_sccache_dir}" ] && export SCCACHE_DIR="${SCCACHE_DIR:-${_env_sccache_dir}}"
    [ -n "${_env_sccache_size}" ] && export SCCACHE_CACHE_SIZE="${SCCACHE_CACHE_SIZE:-${_env_sccache_size}}"
fi

exec sccache "$@"
