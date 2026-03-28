#!/usr/bin/env bash
# build.sh — Build ORE Studio using the make preset.
#
# Usage:
#   build/scripts/build.sh [TARGET...]
#
# If no TARGETs are given, builds everything.
# The preset can be overridden with ORES_BUILD_PRESET env var.
#
# Examples:
#   build/scripts/build.sh
#   build/scripts/build.sh ores.iam.core ores.service
#   ORES_BUILD_PRESET=linux-clang-release-make build/scripts/build.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

PRESET="${ORES_BUILD_PRESET:-linux-clang-debug-make}"
BUILD_DIR="$PROJECT_DIR/build/output/${PRESET}"
TMP_DIR="$PROJECT_DIR/tmp"
export TMPDIR="$TMP_DIR"

if [[ ! -d "$BUILD_DIR" ]]; then
    echo "Build directory '$BUILD_DIR' does not exist."
    echo "Run: cmake --preset $PRESET"
    exit 1
fi

JOBS="${CMAKE_BUILD_PARALLEL_LEVEL:-2}"
LOG_FILE="$PROJECT_DIR/build/tmp/build.log"

make -C "$BUILD_DIR" -j"$JOBS" "$@" 2>&1 | tee "$LOG_FILE"
EXIT_CODE="${PIPESTATUS[0]}"

if [[ "$EXIT_CODE" -ne 0 ]]; then
    echo ""
    echo "=== Build failed (last 40 lines) ==="
    tail -40 "$LOG_FILE"
fi

exit "$EXIT_CODE"
