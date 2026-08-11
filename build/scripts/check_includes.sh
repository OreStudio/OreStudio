#!/usr/bin/env bash
# -*- mode: sh; tab-width: 4; indent-tabs-offset: nil; sh-basic-offset: 4 -*-
#
# check_includes.sh - run clang-tidy's misc-include-cleaner over the tree.
#
# Parses every translation unit recorded in the given preset's
# compile_commands.json and reports include-hygiene problems: a symbol
# used without a directly-included header declaring it (e.g.
# std::to_string without the string header), or an include nothing
# uses. Without --fix it dry-runs; with --fix it applies the suggested
# changes in place, which is what the nightly workflow uses before it
# opens its bot PR.
#
# Usage (from the checkout root):
#   cmake --preset <preset>
#   ./build/scripts/check_includes.sh [preset] [--fix]
#
# Preset defaults to linux-clang-debug-ninja, the preset whose
# compile_commands.json the nightly workflow builds.
#

set -euo pipefail

preset="${1:-linux-clang-debug-ninja}"
mode="${2:-}"
build="build/output/${preset}"
cc="${build}/compile_commands.json"

if [[ ! -f "${cc}" ]]; then
    echo "error: ${cc} not found; configure the preset first:" >&2
    echo "  cmake --preset ${preset}" >&2
    exit 1
fi

fix_args=()
if [[ "${mode}" == "--fix" ]]; then
    fix_args+=(--fix)
elif [[ -n "${mode}" ]]; then
    echo "usage: $0 [preset] [--fix]" >&2
    exit 1
fi

echo "Checking include hygiene against ${cc}..."

# The file list comes from compile_commands.json, not from find: every
# file clang-tidy is handed must have a matching translation unit, so
# sources the preset does not build are skipped instead of erroring.
# A failure to read the file list is a real error and aborts the
# script; a stray translation unit that fails to parse is tolerated
# (clang-tidy prints the reason to stderr) so one broken file cannot
# block the nightly bot PR.
files="$(python3 - "${cc}" <<'PYEOF'
import json
import pathlib
import sys

cc = json.loads(pathlib.Path(sys.argv[1]).read_text())
print("\n".join(sorted({entry["file"] for entry in cc})))
PYEOF
)"

# Parallelism is capped at 4: clang-tidy serialises fix application on
# shared files, and the ubuntu-24.04 runner has four cores.
echo "${files}" | xargs -P4 clang-tidy -p "${build}" \
    --checks='-*,misc-include-cleaner' --header-filter='projects/' \
    "${fix_args[@]}" || true
