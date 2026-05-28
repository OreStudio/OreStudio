#!/bin/bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# Wrapper script for parse_test_results.py
# Parses test result XML files and extracts information about failed tests.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS] [PRESET]

Parse test result XML files and show failed tests with relevant logs.

Arguments:
    PRESET                  CMake preset name (default: linux-clang-debug)
                            Common presets: linux-clang-debug, linux-clang-release,
                            linux-gcc-debug, linux-gcc-release

Options:
    -d, --directory DIR     Specify bin directory directly instead of using preset
    -h, --help              Show this help message

Examples:
    $(basename "$0")                              # Uses linux-clang-debug
    $(basename "$0") linux-clang-release          # Uses linux-clang-release
    $(basename "$0") -d /path/to/bin              # Uses specific directory

The script looks for test-results*.xml files in:
    build/output/<preset>/publish/bin/

EOF
    exit 0
}

# Default preset
PRESET="linux-clang-debug"
BIN_DIR=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -d|--directory)
            BIN_DIR="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        -*)
            echo "Error: Unknown option $1" >&2
            usage
            ;;
        *)
            PRESET="$1"
            shift
            ;;
    esac
done

# If no explicit directory, construct from preset
if [[ -z "${BIN_DIR}" ]]; then
    BIN_DIR="${PROJECT_ROOT}/build/output/${PRESET}/publish/bin"
fi

# Check if directory exists
if [[ ! -d "${BIN_DIR}" ]]; then
    echo "Error: Directory does not exist: ${BIN_DIR}" >&2
    echo "" >&2
    echo "Have you built and run tests? Try:" >&2
    echo "  cmake --build --preset ${PRESET} --target rat" >&2
    exit 1
fi

# Check if there are any test result files
if ! ls "${BIN_DIR}"/test-results*.xml >/dev/null 2>&1; then
    echo "No test-results*.xml files found in: ${BIN_DIR}" >&2
    echo "" >&2
    echo "Have you run tests? Try:" >&2
    echo "  cmake --build --preset ${PRESET} --target rat" >&2
    exit 1
fi

# Run the Python script
exec python3 "${SCRIPT_DIR}/parse_test_results.py" "${BIN_DIR}"
