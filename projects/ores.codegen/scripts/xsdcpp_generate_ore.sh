#!/bin/bash
#
# Generate C++ domain types from ORE XSD schema
#
# This script generates C++ code for ORE XML parsing using xsdcpp.
# All paths are relative to the git repository root.
#
# Usage: ./xsdcpp_generate_ore.sh
#

set -e

# Find git repository root
find_git_root() {
    local dir="$PWD"
    while [ "$dir" != "/" ]; do
        # Check for .git directory or file (worktree)
        if [ -d "$dir/.git" ] || [ -f "$dir/.git" ]; then
            echo "$dir"
            return 0
        fi
        dir="$(dirname "$dir")"
    done
    echo "Error: Not inside a git repository" >&2
    return 1
}

# Check xsdcpp is available
if ! command -v xsdcpp &> /dev/null; then
    echo "Error: xsdcpp not found on PATH" >&2
    echo "Please ensure xsdcpp is installed and available in your PATH" >&2
    exit 1
fi

# Get git root
GIT_ROOT=$(find_git_root)

# Hard-coded paths for ORE XML generation
XSD_PATH="external/ore/xsd/input.xsd"
PROJECT="ores.ore"
NAMESPACE="ores::ore"
NAME="domain"

# Build full paths
XSD_FULL_PATH="${GIT_ROOT}/${XSD_PATH}"
HEADER_OUTPUT="${GIT_ROOT}/projects/${PROJECT}/include/${PROJECT}/${NAME}"
CPP_OUTPUT="${GIT_ROOT}/projects/${PROJECT}/src/${NAME}"

# Validate XSD file exists
if [ ! -f "$XSD_FULL_PATH" ]; then
    echo "Error: XSD file not found: $XSD_FULL_PATH" >&2
    exit 1
fi

# Create output directories if they don't exist
mkdir -p "$HEADER_OUTPUT"
mkdir -p "$CPP_OUTPUT"

echo "=== ORE XSD Code Generation ==="
echo ""
echo "XSD input:      ${XSD_PATH}"
echo "Header output:  projects/${PROJECT}/include/${PROJECT}/${NAME}"
echo "CPP output:     projects/${PROJECT}/src/${NAME}"
echo "Namespace:      ${NAMESPACE}"
echo ""

echo "Running xsdcpp..."
xsdcpp "$XSD_FULL_PATH" \
    "--header-output=$HEADER_OUTPUT" \
    "--cpp-output=$CPP_OUTPUT" \
    "--wrap-namespace=$NAMESPACE" \
    "--name=$NAME"

echo ""
echo "=== Done ==="
