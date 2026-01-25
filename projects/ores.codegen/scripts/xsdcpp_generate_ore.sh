#!/bin/bash
#
# Generate C++ domain types from ORE XSD schema
#
# This script generates C++ code for ORE XML parsing using xsdcpp.
# All paths are relative to the git repository root.
#
# Usage: ./xsdcpp_generate_ore.sh [--dry-run]
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

# Parse arguments
DRY_RUN=false
for arg in "$@"; do
    case $arg in
        --dry-run)
            DRY_RUN=true
            ;;
        --help|-h)
            echo "Usage: $0 [--dry-run]"
            echo ""
            echo "Generate C++ domain types from ORE XSD schema."
            echo ""
            echo "Options:"
            echo "  --dry-run    Print command without executing"
            echo "  --help       Show this help message"
            exit 0
            ;;
        *)
            echo "Error: Unknown option: $arg" >&2
            exit 1
            ;;
    esac
done

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
echo "Git root:       ${GIT_ROOT}"
echo "XSD input:      ${XSD_PATH}"
echo "Header output:  projects/${PROJECT}/include/${PROJECT}/${NAME}"
echo "CPP output:     projects/${PROJECT}/src/${NAME}"
echo "Namespace:      ${NAMESPACE}"
echo ""

# Build command
CMD=(
    xsdcpp
    "$XSD_FULL_PATH"
    "--header-output=$HEADER_OUTPUT"
    "--cpp-output=$CPP_OUTPUT"
    "--wrap-namespace=$NAMESPACE"
    "--name=$NAME"
)

if [ "$DRY_RUN" = true ]; then
    echo "Dry run - would execute:"
    echo "${CMD[*]}"
else
    # Check xsdcpp is available
    if ! command -v xsdcpp &> /dev/null; then
        echo "Error: xsdcpp not found on PATH" >&2
        echo "Please ensure xsdcpp is installed and available in your PATH" >&2
        exit 1
    fi

    echo "Running xsdcpp..."
    "${CMD[@]}"
    echo ""
    echo "=== Done ==="
fi
