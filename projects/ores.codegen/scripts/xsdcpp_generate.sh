#!/bin/bash
#
# Generate C++ code from XSD schema using xsdcpp
#
# This script runs the xsdcpp tool to generate C++ domain types from XSD files.
# All paths are relative to the git repository root.
#
# Usage: ./xsdcpp_generate.sh [options]
#
# Options:
#   --xsd PATH        Path to XSD file (relative to repo root)
#   --project NAME    Target project name (e.g., ores.ore)
#   --namespace NS    C++ namespace to wrap generated code (e.g., ores::ore)
#   --name NAME       Name for generated files (e.g., domain)
#   --dry-run         Print command without executing
#   --help            Show this help message
#
# Example:
#   ./xsdcpp_generate.sh --xsd external/ore/xsd/input.xsd \
#                        --project ores.ore \
#                        --namespace ores::ore \
#                        --name domain
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

# Show usage
show_help() {
    sed -n '2,/^$/p' "$0" | sed 's/^# //; s/^#//'
    exit 0
}

# Default values
XSD_PATH=""
PROJECT=""
NAMESPACE=""
NAME=""
DRY_RUN=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --xsd)
            XSD_PATH="$2"
            shift 2
            ;;
        --project)
            PROJECT="$2"
            shift 2
            ;;
        --namespace)
            NAMESPACE="$2"
            shift 2
            ;;
        --name)
            NAME="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --help|-h)
            show_help
            ;;
        *)
            echo "Error: Unknown option: $1" >&2
            echo "Use --help for usage information" >&2
            exit 1
            ;;
    esac
done

# Validate required arguments
if [ -z "$XSD_PATH" ]; then
    echo "Error: --xsd is required" >&2
    exit 1
fi

if [ -z "$PROJECT" ]; then
    echo "Error: --project is required" >&2
    exit 1
fi

if [ -z "$NAMESPACE" ]; then
    echo "Error: --namespace is required" >&2
    exit 1
fi

if [ -z "$NAME" ]; then
    echo "Error: --name is required" >&2
    exit 1
fi

# Get git root
GIT_ROOT=$(find_git_root)
echo "Git root: $GIT_ROOT"

# Build paths relative to git root
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

echo "=== xsdcpp Code Generation ==="
echo ""
echo "XSD input:      ${XSD_PATH}"
echo "Project:        ${PROJECT}"
echo "Namespace:      ${NAMESPACE}"
echo "Name:           ${NAME}"
echo ""
echo "Header output:  projects/${PROJECT}/include/${PROJECT}/${NAME}"
echo "CPP output:     projects/${PROJECT}/src/${NAME}"
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
    # Check xsdcpp is available (only when actually running)
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
