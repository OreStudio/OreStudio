#!/bin/bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
#
# Generate LEI subset files (small and large)
#
# This script extracts diverse subsets from the GLEIF golden copy data.
# Input files are expected in external/lei/ directory.
# Output subset files are also written to external/lei/ directory.
#
# Usage:
#   ./generate_lei_subsets.sh           # Generate both subsets
#   ./generate_lei_subsets.sh --small   # Generate small subset only
#   ./generate_lei_subsets.sh --large   # Generate large subset only
#   ./generate_lei_subsets.sh --download # Download data first, then generate

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CODEGEN_DIR="$(dirname "$SCRIPT_DIR")"
REPO_ROOT="$(dirname "$(dirname "$CODEGEN_DIR")")"
LEI_SCRIPT="$CODEGEN_DIR/src/lei_extract_subset.py"

# Parse arguments
GENERATE_SMALL=false
GENERATE_LARGE=false
DOWNLOAD=false

if [ $# -eq 0 ]; then
    GENERATE_SMALL=true
    GENERATE_LARGE=true
else
    for arg in "$@"; do
        case $arg in
            --small)
                GENERATE_SMALL=true
                ;;
            --large)
                GENERATE_LARGE=true
                ;;
            --download)
                DOWNLOAD=true
                ;;
            --help|-h)
                echo "Usage: $0 [--small] [--large] [--download]"
                echo ""
                echo "Options:"
                echo "  --small     Generate small subset only"
                echo "  --large     Generate large subset only"
                echo "  --download  Download latest GLEIF data before generating"
                echo ""
                echo "With no options, generates both small and large subsets."
                exit 0
                ;;
            *)
                echo "Unknown option: $arg"
                echo "Use --help for usage information."
                exit 1
                ;;
        esac
    done
fi

# If neither size specified, generate both
if [ "$GENERATE_SMALL" = false ] && [ "$GENERATE_LARGE" = false ]; then
    GENERATE_SMALL=true
    GENERATE_LARGE=true
fi

echo "========================================"
echo "LEI Subset Generator"
echo "========================================"
echo ""
echo "Repository root: $REPO_ROOT"
echo "LEI script: $LEI_SCRIPT"
echo ""

# Check Python script exists
if [ ! -f "$LEI_SCRIPT" ]; then
    echo "Error: LEI extraction script not found at $LEI_SCRIPT"
    exit 1
fi

# Download if requested
if [ "$DOWNLOAD" = true ]; then
    echo "Downloading latest GLEIF data..."
    python3 "$LEI_SCRIPT" --download-only
    echo ""
fi

# Generate small subset
if [ "$GENERATE_SMALL" = true ]; then
    echo "========================================"
    echo "Generating SMALL subset..."
    echo "========================================"
    python3 "$LEI_SCRIPT" --size small
    echo ""
fi

# Generate large subset
if [ "$GENERATE_LARGE" = true ]; then
    echo "========================================"
    echo "Generating LARGE subset..."
    echo "========================================"
    python3 "$LEI_SCRIPT" --size large
    echo ""
fi

echo "========================================"
echo "All requested subsets generated."
echo "========================================"
