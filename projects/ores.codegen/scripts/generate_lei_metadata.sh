#!/bin/bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
#
# Generate LEI metadata SQL populate files
#
# This script generates SQL populate scripts from GLEIF LEI CSV subset files.
# Input: external/lei/ (manifest.json + CSV subset files)
# Output: projects/ores.sql/populate/lei/ (SQL populate files)
#
# Usage:
#   ./generate_lei_metadata.sh
#   ./generate_lei_metadata.sh --manifest-dir /path/to/external/lei
#   ./generate_lei_metadata.sh --output-dir /path/to/output

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CODEGEN_DIR="$(dirname "$SCRIPT_DIR")"
LEI_SCRIPT="$CODEGEN_DIR/src/lei_generate_metadata_sql.py"

echo "========================================"
echo "LEI Metadata SQL Generator"
echo "========================================"
echo ""
echo "Codegen dir: $CODEGEN_DIR"
echo "Generator:   $LEI_SCRIPT"
echo ""

# Check Python script exists
if [ ! -f "$LEI_SCRIPT" ]; then
    echo "Error: LEI metadata generator not found at $LEI_SCRIPT"
    exit 1
fi

# Pass all arguments through to the Python script
python3 "$LEI_SCRIPT" "$@"

echo ""
echo "========================================"
echo "LEI metadata SQL generation complete."
echo "========================================"
