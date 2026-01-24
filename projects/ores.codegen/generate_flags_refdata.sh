#!/bin/bash

# Script to generate all flag reference data SQL files
#
# This script:
# 1. Generates methodology and dataset SQL from manifest.json
# 2. Generates flag images SQL from SVG files

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_PATH="$SCRIPT_DIR/venv"

# Check virtual environment
if [ ! -d "$VENV_PATH" ]; then
    echo "Error: Virtual environment not found at $VENV_PATH"
    echo "Please run: python3 -m venv venv && source venv/bin/activate && pip install -r requirements.txt"
    exit 1
fi

source "$VENV_PATH/bin/activate"

# Get repo root (two levels up from codegen)
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$REPO_ROOT"

# Parse command line arguments
SKIP_IMAGES=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --skip-images)
            SKIP_IMAGES=true
            shift
            ;;
        --help)
            echo "Usage: $0 [options]"
            echo ""
            echo "Options:"
            echo "  --skip-images    Skip regenerating images SQL (large file)"
            echo "  --help           Show this help"
            echo ""
            echo "Examples:"
            echo "  $0                     # Generate all files"
            echo "  $0 --skip-images       # Skip images, just regenerate metadata"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "=============================================="
echo "Flag Icons Reference Data Code Generator"
echo "=============================================="
echo ""

# Step 1: Generate methodology and dataset SQL
echo "Step 1: Generating methodology and dataset SQL..."
echo "----------------------------------------------"
python "$SCRIPT_DIR/src/flags_generate_metadata_sql.py"
echo ""

# Step 2: Generate images SQL (optional)
if [ "$SKIP_IMAGES" = false ]; then
    echo "Step 2: Generating images SQL..."
    echo "----------------------------------------------"
    python "$SCRIPT_DIR/src/images_generate_sql.py" --config flags
    echo ""
else
    echo "Step 2: Skipping images SQL (--skip-images)"
    echo ""
fi

echo "=============================================="
echo "Generation complete!"
echo "=============================================="
echo ""
echo "Generated files:"
echo "  - flags_methodology_populate.sql"
echo "  - flags_dataset_populate.sql"
echo "  - flags_dataset_tag_populate.sql"
echo "  - flags.sql (master include)"
if [ "$SKIP_IMAGES" = false ]; then
    echo "  - flags_images_artefact_populate.sql"
fi
