#!/bin/bash

# Script to generate all Solvaris reference data SQL files
#
# This script generates Solvaris artefacts using the code generator
# from models/slovaris/model.json

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

echo "=============================================="
echo "Solvaris Reference Data Code Generator"
echo "=============================================="
echo ""

echo "Generating Solvaris artefacts from model..."
echo "----------------------------------------------"

# Generate all artefacts by processing the model
# This will automatically generate all dependent files first, then the master include file
python "$SCRIPT_DIR/src/generator.py" \
    "$SCRIPT_DIR/models/slovaris/model.json" \
    "$SCRIPT_DIR/../ores.sql/populate/solvaris"

echo ""
echo "=============================================="
echo "Generation complete!"
echo "=============================================="
echo ""
echo "Generated files (in projects/ores.sql/populate/solvaris/):"
echo "  - solvaris.sql (master include)"
echo "  - solvaris_catalog_populate.sql"
echo "  - solvaris_methodology_populate.sql"
echo "  - solvaris_dataset_populate.sql"
echo "  - solvaris_dataset_dependency_populate.sql"
echo "  - solvaris_tag_populate.sql"
echo "  - solvaris_flag_populate.sql"
echo "  - solvaris_currency_populate.sql"
echo "  - solvaris_country_populate.sql"
