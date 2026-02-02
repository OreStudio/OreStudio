#!/bin/bash

# Script to generate Refdata schema files
#
# This script generates SQL schema files for reference data entities
# in the refdata component directly into projects/ores.sql/create/refdata/

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
cd "$SCRIPT_DIR"

# Directories
SCHEMA_DIR="$SCRIPT_DIR/../ores.sql/create/refdata"

# Ensure output directory exists
mkdir -p "$SCHEMA_DIR"

echo "=============================================="
echo "Refdata Schema Code Generator"
echo "=============================================="
echo ""

# Step 1: Generate entity schema files
echo "Step 1: Generating entity schema files..."
echo "----------------------------------------------"
for model_file in "$SCRIPT_DIR/models/refdata/"*_entity.json; do
    if [ -f "$model_file" ]; then
        entity_name=$(basename "$model_file" _entity.json)
        echo "  Processing: $entity_name"
        python "$SCRIPT_DIR/src/generator.py" "$model_file" "$SCHEMA_DIR/" --template sql_schema_table_create.mustache
    fi
done
echo ""

echo "=============================================="
echo "Generation complete!"
echo "=============================================="
echo ""
echo "Schema files: $SCHEMA_DIR/"
echo ""
