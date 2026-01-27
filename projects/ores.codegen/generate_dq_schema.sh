#!/bin/bash

# Script to generate DQ (Data Quality) schema files
#
# This script generates SQL schema files for domain entities and junction tables
# in the dq component directly into projects/ores.sql/create/dq/

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
SCHEMA_DIR="$SCRIPT_DIR/../ores.sql/create/dq"

# Ensure output directory exists
mkdir -p "$SCHEMA_DIR"

echo "=============================================="
echo "DQ Schema Code Generator"
echo "=============================================="
echo ""

# Step 1: Generate domain entity schema files
echo "Step 1: Generating domain entity schema files..."
echo "----------------------------------------------"
for model_file in "$SCRIPT_DIR/models/dq/"*_domain_entity.json; do
    if [ -f "$model_file" ]; then
        entity_name=$(basename "$model_file" _domain_entity.json)
        echo "  Processing: $entity_name"
        python "$SCRIPT_DIR/src/generator.py" "$model_file" "$SCHEMA_DIR/"
    fi
done
echo ""

# Step 2: Generate junction table schema files
echo "Step 2: Generating junction table schema files..."
echo "----------------------------------------------"
for model_file in "$SCRIPT_DIR/models/dq/"*_junction.json; do
    if [ -f "$model_file" ]; then
        junction_name=$(basename "$model_file" _junction.json)
        echo "  Processing: $junction_name"
        python "$SCRIPT_DIR/src/generator.py" "$model_file" "$SCHEMA_DIR/"
    fi
done
echo ""

echo "=============================================="
echo "Generation complete!"
echo "=============================================="
echo ""
echo "Schema files: $SCHEMA_DIR/"
echo ""
