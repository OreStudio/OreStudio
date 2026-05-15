#!/bin/bash

# Script to generate workflow schema files
#
# Generates SQL schema files for workflow domain entities directly into
# projects/ores.sql/create/workflow/

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_PATH="$SCRIPT_DIR/venv"

if [ ! -d "$VENV_PATH" ]; then
    echo "Error: Virtual environment not found at $VENV_PATH"
    echo "Please run: python3 -m venv venv && source venv/bin/activate && pip install -r requirements.txt"
    exit 1
fi

source "$VENV_PATH/bin/activate"
cd "$SCRIPT_DIR"

SCHEMA_DIR="$SCRIPT_DIR/../ores.sql/create/workflow"
mkdir -p "$SCHEMA_DIR"

echo "=============================================="
echo "Workflow Schema Code Generator"
echo "=============================================="
echo ""

echo "Generating workflow domain entity schema files..."
for model_file in "$SCRIPT_DIR/models/workflow/"*_domain_entity.json; do
    if [ -f "$model_file" ]; then
        entity_name=$(basename "$model_file" _domain_entity.json)
        echo "  Processing: $entity_name"
        python "$SCRIPT_DIR/src/generator.py" "$model_file" "$SCHEMA_DIR/" \
            --template sql_schema_non_temporal_create.mustache
    fi
done

echo ""
echo "=============================================="
echo "Generation complete!"
echo "=============================================="
echo ""
echo "Schema files: $SCHEMA_DIR/"
echo ""
