#!/bin/bash

# Script to generate all Slovaris artefacts using the code generator

# Exit on any error
set -e

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Activate the virtual environment
VENV_PATH="$SCRIPT_DIR/venv"
if [ ! -d "$VENV_PATH" ]; then
    echo "Error: Virtual environment not found at $VENV_PATH"
    echo "Please run: python3 -m venv venv && source venv/bin/activate && pip install -r requirements.txt"
    exit 1
fi

# Source the virtual environment
source "$VENV_PATH/bin/activate"

# Change to the project directory
cd "$SCRIPT_DIR"

echo "Generating all Slovaris artefacts..."

# Generate catalogs
echo "Generating catalogs..."
python src/generator.py models/slovaris/catalogs.json

# Generate datasets
echo "Generating datasets..."
python src/generator.py models/slovaris/datasets.json

# Generate methodologies
echo "Generating methodologies..."
python src/generator.py models/slovaris/methodologies.json

echo ""
echo "All Slovaris artefacts generated successfully!"
echo "Output files are in: $SCRIPT_DIR/output/"
echo ""
echo "Generated files:"
ls -la output/