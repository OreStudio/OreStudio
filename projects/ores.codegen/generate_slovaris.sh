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

echo "Generating all Slovaris artefacts from batch execution model..."

# Generate all artefacts by processing the batch execution model
# This will automatically generate all dependent files first, then the batch execution file
python src/generator.py models/slovaris/model.json "$SCRIPT_DIR/../ores.sql/populate"

echo ""
echo "All Slovaris artefacts generated successfully!"
echo "Output files are in: projects/ores.sql/populate/"
