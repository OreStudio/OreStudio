#!/bin/bash

# Script to run the code generator with proper environment setup

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

# Check if a model path was provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <model_path>"
    echo "Example: $0 models/slovaris/catalogs.json"
    echo ""
    echo "Available models:"
    find models -name "*.json" -type f | head -10
    exit 1
fi

MODEL_PATH="$1"

# Run the generator with the provided model path
python src/generator.py "$MODEL_PATH"

echo ""
echo "Generation completed successfully!"
echo "Output files are in: $SCRIPT_DIR/output/"