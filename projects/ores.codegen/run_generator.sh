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
    echo "Usage: $0 <model_path> [output_dir]"
    echo "Example: $0 models/slovaris/catalogs.json"
    echo "Example with custom output: $0 models/slovaris/catalogs.json custom_output/"
    echo ""
    echo "Available models:"
    find models -name "*.json" -type f | head -10
    exit 1
fi

MODEL_PATH="$1"
OUTPUT_DIR="$2"

# Run the generator with the provided model path and optional output directory
if [ -z "$OUTPUT_DIR" ]; then
    python src/generator.py "$MODEL_PATH"
else
    python src/generator.py "$MODEL_PATH" "$OUTPUT_DIR"
fi

echo ""
echo "Generation completed successfully!"
if [ -z "$OUTPUT_DIR" ]; then
    OUTPUT_DIR="output/"
fi
echo "Output files are in: $SCRIPT_DIR/$OUTPUT_DIR"