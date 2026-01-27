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

# Parse arguments
MODEL_PATH=""
OUTPUT_DIR=""
TEMPLATE=""
PROFILE=""
LIST_PROFILES=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --template|-t)
            TEMPLATE="$2"
            shift 2
            ;;
        --profile|-p)
            PROFILE="$2"
            shift 2
            ;;
        --list-profiles)
            LIST_PROFILES=true
            shift
            ;;
        -*)
            echo "Unknown option: $1"
            exit 1
            ;;
        *)
            if [ -z "$MODEL_PATH" ]; then
                MODEL_PATH="$1"
            elif [ -z "$OUTPUT_DIR" ]; then
                OUTPUT_DIR="$1"
            else
                echo "Too many positional arguments"
                exit 1
            fi
            shift
            ;;
    esac
done

# Handle --list-profiles
if [ "$LIST_PROFILES" = true ]; then
    python src/generator.py --list-profiles
    exit 0
fi

# Check if a model path was provided
if [ -z "$MODEL_PATH" ]; then
    echo "Usage: $0 <model_path> [output_dir] [options]"
    echo ""
    echo "Options:"
    echo "  --template, -t <name>   Generate only the specified template"
    echo "  --profile, -p <name>    Generate all templates in a profile (e.g., qt, sql)"
    echo "  --list-profiles         List available profiles and exit"
    echo ""
    echo "Examples:"
    echo "  $0 models/slovaris/catalogs.json"
    echo "  $0 models/slovaris/catalogs.json custom_output/"
    echo "  $0 models/dq/dataset_bundle_domain_entity.json output/ --template cpp_protocol.hpp.mustache"
    echo "  $0 models/dq/dataset_bundle_domain_entity.json output/ --profile qt"
    echo "  $0 --list-profiles"
    echo ""
    echo "Available models:"
    find models -name "*.json" -type f | head -10
    exit 1
fi

# Build the command
CMD="python src/generator.py \"$MODEL_PATH\""
if [ -n "$OUTPUT_DIR" ]; then
    CMD="$CMD \"$OUTPUT_DIR\""
fi
if [ -n "$TEMPLATE" ]; then
    CMD="$CMD --template \"$TEMPLATE\""
fi
if [ -n "$PROFILE" ]; then
    CMD="$CMD --profile \"$PROFILE\""
fi

# Run the generator
eval $CMD

echo ""
echo "Generation completed successfully!"
if [ -z "$OUTPUT_DIR" ]; then
    OUTPUT_DIR="output/"
fi
echo "Output files are in: $SCRIPT_DIR/$OUTPUT_DIR"
