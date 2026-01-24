#!/bin/bash
#
# Validate SQL schema conventions
#
# Runs the ER diagram parser in validation-only mode with strict checking.
# Exits with code 1 if any validation warnings are found.
#
# Usage: ./validate_schema.sh
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CODEGEN_DIR="${SCRIPT_DIR}/../ores.codegen"

# Check for Python
if ! command -v python3 &> /dev/null; then
    echo "Error: python3 is required but not installed"
    exit 1
fi

# Set up virtual environment if needed
VENV_PATH="$CODEGEN_DIR/venv"
if [ ! -d "$VENV_PATH" ]; then
    echo "Setting up virtual environment..."
    python3 -m venv "$VENV_PATH"
    source "$VENV_PATH/bin/activate"
    pip install --quiet pystache
else
    source "$VENV_PATH/bin/activate"
fi

echo "=== SQL Schema Validation ==="
echo ""

# Run validation in strict mode
python3 "${CODEGEN_DIR}/src/plantuml_er_parse_sql.py" \
    --create-dir "${SCRIPT_DIR}/create" \
    --drop-dir "${SCRIPT_DIR}/drop" \
    --ignore-file "${SCRIPT_DIR}/validation_ignore.txt" \
    --validate-only \
    --strict

echo ""
echo "=== Validation Passed ==="
