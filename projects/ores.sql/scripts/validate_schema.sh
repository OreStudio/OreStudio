#!/bin/bash
#
# Validate SQL schema against conventions defined in SKILL.org
#
# This is a thin wrapper around the codegen parser that runs validation only.
# Usage:
#   ./validate_schema.sh           # Print warnings
#   ./validate_schema.sh --strict  # Exit with error if any warnings
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SQL_DIR="${SCRIPT_DIR}/.."
CODEGEN_DIR="${SCRIPT_DIR}/../../ores.codegen"

# Activate the virtual environment
VENV_PATH="$CODEGEN_DIR/venv"
if [ ! -d "$VENV_PATH" ]; then
    echo "Error: Virtual environment not found at $VENV_PATH"
    echo "Please set up the codegen venv first."
    exit 1
fi

source "$VENV_PATH/bin/activate"

# Run parser in validate-only mode
python "${CODEGEN_DIR}/src/plantuml_er_parse_sql.py" \
    --create-dir "${SQL_DIR}/create" \
    --drop-dir "${SQL_DIR}/drop" \
    --validate-only \
    --warn \
    "$@"
