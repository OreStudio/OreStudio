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
SQL_DIR="${SCRIPT_DIR}/.."
CODEGEN_DIR="${SCRIPT_DIR}/../../ores.codegen"

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

# Check that DQ populate functions use ores_iam_current_actor_fn() for
# modified_by rather than hardcoding current_user. The latter captures the
# database role (ores_ddl_user) instead of the authenticated session user.
echo "--- Checking: DQ populate functions use session actor for modified_by ---"
HARDCODED_ACTOR_FILES=$(grep -rl \
    "current_user, current_user, 'system\.external_data_import'" \
    "${SQL_DIR}/create/dq/" 2>/dev/null || true)
if [ -n "$HARDCODED_ACTOR_FILES" ]; then
    echo "ERROR: The following DQ SQL files use hardcoded current_user for"
    echo "modified_by in external data imports. Use"
    echo "  coalesce(ores_iam_current_actor_fn(), current_user)"
    echo "instead so the authenticated session user is recorded:"
    echo "$HARDCODED_ACTOR_FILES"
    exit 1
fi
echo "OK"
echo ""

# Run validation in strict mode
python3 "${CODEGEN_DIR}/src/plantuml_er_parse_sql.py" \
    --create-dir "${SQL_DIR}/create" \
    --drop-dir "${SQL_DIR}/drop" \
    --ignore-file "${SCRIPT_DIR}/validation_ignore.txt" \
    --validate-only \
    --strict

echo ""
echo "=== Validation Passed ==="
