#!/bin/bash

# Wrapper for codegen.py; bootstraps a Python virtual environment and
# delegates all arguments to codegen.py. Run './codegen.sh --help' for
# the full, always-current command set.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_PATH="$SCRIPT_DIR/venv"

if [ ! -d "$VENV_PATH" ]; then
    echo "Virtual environment not found. Creating one at $VENV_PATH..."
    if ! python3 -m venv "$VENV_PATH"; then
        echo "Error: Failed to create virtual environment."
        echo "You may need: sudo apt install python3-venv"
        exit 1
    fi
    if [ -f "$SCRIPT_DIR/requirements.txt" ]; then
        echo "Installing dependencies from requirements.txt..."
        "$VENV_PATH/bin/pip" install --upgrade pip -q
        "$VENV_PATH/bin/pip" install -r "$SCRIPT_DIR/requirements.txt" -q
    fi
    echo "Virtual environment ready."
fi

source "$VENV_PATH/bin/activate"

REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel 2>/dev/null || echo "$SCRIPT_DIR")
cd "$REPO_ROOT"

python3 "$SCRIPT_DIR/codegen.py" "$@"
