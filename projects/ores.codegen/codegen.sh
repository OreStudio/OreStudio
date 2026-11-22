#!/bin/bash

# Wrapper for codegen.py; bootstraps a Python virtual environment and
# delegates all arguments to codegen.py. Run './codegen.sh --help' for
# the full, always-current command set.

set -e

# generate and regenerate write through compass-ng, which writes only the
# files whose content differs; the Python generator rewrites every file it
# renders. A dry run becomes compass-ng diff. The model, address, component
# and entity arguments no longer narrow the run.
case "${1:-}" in
    generate|gen|regenerate|regen)
        if ! command -v compass-ng >/dev/null 2>&1; then
            echo "Error: compass-ng is not on the PATH; it writes generated code." >&2
            exit 2
        fi
        REPO_ROOT=$(git -C "$(dirname "${BASH_SOURCE[0]}")" rev-parse --show-toplevel)
        verb=generate
        for arg in "$@"; do
            [ "$arg" = "--dry-run" ] && verb=diff
        done
        echo "codegen.sh $1 writes through compass-ng $verb, over the whole tree."
        exec compass-ng -C "$REPO_ROOT" "$verb"
        ;;
esac

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_PATH="$SCRIPT_DIR/venv"

if [ ! -d "$VENV_PATH" ]; then
    echo "Virtual environment not found. Creating one at $VENV_PATH..."
    if ! python3 -m venv "$VENV_PATH"; then
        echo "Error: Failed to create virtual environment."
        echo "You may need: sudo apt install python3-venv"
        exit 1
    fi
fi

REQUIREMENTS_FILE="$SCRIPT_DIR/requirements.txt"
STAMP_PATH="$VENV_PATH/.requirements.stamp"

if [ -f "$REQUIREMENTS_FILE" ] && { [ ! -f "$STAMP_PATH" ] || [ "$REQUIREMENTS_FILE" -nt "$STAMP_PATH" ]; }; then
    echo "Installing dependencies from requirements.txt..."
    "$VENV_PATH/bin/pip" install --upgrade pip -q
    "$VENV_PATH/bin/pip" install -r "$REQUIREMENTS_FILE" -q
    cp "$REQUIREMENTS_FILE" "$STAMP_PATH"
fi

source "$VENV_PATH/bin/activate"

REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel 2>/dev/null || echo "$SCRIPT_DIR")
cd "$REPO_ROOT"

python3 "$SCRIPT_DIR/codegen.py" "$@"
