#!/bin/bash

# Script to manage Compass: NLP/FTS search for Org-Roam notes
#
# This script:
# 1. Bootstraps a Python virtual environment if it doesn't exist
# 2. Indexes/updates notes from org-roam.db into a local SQLite FTS5 database
# 3. Searches notes using fast full-text search

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_PATH="$SCRIPT_DIR/venv"

# --- Auto-bootstrap Virtual Environment ---
if [ ! -d "$VENV_PATH" ]; then
    echo "🌱 Virtual environment not found. Creating one at $VENV_PATH..."

    if ! python3 -m venv "$VENV_PATH"; then
        echo "❌ Error: Failed to create virtual environment."
        echo "   You may need to install the venv package: sudo apt install python3-venv"
        exit 1
    fi

    # Install requirements if they exist (future-proofing for NLP packages)
    if [ -f "$SCRIPT_DIR/requirements.txt" ]; then
        echo "📦 Installing dependencies from requirements.txt..."
        "$VENV_PATH/bin/pip" install --upgrade pip -q
        "$VENV_PATH/bin/pip" install -r "$SCRIPT_DIR/requirements.txt" -q
    else
        echo "ℹ️  No requirements.txt found. Using standard library only."
    fi
    echo "✅ Virtual environment ready."
fi

source "$VENV_PATH/bin/activate"

# Change to the repository root so that relative paths (e.g. --parent-dir
# doc/agile/...) resolve correctly. Use git rev-parse for robustness;
# fall back to SCRIPT_DIR if not inside a git repository.
REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel 2>/dev/null || echo "$SCRIPT_DIR")
cd "$REPO_ROOT"

# --- Parse command line arguments ---
PYTHON_ARGS=()
SHOW_HELP=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --help|-h)
            SHOW_HELP=true
            shift
            ;;
        *)
            PYTHON_ARGS+=("$1")
            shift
            ;;
    esac
done

if [ "$SHOW_HELP" = true ]; then
    echo "Usage: $0 <command> [options]"
    echo ""
    echo "Commands:"
    echo "  index                 Index or update notes from org-roam.db"
    echo "  search <query>        Search your notes using FTS5"
    echo ""
    echo "Options:"
    echo "  -l, --limit <N>       Max results for search (default 10)"
    echo "  --help, -h            Show this help"
    echo ""
    echo "Examples:"
    echo "  $0 index                       # Index/update all changed notes"
    echo "  $0 search \"deployment\"         # Search for 'deployment'"
    exit 0
fi

if [ ${#PYTHON_ARGS[@]} -eq 0 ]; then
    echo "Error: No command provided. Run '$0 --help' for usage information."
    exit 1
fi

# --- Execute the Python script ---
python3 "$SCRIPT_DIR/src/compass.py" "${PYTHON_ARGS[@]}"
