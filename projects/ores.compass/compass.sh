#!/bin/bash

# Wrapper for Compass, the ORE Studio orientation tool. It bootstraps a
# Python virtual environment, then hands every argument to src/compass.py.
# Run './compass.sh --help' for the full, always-current command set
# (index, search, where/status, fleet, list, show, add, goto, ...).

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_PATH="$SCRIPT_DIR/venv"

# --- Optional leading flag: --no-deps ---
# Bootstrap the venv but skip the pip install. The only dependency
# (pystache) is needed solely by the `add`/scaffold command; the stdlib-only
# commands (env, where, status, search, list, show, ...) never import it. With
# --no-deps a fresh checkout bootstraps with just `python3 -m venv` — no
# network — which is what CI uses to run `env init`. Must be the first arg.
NO_DEPS=0
if [ "$1" = "--no-deps" ]; then
    NO_DEPS=1
    shift
fi

# --- Auto-bootstrap Virtual Environment ---
if [ ! -d "$VENV_PATH" ]; then
    echo "🌱 Virtual environment not found. Creating one at $VENV_PATH..."

    if ! python3 -m venv "$VENV_PATH"; then
        echo "❌ Error: Failed to create virtual environment."
        echo "   You may need to install the venv package: sudo apt install python3-venv"
        exit 1
    fi

    # Install requirements if they exist (future-proofing for NLP packages),
    # unless --no-deps requested a stdlib-only bootstrap.
    if [ "$NO_DEPS" -eq 1 ]; then
        echo "ℹ️  --no-deps: skipping dependency install (stdlib-only use)."
    elif [ -f "$SCRIPT_DIR/requirements.txt" ]; then
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

# --- Execute the Python CLI ---
# Pass every argument straight through to compass.py, including --help/-h,
# so the help shown is compass.py's own (complete, always-current) command
# set rather than a hand-maintained copy that drifts out of date.
python3 "$SCRIPT_DIR/src/compass.py" "$@"
