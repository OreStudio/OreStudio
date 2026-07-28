#!/bin/bash

# Wrapper for Compass, the ORE Studio orientation tool. It bootstraps a
# Python virtual environment, then hands every argument to src/compass.py.
# Run './compass.sh --help' for the full, always-current command set
# (index, search, where/status, fleet, list, show, add, goto, ...).

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_PATH="$SCRIPT_DIR/venv"

# On Windows (Git Bash / MSYS2) the venv layout uses Scripts/ not bin/.
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" ]]; then
    VENV_BIN="$VENV_PATH/Scripts"
else
    VENV_BIN="$VENV_PATH/bin"
fi

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
fi

# Install/refresh requirements if they exist (future-proofing for NLP
# packages), unless --no-deps requested a stdlib-only bootstrap. Re-syncs
# whenever requirements.txt changes, not just on first venv creation, so a
# stale venv from before a new dependency was added self-heals.
REQUIREMENTS_FILE="$SCRIPT_DIR/requirements.txt"
STAMP_PATH="$VENV_PATH/.requirements.stamp"
if [ "$NO_DEPS" -eq 1 ]; then
    :
elif [ -f "$REQUIREMENTS_FILE" ] && { [ ! -f "$STAMP_PATH" ] || [ "$REQUIREMENTS_FILE" -nt "$STAMP_PATH" ]; }; then
    echo "📦 Installing dependencies from requirements.txt..."
    # Upgrade via `python -m pip`, not the `pip` executable directly: on
    # Windows, pip.exe cannot overwrite its own running binary and fails
    # with "ERROR: To modify pip, please run the following command:
    # ... -m pip install --upgrade pip" -- invoking through the
    # interpreter module sidesteps the self-lock.
    "$VENV_BIN/python3" -m pip install --upgrade pip -q
    "$VENV_BIN/pip" install -r "$REQUIREMENTS_FILE" -q
    cp "$REQUIREMENTS_FILE" "$STAMP_PATH"
fi

source "$VENV_BIN/activate"

# Change to the repository root so that relative paths (e.g. --parent-dir
# doc/agile/...) resolve correctly. Use git rev-parse for robustness;
# fall back to SCRIPT_DIR if not inside a git repository.
REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel 2>/dev/null || echo "$SCRIPT_DIR")
cd "$REPO_ROOT"

# --- SSH agent (sandboxed sessions) ---
# Sandboxed shells do not inherit the user's SSH agent socket, so any git
# push/fetch spawned by compass (pr create, capture promote, task start)
# would fail publickey auth. When SSH_AUTH_SOCK is unset or dead, adopt the
# sole socket in the agent directory: ORES_SSH_AGENT_DIR from .env when set,
# else ~/.ssh/agent. See
# doc/llm/memory/set_ssh_auth_sock_for_git_operations.org.
if [ ! -S "${SSH_AUTH_SOCK:-}" ]; then
    AGENT_DIR=$(grep -s '^ORES_SSH_AGENT_DIR=' "$REPO_ROOT/.env" | head -1 | cut -d= -f2-)
    # Normalise the raw value the way compass.py parses .env: strip CR
    # (CRLF .env on Windows), surrounding quotes, and whitespace; expand a
    # leading ~/.
    AGENT_DIR=$(printf '%s' "$AGENT_DIR" | tr -d '\r"'\' \
        | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
    AGENT_DIR="${AGENT_DIR:-$HOME/.ssh/agent}"
    case "$AGENT_DIR" in
        "~/"*) AGENT_DIR="$HOME/${AGENT_DIR#\~/}" ;;
    esac
    if [ -d "$AGENT_DIR" ]; then
        for sock in "$AGENT_DIR"/*; do
            if [ -S "$sock" ]; then
                export SSH_AUTH_SOCK="$sock"
                break
            fi
        done
    fi
fi

# --- Execute the Python CLI ---
# Pass every argument straight through to compass.py, including --help/-h,
# so the help shown is compass.py's own (complete, always-current) command
# set rather than a hand-maintained copy that drifts out of date.
python3 "$SCRIPT_DIR/src/compass.py" "$@"
