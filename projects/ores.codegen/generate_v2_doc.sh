#!/bin/bash

# Generate a v2 information-architecture document (task, story, sprint,
# or version) from a Mustache template. See src/v2_doc_generate.py for
# the contract and doc/v2/meta/document_types.org for the document
# format the templates follow.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_PATH="$SCRIPT_DIR/venv"

if [ ! -d "$VENV_PATH" ]; then
    echo "Error: virtual environment not found at $VENV_PATH"
    echo "Please run: python3 -m venv venv && source venv/bin/activate && pip install -r requirements.txt"
    exit 1
fi

source "$VENV_PATH/bin/activate"
exec python3 "$SCRIPT_DIR/src/v2_doc_generate.py" "$@"
