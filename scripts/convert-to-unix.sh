#!/bin/bash

set -euo pipefail

PROJECT_ROOT="${1:-.}"

if [[ ! -d "$PROJECT_ROOT" ]]; then
    echo "Error: '$PROJECT_ROOT' is not a valid directory." >&2
    exit 1
fi

if ! command -v dos2unix >/dev/null 2>&1; then
    echo "Error: 'dos2unix' is not installed." >&2
    exit 1
fi

echo "Converting text files in: $PROJECT_ROOT (excluding vcpkg/ and output/)"

# Use find with -not -path to exclude directories
find "$PROJECT_ROOT" -type f \
     -not -path "./vcpkg/*" \
     -not -path "./.opencode/*" \
     -not -path "./build/output/*" \
     -not -path "./.packages/*" \
     -not -path "./.git" \
     -not -path "./.gitmodules" \
     -not -path "./.gitattributes" \
     -not -path "./build/cmake/overlays/*" \
     -print0 | while IFS= read -r -d '' f; do
    if file --brief --mime-type "$f" | grep -q '^text/'; then
        echo "Converting: $f"
        dos2unix "$f" >/dev/null
    fi
done

echo "Done."
