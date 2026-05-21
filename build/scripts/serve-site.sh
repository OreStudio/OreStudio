#!/usr/bin/env bash
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# Serve the published org-mode site locally for previewing.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
BUILD_DIR="$REPO_ROOT/build/output/site/OreStudio"
PORT="${PORT:-8000}"

usage() {
    cat <<EOF
Usage: $(basename "$0") [--compile] [--port PORT]

  --compile   Rebuild the site (emacs -Q --script .build-site.el) before serving.
  --port      Port to serve on (default: 8000; can also be set via PORT env var).
  -h, --help  Show this help.
EOF
    exit 1
}

COMPILE=0
while [[ $# -gt 0 ]]; do
    case "$1" in
        --compile) COMPILE=1; shift ;;
        --port)
            if [[ -n "${2:-}" && "${2:-}" != -* ]]; then
                PORT="$2"
                shift 2
            else
                echo "Error: --port requires a non-option argument" >&2
                usage
            fi
            ;;
        -h|--help) usage ;;
        *) echo "Unknown option: $1" >&2; usage ;;
    esac
done

if [[ $COMPILE -eq 1 ]]; then
    echo "Building site..."
    (cd "$REPO_ROOT" && emacs -Q --script .build-site.el)
fi

if [[ ! -d "$BUILD_DIR" ]]; then
    echo "Build directory not found: $BUILD_DIR" >&2
    echo "Run with --compile, or build first via: emacs -Q --script .build-site.el" >&2
    exit 1
fi

echo "Serving $BUILD_DIR on http://localhost:$PORT/OreStudio/"
cd "$BUILD_DIR"
exec python3 -m http.server "$PORT"
