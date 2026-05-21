#!/bin/bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
# Validate ORE Studio documentation structure.
#
# Runs two checks in sequence:
#
#   1. validate_docs.py — component documentation structure:
#      - modeling/component_overview.org presence
#      - Required v2 frontmatter (:ID:, #+type: component, #+description:)
#      - Required sections (Summary, Inputs, Outputs, Entry points,
#        Dependencies, See also)
#      - At least one .puml file in modeling/
#
#   2. scripts/doc_version_audit.py --check — every .org file in the
#      repo carries a #+version: 1|2 marker.
#
# Exceptions for the component check are in docs_exceptions.txt.
# Format: CHECK_CODE component_name (one per line, # comments ignored).
#
# Usage: ./validate_docs.sh
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if ! command -v python3 &> /dev/null; then
    echo "Error: python3 is required but not installed"
    exit 1
fi

VENV_PATH="$SCRIPT_DIR/venv"
if [ ! -d "$VENV_PATH" ]; then
    echo "Setting up virtual environment..."
    python3 -m venv "$VENV_PATH"
fi

python3 "$SCRIPT_DIR/validate_docs.py" "$@"
python3 "$SCRIPT_DIR/scripts/doc_version_audit.py" --check
