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
# Validate ORE Studio component documentation structure.
#
# Checks every projects/ores.*/ directory for:
#   - modeling/component_overview.org presence
#   - Required v2 frontmatter (:ID:, #+type: component, #+description:)
#   - Required sections (Summary, Inputs, Outputs, Entry points,
#     Dependencies, See also)
#   - At least one .puml file in modeling/
#
# Exceptions are read from docs_exceptions.txt in the same directory.
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

exec python3 "$SCRIPT_DIR/validate_docs.py" "$@"
