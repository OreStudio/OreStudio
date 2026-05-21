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
# Show one document by UUID (or unambiguous prefix of >= 6 hex chars).
#
# Prints: header metadata, blurb, outgoing links, incoming links. Resolves
# both file-level and heading-level :ID:s — heading-level matches are
# shown with their parent doc and surrounding section body.
#
# Usage:
#   ./doc_show.sh <uuid-or-prefix>
#
#   ./doc_show.sh 47341fc6                   # Glossary doc
#   ./doc_show.sh fabb50e5                   # Release entry (anchor in glossary)
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if ! command -v python3 &> /dev/null; then
    echo "Error: python3 is required but not installed"
    exit 1
fi

python3 "$SCRIPT_DIR/scripts/doc_show.py" "$@"
