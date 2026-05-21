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
# List addressable documents (one line per doc) with regex / tag / type /
# path filters.
#
# Each line: <uuid> | <type> | <title> — <description>   (rel/path)
#
# Filters (all optional, combine with AND semantics):
#   --regex PATTERN   Case-insensitive regex against title OR description.
#   --tag TAG         Require this filetag. Repeatable; all required.
#   --type TYPE       Require this #+type: (recipe, knowledge, task, ...).
#   --under PATH      Restrict to this path prefix. Repeatable.
#   --sort {title,updated,path}    Sort key (default: path).
#   --count           Print match count only.
#   --paths           Print one path per line (for piping to grep / xargs).
#
# Usage:
#   ./doc_list.sh --type recipe --regex trade
#   ./doc_list.sh --tag cli --type recipe --count
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if ! command -v python3 &> /dev/null; then
    echo "Error: python3 is required but not installed"
    exit 1
fi

python3 "$SCRIPT_DIR/scripts/doc_list.py" "$@"
