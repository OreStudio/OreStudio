#!/bin/bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# Wrapper script for pr_watch.py
# Background PR monitor: polls CI and review threads, invokes claude only
# when something actionable is detected.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
    cat <<EOF
Usage: $(basename "$0") PR_NUMBER [OPTIONS]

Monitor a GitHub pull request in the background. Polls CI status and review
threads at a fixed interval and invokes \`claude -p\` only when there is
something actionable (CI failure or new unresolved review comment).
Exits cleanly once all CI checks are green and all review threads are resolved.

Arguments:
    PR_NUMBER               Pull request number to watch

Options:
    --interval MINUTES      Polling interval in minutes (default: 5)
    --repo OWNER/REPO       Repository slug (default: derived from git remote)
    --ignore-logins LOGINS  Comma-separated logins whose review threads are
                            skipped (default: current authenticated gh user)
    -h, --help              Show this help message

Examples:
    $(basename "$0") 574
    $(basename "$0") 574 --interval 10
    $(basename "$0") 574 --repo OreStudio/OreStudio --interval 3
    $(basename "$0") 574 --ignore-logins mcraveiro,bot

EOF
    exit 0
}

if [[ $# -eq 0 ]] || [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
    usage
fi

# Check dependencies
for cmd in gh claude python3; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "Error: '$cmd' not found in PATH" >&2
        exit 1
    fi
done

exec python3 "${SCRIPT_DIR}/pr_watch.py" "$@"
