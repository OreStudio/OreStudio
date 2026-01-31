#!/bin/bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
# Recreates just the ores_admin database.
# Contains cluster-level admin utilities (whimsical names, cleanup functions).

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Recreates the ores_admin database only.
This contains cluster-level admin utilities.

Optional arguments:
    -y, --yes                           Assume yes to all prompts (skip confirmation)
    -H, --help                          Show this help message

Environment Variables:
    PGPASSWORD                          Password for the postgres superuser

Example:
    $(basename "$0")
    $(basename "$0") -y

EOF
    exit 1
}

# Parse arguments
OPTS=$(getopt -o yH --long yes,help -n "$(basename "$0")" -- "$@")
if [[ $? -ne 0 ]]; then
    usage
fi

POSTGRES_PASSWORD="${PGPASSWORD:-}"
ASSUME_YES=""

eval set -- "$OPTS"

while true; do
    case "$1" in
        -y|--yes)
            ASSUME_YES="1"
            shift
            ;;
        -H|--help)
            usage
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Error: Invalid option $1" >&2
            usage
            ;;
    esac
done

# Validate postgres password
if [[ -z "${POSTGRES_PASSWORD}" ]]; then
    echo "Error: PGPASSWORD environment variable is required" >&2
    exit 1
fi

echo "=== ORE Studio Admin Database Recreation ==="
echo ""

cd "${SCRIPT_DIR}"

# Source the connection check utility
source "${SCRIPT_DIR}/utility/check_db_connections.sh"

# Check for active connections before proceeding
if ! check_db_connections "ores_admin"; then
    exit 1
fi

# Confirmation prompt
if [[ -z "${ASSUME_YES}" ]]; then
    echo "WARNING: This will DROP and recreate ores_admin!"
    echo ""
    read -p "Type 'yes' to proceed: " confirm
    if [[ "${confirm}" != "yes" ]]; then
        echo "Aborted."
        exit 1
    fi
    echo ""
fi

# Drop admin database
echo "--- Dropping ores_admin ---"
PGPASSWORD="${POSTGRES_PASSWORD}" psql -h localhost -U postgres -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = 'ores_admin' AND pid <> pg_backend_pid();"
PGPASSWORD="${POSTGRES_PASSWORD}" psql -h localhost -U postgres -c "DROP DATABASE IF EXISTS ores_admin;"

# Recreate admin database (run from admin directory for relative paths)
echo "--- Creating ores_admin ---"
cd "${SCRIPT_DIR}/admin"
PGPASSWORD="${POSTGRES_PASSWORD}" psql \
    -h localhost \
    -U postgres \
    -f ./setup_admin.sql

echo ""
echo "=== Admin database recreation complete ==="
echo ""
echo "The ores_admin database has been recreated."
echo "It contains cluster-level admin utilities."
