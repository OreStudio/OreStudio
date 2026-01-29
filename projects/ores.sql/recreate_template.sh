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
# Recreates just the ores_template database.
# Used for test infrastructure - tests create temporary databases from template.
# Does NOT affect environment-specific databases (ores_dev_*).

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Recreates the ores_template database only.
This is used by the test infrastructure to create temporary test databases.
Does NOT affect environment-specific databases (ores_dev_*).

Optional arguments:
    -y, --yes                           Assume yes to all prompts (skip confirmation)
    --no-sql-validation                 Skip input validation in seed functions (faster)
    -H, --help                          Show this help message

Environment Variables:
    PGPASSWORD                          Password for the postgres superuser

Example:
    $(basename "$0")
    $(basename "$0") -y --no-sql-validation

EOF
    exit 1
}

# Parse arguments
OPTS=$(getopt -o yH --long yes,no-sql-validation,help -n "$(basename "$0")" -- "$@")
if [[ $? -ne 0 ]]; then
    usage
fi

POSTGRES_PASSWORD="${PGPASSWORD:-}"
ASSUME_YES=""
SKIP_VALIDATION="off"

eval set -- "$OPTS"

while true; do
    case "$1" in
        -y|--yes)
            ASSUME_YES="1"
            shift
            ;;
        --no-sql-validation)
            SKIP_VALIDATION="on"
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

echo "=== ORE Studio Template Recreation ==="
echo "Skip validation: ${SKIP_VALIDATION}"
echo ""

cd "${SCRIPT_DIR}"

# Source the connection check utility
source "${SCRIPT_DIR}/utility/check_db_connections.sh"

# Check for active connections before proceeding
if ! check_db_connections "ores_template"; then
    exit 1
fi

# Confirmation prompt
if [[ -z "${ASSUME_YES}" ]]; then
    echo "WARNING: This will DROP and recreate ores_template!"
    echo "Test databases created from the template will need to be recreated."
    echo ""
    read -p "Type 'yes' to proceed: " confirm
    if [[ "${confirm}" != "yes" ]]; then
        echo "Aborted."
        exit 1
    fi
    echo ""
fi

# Drop and recreate template
echo "--- Dropping ores_template ---"
PGPASSWORD="${POSTGRES_PASSWORD}" psql -h localhost -U postgres -c "UPDATE pg_database SET datistemplate = false WHERE datname = 'ores_template';"
PGPASSWORD="${POSTGRES_PASSWORD}" psql -h localhost -U postgres -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = 'ores_template' AND pid <> pg_backend_pid();"
PGPASSWORD="${POSTGRES_PASSWORD}" psql -h localhost -U postgres -c "DROP DATABASE IF EXISTS ores_template;"

echo "--- Creating ores_template ---"
PGPASSWORD="${POSTGRES_PASSWORD}" psql \
    -h localhost \
    -U postgres \
    -v skip_validation="${SKIP_VALIDATION}" \
    -f ./setup_template.sql

echo ""
echo "=== Template recreation complete ==="
echo ""
echo "The ores_template database has been recreated."
echo "Test databases will be created from this template."
