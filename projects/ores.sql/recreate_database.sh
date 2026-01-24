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

set -e

# Default values
DEFAULT_DB_NAME="ores_frosty_leaf"

# Script directory (where this script and SQL files are located)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Recreates the ORE Studio database from scratch.
Population scripts are automatically run as part of database recreation.

Required arguments:
    -p, --postgres-password PASSWORD    Password for the postgres superuser
    -o, --ores-password PASSWORD        Password for the ores database user

Optional arguments:
    -d, --database NAME                 Database name (default: ${DEFAULT_DB_NAME})
    -y, --yes                           Assume yes to all prompts (skip confirmation)
    --no-sql-validation                 Skip input validation in seed functions (faster)
    -h, --help                          Show this help message

Example:
    $(basename "$0") -p myPostgresPass -o myOresPass
    $(basename "$0") --postgres-password myPostgresPass --ores-password myOresPass
    $(basename "$0") -p myPostgresPass -o myOresPass -d my_custom_db
    $(basename "$0") -p myPostgresPass -o myOresPass -y --no-sql-validation

EOF
    exit 1
}

# Parse arguments using getopt for long option support
OPTS=$(getopt -o p:o:d:yh --long postgres-password:,ores-password:,database:,yes,no-sql-validation,help -n "$(basename "$0")" -- "$@")
if [[ $? -ne 0 ]]; then
    usage
fi

eval set -- "$OPTS"

POSTGRES_PASSWORD=""
ORES_PASSWORD=""
DB_NAME="${DEFAULT_DB_NAME}"
ASSUME_YES=""
SKIP_VALIDATION="off"

while true; do
    case "$1" in
        -p|--postgres-password)
            POSTGRES_PASSWORD="$2"
            shift 2
            ;;
        -o|--ores-password)
            ORES_PASSWORD="$2"
            shift 2
            ;;
        -d|--database)
            DB_NAME="$2"
            shift 2
            ;;
        -y|--yes)
            ASSUME_YES="1"
            shift
            ;;
        --no-sql-validation)
            SKIP_VALIDATION="on"
            shift
            ;;
        -h|--help)
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

# Validate required arguments
if [[ -z "${POSTGRES_PASSWORD}" ]]; then
    echo "Error: PostgreSQL password (-p) is required" >&2
    usage
fi

if [[ -z "${ORES_PASSWORD}" ]]; then
    echo "Error: ORES user password (-o) is required" >&2
    usage
fi

echo "=== ORE Studio Database Recreation ==="
echo "Database name: ${DB_NAME}"
echo "Assume yes: ${ASSUME_YES:-no}"
echo "Skip validation: ${SKIP_VALIDATION}"
echo "Script directory: ${SCRIPT_DIR}"
echo ""

# Confirmation prompt (unless -y flag is set)
if [[ -z "${ASSUME_YES}" ]]; then
    echo "WARNING: This will DROP all ORES databases and recreate from scratch!"
    echo ""
    read -p "Type 'yes' to proceed: " confirm
    if [[ "${confirm}" != "yes" ]]; then
        echo "Aborted."
        exit 1
    fi
    echo ""
fi

# Change to script directory so relative paths in SQL files work
cd "${SCRIPT_DIR}"

# Run the recreate_database.sql script
# Note: psql's :'var' syntax handles quoting for string literals
# Note: db_name should NOT have quotes (it's an identifier in SQL)
# Note: -h localhost forces TCP connection (password auth vs peer auth on socket)
PGPASSWORD="${POSTGRES_PASSWORD}" psql \
    -h localhost \
    -f ./recreate_database.sql \
    -U postgres \
    -v ores_password="${ORES_PASSWORD}" \
    -v db_name="${DB_NAME}" \
    -v skip_validation="${SKIP_VALIDATION}"

echo ""
echo "=== Database recreation complete ==="
