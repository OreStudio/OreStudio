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
Usage: $(basename "$0") -p POSTGRES_PASSWORD -o ORES_PASSWORD [-d DB_NAME] [-y] [-n]

Recreates the ORE Studio database from scratch.
Population scripts are automatically run as part of database recreation.

Required arguments:
    -p POSTGRES_PASSWORD    Password for the postgres superuser
    -o ORES_PASSWORD        Password for the ores database user

Optional arguments:
    -d DB_NAME              Database name (default: ${DEFAULT_DB_NAME})
    -y                      Assume yes to all prompts (skip confirmation)
    -n                      Skip input validation in seed functions (faster)
    -h                      Show this help message

Example:
    $(basename "$0") -p myPostgresPass -o myOresPass
    $(basename "$0") -p myPostgresPass -o myOresPass -d my_custom_db
    $(basename "$0") -p myPostgresPass -o myOresPass -y -n

EOF
    exit 1
}

# Parse arguments
POSTGRES_PASSWORD=""
ORES_PASSWORD=""
DB_NAME="${DEFAULT_DB_NAME}"
ASSUME_YES=""
SKIP_VALIDATION="off"

while getopts "p:o:d:ynh" opt; do
    case ${opt} in
        p)
            POSTGRES_PASSWORD="${OPTARG}"
            ;;
        o)
            ORES_PASSWORD="${OPTARG}"
            ;;
        d)
            DB_NAME="${OPTARG}"
            ;;
        y)
            ASSUME_YES="1"
            ;;
        n)
            SKIP_VALIDATION="on"
            ;;
        h)
            usage
            ;;
        \?)
            echo "Error: Invalid option -${OPTARG}" >&2
            usage
            ;;
        :)
            echo "Error: Option -${OPTARG} requires an argument" >&2
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

# Change to script directory so relative paths in SQL files work
cd "${SCRIPT_DIR}"

# Build psql arguments
PSQL_ARGS=(
    -h localhost
    -f ./recreate_database.sql
    -U postgres
    -v ores_password="${ORES_PASSWORD}"
    -v db_name="${DB_NAME}"
    -v skip_validation="${SKIP_VALIDATION}"
)

# Add -y flag if assume yes is set
if [[ -n "${ASSUME_YES}" ]]; then
    PSQL_ARGS+=(-v y=1)
fi

# Run the recreate_database.sql script
# Note: psql's :'var' syntax handles quoting for string literals
# Note: db_name should NOT have quotes (it's an identifier in SQL)
# Note: -h localhost forces TCP connection (password auth vs peer auth on socket)
PGPASSWORD="${POSTGRES_PASSWORD}" psql "${PSQL_ARGS[@]}"

echo ""
echo "=== Database recreation complete ==="
