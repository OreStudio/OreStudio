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
Usage: $(basename "$0") -p POSTGRES_PASSWORD -o ORES_PASSWORD [-d DB_NAME] [-P]

Recreates the ORE Studio database from scratch.

Required arguments:
    -p POSTGRES_PASSWORD    Password for the postgres superuser
    -o ORES_PASSWORD        Password for the ores database user

Optional arguments:
    -d DB_NAME              Database name (default: ${DEFAULT_DB_NAME})
    -P                      Run population scripts after database creation
    -h                      Show this help message

Example:
    $(basename "$0") -p myPostgresPass -o myOresPass
    $(basename "$0") -p myPostgresPass -o myOresPass -d my_custom_db
    $(basename "$0") -p myPostgresPass -o myOresPass -P

EOF
    exit 1
}

# Parse arguments
POSTGRES_PASSWORD=""
ORES_PASSWORD=""
DB_NAME="${DEFAULT_DB_NAME}"
RUN_POPULATE=false

while getopts "p:o:d:Ph" opt; do
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
        P)
            RUN_POPULATE=true
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
echo "Run populate: ${RUN_POPULATE}"
echo "Script directory: ${SCRIPT_DIR}"
echo ""

# Change to script directory so relative paths in SQL files work
cd "${SCRIPT_DIR}"

# Run the recreate_database.sql script
# Note: ores_password needs quotes (it's a string literal in SQL)
# Note: db_name should NOT have quotes (it's an identifier in SQL)
PGPASSWORD="${POSTGRES_PASSWORD}" psql \
    -f ./recreate_database.sql \
    -U postgres \
    -v ores_password="'${ORES_PASSWORD}'" \
    -v db_name="${DB_NAME}"

# Run population scripts if requested
if [[ "${RUN_POPULATE}" == "true" ]]; then
    echo ""
    echo "=== Running population scripts ==="
    PGPASSWORD="${ORES_PASSWORD}" psql \
        -f ./populate/populate.sql \
        -U ores \
        -d "${DB_NAME}"
fi

echo ""
echo "=== Database recreation complete ==="
