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
# Drops a single ORES database safely.
# Checks for active connections and refuses to drop protected databases.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source the connection checking utility
source "${SCRIPT_DIR}/utility/check_db_connections.sh"

# Protected databases that cannot be dropped
PROTECTED_DBS=("postgres" "template0" "template1")

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS] DATABASE_NAME

Drop an ORES database safely.

Arguments:
    DATABASE_NAME               Name of the database to drop

Options:
    -h, --host HOST             PostgreSQL host (default: localhost)
    -y, --yes                   Skip confirmation prompt
    -H, --help                  Show this help message

Environment Variables:
    PGPASSWORD                  Password for the postgres superuser (required)

Protected Databases (cannot be dropped):
    postgres, template0, template1

Example:
    export PGPASSWORD=myPostgresPass
    $(basename "$0") ores_dev_local1
    $(basename "$0") -y ores_test_whimsical_name

EOF
    exit 1
}

# Parse arguments
HOST="localhost"
ASSUME_YES=""
DB_NAME=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--host)
            HOST="$2"
            shift 2
            ;;
        -y|--yes)
            ASSUME_YES="1"
            shift
            ;;
        -H|--help)
            usage
            ;;
        -*)
            echo "Error: Unknown option $1" >&2
            usage
            ;;
        *)
            if [[ -z "${DB_NAME}" ]]; then
                DB_NAME="$1"
            else
                echo "Error: Multiple database names provided" >&2
                usage
            fi
            shift
            ;;
    esac
done

# Validate arguments
if [[ -z "${DB_NAME}" ]]; then
    echo "Error: Database name is required" >&2
    usage
fi

if [[ -z "${PGPASSWORD}" ]]; then
    echo "Error: PGPASSWORD environment variable is required" >&2
    exit 1
fi

# Check if database is protected
for protected in "${PROTECTED_DBS[@]}"; do
    if [[ "${DB_NAME}" == "${protected}" ]]; then
        echo "Error: Cannot drop protected database: ${DB_NAME}" >&2
        exit 1
    fi
done

# Check if database exists
DB_EXISTS=$(PGPASSWORD="${PGPASSWORD}" psql -h "${HOST}" -U postgres -At -c \
    "SELECT 1 FROM pg_database WHERE datname = '${DB_NAME}';" 2>/dev/null || echo "")

if [[ -z "${DB_EXISTS}" ]]; then
    echo "Database '${DB_NAME}' does not exist on ${HOST}."
    exit 0
fi

# Check for active connections
if ! check_db_connections "${DB_NAME}" "${HOST}"; then
    exit 1
fi

# Confirmation prompt
if [[ -z "${ASSUME_YES}" ]]; then
    echo ""
    echo "WARNING: About to drop database '${DB_NAME}' on ${HOST}"
    echo "This action cannot be undone!"
    echo ""
    read -p "Type 'yes' to confirm: " confirm
    if [[ "${confirm}" != "yes" ]]; then
        echo "Aborted."
        exit 1
    fi
fi

# Unset template flag if this is a template database
PGPASSWORD="${PGPASSWORD}" psql -h "${HOST}" -U postgres -c \
    "UPDATE pg_database SET datistemplate = false WHERE datname = '${DB_NAME}';" 2>/dev/null

# Drop the database
echo "Dropping database '${DB_NAME}'..."
PGPASSWORD="${PGPASSWORD}" psql -h "${HOST}" -U postgres -c "DROP DATABASE IF EXISTS ${DB_NAME};"

echo ""
echo "Database '${DB_NAME}' has been dropped."
