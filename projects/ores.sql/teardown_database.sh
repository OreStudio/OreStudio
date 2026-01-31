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
# Tears down a database: kills all connections then drops it.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Protected databases that cannot be dropped
PROTECTED_DBS=("postgres" "template0" "template1")

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS] DATABASE_NAME

Teardown a database: kill all connections and drop it.

Arguments:
    DATABASE_NAME               Name of the database to teardown

Options:
    -h, --host HOST             PostgreSQL host (default: localhost)
    -y, --yes                   Skip confirmation prompt
    -H, --help                  Show this help message

Environment Variables:
    PGPASSWORD                  Password for the postgres superuser (required)

Protected Databases (cannot be dropped):
    ores_admin, ores_template, postgres, template0, template1

Example:
    export PGPASSWORD=myPostgresPass
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
        echo "Error: Cannot teardown protected database: ${DB_NAME}" >&2
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

# Confirmation prompt
if [[ -z "${ASSUME_YES}" ]]; then
    echo ""
    echo "WARNING: About to teardown database '${DB_NAME}' on ${HOST}"
    echo "This will kill all connections and drop the database!"
    echo ""
    read -p "Type 'yes' to confirm: " confirm
    if [[ "${confirm}" != "yes" ]]; then
        echo "Aborted."
        exit 1
    fi
fi

# Kill all connections
echo "Killing connections to '${DB_NAME}'..."
TERMINATED=$(PGPASSWORD="${PGPASSWORD}" psql -h "${HOST}" -U postgres -At -c \
    "SELECT count(*) FROM (
        SELECT pg_terminate_backend(pid)
        FROM pg_stat_activity
        WHERE datname = '${DB_NAME}' AND pid <> pg_backend_pid()
    ) t;" 2>/dev/null || echo "0")

echo "Terminated ${TERMINATED} connection(s)."

# Drop the database
echo "Dropping database '${DB_NAME}'..."
PGPASSWORD="${PGPASSWORD}" psql -h "${HOST}" -U postgres -c "DROP DATABASE IF EXISTS ${DB_NAME};"

echo ""
echo "Database '${DB_NAME}' has been torn down."
