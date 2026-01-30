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
# Terminates all connections to a database (except our own).

set -e

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS] DATABASE_NAME

Terminate all connections to a PostgreSQL database.

Arguments:
    DATABASE_NAME               Name of the database

Options:
    -h, --host HOST             PostgreSQL host (default: localhost)
    -H, --help                  Show this help message

Environment Variables:
    PGPASSWORD                  Password for the postgres superuser (required)

Example:
    export PGPASSWORD=myPostgresPass
    $(basename "$0") ores_dev_local1

EOF
    exit 1
}

# Parse arguments
HOST="localhost"
DB_NAME=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--host)
            HOST="$2"
            shift 2
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

# Terminate connections
TERMINATED=$(PGPASSWORD="${PGPASSWORD}" psql -h "${HOST}" -U postgres -At -c \
    "SELECT count(*) FROM (
        SELECT pg_terminate_backend(pid)
        FROM pg_stat_activity
        WHERE datname = '${DB_NAME}' AND pid <> pg_backend_pid()
    ) t;" 2>/dev/null)

if [[ -z "${TERMINATED}" ]]; then
    echo "Failed to terminate connections (database may not exist)."
    exit 1
fi

echo "Terminated ${TERMINATED} connection(s) to '${DB_NAME}'."
