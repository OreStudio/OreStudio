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
# Lists ORES databases with creation timestamps.
# Output format: datname|created (tab-separated)

set -e

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

List ORES databases with creation timestamps.

Options:
    -h, --host HOST             PostgreSQL host (default: localhost)
    -p, --port PORT             PostgreSQL port (default: 5432)
    -H, --help                  Show this help message

Environment Variables:
    PGPASSWORD                  Password for the postgres superuser (required)

Output:
    Tab-separated: datname, created timestamp

Example:
    export PGPASSWORD=myPostgresPass
    $(basename "$0")
    $(basename "$0") -h 192.168.1.22 -p 5433

EOF
    exit 1
}

# Parse arguments
HOST="localhost"
PORT="5432"

while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--host)
            HOST="$2"
            shift 2
            ;;
        -p|--port)
            PORT="$2"
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
            echo "Error: Unexpected argument $1" >&2
            usage
            ;;
    esac
done

if [[ -z "${PGPASSWORD}" ]]; then
    echo "Error: PGPASSWORD environment variable is required" >&2
    exit 1
fi

# Query databases with creation time from filesystem
# pg_stat_file requires superuser, falls back to empty if not available
PGPASSWORD="${PGPASSWORD}" psql -h "${HOST}" -p "${PORT}" -U postgres -At -F $'\t' -c "
SELECT
    datname,
    COALESCE(
        (pg_stat_file('base/' || oid)).modification::timestamp(0)::text,
        ''
    ) AS created
FROM pg_database
WHERE datname LIKE 'ores_%'
ORDER BY datname;
" 2>/dev/null
