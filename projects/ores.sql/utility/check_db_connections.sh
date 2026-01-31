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
# Checks if there are active connections to a database.
# If connections exist, prints details and exits with error.
# Used by recreate scripts to prevent data loss.
#
# Usage:
#   source check_db_connections.sh
#   check_db_connections "database_name"
#
# Returns:
#   0 if no connections (safe to proceed)
#   1 if connections exist (should abort)

check_db_connections() {
    local db_name="$1"
    local host="${2:-localhost}"

    if [[ -z "${db_name}" ]]; then
        echo "Error: Database name is required" >&2
        return 1
    fi

    if [[ -z "${PGPASSWORD}" ]]; then
        echo "Error: PGPASSWORD environment variable is required" >&2
        return 1
    fi

    # Check for active connections (excluding our own backend)
    local conn_count
    conn_count=$(PGPASSWORD="${PGPASSWORD}" psql -h "${host}" -U postgres -At -c \
        "SELECT count(*) FROM pg_stat_activity WHERE datname = '${db_name}' AND pid <> pg_backend_pid();" 2>/dev/null)

    if [[ -z "${conn_count}" ]]; then
        # Database might not exist, which is fine for recreation
        return 0
    fi

    if [[ "${conn_count}" -gt 0 ]]; then
        echo ""
        echo "ERROR: Cannot proceed - there are ${conn_count} active connection(s) to ${db_name}!"
        echo ""
        echo "Active connections:"
        echo "==================="
        PGPASSWORD="${PGPASSWORD}" psql -h "${host}" -U postgres -c \
            "SELECT pid, usename AS user, client_addr AS client, state,
                    to_char(backend_start, 'YYYY-MM-DD HH24:MI:SS') AS connected_since,
                    left(query, 50) AS current_query
             FROM pg_stat_activity
             WHERE datname = '${db_name}' AND pid <> pg_backend_pid()
             ORDER BY backend_start;"
        echo ""
        echo "Please disconnect all clients before recreating the database."
        echo "You can terminate connections with:"
        echo "  \${SCRIPT_DIR}/utility/kill_db_connections.sh ${db_name}"
        echo ""
        return 1
    fi

    return 0
}
