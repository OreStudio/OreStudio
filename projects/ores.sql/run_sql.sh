#!/bin/bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
#
# Run SQL files or commands against the development database.
#
# Prerequisites:
#   - PGPASSWORD must be set for the postgres user
#   - ORES_TEST_DB_HOST must be set (defaults to localhost)
#   - ORES_TEST_DB_DATABASE must be set
#
# Usage:
#   ./run_sql.sh <sql_file>           # Execute a SQL file
#   ./run_sql.sh -c "SELECT 1"        # Execute a SQL command
#
set -e

DB_HOST="${ORES_TEST_DB_HOST:-localhost}"
DB_NAME="${ORES_TEST_DB_DATABASE}"
DB_USER="postgres"

if [[ -z "$PGPASSWORD" ]]; then
    echo "Error: PGPASSWORD must be set for postgres user" >&2
    exit 1
fi

if [[ -z "$DB_NAME" ]]; then
    echo "Error: ORES_TEST_DB_DATABASE must be set" >&2
    exit 1
fi

exec psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" "$@"
