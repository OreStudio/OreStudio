#!/bin/bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
#
# Run SQL files or commands against the development database.
#
# Reads credentials from .env in the repository root (PGPASSWORD,
# ORES_TEST_DB_HOST, ORES_TEST_DB_DATABASE). Individual environment
# variables override .env values when set.
#
# Usage:
#   ./run_sql.sh <sql_file>           # Execute a SQL file
#   ./run_sql.sh -c "SELECT 1"        # Execute a SQL command
#
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ENV_FILE="${REPO_ROOT}/.env"

if [[ -f "${ENV_FILE}" ]]; then
    set -a
    # shellcheck disable=SC1090
    source "${ENV_FILE}"
    set +a
fi

DB_HOST="${ORES_TEST_DB_HOST:-localhost}"
DB_NAME="${ORES_TEST_DB_DATABASE}"
DB_USER="postgres"

if [[ -z "$PGPASSWORD" ]]; then
    echo "Error: PGPASSWORD not set and not found in .env" >&2
    exit 1
fi

if [[ -z "$DB_NAME" ]]; then
    echo "Error: ORES_TEST_DB_DATABASE not set and not found in .env" >&2
    exit 1
fi

exec psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" "$@"
