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
#   ./run_sql.sh <sql_file>                    # Execute a SQL file as postgres
#   ./run_sql.sh -c "SELECT 1"                 # Execute a SQL command
#   ./run_sql.sh --user ddl <sql_file>         # Execute as the DDL user
#                                              # (needed when inserts flow
#                                              # through the account-validation
#                                              # trigger, which rejects
#                                              # postgres as an actor)
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

DB_ROLE="postgres"
while [[ $# -gt 0 ]]; do
    case "$1" in
        -u|--user)
            if [[ -z "${2:-}" || "$2" == -* ]]; then
                echo "Error: -u/--user requires a valid argument" >&2
                exit 1
            fi
            DB_ROLE="$2"
            shift 2
            ;;
        --user=*)
            DB_ROLE="${1#--user=}"
            shift
            ;;
        *)
            break
            ;;
    esac
done

DB_HOST="${ORES_TEST_DB_HOST:-localhost}"
DB_NAME="${ORES_TEST_DB_DATABASE}"

case "$DB_ROLE" in
    postgres)
        DB_USER="postgres"
        # PGPASSWORD comes from .env
        ;;
    ddl)
        DB_USER="${ORES_DB_DDL_USER}"
        export PGPASSWORD="${ORES_DB_DDL_PASSWORD}"
        if [[ -z "$DB_USER" ]]; then
            echo "Error: ORES_DB_DDL_USER not set in .env" >&2
            exit 1
        fi
        if [[ -z "$PGPASSWORD" ]]; then
            echo "Error: ORES_DB_DDL_PASSWORD not set in .env" >&2
            exit 1
        fi
        ;;
    *)
        echo "Error: --user must be 'postgres' or 'ddl' (got: $DB_ROLE)" >&2
        exit 1
        ;;
esac

if [[ -z "$PGPASSWORD" ]]; then
    echo "Error: PGPASSWORD not set and not found in .env" >&2
    exit 1
fi

if [[ -z "$DB_NAME" ]]; then
    echo "Error: ORES_TEST_DB_DATABASE not set and not found in .env" >&2
    exit 1
fi

exec psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_NAME" "$@"
