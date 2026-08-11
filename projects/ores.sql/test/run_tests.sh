#!/bin/bash
# -*- mode: sh; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# Run pgTAP tests using environment variables for database connection.
#
# Environment variables:
#   ORES_TEST_DB_HOST     - Database host (default: localhost)
#   ORES_TEST_DB_PORT     - Database port (default: 5432)
#   ORES_TEST_DB_DATABASE - Database name (required)
#   ORES_TEST_DB_USER     - Database user (default: postgres)
#
# The checkout's .env is sourced first: the DB user and password live
# there, and pg_prove needs them exported. The test DML user's password
# is ORES_TEST_DB_PASSWORD; the ambient PGPASSWORD belongs to postgres.
#
# Usage:
#   ./run_tests.sh                    # Run all tests
#   ./run_tests.sh test_file.sql      # Run specific test
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Load the environment so the DB user and password reach pg_prove.
if [ -f "$SCRIPT_DIR/../../../.env" ]; then
    set -a
    # shellcheck disable=SC1091
    source "$SCRIPT_DIR/../../../.env"
    set +a
fi

# Database connection defaults
DB_HOST="${ORES_TEST_DB_HOST:-localhost}"
DB_PORT="${ORES_TEST_DB_PORT:-5432}"
DB_USER="${ORES_TEST_DB_USER:-postgres}"
DB_NAME="${ORES_TEST_DB_DATABASE:-}"

if [ -z "$DB_NAME" ]; then
    echo "Error: ORES_TEST_DB_DATABASE environment variable is required"
    echo "Example: ORES_TEST_DB_DATABASE=ores_dev_local1 ./run_tests.sh"
    exit 1
fi

# pg_prove connects through libpq, which reads PGPASSWORD.
export PGPASSWORD="${ORES_TEST_DB_PASSWORD:-$PGPASSWORD}"

# Build connection string for pg_prove
PG_PROVE_OPTS="-h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME"

echo "Running pgTAP tests against $DB_NAME@$DB_HOST:$DB_PORT as $DB_USER"
echo "-------------------------------------------------------------------"

if [ $# -eq 0 ]; then
    # Run all tests (exclude test.sql which is just extension setup)
    pg_prove $PG_PROVE_OPTS "$SCRIPT_DIR"/*_test.sql
else
    # Run specified tests
    pg_prove $PG_PROVE_OPTS "$@"
fi
