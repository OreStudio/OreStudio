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
# Usage:
#   ./run_tests.sh                    # Run all tests
#   ./run_tests.sh test_file.sql      # Run specific test
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

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
