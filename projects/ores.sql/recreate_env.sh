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
# Recreates an environment-specific database (e.g., ores_dev_local2).
# These databases are created from scratch (NOT from template) to ensure
# environment isolation - recreating one environment does not affect others.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
    cat <<EOF
Usage: $(basename "$0") -e ENVIRONMENT [OPTIONS]

Recreates an environment-specific database (ores_dev_<environment>).
Uses two-phase creation: postgres creates the database, ores_ddl_user sets up schema.

Required arguments:
    -e, --env ENVIRONMENT               Environment name (e.g., local1, local2, remote)

Optional arguments:
    -y, --yes                           Assume yes to all prompts (skip confirmation)
    --no-sql-validation                 Skip input validation in seed functions (faster)
    -H, --help                          Show this help message

Environment Variables:
    PGPASSWORD                          Password for the postgres superuser
    ORES_DB_DDL_PASSWORD                Password for the DDL database user

Example:
    $(basename "$0") -e local2
    $(basename "$0") --env local1 -y --no-sql-validation

Database naming convention:
    ores_dev_local1, ores_dev_local2, ores_dev_remote, etc.

EOF
    exit 1
}

# Parse arguments
OPTS=$(getopt -o e:yH --long env:,yes,no-sql-validation,help -n "$(basename "$0")" -- "$@")
if [[ $? -ne 0 ]]; then
    usage
fi

POSTGRES_PASSWORD="${PGPASSWORD:-}"
DDL_PASSWORD="${ORES_DB_DDL_PASSWORD:-}"
ENVIRONMENT=""
ASSUME_YES=""
SKIP_VALIDATION="off"

eval set -- "$OPTS"

while true; do
    case "$1" in
        -e|--env)
            ENVIRONMENT="$2"
            shift 2
            ;;
        -y|--yes)
            ASSUME_YES="1"
            shift
            ;;
        --no-sql-validation)
            SKIP_VALIDATION="on"
            shift
            ;;
        -H|--help)
            usage
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Error: Invalid option $1" >&2
            usage
            ;;
    esac
done

# Validate required arguments
if [[ -z "${ENVIRONMENT}" ]]; then
    echo "Error: Environment (-e) is required" >&2
    usage
fi

if [[ -z "${POSTGRES_PASSWORD}" ]]; then
    echo "Error: PGPASSWORD environment variable is required" >&2
    exit 1
fi

if [[ -z "${DDL_PASSWORD}" ]]; then
    echo "Error: ORES_DB_DDL_PASSWORD environment variable is required" >&2
    exit 1
fi

DB_NAME="ores_dev_${ENVIRONMENT}"

echo "=== ORE Studio Environment Database Recreation ==="
echo "Environment: ${ENVIRONMENT}"
echo "Database: ${DB_NAME}"
echo "Skip validation: ${SKIP_VALIDATION}"
echo ""

cd "${SCRIPT_DIR}"

# Source the connection check utility
source "${SCRIPT_DIR}/utility/check_db_connections.sh"

# Check for active connections before proceeding
if ! check_db_connections "${DB_NAME}"; then
    exit 1
fi

# Confirmation prompt
if [[ -z "${ASSUME_YES}" ]]; then
    echo "WARNING: This will DROP and recreate ${DB_NAME}!"
    echo ""
    read -p "Type 'yes' to proceed: " confirm
    if [[ "${confirm}" != "yes" ]]; then
        echo "Aborted."
        exit 1
    fi
    echo ""
fi

# Drop existing database if it exists
echo "--- Dropping ${DB_NAME} (if exists) ---"
PGPASSWORD="${POSTGRES_PASSWORD}" psql -h localhost -U postgres -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '${DB_NAME}' AND pid <> pg_backend_pid();"
PGPASSWORD="${POSTGRES_PASSWORD}" psql -h localhost -U postgres -c "DROP DATABASE IF EXISTS ${DB_NAME};"

# Phase 1: Create database as postgres (infrastructure)
echo "--- Creating ${DB_NAME} (postgres phase) ---"
PGPASSWORD="${POSTGRES_PASSWORD}" psql \
    -h localhost \
    -U postgres \
    -v db_name="${DB_NAME}" \
    -f ./create_database.sql

# Phase 2: Setup schema as ores_ddl_user
echo "--- Setting up schema (ores_ddl_user phase) ---"
PGPASSWORD="${DDL_PASSWORD}" psql \
    -h localhost \
    -U ores_ddl_user \
    -d "${DB_NAME}" \
    -v skip_validation="${SKIP_VALIDATION}" \
    -f ./setup_schema.sql

echo ""
echo "=== Environment database recreation complete ==="
echo ""
echo "Database: ${DB_NAME}"
echo ""
echo "Connect with:"
echo "  psql -U ores_cli_user -d ${DB_NAME}"
echo ""
echo "Or set in your environment:"
echo "  export ORES_DATABASE=${DB_NAME}"
