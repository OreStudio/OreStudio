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
CHECKOUT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ENV_FILE="${CHECKOUT_ROOT}/.env"
if [[ -f "${ENV_FILE}" ]]; then
    set -o allexport
    # shellcheck source=/dev/null
    source "${ENV_FILE}"
    set +o allexport
fi

usage() {
    cat <<EOF
Usage: $(basename "$0") -e ENVIRONMENT [OPTIONS]

Recreates an environment-specific database (ores_dev_<environment>).
Uses two-phase creation: postgres creates the database, the DDL user sets up the schema.

Required arguments:
    -e, --env ENVIRONMENT               Environment name (e.g., local1, local2, remote)

Optional arguments:
    -y, --yes                           Assume yes to all prompts (skip confirmation)
    -k, --kill                          Kill active database connections before recreating
    --no-sql-validation                 Skip input validation in seed functions (faster)
    -H, --help                          Show this help message

Environment Variables:
    PGPASSWORD                          Password for the postgres superuser
    ORES_DB_DDL_PASSWORD                Password for the DDL database user

Example:
    $(basename "$0") -e local2
    $(basename "$0") --env local1 -y --kill
    $(basename "$0") --env local1 -y --no-sql-validation

Database naming convention:
    ores_dev_local1, ores_dev_local2, ores_dev_remote, etc.

EOF
    exit 1
}

POSTGRES_PASSWORD="${PGPASSWORD:-}"
DDL_PASSWORD="${ORES_DB_DDL_PASSWORD:-}"
ENVIRONMENT=""
ASSUME_YES=""
KILL_CONNECTIONS=""
SKIP_VALIDATION="off"

# Parse arguments using portable while/case (works on both GNU and BSD).
while [[ $# -gt 0 ]]; do
    case "$1" in
        -e|--env)              ENVIRONMENT="$2";      shift 2 ;;
        -y|--yes)              ASSUME_YES="1";        shift   ;;
        -k|--kill)             KILL_CONNECTIONS="1";   shift   ;;
        --no-sql-validation)   SKIP_VALIDATION="on";   shift   ;;
        -H|--help)             usage ;;
        --)                    shift; break ;;
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

echo ""
echo "=========================================="
echo "  ORE Studio Environment DB Recreation"
echo "=========================================="
echo ""
echo "  Environment : ${ENVIRONMENT}"
echo "  Database    : ${DB_NAME}"
echo "  Host        : ${ORES_DB_HOST:?ORES_DB_HOST must be set}"
echo "  Validation  : ${SKIP_VALIDATION}"
echo ""
echo "=========================================="
echo ""

cd "${SCRIPT_DIR}"

# Export PGPASSWORD so that sourced utilities (e.g. check_db_connections.sh)
# and child psql processes can authenticate.
export PGPASSWORD="${POSTGRES_PASSWORD}"

# Source the connection check utility
source "${SCRIPT_DIR}/utility/check_db_connections.sh"

# Check for active connections before proceeding
if ! check_db_connections "${DB_NAME}"; then
    if [[ -n "${KILL_CONNECTIONS}" ]]; then
        echo "Killing active connections (--kill flag is set)..."
        "${SCRIPT_DIR}/utility/kill_db_connections.sh" "${DB_NAME}"
    else
        echo "Hint: Use --kill (-k) flag to automatically terminate connections."
        exit 1
    fi
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

# Verify that the cluster-level roles for this environment already exist.
# recreate_env.sh does not create roles — that is done once by recreate_database.sh.
echo "--- Checking environment roles ---"
missing_roles=()
for role in "${ORES_DB_OWNER_ROLE}" "${ORES_DB_RW_ROLE}" "${ORES_DB_RO_ROLE}" "${ORES_DB_SERVICE_ROLE}"; do
    exists=$(psql -h "${ORES_DB_HOST}" -U postgres -tAc \
        "SELECT 1 FROM pg_roles WHERE rolname = '${role}';" 2>/dev/null)
    if [[ "${exists}" != "1" ]]; then
        missing_roles+=("${role}")
    fi
done

if [[ ${#missing_roles[@]} -gt 0 ]]; then
    echo ""
    echo "ERROR: The following cluster-level roles are missing:"
    for role in "${missing_roles[@]}"; do
        echo "  - ${role}"
    done
    echo ""
    echo "This script does not create roles. Run recreate_database.sh once to"
    echo "set up roles and users for this environment, then use recreate_env.sh"
    echo "for subsequent schema resets:"
    echo ""
    echo "  ./recreate_database.sh -e ${ENVIRONMENT} -y"
    echo ""
    exit 1
fi
echo "Environment roles OK."
echo ""

# Drop existing database if it exists
echo "--- Dropping ${DB_NAME} (if exists) ---"
psql -h "${ORES_DB_HOST:?ORES_DB_HOST must be set}" -U postgres -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '${DB_NAME}' AND pid <> pg_backend_pid();"
psql -h "${ORES_DB_HOST}" -U postgres -c "DROP DATABASE IF EXISTS ${DB_NAME};"

# Create database, schema, and metadata via shared helper
SKIP_ARG=""
[[ "${SKIP_VALIDATION}" == "on" ]] && SKIP_ARG="--skip-validation"
"${SCRIPT_DIR}/setup_database.sh" "${DB_NAME}" ${SKIP_ARG}

echo ""
echo "=========================================="
echo "  Recreation complete: ${DB_NAME}"
echo "=========================================="
echo ""
CLI_USER="${ORES_DB_CLI_USER:-ores_cli_user}"
echo "Connect with:"
echo "  psql -U ${CLI_USER} -d ${DB_NAME}"
echo ""
echo "Or set in your environment:"
echo "  export ORES_DATABASE=${DB_NAME}"
