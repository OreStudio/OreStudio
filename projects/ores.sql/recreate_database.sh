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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#

set -e

# Source .env if present (local development). In CI, env vars are exported directly.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECKOUT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ENV_FILE="${CHECKOUT_ROOT}/.env"
if [[ -f "${ENV_FILE}" ]]; then
    set -o allexport
    # shellcheck source=/dev/null
    source "${ENV_FILE}"
    set +o allexport
fi

# Default values
DEFAULT_DB_NAME="${ORES_DATABASE_NAME:-ores_frosty_leaf}"

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Recreates the ORE Studio database from scratch.
Population scripts are automatically run as part of database recreation.

Passwords are read from environment variables (set via .env or CI environment):
    PGPASSWORD                          Password for the postgres superuser
    ORES_DB_OWNER_ROLE                  Owner group role name (env-prefixed)
    ORES_DB_RW_ROLE                     Read-write group role name (env-prefixed)
    ORES_DB_RO_ROLE                     Read-only group role name (env-prefixed)
    ORES_DB_DDL_USER                    DDL database user name (env-prefixed)
    ORES_DB_CLI_USER                    CLI database user name (env-prefixed)
    ORES_DB_WT_USER                     Web Toolkit database user name (env-prefixed)
    ORES_DB_COMMS_USER                  Communications database user name (env-prefixed)
    ORES_DB_HTTP_USER                   HTTP database user name (env-prefixed)
    ORES_DB_READONLY_USER               Read-only database user name (env-prefixed)
    ORES_TEST_DB_DDL_USER               Test DDL database user name (env-prefixed)
    ORES_TEST_DB_USER                   Test DML database user name (env-prefixed)
    ORES_IAM_SERVICE_DB_USER            IAM domain service user name (env-prefixed)
    ORES_REFDATA_SERVICE_DB_USER        Reference Data domain service user name
    ORES_DQ_SERVICE_DB_USER             Data Quality domain service user name
    ORES_VARIABILITY_SERVICE_DB_USER    Variability domain service user name
    ORES_ASSETS_SERVICE_DB_USER         Assets domain service user name
    ORES_SYNTHETIC_SERVICE_DB_USER      Synthetic domain service user name
    ORES_SCHEDULER_SERVICE_DB_USER      Scheduler domain service user name
    ORES_REPORTING_SERVICE_DB_USER      Reporting domain service user name
    ORES_TELEMETRY_SERVICE_DB_USER      Telemetry domain service user name
    ORES_TRADING_SERVICE_DB_USER        Trading domain service user name
    ORES_COMPUTE_SERVICE_DB_USER        Compute domain service user name
    ORES_DB_DDL_PASSWORD                Password for the DDL database user
    ORES_DB_CLI_PASSWORD                Password for the CLI database user
    ORES_DB_WT_PASSWORD                 Password for the Web Toolkit database user
    ORES_DB_COMMS_PASSWORD              Password for the Communications database user
    ORES_DB_HTTP_PASSWORD               Password for the HTTP database user
    ORES_TEST_DB_DDL_PASSWORD           Password for the test DDL database user
    ORES_TEST_DB_PASSWORD               Password for the test DML database user
    ORES_DB_READONLY_PASSWORD           Password for the read-only database user
    ORES_IAM_SERVICE_DB_PASSWORD        Password for the IAM domain service user
    ORES_REFDATA_SERVICE_DB_PASSWORD    Password for the Reference Data domain service user
    ORES_DQ_SERVICE_DB_PASSWORD         Password for the Data Quality domain service user
    ORES_VARIABILITY_SERVICE_DB_PASSWORD Password for the Variability domain service user
    ORES_ASSETS_SERVICE_DB_PASSWORD     Password for the Assets domain service user
    ORES_SYNTHETIC_SERVICE_DB_PASSWORD  Password for the Synthetic domain service user
    ORES_SCHEDULER_SERVICE_DB_PASSWORD  Password for the Scheduler domain service user
    ORES_REPORTING_SERVICE_DB_PASSWORD  Password for the Reporting domain service user
    ORES_TELEMETRY_SERVICE_DB_PASSWORD  Password for the Telemetry domain service user
    ORES_TRADING_SERVICE_DB_PASSWORD    Password for the Trading domain service user
    ORES_COMPUTE_SERVICE_DB_PASSWORD    Password for the Compute domain service user

Optional arguments:
    -D, --database NAME                 Database name (default: ${DEFAULT_DB_NAME})
    -y, --yes                           Assume yes to all prompts (skip confirmation)
    --no-sql-validation                 Skip input validation in seed functions (faster)
    -H, --help                          Show this help message

Example:
    # Local development (reads from .env automatically)
    $(basename "$0") -y

    # Specify a custom database name
    $(basename "$0") -y -D my_custom_db

    # CI (env vars set in the CI environment)
    $(basename "$0") -y -D ores_ci

EOF
    exit 1
}

DB_NAME="${DEFAULT_DB_NAME}"
ASSUME_YES=""
SKIP_VALIDATION="off"

# Parse arguments using portable while/case (works on both GNU and BSD).
while [[ $# -gt 0 ]]; do
    case "$1" in
        -D|--database)           DB_NAME="$2";            shift 2 ;;
        -y|--yes)                ASSUME_YES="1";          shift   ;;
        --no-sql-validation)     SKIP_VALIDATION="on";    shift   ;;
        -H|--help)               usage ;;
        --)                      shift; break ;;
        *)
            echo "Error: Invalid option $1" >&2
            usage
            ;;
    esac
done

# Validate required user names and passwords are present in the environment
MISSING_PASSWORDS=()
[[ -z "${PGPASSWORD:-}" ]] && MISSING_PASSWORDS+=("PGPASSWORD")
[[ -z "${ORES_DB_OWNER_ROLE:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_OWNER_ROLE")
[[ -z "${ORES_DB_RW_ROLE:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_RW_ROLE")
[[ -z "${ORES_DB_RO_ROLE:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_RO_ROLE")
[[ -z "${ORES_DB_DDL_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_DDL_USER")
[[ -z "${ORES_DB_CLI_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_CLI_USER")
[[ -z "${ORES_DB_WT_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_WT_USER")
[[ -z "${ORES_DB_COMMS_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_COMMS_USER")
[[ -z "${ORES_DB_HTTP_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_HTTP_USER")
[[ -z "${ORES_DB_READONLY_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_READONLY_USER")
[[ -z "${ORES_TEST_DB_DDL_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_TEST_DB_DDL_USER")
[[ -z "${ORES_TEST_DB_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_TEST_DB_USER")
[[ -z "${ORES_IAM_SERVICE_DB_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_IAM_SERVICE_DB_USER")
[[ -z "${ORES_REFDATA_SERVICE_DB_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_REFDATA_SERVICE_DB_USER")
[[ -z "${ORES_DQ_SERVICE_DB_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_DQ_SERVICE_DB_USER")
[[ -z "${ORES_VARIABILITY_SERVICE_DB_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_VARIABILITY_SERVICE_DB_USER")
[[ -z "${ORES_ASSETS_SERVICE_DB_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_ASSETS_SERVICE_DB_USER")
[[ -z "${ORES_SYNTHETIC_SERVICE_DB_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_SYNTHETIC_SERVICE_DB_USER")
[[ -z "${ORES_SCHEDULER_SERVICE_DB_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_SCHEDULER_SERVICE_DB_USER")
[[ -z "${ORES_REPORTING_SERVICE_DB_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_REPORTING_SERVICE_DB_USER")
[[ -z "${ORES_TELEMETRY_SERVICE_DB_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_TELEMETRY_SERVICE_DB_USER")
[[ -z "${ORES_TRADING_SERVICE_DB_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_TRADING_SERVICE_DB_USER")
[[ -z "${ORES_COMPUTE_SERVICE_DB_USER:-}" ]] && MISSING_PASSWORDS+=("ORES_COMPUTE_SERVICE_DB_USER")
[[ -z "${ORES_DB_DDL_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_DDL_PASSWORD")
[[ -z "${ORES_DB_CLI_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_CLI_PASSWORD")
[[ -z "${ORES_DB_WT_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_WT_PASSWORD")
[[ -z "${ORES_DB_COMMS_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_COMMS_PASSWORD")
[[ -z "${ORES_DB_HTTP_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_HTTP_PASSWORD")
[[ -z "${ORES_TEST_DB_DDL_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_TEST_DB_DDL_PASSWORD")
[[ -z "${ORES_TEST_DB_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_TEST_DB_PASSWORD")
[[ -z "${ORES_DB_READONLY_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_DB_READONLY_PASSWORD")
[[ -z "${ORES_IAM_SERVICE_DB_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_IAM_SERVICE_DB_PASSWORD")
[[ -z "${ORES_REFDATA_SERVICE_DB_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_REFDATA_SERVICE_DB_PASSWORD")
[[ -z "${ORES_DQ_SERVICE_DB_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_DQ_SERVICE_DB_PASSWORD")
[[ -z "${ORES_VARIABILITY_SERVICE_DB_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_VARIABILITY_SERVICE_DB_PASSWORD")
[[ -z "${ORES_ASSETS_SERVICE_DB_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_ASSETS_SERVICE_DB_PASSWORD")
[[ -z "${ORES_SYNTHETIC_SERVICE_DB_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_SYNTHETIC_SERVICE_DB_PASSWORD")
[[ -z "${ORES_SCHEDULER_SERVICE_DB_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_SCHEDULER_SERVICE_DB_PASSWORD")
[[ -z "${ORES_REPORTING_SERVICE_DB_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_REPORTING_SERVICE_DB_PASSWORD")
[[ -z "${ORES_TELEMETRY_SERVICE_DB_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_TELEMETRY_SERVICE_DB_PASSWORD")
[[ -z "${ORES_TRADING_SERVICE_DB_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_TRADING_SERVICE_DB_PASSWORD")
[[ -z "${ORES_COMPUTE_SERVICE_DB_PASSWORD:-}" ]] && MISSING_PASSWORDS+=("ORES_COMPUTE_SERVICE_DB_PASSWORD")

if [[ ${#MISSING_PASSWORDS[@]} -gt 0 ]]; then
    echo "Error: Missing required password environment variables:" >&2
    for pw in "${MISSING_PASSWORDS[@]}"; do
        echo "  - ${pw}" >&2
    done
    echo "" >&2
    echo "Run ./build/scripts/init-environment.sh to generate a .env file," >&2
    echo "or set the variables directly in your CI environment." >&2
    exit 1
fi

echo "=== ORE Studio Database Recreation ==="
echo "Database name: ${DB_NAME}"
echo "Assume yes: ${ASSUME_YES:-no}"
echo "Skip validation: ${SKIP_VALIDATION}"
echo "Script directory: ${SCRIPT_DIR}"
echo ""

# Change to script directory so relative paths in SQL files work
cd "${SCRIPT_DIR}"

# Export PGPASSWORD so that sourced utilities (e.g. check_db_connections.sh)
# and child psql processes can authenticate.
export PGPASSWORD="${PGPASSWORD}"

# Source the connection check utility
source "${SCRIPT_DIR}/utility/check_db_connections.sh"

# Check for active connections to the target database only
echo "--- Checking for active connections ---"
HAS_CONNECTIONS=0
if ! check_db_connections "${DB_NAME}" >/dev/null 2>&1; then
    check_db_connections "${DB_NAME}"
    HAS_CONNECTIONS=1
fi

if [[ "${HAS_CONNECTIONS}" -eq 1 ]]; then
    echo ""
    echo "ERROR: Cannot proceed with recreation - active connections exist."
    echo "Please disconnect all clients before recreating the database."
    exit 1
fi
echo "No active connections found."
echo ""

# Confirmation prompt (unless -y flag is set)
if [[ -z "${ASSUME_YES}" ]]; then
    echo "WARNING: This will DROP all ORES databases and recreate from scratch!"
    echo ""
    read -p "Type 'yes' to proceed: " confirm
    if [[ "${confirm}" != "yes" ]]; then
        echo "Aborted."
        exit 1
    fi
    echo ""
fi

# Phase 1: Drop database, recreate roles and users (postgres superuser)
# Note: psql's :'var' syntax handles quoting for string literals
# Note: db_name should NOT have quotes (it's an identifier in SQL)
# Note: -h localhost forces TCP connection (password auth vs peer auth on socket)
PGPASSWORD="${PGPASSWORD}" psql \
    -h localhost \
    -f ./recreate_database.sql \
    -U postgres \
    -v owner_role="${ORES_DB_OWNER_ROLE}" \
    -v rw_role="${ORES_DB_RW_ROLE}" \
    -v ro_role="${ORES_DB_RO_ROLE}" \
    -v ddl_user="${ORES_DB_DDL_USER}" \
    -v cli_user="${ORES_DB_CLI_USER}" \
    -v wt_user="${ORES_DB_WT_USER}" \
    -v comms_user="${ORES_DB_COMMS_USER}" \
    -v http_user="${ORES_DB_HTTP_USER}" \
    -v readonly_user="${ORES_DB_READONLY_USER}" \
    -v test_ddl_user="${ORES_TEST_DB_DDL_USER}" \
    -v test_dml_user="${ORES_TEST_DB_USER}" \
    -v iam_service_user="${ORES_IAM_SERVICE_DB_USER}" \
    -v refdata_service_user="${ORES_REFDATA_SERVICE_DB_USER}" \
    -v dq_service_user="${ORES_DQ_SERVICE_DB_USER}" \
    -v variability_service_user="${ORES_VARIABILITY_SERVICE_DB_USER}" \
    -v assets_service_user="${ORES_ASSETS_SERVICE_DB_USER}" \
    -v synthetic_service_user="${ORES_SYNTHETIC_SERVICE_DB_USER}" \
    -v scheduler_service_user="${ORES_SCHEDULER_SERVICE_DB_USER}" \
    -v reporting_service_user="${ORES_REPORTING_SERVICE_DB_USER}" \
    -v telemetry_service_user="${ORES_TELEMETRY_SERVICE_DB_USER}" \
    -v trading_service_user="${ORES_TRADING_SERVICE_DB_USER}" \
    -v compute_service_user="${ORES_COMPUTE_SERVICE_DB_USER}" \
    -v ddl_password="${ORES_DB_DDL_PASSWORD}" \
    -v cli_password="${ORES_DB_CLI_PASSWORD}" \
    -v wt_password="${ORES_DB_WT_PASSWORD}" \
    -v comms_password="${ORES_DB_COMMS_PASSWORD}" \
    -v http_password="${ORES_DB_HTTP_PASSWORD}" \
    -v test_ddl_password="${ORES_TEST_DB_DDL_PASSWORD}" \
    -v test_dml_password="${ORES_TEST_DB_PASSWORD}" \
    -v ro_password="${ORES_DB_READONLY_PASSWORD}" \
    -v iam_service_password="${ORES_IAM_SERVICE_DB_PASSWORD}" \
    -v refdata_service_password="${ORES_REFDATA_SERVICE_DB_PASSWORD}" \
    -v dq_service_password="${ORES_DQ_SERVICE_DB_PASSWORD}" \
    -v variability_service_password="${ORES_VARIABILITY_SERVICE_DB_PASSWORD}" \
    -v assets_service_password="${ORES_ASSETS_SERVICE_DB_PASSWORD}" \
    -v synthetic_service_password="${ORES_SYNTHETIC_SERVICE_DB_PASSWORD}" \
    -v scheduler_service_password="${ORES_SCHEDULER_SERVICE_DB_PASSWORD}" \
    -v reporting_service_password="${ORES_REPORTING_SERVICE_DB_PASSWORD}" \
    -v telemetry_service_password="${ORES_TELEMETRY_SERVICE_DB_PASSWORD}" \
    -v trading_service_password="${ORES_TRADING_SERVICE_DB_PASSWORD}" \
    -v compute_service_password="${ORES_COMPUTE_SERVICE_DB_PASSWORD}" \
    -v db_name="${DB_NAME}"

# Phases 2–4: Create database, schema, and metadata via shared helper
SKIP_ARG=""
[[ "${SKIP_VALIDATION}" == "on" ]] && SKIP_ARG="--skip-validation"
export ORES_DB_DDL_PASSWORD="${ORES_DB_DDL_PASSWORD}"
"${SCRIPT_DIR}/setup_database.sh" "${DB_NAME}" ${SKIP_ARG}

echo ""
echo "=== Database recreation complete ==="
