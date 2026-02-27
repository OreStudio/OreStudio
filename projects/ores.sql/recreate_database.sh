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

set -e

# Default values
DEFAULT_DB_NAME="ores_frosty_leaf"

# Script directory (where this script and SQL files are located)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Recreates the ORE Studio database from scratch.
Population scripts are automatically run as part of database recreation.

Required arguments:
    -p, --postgres-password PASSWORD    Password for the postgres superuser
    -d, --ddl-password PASSWORD         Password for the DDL database user
    -c, --cli-password PASSWORD         Password for the CLI database user
    -w, --wt-password PASSWORD          Password for the Web Toolkit database user
    -m, --comms-password PASSWORD       Password for the Communications database user
    -h, --http-password PASSWORD        Password for the HTTP database user
    -t, --test-ddl-password PASSWORD    Password for the test DDL database user
    -T, --test-dml-password PASSWORD    Password for the test DML database user
    -r, --ro-password PASSWORD          Password for the read-only database user

Optional arguments:
    -D, --database NAME                 Database name (default: ${DEFAULT_DB_NAME})
    -y, --yes                           Assume yes to all prompts (skip confirmation)
    --no-sql-validation                 Skip input validation in seed functions (faster)
    -H, --help                          Show this help message

Environment Variables:
    PGPASSWORD                          Password for the postgres superuser (overridden by -p)
    ORES_DB_DDL_PASSWORD                Password for the DDL database user (overridden by -d)
    ORES_DB_CLI_PASSWORD                Password for the CLI database user (overridden by -c)
    ORES_DB_WT_PASSWORD                 Password for the Web Toolkit database user (overridden by -w)
    ORES_DB_COMMS_PASSWORD              Password for the Communications database user (overridden by -m)
    ORES_DB_HTTP_PASSWORD               Password for the HTTP database user (overridden by -h)
    ORES_TEST_DB_DDL_PASSWORD           Password for the test DDL database user (overridden by -t)
    ORES_TEST_DB_PASSWORD               Password for the test DML database user (overridden by -T)
    ORES_DB_READONLY_PASSWORD           Password for the read-only database user (overridden by -r)

Example:
    # Using command line arguments
    $(basename "$0") -p myPostgresPass -d ddlPass -c cliPass -w wtPass -m commsPass -h httpPass -t testDdlPass -T testDmlPass -r roPass

    # Using environment variables
    export PGPASSWORD=myPostgresPass
    export ORES_DB_DDL_PASSWORD=ddlPass
    export ORES_DB_CLI_PASSWORD=cliPass
    export ORES_DB_WT_PASSWORD=wtPass
    export ORES_DB_COMMS_PASSWORD=commsPass
    export ORES_DB_HTTP_PASSWORD=httpPass
    export ORES_TEST_DB_DDL_PASSWORD=testDdlPass
    export ORES_TEST_DB_PASSWORD=testDmlPass
    export ORES_DB_READONLY_PASSWORD=roPass
    $(basename "$0")  # No need to specify passwords on command line

    # Mixed usage (command line takes precedence)
    $(basename "$0") --wt-password differentWtPass  # Uses different password for WT only

EOF
    exit 1
}

# Initialize variables with environment variables as defaults
POSTGRES_PASSWORD="${PGPASSWORD:-}"
DDL_PASSWORD="${ORES_DB_DDL_PASSWORD:-}"
CLI_PASSWORD="${ORES_DB_CLI_PASSWORD:-}"
WT_PASSWORD="${ORES_DB_WT_PASSWORD:-}"
COMMS_PASSWORD="${ORES_DB_COMMS_PASSWORD:-}"
HTTP_PASSWORD="${ORES_DB_HTTP_PASSWORD:-}"
TEST_DDL_PASSWORD="${ORES_TEST_DB_DDL_PASSWORD:-}"
TEST_DML_PASSWORD="${ORES_TEST_DB_PASSWORD:-}"
RO_PASSWORD="${ORES_DB_READONLY_PASSWORD:-}"
DB_NAME="${DEFAULT_DB_NAME}"
ASSUME_YES=""
SKIP_VALIDATION="off"

# Parse arguments using portable while/case (works on both GNU and BSD).
while [[ $# -gt 0 ]]; do
    case "$1" in
        -p|--postgres-password)  POSTGRES_PASSWORD="$2";  shift 2 ;;
        -d|--ddl-password)       DDL_PASSWORD="$2";       shift 2 ;;
        -c|--cli-password)       CLI_PASSWORD="$2";       shift 2 ;;
        -w|--wt-password)        WT_PASSWORD="$2";        shift 2 ;;
        -m|--comms-password)     COMMS_PASSWORD="$2";     shift 2 ;;
        -h|--http-password)      HTTP_PASSWORD="$2";      shift 2 ;;
        -t|--test-ddl-password)  TEST_DDL_PASSWORD="$2";  shift 2 ;;
        -T|--test-dml-password)  TEST_DML_PASSWORD="$2";  shift 2 ;;
        -r|--ro-password)        RO_PASSWORD="$2";        shift 2 ;;
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

# Validate required arguments - collect all missing passwords
MISSING_PASSWORDS=()
[[ -z "${POSTGRES_PASSWORD}" ]] && MISSING_PASSWORDS+=("postgres (-p or PGPASSWORD)")
[[ -z "${DDL_PASSWORD}" ]] && MISSING_PASSWORDS+=("ddl (-d or ORES_DB_DDL_PASSWORD)")
[[ -z "${CLI_PASSWORD}" ]] && MISSING_PASSWORDS+=("cli (-c or ORES_DB_CLI_PASSWORD)")
[[ -z "${WT_PASSWORD}" ]] && MISSING_PASSWORDS+=("wt (-w or ORES_DB_WT_PASSWORD)")
[[ -z "${COMMS_PASSWORD}" ]] && MISSING_PASSWORDS+=("comms (-m or ORES_DB_COMMS_PASSWORD)")
[[ -z "${HTTP_PASSWORD}" ]] && MISSING_PASSWORDS+=("http (-h or ORES_DB_HTTP_PASSWORD)")
[[ -z "${TEST_DDL_PASSWORD}" ]] && MISSING_PASSWORDS+=("test_ddl (-t or ORES_TEST_DB_DDL_PASSWORD)")
[[ -z "${TEST_DML_PASSWORD}" ]] && MISSING_PASSWORDS+=("test_dml (-T or ORES_TEST_DB_PASSWORD)")
[[ -z "${RO_PASSWORD}" ]] && MISSING_PASSWORDS+=("ro (-r or ORES_DB_READONLY_PASSWORD)")

if [[ ${#MISSING_PASSWORDS[@]} -gt 0 ]]; then
    echo "Error: Missing required passwords:" >&2
    for pw in "${MISSING_PASSWORDS[@]}"; do
        echo "  - ${pw}" >&2
    done
    echo "" >&2
    echo "Set via command line flags or environment variables." >&2
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
export PGPASSWORD="${POSTGRES_PASSWORD}"

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
PGPASSWORD="${POSTGRES_PASSWORD}" psql \
    -h localhost \
    -f ./recreate_database.sql \
    -U postgres \
    -v ddl_password="${DDL_PASSWORD}" \
    -v cli_password="${CLI_PASSWORD}" \
    -v wt_password="${WT_PASSWORD}" \
    -v comms_password="${COMMS_PASSWORD}" \
    -v http_password="${HTTP_PASSWORD}" \
    -v test_ddl_password="${TEST_DDL_PASSWORD}" \
    -v test_dml_password="${TEST_DML_PASSWORD}" \
    -v ro_password="${RO_PASSWORD}" \
    -v db_name="${DB_NAME}"

# Phases 2–4: Create database, schema, and metadata via shared helper
SKIP_ARG=""
[[ "${SKIP_VALIDATION}" == "on" ]] && SKIP_ARG="--skip-validation"
export ORES_DB_DDL_PASSWORD="${DDL_PASSWORD}"
"${SCRIPT_DIR}/setup_database.sh" "${DB_NAME}" ${SKIP_ARG}

echo ""
echo "=== Database recreation complete ==="
