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

# Parse arguments using getopt for long option support
OPTS=$(getopt -o p:d:c:w:m:h:t:T:r:D:yH --long postgres-password:,ddl-password:,cli-password:,wt-password:,comms-password:,http-password:,test-ddl-password:,test-dml-password:,ro-password:,database:,yes,no-sql-validation,help -n "$(basename "$0")" -- "$@")
if [[ $? -ne 0 ]]; then
    usage
fi

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

eval set -- "$OPTS"

while true; do
    case "$1" in
        -p|--postgres-password)
            POSTGRES_PASSWORD="$2"
            shift 2
            ;;
        -d|--ddl-password)
            DDL_PASSWORD="$2"
            shift 2
            ;;
        -c|--cli-password)
            CLI_PASSWORD="$2"
            shift 2
            ;;
        -w|--wt-password)
            WT_PASSWORD="$2"
            shift 2
            ;;
        -m|--comms-password)
            COMMS_PASSWORD="$2"
            shift 2
            ;;
        -h|--http-password)
            HTTP_PASSWORD="$2"
            shift 2
            ;;
        -t|--test-ddl-password)
            TEST_DDL_PASSWORD="$2"
            shift 2
            ;;
        -T|--test-dml-password)
            TEST_DML_PASSWORD="$2"
            shift 2
            ;;
        -r|--ro-password)
            RO_PASSWORD="$2"
            shift 2
            ;;
        -D|--database)
            DB_NAME="$2"
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

# Source the connection check utility
source "${SCRIPT_DIR}/utility/check_db_connections.sh"

# Check for active connections to ANY ores database
echo "--- Checking for active connections ---"
ORES_DATABASES=$(PGPASSWORD="${POSTGRES_PASSWORD}" psql -h localhost -U postgres -At -c \
    "SELECT datname FROM pg_database WHERE datname LIKE 'ores_%' ORDER BY datname;" 2>/dev/null)

HAS_CONNECTIONS=0
for db in ${ORES_DATABASES}; do
    if ! check_db_connections "${db}" >/dev/null 2>&1; then
        check_db_connections "${db}"
        HAS_CONNECTIONS=1
    fi
done

if [[ "${HAS_CONNECTIONS}" -eq 1 ]]; then
    echo ""
    echo "ERROR: Cannot proceed with recreation - active connections exist."
    echo "Please disconnect all clients before running the nuclear recreate."
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

# Phase 1: Run recreate_database.sql as postgres (teardown + users + create database)
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

# Phase 2: Run setup_schema.sql as ores_ddl_user (schema + data + grants)
echo ""
echo "--- Setting up schema as ores_ddl_user ---"
PGPASSWORD="${DDL_PASSWORD}" psql \
    -h localhost \
    -U ores_ddl_user \
    -d "${DB_NAME}" \
    -v skip_validation="${SKIP_VALIDATION}" \
    -f ./setup_schema.sql

echo ""
echo "=== Database recreation complete ==="
