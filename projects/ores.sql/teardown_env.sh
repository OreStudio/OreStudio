#!/bin/bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
# Tears down an environment completely: kills connections, drops the database,
# then removes all cluster-level roles belonging to the environment.
#
# This is the inverse of the full setup sequence (setup_user.sql +
# recreate_database.sh). After running this script, the environment can be
# re-created from scratch without leaving orphaned roles.

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

Tears down an environment completely:
  1. Kills all active connections to the database
  2. Drops the environment database (ores_dev_<environment>)
  3. Removes all cluster-level roles for the environment

Required arguments:
    -e, --env ENVIRONMENT               Environment label (e.g., local1, local2)

Optional arguments:
    -y, --yes                           Skip confirmation prompt
    -H, --help                          Show this help message

Environment Variables:
    PGPASSWORD                          Password for the postgres superuser (required)

Example:
    export PGPASSWORD=myPass
    $(basename "$0") -e local2 -y

EOF
    exit 1
}

ENVIRONMENT=""
ASSUME_YES=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        -e|--env)   ENVIRONMENT="$2"; shift 2 ;;
        -y|--yes)   ASSUME_YES="1";   shift   ;;
        -H|--help)  usage ;;
        --)         shift; break ;;
        *)
            echo "Error: Invalid option $1" >&2
            usage
            ;;
    esac
done

if [[ -z "${ENVIRONMENT}" ]]; then
    echo "Error: Environment (-e) is required" >&2
    usage
fi

if [[ -z "${PGPASSWORD:-}" ]]; then
    echo "Error: PGPASSWORD environment variable is required" >&2
    exit 1
fi

DB_NAME="ores_dev_${ENVIRONMENT}"

echo ""
echo "=========================================="
echo "  ORE Studio Environment Teardown"
echo "=========================================="
echo ""
echo "  Environment : ${ENVIRONMENT}"
echo "  Database    : ${DB_NAME}"
echo "  Roles       : ores_${ENVIRONMENT}_* (all)"
echo ""
echo "=========================================="
echo ""

if [[ -z "${ASSUME_YES}" ]]; then
    echo "WARNING: This will permanently drop the database AND all roles"
    echo "         for environment '${ENVIRONMENT}'!"
    echo ""
    read -p "Type 'yes' to proceed: " confirm
    if [[ "${confirm}" != "yes" ]]; then
        echo "Aborted."
        exit 1
    fi
    echo ""
fi

export PGPASSWORD="${PGPASSWORD}"

# Step 1: Kill all connections to the database
echo "--- Step 1: Killing connections to ${DB_NAME} ---"
PGPASSWORD="${PGPASSWORD}" psql -h localhost -U postgres -At -c \
    "SELECT pg_terminate_backend(pid)
     FROM pg_stat_activity
     WHERE datname = '${DB_NAME}' AND pid <> pg_backend_pid();" \
    > /dev/null 2>&1 || true
echo "Connections terminated."
echo ""

# Step 2: Drop the database
echo "--- Step 2: Dropping database ${DB_NAME} ---"
PGPASSWORD="${PGPASSWORD}" psql -h localhost -U postgres \
    -c "DROP DATABASE IF EXISTS ${DB_NAME};"
echo "Database dropped."
echo ""

# Step 3: Drop all cluster-level roles for this environment
echo "--- Step 3: Dropping roles for environment '${ENVIRONMENT}' ---"
PGPASSWORD="${PGPASSWORD}" psql \
    -h localhost \
    -U postgres \
    --set ON_ERROR_STOP=on \
    -v env_label="${ENVIRONMENT}" \
    -f "${SCRIPT_DIR}/drop_roles.sql"

echo ""
echo "=========================================="
echo "  Teardown complete: ${ENVIRONMENT}"
echo "=========================================="
echo ""
echo "To recreate, run:"
echo "  ./projects/ores.sql/recreate_database.sh -y"
echo ""
