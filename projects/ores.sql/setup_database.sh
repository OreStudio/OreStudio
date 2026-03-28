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
# Creates a fresh ORES database from scratch:
#   Phase 1 - Create DB, extensions, and schema grants (postgres superuser)
#   Phase 2 - Create tables, populate seed data, and grant permissions (DDL user)
#   Phase 3 - Insert database metadata row (schema version, git info)
#
# Called by recreate_database.sh and recreate_env.sh. Not intended to be run
# standalone (assumes the database does not exist yet and that roles/users are
# already in place).
#
# Usage:
#   ./setup_database.sh <db_name> [--skip-validation]
#
# Environment variables (read from .env or exported by caller):
#   PGPASSWORD              Password for the postgres superuser (must be exported)
#   ORES_DB_OWNER_ROLE      Owner group role name (env-prefixed)
#   ORES_DB_RW_ROLE         Read-write group role name (env-prefixed)
#   ORES_DB_RO_ROLE         Read-only group role name (env-prefixed)
#   ORES_DB_DDL_USER        DDL database user name (env-prefixed)
#   ORES_DB_DDL_PASSWORD    Password for the DDL database user
#   ORES_BUILD_ENVIRONMENT  Build environment label (default: local)
#   (plus all service user names — see init-environment.sh)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECKOUT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Source .env if present (provides role names and passwords)
ENV_FILE="${CHECKOUT_ROOT}/.env"
if [[ -f "${ENV_FILE}" ]]; then
    set -o allexport
    # shellcheck source=/dev/null
    source "${ENV_FILE}"
    set +o allexport
fi

DB_NAME="$1"
SKIP_VALIDATION="off"

if [[ -z "${DB_NAME}" ]]; then
    echo "Error: database name required as first argument" >&2
    exit 1
fi

for arg in "${@:2}"; do
    case "${arg}" in
        --skip-validation) SKIP_VALIDATION="on" ;;
        *) echo "Error: unknown argument: ${arg}" >&2; exit 1 ;;
    esac
done

DDL_USER="${ORES_DB_DDL_USER:-}"
if [[ -z "${DDL_USER}" ]]; then
    echo "Error: ORES_DB_DDL_USER must be set" >&2
    exit 1
fi

DDL_PASSWORD="${ORES_DB_DDL_PASSWORD:-}"
if [[ -z "${DDL_PASSWORD}" ]]; then
    echo "Error: ORES_DB_DDL_PASSWORD must be set" >&2
    exit 1
fi

# Role names (required for GRANT statements in setup_schema.sql)
OWNER_ROLE="${ORES_DB_OWNER_ROLE:-}"
RW_ROLE="${ORES_DB_RW_ROLE:-}"
RO_ROLE="${ORES_DB_RO_ROLE:-}"
SERVICE_ROLE="${ORES_DB_SERVICE_ROLE:-}"
for var_name in OWNER_ROLE RW_ROLE RO_ROLE SERVICE_ROLE; do
    if [[ -z "${!var_name}" ]]; then
        echo "Error: ORES_DB_${var_name} must be set" >&2
        exit 1
    fi
done

# Phase 1: Create database, extensions, and schema grants (postgres superuser)
echo "--- Phase 1: Creating database ${DB_NAME} ---"
psql \
    -h localhost \
    -U postgres \
    --set ON_ERROR_STOP=on \
    -v db_name="${DB_NAME}" \
    -v owner_role="${OWNER_ROLE}" \
    -v rw_role="${RW_ROLE}" \
    -v ro_role="${RO_ROLE}" \
    -v service_role="${SERVICE_ROLE}" \
    -v ddl_user="${DDL_USER}" \
    -f "${SCRIPT_DIR}/create_database.sql"

# Phase 2: Create tables, populate seed data, grant permissions (DDL user)
echo "--- Phase 2: Setting up schema ---"
PGPASSWORD="${DDL_PASSWORD}" psql \
    -h localhost \
    -U "${DDL_USER}" \
    -d "${DB_NAME}" \
    --set ON_ERROR_STOP=on \
    -v skip_validation="${SKIP_VALIDATION}" \
    -v owner_role="${OWNER_ROLE}" \
    -v rw_role="${RW_ROLE}" \
    -v ro_role="${RO_ROLE}" \
    -v ddl_user="${DDL_USER}" \
    -v cli_user="${ORES_DB_CLI_USER:-}" \
    -v wt_user="${ORES_DB_WT_USER:-}" \
    -v comms_user="${ORES_DB_COMMS_USER:-}" \
    -v http_user="${ORES_DB_HTTP_USER:-}" \
    -v test_ddl_user="${ORES_TEST_DB_DDL_USER:-}" \
    -v test_dml_user="${ORES_TEST_DB_USER:-}" \
    -v iam_service_user="${ORES_IAM_SERVICE_DB_USER:-}" \
    -v refdata_service_user="${ORES_REFDATA_SERVICE_DB_USER:-}" \
    -v dq_service_user="${ORES_DQ_SERVICE_DB_USER:-}" \
    -v variability_service_user="${ORES_VARIABILITY_SERVICE_DB_USER:-}" \
    -v assets_service_user="${ORES_ASSETS_SERVICE_DB_USER:-}" \
    -v synthetic_service_user="${ORES_SYNTHETIC_SERVICE_DB_USER:-}" \
    -v scheduler_service_user="${ORES_SCHEDULER_SERVICE_DB_USER:-}" \
    -v reporting_service_user="${ORES_REPORTING_SERVICE_DB_USER:-}" \
    -v telemetry_service_user="${ORES_TELEMETRY_SERVICE_DB_USER:-}" \
    -v trading_service_user="${ORES_TRADING_SERVICE_DB_USER:-}" \
    -v compute_service_user="${ORES_COMPUTE_SERVICE_DB_USER:-}" \
    -f "${SCRIPT_DIR}/setup_schema.sql"

# Phase 3: Insert database metadata (schema version, build info, git info)
echo "--- Phase 3: Populating database metadata ---"

SCHEMA_VERSION=$(grep -oP 'project\(OreStudio VERSION \K[0-9]+\.[0-9]+\.[0-9]+' \
    "${SCRIPT_DIR}/../../CMakeLists.txt" 2>/dev/null || echo "0.0.0")
BUILD_ENVIRONMENT="${ORES_BUILD_ENVIRONMENT:-local}"
GIT_COMMIT=$(git -C "${SCRIPT_DIR}" rev-parse --short HEAD 2>/dev/null || echo "unknown")
GIT_STATUS=$(git -C "${SCRIPT_DIR}" status --porcelain 2>/dev/null || echo "")
if [[ -n "${GIT_STATUS}" ]]; then
    GIT_COMMIT="${GIT_COMMIT}-dirty"
fi
GIT_DATE=$(git -C "${SCRIPT_DIR}" log -1 --format='%ad' \
    --date='format:%Y/%m/%d %H:%M:%S' HEAD 2>/dev/null || echo "unknown")

echo "  Schema version:    ${SCHEMA_VERSION}"
echo "  Build environment: ${BUILD_ENVIRONMENT}"
echo "  Git commit:        ${GIT_COMMIT}"
echo "  Git date:          ${GIT_DATE}"

psql \
    -h localhost \
    -U postgres \
    -d "${DB_NAME}" \
    --set ON_ERROR_STOP=on \
    -c "INSERT INTO ores_database_info_tbl
            (id, schema_version, build_environment, git_commit, git_date)
        VALUES
            (gen_random_uuid(), '${SCHEMA_VERSION}', '${BUILD_ENVIRONMENT}',
             '${GIT_COMMIT}', '${GIT_DATE}');"

echo ""
echo "Database ${DB_NAME} is ready."
