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
# Environment variables:
#   PGPASSWORD              Password for the postgres superuser (must be exported)
#   ORES_DB_DDL_PASSWORD    Password for the DDL database user
#   ORES_BUILD_ENVIRONMENT  Build environment label (default: local)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

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

DDL_PASSWORD="${ORES_DB_DDL_PASSWORD:-}"
if [[ -z "${DDL_PASSWORD}" ]]; then
    echo "Error: ORES_DB_DDL_PASSWORD must be set" >&2
    exit 1
fi

# Phase 1: Create database, extensions, and schema grants (postgres superuser)
echo "--- Phase 1: Creating database ${DB_NAME} ---"
psql \
    -h localhost \
    -U postgres \
    --set ON_ERROR_STOP=on \
    -v db_name="${DB_NAME}" \
    -f "${SCRIPT_DIR}/create_database.sql"

# Phase 2: Create tables, populate seed data, grant permissions (DDL user)
echo "--- Phase 2: Setting up schema ---"
PGPASSWORD="${DDL_PASSWORD}" psql \
    -h localhost \
    -U ores_ddl_user \
    -d "${DB_NAME}" \
    --set ON_ERROR_STOP=on \
    -v skip_validation="${SKIP_VALIDATION}" \
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
