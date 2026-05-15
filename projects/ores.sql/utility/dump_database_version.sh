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
# Print the schema version and creation metadata for the current database.
# Reads credentials from .env in the repository root.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
RUN_SQL="${SCRIPT_DIR}/../run_sql.sh"
ENV_FILE="${REPO_ROOT}/.env"

if [[ -f "${ENV_FILE}" ]]; then
    set -o allexport
    # shellcheck source=/dev/null
    source "${ENV_FILE}"
    set +o allexport
fi

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Print the schema version and creation metadata stored in ores_database_info_tbl.

Options:
    -H, --help    Show this help message

Environment Variables:
    PGPASSWORD               Password for the postgres superuser (required)
    ORES_TEST_DB_HOST        PostgreSQL host (required unless set in .env)
    ORES_TEST_DB_DATABASE    Database name (required unless set in .env)

Example:
    $(basename "$0")
EOF
    exit 1
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        -H|--help)  usage ;;
        -*)         echo "Error: Unknown option $1" >&2; usage ;;
        *)          echo "Error: Unexpected argument $1" >&2; usage ;;
    esac
done

if [[ -z "${PGPASSWORD:-}" ]]; then
    echo "Error: PGPASSWORD environment variable is required" >&2
    exit 1
fi

echo "=== OreStudio Database Version ==="
echo ""

"${RUN_SQL}" -c "
select
    'Schema version  : ' || schema_version    as info
from ores_database_info_fn()
union all
select 'Build environment: ' || build_environment
from ores_database_info_fn()
union all
select 'Git commit       : ' || git_commit
from ores_database_info_fn()
union all
select 'Git date         : ' || git_date
from ores_database_info_fn()
union all
select 'Created at       : ' || to_char(created_at, 'YYYY-MM-DD HH24:MI:SS TZ')
from ores_database_info_fn();
"

echo ""
echo "=== Done ==="
