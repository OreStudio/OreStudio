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
# Show all OreStudio roles for an environment, with login capability and group
# memberships. Run before and after recreate_database.sh to verify that roles
# are correctly dropped and recreated.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECKOUT_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
ENV_FILE="${CHECKOUT_ROOT}/.env"
if [[ -f "${ENV_FILE}" ]]; then
    set -o allexport
    # shellcheck source=/dev/null
    source "${ENV_FILE}"
    set +o allexport
fi

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Show all OreStudio roles for a given environment, with login capability and
group memberships. Run before and after recreate_database.sh to verify that
roles are correctly dropped and recreated.

Options:
    -e, --env ENV_LABEL         Environment label (e.g. local1, local2).
                                Defaults to env_label derived from ORES_DB_OWNER_ROLE.
    -H, --help                  Show this help message

Environment Variables:
    PGPASSWORD                  Password for the postgres superuser (required)
    ORES_DB_HOST                PostgreSQL host (required unless set in .env)
    ORES_DB_OWNER_ROLE          Owner role name, used to derive env_label if -e
                                is not provided (e.g. ores_local1_owner)

Example:
    # Derive env_label from .env
    $(basename "$0")

    # Explicit env label
    $(basename "$0") -e local2

EOF
    exit 1
}

ENV_LABEL=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        -e|--env)   ENV_LABEL="$2"; shift 2 ;;
        -H|--help)  usage ;;
        -*)         echo "Error: Unknown option $1" >&2; usage ;;
        *)          echo "Error: Unexpected argument $1" >&2; usage ;;
    esac
done

if [[ -z "${PGPASSWORD:-}" ]]; then
    echo "Error: PGPASSWORD environment variable is required" >&2
    exit 1
fi

if [[ -z "${ORES_DB_HOST:-}" ]]; then
    echo "Error: ORES_DB_HOST environment variable is required" >&2
    exit 1
fi

# Derive env_label from ORES_DB_OWNER_ROLE if not supplied explicitly
if [[ -z "${ENV_LABEL}" ]]; then
    if [[ -z "${ORES_DB_OWNER_ROLE:-}" ]]; then
        echo "Error: supply -e ENV_LABEL or set ORES_DB_OWNER_ROLE in the environment" >&2
        exit 1
    fi
    ENV_LABEL="${ORES_DB_OWNER_ROLE#ores_}"
    ENV_LABEL="${ENV_LABEL%_owner}"
fi

echo "=== OreStudio roles for environment: ${ENV_LABEL} ==="
echo ""

PGPASSWORD="${PGPASSWORD}" psql \
    -h "${ORES_DB_HOST}" -U postgres -At -F $'\t' \
    -v env_label="${ENV_LABEL}" \
    <<'SQL'
select
    r.rolname                                           as role,
    case when r.rolcanlogin then 'login' else 'nologin' end as login,
    coalesce(
        string_agg(g.rolname, ', ' order by g.rolname),
        '(none)'
    )                                                   as member_of
from pg_roles r
left join pg_auth_members m on m.member = r.oid
left join pg_roles g        on g.oid    = m.roleid
where r.rolname like 'ores_' || :'env_label' || '_%'
group by r.rolname, r.rolcanlogin
order by
    r.rolcanlogin,   -- group roles (nologin) first
    r.rolname;
SQL

echo ""
echo "=== Done ==="
