#!/usr/bin/env bash

# -*- mode: sh; tab-width: 4; indent-tabs-mode: nil -*-
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

# Seed a deterministic test account for the TypeScript client.
#
# The password hash must come from the project's own hasher, so this script
# calls a small C++ helper linked against libores.security rather than
# reimplementing scrypt here. A hash built any other way would risk a subtly
# different format that the server rejects.
#
# The service under test runs with a tenant context, and row level security
# restricts every read to that tenant. The account is therefore created in an
# explicit tenant and granted that tenant's parties, so the login resolves and
# the party selection path has something to select.
#
# Idempotent: re-running retires the previous row and inserts a fresh one, so
# exactly one current row exists and the password is whatever was passed.
#
# Usage:
#   scripts/seed-test-account.sh [username] [password]
#
# Environment:
#   TENANT_ID      Tenant for the new account (default: the system tenant).
#   AUDIT_ACCOUNT  Real username recorded in the audit columns.
set -euo pipefail

USERNAME="${1:-ores_web_probe}"
PASSWORD="${2:-Secure-Password-123}"
TENANT_ID="${TENANT_ID:-ffffffff-ffff-ffff-ffff-ffffffffffff}"
AUDIT_ACCOUNT="${AUDIT_ACCOUNT:-sysadmin}"
CHANGE_REASON="${CHANGE_REASON:-system.test}"
# The role that makes the account able to write. TenantAdmin has full access
# within its tenant, which is what an account used for verification needs.
ROLE="${ROLE:-TenantAdmin}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$WORKSPACE/../.." && pwd)"
ENV_FILE="$REPO_ROOT/.env"
HELPER="$WORKSPACE/.runtime/build/make-test-hash"

env_value() {
  sed -n "s/^$1=//p" "$ENV_FILE" | head -1
}

PRESET="$(env_value ORES_PRESET)"
PRESET="${PRESET:-linux-clang-debug-make}"
DB_HOST="$(env_value ORES_DB_HOST)"
DB_HOST="${DB_HOST:-localhost}"
DB_DATABASE="$(env_value ORES_DATABASE_NAME)"
# PGPASSWORD belongs to the postgres superuser that `compass db` also uses, so
# the user defaults to postgres unless the environment names another.
DB_USER="$(env_value PGUSER)"
DB_USER="${DB_USER:-postgres}"

if [[ -z "$DB_DATABASE" ]]; then
  echo "no ORES_DATABASE_NAME in $ENV_FILE" >&2
  exit 1
fi

if [[ ! -x "$HELPER" ]]; then
  echo "helper not built: $HELPER (build with the command in the README)" >&2
  exit 1
fi

export PGPASSWORD="$(env_value PGPASSWORD)"

psql_admin() {
  psql -h "$DB_HOST" -U "$DB_USER" -d "$DB_DATABASE" -v ON_ERROR_STOP=1 -qtA "$@"
}

HASH="$(LD_LIBRARY_PATH="$REPO_ROOT/build/output/$PRESET/publish/lib" \
  "$HELPER" "$USERNAME" "$PASSWORD" | cut -f2)"

if [[ -z "$HASH" ]]; then
  echo "helper produced no hash" >&2
  exit 1
fi

# Every mutation is a data-modifying CTE, and the final `select` is what makes
# the engine evaluate them. `target` pins the current row before retirement, so
# the party rows close against the same account id.
psql_admin <<SQL
begin;

with target as (
  select id, tenant_id
    from ores_iam_accounts_tbl
   where username = '${USERNAME}'
     and valid_to = ores_utility_infinity_timestamp_fn()
),
retire_parties as (
  update ores_iam_account_parties_tbl
     set valid_to = now()
   where (account_id, tenant_id) in (select id, tenant_id from target)
     and valid_to = ores_utility_infinity_timestamp_fn()
  returning 1
),
retire_account as (
  update ores_iam_accounts_tbl
     set valid_to = now()
   where (id, tenant_id) in (select id, tenant_id from target)
     and valid_to = ores_utility_infinity_timestamp_fn()
  returning 1
),
inserted_account as (
  insert into ores_iam_accounts_tbl (
    id, tenant_id, version, account_type, username, full_name,
    password_hash, password_salt, totp_secret, email,
    modified_by, change_reason_code, change_commentary, performed_by,
    valid_from, valid_to
  )
  values (
    gen_random_uuid(),
    '${TENANT_ID}',
    0,
    'user',
    '${USERNAME}',
    'ORE Studio Web Test Account',
    '${HASH}',
    '',
    '',
    '${USERNAME}@ores.web.test',
    '${AUDIT_ACCOUNT}',
    '${CHANGE_REASON}',
    'seeded by ores.web seed-test-account.sh',
    '${AUDIT_ACCOUNT}',
    now(),
    ores_utility_infinity_timestamp_fn()
  )
  returning id, tenant_id
),
granted_parties as (
  insert into ores_iam_account_parties_tbl (
    account_id, tenant_id, party_id, version,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
  )
  select
    ia.id, ia.tenant_id, p.id, 0,
    '${AUDIT_ACCOUNT}', '${AUDIT_ACCOUNT}', '${CHANGE_REASON}',
    'seeded by ores.web seed-test-account.sh',
    now(), ores_utility_infinity_timestamp_fn()
  from inserted_account ia
  join ores_refdata_parties_tbl p
    on p.tenant_id = ia.tenant_id
   and p.valid_to = ores_utility_infinity_timestamp_fn()
   and p.status = 'Active'
  returning 1
),
-- The service authorises every write against the account's roles, and an account
-- with none can read but not write. That failure arrives as a server error the
-- client cannot distinguish from an expired token, which is a long detour to
-- take, so the role is granted here.
granted_roles as (
  insert into ores_iam_account_roles_tbl (
    account_id, tenant_id, role_id, assigned_by, assigned_at,
    change_reason_code, change_commentary, valid_from, valid_to
  )
  select
    ia.id, ia.tenant_id, r.id, '${AUDIT_ACCOUNT}', now(),
    '${CHANGE_REASON}', 'seeded by ores.web seed-test-account.sh',
    now(), ores_utility_infinity_timestamp_fn()
  from inserted_account ia
  join ores_iam_roles_tbl r
    on r.tenant_id = ia.tenant_id
   and r.name = '${ROLE}'
   and r.valid_to = ores_utility_infinity_timestamp_fn()
  returning 1
),
-- The login handler refuses an account with no tracking row, and increments
-- failed_logins on it, so every loginable account needs one.
login_tracking as (
  insert into ores_iam_login_info_tbl (
    tenant_id, account_id, last_ip, last_attempt_ip,
    failed_logins, locked, last_login, online, password_reset_required
  )
  select
    ia.tenant_id, ia.id, '0.0.0.0', '0.0.0.0',
    0, 0, now(), 0, 0
  from inserted_account ia
  on conflict (account_id) do update
    set tenant_id = excluded.tenant_id,
        failed_logins = 0,
        locked = 0,
        online = 0,
        password_reset_required = 0
  returning 1
)
select
  (select count(*) from retire_account) as retired,
  (select count(*) from inserted_account) as inserted,
  (select count(*) from granted_parties) as parties,
  (select count(*) from login_tracking) as tracking;

commit;
SQL

echo "seeded ${USERNAME} in tenant ${TENANT_ID} with password ${PASSWORD}"
psql_admin -c "
select a.username || ' tenant=' || a.tenant_id || ' parties=' || count(ap.party_id)
from ores_iam_accounts_tbl a
left join ores_iam_account_parties_tbl ap
  on ap.account_id = a.id and ap.tenant_id = a.tenant_id
 and ap.valid_to = ores_utility_infinity_timestamp_fn()
where a.username = '${USERNAME}'
  and a.valid_to = ores_utility_infinity_timestamp_fn()
group by a.username, a.tenant_id"
