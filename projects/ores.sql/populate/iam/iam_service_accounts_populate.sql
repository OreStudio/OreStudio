/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * Service Accounts Population Script
 *
 * Creates system service accounts for non-human processes.
 * Service accounts belong to the system tenant and cannot login with passwords.
 * They authenticate by creating sessions directly at startup.
 *
 * Account names match database user names from setup_user.sql for consistency.
 *
 * This script is idempotent.
 */

\echo '--- Service Accounts ---'

-- Service account for DDL operations (schema migrations)
insert into ores_iam_accounts_tbl (
    id, tenant_id, version, account_type, username, password_hash, password_salt,
    totp_secret, email, modified_by, change_reason_code, change_commentary
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    0,
    'service',
    'ores_ddl_user',
    null,
    '',
    '',
    'ddl@system.ores',
    current_user,
    'system.initial_load',
    'System service account for DDL operations and schema migrations'
where not exists (
    select 1 from ores_iam_accounts_tbl
    where username = 'ores_ddl_user'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Service account for CLI operations
insert into ores_iam_accounts_tbl (
    id, tenant_id, version, account_type, username, password_hash, password_salt,
    totp_secret, email, modified_by, change_reason_code, change_commentary
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    0,
    'service',
    'ores_cli_user',
    null,
    '',
    '',
    'cli@system.ores',
    current_user,
    'system.initial_load',
    'System service account for CLI operations'
where not exists (
    select 1 from ores_iam_accounts_tbl
    where username = 'ores_cli_user'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Service account for Wt web application
insert into ores_iam_accounts_tbl (
    id, tenant_id, version, account_type, username, password_hash, password_salt,
    totp_secret, email, modified_by, change_reason_code, change_commentary
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    0,
    'service',
    'ores_wt_user',
    null,
    '',
    '',
    'wt@system.ores',
    current_user,
    'system.initial_load',
    'System service account for Wt web application'
where not exists (
    select 1 from ores_iam_accounts_tbl
    where username = 'ores_wt_user'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Service account for binary protocol server (comms)
insert into ores_iam_accounts_tbl (
    id, tenant_id, version, account_type, username, password_hash, password_salt,
    totp_secret, email, modified_by, change_reason_code, change_commentary
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    0,
    'service',
    'ores_comms_user',
    null,
    '',
    '',
    'comms@system.ores',
    current_user,
    'system.initial_load',
    'System service account for binary protocol server'
where not exists (
    select 1 from ores_iam_accounts_tbl
    where username = 'ores_comms_user'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Service account for HTTP REST API server
insert into ores_iam_accounts_tbl (
    id, tenant_id, version, account_type, username, password_hash, password_salt,
    totp_secret, email, modified_by, change_reason_code, change_commentary
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    0,
    'service',
    'ores_http_user',
    null,
    '',
    '',
    'http@system.ores',
    current_user,
    'system.initial_load',
    'System service account for HTTP REST API server'
where not exists (
    select 1 from ores_iam_accounts_tbl
    where username = 'ores_http_user'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Service account for test DDL operations
insert into ores_iam_accounts_tbl (
    id, tenant_id, version, account_type, username, password_hash, password_salt,
    totp_secret, email, modified_by, change_reason_code, change_commentary
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    0,
    'service',
    'ores_test_ddl_user',
    null,
    '',
    '',
    'test_ddl@system.ores',
    current_user,
    'system.initial_load',
    'System service account for test DDL operations'
where not exists (
    select 1 from ores_iam_accounts_tbl
    where username = 'ores_test_ddl_user'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Service account for test DML operations
insert into ores_iam_accounts_tbl (
    id, tenant_id, version, account_type, username, password_hash, password_salt,
    totp_secret, email, modified_by, change_reason_code, change_commentary
)
select
    gen_random_uuid(),
    ores_iam_system_tenant_id_fn(),
    0,
    'service',
    'ores_test_dml_user',
    null,
    '',
    '',
    'test_dml@system.ores',
    current_user,
    'system.initial_load',
    'System service account for test DML operations'
where not exists (
    select 1 from ores_iam_accounts_tbl
    where username = 'ores_test_dml_user'
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- Summary
select 'Service Accounts' as entity, count(*) as count
from ores_iam_accounts_tbl
where account_type != 'user'
  and valid_to = ores_utility_infinity_timestamp_fn();
