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

-- DDL user for schema migrations
select ores_iam_service_accounts_upsert_fn(
    'ores_ddl_user',
    'ddl@system.ores',
    'System service account for DDL operations and schema migrations'
);

-- CLI user for command-line operations
select ores_iam_service_accounts_upsert_fn(
    'ores_cli_user',
    'cli@system.ores',
    'System service account for CLI operations'
);

-- Wt user for web application
select ores_iam_service_accounts_upsert_fn(
    'ores_wt_user',
    'wt@system.ores',
    'System service account for Wt web application'
);

-- Comms user for binary protocol server
select ores_iam_service_accounts_upsert_fn(
    'ores_comms_user',
    'comms@system.ores',
    'System service account for binary protocol server'
);

-- HTTP user for REST API server
select ores_iam_service_accounts_upsert_fn(
    'ores_http_user',
    'http@system.ores',
    'System service account for HTTP REST API server'
);

-- Test DDL user for test schema operations
select ores_iam_service_accounts_upsert_fn(
    'ores_test_ddl_user',
    'test_ddl@system.ores',
    'System service account for test DDL operations'
);

-- Test DML user for test data operations
select ores_iam_service_accounts_upsert_fn(
    'ores_test_dml_user',
    'test_dml@system.ores',
    'System service account for test DML operations'
);

-- Summary
select 'Service Accounts' as entity, count(*) as count
from ores_iam_accounts_tbl
where account_type != 'user'
  and valid_to = ores_utility_infinity_timestamp_fn();
