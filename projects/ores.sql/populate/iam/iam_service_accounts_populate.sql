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
 * Account names match the environment-scoped database user names from
 * setup_user.sql (e.g. ores_local2_iam_service).
 *
 * Required psql variables (set by setup_database.sh / recreate_database.sh):
 *   :ddl_user, :cli_user, :wt_user, :shell_user, :http_user,
 *   :test_ddl_user, :test_dml_user,
 *   :iam_service_user, :refdata_service_user, :dq_service_user,
 *   :variability_service_user, :assets_service_user,
 *   :synthetic_service_user, :scheduler_service_user,
 *   :reporting_service_user, :telemetry_service_user,
 *   :trading_service_user, :compute_service_user
 *
 * This script is idempotent.
 */

\echo '--- Service Accounts ---'

-- Capture service DB passwords from the environment so they are never stored
-- in plaintext in committed SQL. The backtick syntax runs a shell command at
-- psql execution time; the result is bound to the named variable.
\set iam_service_pw         `echo "$ORES_IAM_SERVICE_DB_PASSWORD"`
\set refdata_service_pw     `echo "$ORES_REFDATA_SERVICE_DB_PASSWORD"`
\set dq_service_pw          `echo "$ORES_DQ_SERVICE_DB_PASSWORD"`
\set variability_service_pw `echo "$ORES_VARIABILITY_SERVICE_DB_PASSWORD"`
\set assets_service_pw      `echo "$ORES_ASSETS_SERVICE_DB_PASSWORD"`
\set synthetic_service_pw   `echo "$ORES_SYNTHETIC_SERVICE_DB_PASSWORD"`
\set scheduler_service_pw   `echo "$ORES_SCHEDULER_SERVICE_DB_PASSWORD"`
\set reporting_service_pw   `echo "$ORES_REPORTING_SERVICE_DB_PASSWORD"`
\set telemetry_service_pw   `echo "$ORES_TELEMETRY_SERVICE_DB_PASSWORD"`
\set trading_service_pw     `echo "$ORES_TRADING_SERVICE_DB_PASSWORD"`
\set compute_service_pw     `echo "$ORES_COMPUTE_SERVICE_DB_PASSWORD"`

select ores_iam_service_accounts_upsert_fn(
    :'ddl_user',
    'ddl@system.ores',
    'System service account for DDL operations and schema migrations'
);

select ores_iam_service_accounts_upsert_fn(
    :'cli_user',
    'cli@system.ores',
    'System service account for CLI operations'
);

select ores_iam_service_accounts_upsert_fn(
    :'wt_user',
    'wt@system.ores',
    'System service account for Wt web application'
);

select ores_iam_service_accounts_upsert_fn(
    :'shell_user',
    'shell@system.ores',
    'System service account for interactive shell'
);

select ores_iam_service_accounts_upsert_fn(
    :'http_user',
    'http@system.ores',
    'System service account for HTTP REST API server'
);

select ores_iam_service_accounts_upsert_fn(
    :'test_ddl_user',
    'test_ddl@system.ores',
    'System service account for test DDL operations'
);

select ores_iam_service_accounts_upsert_fn(
    :'test_dml_user',
    'test_dml@system.ores',
    'System service account for test DML operations'
);

select ores_iam_service_accounts_upsert_fn(
    :'iam_service_user',
    'iam_service@system.ores',
    'System service account for IAM NATS domain service',
    :'iam_service_pw'
);

select ores_iam_service_accounts_upsert_fn(
    :'refdata_service_user',
    'refdata_service@system.ores',
    'System service account for Reference Data NATS domain service',
    :'refdata_service_pw'
);

select ores_iam_service_accounts_upsert_fn(
    :'dq_service_user',
    'dq_service@system.ores',
    'System service account for Data Quality NATS domain service',
    :'dq_service_pw'
);

select ores_iam_service_accounts_upsert_fn(
    :'variability_service_user',
    'variability_service@system.ores',
    'System service account for Variability NATS domain service',
    :'variability_service_pw'
);

select ores_iam_service_accounts_upsert_fn(
    :'assets_service_user',
    'assets_service@system.ores',
    'System service account for Assets NATS domain service',
    :'assets_service_pw'
);

select ores_iam_service_accounts_upsert_fn(
    :'synthetic_service_user',
    'synthetic_service@system.ores',
    'System service account for Synthetic NATS domain service',
    :'synthetic_service_pw'
);

select ores_iam_service_accounts_upsert_fn(
    :'scheduler_service_user',
    'scheduler_service@system.ores',
    'System service account for Scheduler NATS domain service',
    :'scheduler_service_pw'
);

select ores_iam_service_accounts_upsert_fn(
    :'reporting_service_user',
    'reporting_service@system.ores',
    'System service account for Reporting NATS domain service',
    :'reporting_service_pw'
);

select ores_iam_service_accounts_upsert_fn(
    :'telemetry_service_user',
    'telemetry_service@system.ores',
    'System service account for Telemetry NATS domain service',
    :'telemetry_service_pw'
);

select ores_iam_service_accounts_upsert_fn(
    :'trading_service_user',
    'trading_service@system.ores',
    'System service account for Trading NATS domain service',
    :'trading_service_pw'
);

select ores_iam_service_accounts_upsert_fn(
    :'compute_service_user',
    'compute_service@system.ores',
    'System service account for Compute Grid NATS domain service',
    :'compute_service_pw'
);

-- Summary
select 'Service Accounts' as entity, count(*) as count
from ores_iam_accounts_tbl
where account_type != 'user'
  and valid_to = ores_utility_infinity_timestamp_fn();
