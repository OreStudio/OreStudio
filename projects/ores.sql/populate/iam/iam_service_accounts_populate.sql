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

-- IAM domain service
select ores_iam_service_accounts_upsert_fn(
    'ores_iam_service',
    'iam_service@system.ores',
    'System service account for IAM NATS domain service'
);

-- Reference Data domain service
select ores_iam_service_accounts_upsert_fn(
    'ores_refdata_service',
    'refdata_service@system.ores',
    'System service account for Reference Data NATS domain service'
);

-- Data Quality domain service
select ores_iam_service_accounts_upsert_fn(
    'ores_dq_service',
    'dq_service@system.ores',
    'System service account for Data Quality NATS domain service'
);

-- Variability domain service
select ores_iam_service_accounts_upsert_fn(
    'ores_variability_service',
    'variability_service@system.ores',
    'System service account for Variability NATS domain service'
);

-- Assets domain service
select ores_iam_service_accounts_upsert_fn(
    'ores_assets_service',
    'assets_service@system.ores',
    'System service account for Assets NATS domain service'
);

-- Synthetic domain service
select ores_iam_service_accounts_upsert_fn(
    'ores_synthetic_service',
    'synthetic_service@system.ores',
    'System service account for Synthetic NATS domain service'
);

-- Scheduler domain service
select ores_iam_service_accounts_upsert_fn(
    'ores_scheduler_service',
    'scheduler_service@system.ores',
    'System service account for Scheduler NATS domain service'
);

-- Reporting domain service
select ores_iam_service_accounts_upsert_fn(
    'ores_reporting_service',
    'reporting_service@system.ores',
    'System service account for Reporting NATS domain service'
);

-- Telemetry domain service
select ores_iam_service_accounts_upsert_fn(
    'ores_telemetry_service',
    'telemetry_service@system.ores',
    'System service account for Telemetry NATS domain service'
);

-- Trading domain service
select ores_iam_service_accounts_upsert_fn(
    'ores_trading_service',
    'trading_service@system.ores',
    'System service account for Trading NATS domain service'
);

-- Compute domain service
select ores_iam_service_accounts_upsert_fn(
    'ores_compute_service',
    'compute_service@system.ores',
    'System service account for Compute Grid NATS domain service'
);

-- Summary
select 'Service Accounts' as entity, count(*) as count
from ores_iam_accounts_tbl
where account_type != 'user'
  and valid_to = ores_utility_infinity_timestamp_fn();
