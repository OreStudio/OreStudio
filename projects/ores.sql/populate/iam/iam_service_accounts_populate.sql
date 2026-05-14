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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_service_accounts_populate.mustache
 *
 * Service Accounts Population Script
 *
 * Creates system service accounts for non-human processes.
 * Service accounts belong to the system tenant and cannot login with passwords.
 * They authenticate by creating sessions directly at startup.
 *
 * Account names match the environment-scoped database user names
 * (e.g. ores_local2_iam_service).
 *
 * This script is idempotent.
 */

-- Capture service DB passwords from the environment so they are never stored
-- in plaintext in committed SQL. The backtick syntax runs a shell command at
-- psql execution time; the result is bound to the named variable.
\set wt_service_pw          `echo "$ORES_WT_DB_PASSWORD"`
\set http_service_pw        `echo "$ORES_HTTP_SERVER_DB_PASSWORD"`
\set iam_service_pw    `echo "$ORES_IAM_SERVICE_DB_PASSWORD"`
\set refdata_service_pw    `echo "$ORES_REFDATA_SERVICE_DB_PASSWORD"`
\set dq_service_pw    `echo "$ORES_DQ_SERVICE_DB_PASSWORD"`
\set variability_service_pw    `echo "$ORES_VARIABILITY_SERVICE_DB_PASSWORD"`
\set assets_service_pw    `echo "$ORES_ASSETS_SERVICE_DB_PASSWORD"`
\set scheduler_service_pw    `echo "$ORES_SCHEDULER_SERVICE_DB_PASSWORD"`
\set reporting_service_pw    `echo "$ORES_REPORTING_SERVICE_DB_PASSWORD"`
\set telemetry_service_pw    `echo "$ORES_TELEMETRY_SERVICE_DB_PASSWORD"`
\set trading_service_pw    `echo "$ORES_TRADING_SERVICE_DB_PASSWORD"`
\set compute_service_pw    `echo "$ORES_COMPUTE_SERVICE_DB_PASSWORD"`
\set synthetic_service_pw    `echo "$ORES_SYNTHETIC_SERVICE_DB_PASSWORD"`
\set workflow_service_pw    `echo "$ORES_WORKFLOW_SERVICE_DB_PASSWORD"`
\set ore_service_pw    `echo "$ORES_ORE_SERVICE_DB_PASSWORD"`
\set marketdata_service_pw    `echo "$ORES_MARKETDATA_SERVICE_DB_PASSWORD"`
\set controller_service_pw    `echo "$ORES_CONTROLLER_SERVICE_DB_PASSWORD"`
\set analytics_service_pw    `echo "$ORES_ANALYTICS_SERVICE_DB_PASSWORD"`

SET "ores.ddl_user"                 = :'ddl_user';
SET "ores.cli_user"                 = :'cli_user';
SET "ores.wt_user"                  = :'wt_user';
SET "ores.shell_user"               = :'shell_user';
SET "ores.http_user"                = :'http_user';
SET "ores.test_ddl_user"            = :'test_ddl_user';
SET "ores.test_dml_user"            = :'test_dml_user';
SET "ores.iam_service_user"         = :'iam_service_user';
SET "ores.refdata_service_user"     = :'refdata_service_user';
SET "ores.dq_service_user"          = :'dq_service_user';
SET "ores.variability_service_user" = :'variability_service_user';
SET "ores.assets_service_user"      = :'assets_service_user';
SET "ores.scheduler_service_user"   = :'scheduler_service_user';
SET "ores.reporting_service_user"   = :'reporting_service_user';
SET "ores.telemetry_service_user"   = :'telemetry_service_user';
SET "ores.trading_service_user"     = :'trading_service_user';
SET "ores.compute_service_user"     = :'compute_service_user';
SET "ores.synthetic_service_user"   = :'synthetic_service_user';
SET "ores.workflow_service_user"    = :'workflow_service_user';
SET "ores.ore_service_user"         = :'ore_service_user';
SET "ores.marketdata_service_user"  = :'marketdata_service_user';
SET "ores.controller_service_user"  = :'controller_service_user';
SET "ores.analytics_service_user"   = :'analytics_service_user';
SET "ores.wt_service_pw"            = :'wt_service_pw';
SET "ores.http_service_pw"          = :'http_service_pw';
SET "ores.iam_service_pw"           = :'iam_service_pw';
SET "ores.refdata_service_pw"       = :'refdata_service_pw';
SET "ores.dq_service_pw"            = :'dq_service_pw';
SET "ores.variability_service_pw"   = :'variability_service_pw';
SET "ores.assets_service_pw"        = :'assets_service_pw';
SET "ores.scheduler_service_pw"     = :'scheduler_service_pw';
SET "ores.reporting_service_pw"     = :'reporting_service_pw';
SET "ores.telemetry_service_pw"     = :'telemetry_service_pw';
SET "ores.trading_service_pw"       = :'trading_service_pw';
SET "ores.compute_service_pw"       = :'compute_service_pw';
SET "ores.synthetic_service_pw"     = :'synthetic_service_pw';
SET "ores.workflow_service_pw"      = :'workflow_service_pw';
SET "ores.ore_service_pw"           = :'ore_service_pw';
SET "ores.marketdata_service_pw"    = :'marketdata_service_pw';
SET "ores.controller_service_pw"    = :'controller_service_pw';
SET "ores.analytics_service_pw"     = :'analytics_service_pw';

DO $$
BEGIN
    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.ddl_user'),
        'ddl@system.ores',
        'System service account for DDL operations and schema migrations'
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.cli_user'),
        'cli@system.ores',
        'System service account for CLI operations'
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.wt_user'),
        'wt@system.ores',
        'System service account for Wt web application',
        current_setting('ores.wt_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.shell_user'),
        'shell@system.ores',
        'System service account for interactive shell'
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.http_user'),
        'http@system.ores',
        'System service account for HTTP REST API server',
        current_setting('ores.http_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.test_ddl_user'),
        'test_ddl@system.ores',
        'System service account for test DDL operations'
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.test_dml_user'),
        'test_dml@system.ores',
        'System service account for test DML operations'
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.iam_service_user'),
        'iam_service@system.ores',
        'System service account for IAM NATS domain service',
        current_setting('ores.iam_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.refdata_service_user'),
        'refdata_service@system.ores',
        'System service account for Reference Data NATS domain service',
        current_setting('ores.refdata_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.dq_service_user'),
        'dq_service@system.ores',
        'System service account for Data Quality NATS domain service',
        current_setting('ores.dq_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.variability_service_user'),
        'variability_service@system.ores',
        'System service account for Variability NATS domain service',
        current_setting('ores.variability_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.assets_service_user'),
        'assets_service@system.ores',
        'System service account for Assets NATS domain service',
        current_setting('ores.assets_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.scheduler_service_user'),
        'scheduler_service@system.ores',
        'System service account for Scheduler NATS domain service',
        current_setting('ores.scheduler_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.reporting_service_user'),
        'reporting_service@system.ores',
        'System service account for Reporting NATS domain service',
        current_setting('ores.reporting_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.telemetry_service_user'),
        'telemetry_service@system.ores',
        'System service account for Telemetry NATS domain service',
        current_setting('ores.telemetry_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.trading_service_user'),
        'trading_service@system.ores',
        'System service account for Trading NATS domain service',
        current_setting('ores.trading_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.compute_service_user'),
        'compute_service@system.ores',
        'System service account for Compute Grid NATS domain service',
        current_setting('ores.compute_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.synthetic_service_user'),
        'synthetic_service@system.ores',
        'System service account for Synthetic NATS domain service',
        current_setting('ores.synthetic_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.workflow_service_user'),
        'workflow_service@system.ores',
        'System service account for Workflow Orchestration NATS domain service',
        current_setting('ores.workflow_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.ore_service_user'),
        'ore_service@system.ores',
        'System service account for ORE Import NATS domain service',
        current_setting('ores.ore_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.marketdata_service_user'),
        'marketdata_service@system.ores',
        'System service account for Market Data NATS domain service',
        current_setting('ores.marketdata_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.controller_service_user'),
        'controller_service@system.ores',
        'System service account for Service Controller NATS domain service',
        current_setting('ores.controller_service_pw')
    );

    PERFORM ores_iam_service_accounts_upsert_fn(
        current_setting('ores.analytics_service_user'),
        'analytics_service@system.ores',
        'System service account for Analytics NATS domain service',
        current_setting('ores.analytics_service_pw')
    );
END $$;


-- Summary
select 'Service Accounts' as entity, count(*) as count
from ores_iam_accounts_tbl
where account_type != 'user'
  and valid_to = ores_utility_infinity_timestamp_fn();
