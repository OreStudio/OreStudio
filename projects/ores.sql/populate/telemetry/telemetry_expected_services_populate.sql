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
 * Template: sql_service_expected_services_populate.mustache
 *
 * Expected Services
 *
 * The installation's expected services and their replica counts, from the
 * service registry. The services roster reads these rows.
 *
 * This script is idempotent.
 */

\echo '--- Expected Services ---'

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.iam.service', 1, 'Signs people and services in, issues their tokens, and serves tenants, accounts, roles and permissions.',
        :'iam_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.refdata.service', 1, 'Serves reference data: currencies, countries, parties, counterparties, books, portfolios and market conventions.',
        :'refdata_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.workspace.service', 1, 'Serves each person''s workspaces and the layout preferences saved with them.',
        :'workspace_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.dq.service', 1, 'Serves data quality: datasets and their bundles, code domains, badges and severities, and publishes datasets into tenants.',
        :'dq_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.variability.service', 1, 'Serves the typed system settings each tenant and party keeps, with their history.',
        :'variability_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.assets.service', 1, 'Serves images, their tags and the pictures attached to accounts and parties.',
        :'assets_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.scheduler.service', 1, 'Runs scheduled jobs on their cron expressions.',
        :'scheduler_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.reporting.service', 1, 'Serves report definitions and risk report configurations, and tracks each report instance through its lifecycle.',
        :'reporting_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.telemetry.service', 1, 'Stores logs, service heartbeats and message-bus samples, and serves them to the operations screens.',
        :'telemetry_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.trading.service', 1, 'Serves trades, their instruments, lifecycle events and party roles.',
        :'trading_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.compute.service', 1, 'Orchestrates the compute grid: hosts, apps, batches and the work units it hands to the wrapper nodes.',
        :'compute_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.synthetic.service', 1, 'Generates synthetic market data from its generation configs and publishes the ticks.',
        :'synthetic_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.workflow.service', 1, 'Runs multi-step workflows such as tenant provisioning, step by step, with compensation when a step fails.',
        :'workflow_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.ore.service', 1, 'Imports and exports ORE documents and market data, and plans imports into the domain model.',
        :'ore_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.marketdata.service', 1, 'Serves market series, observations, fixings and feed bindings.',
        :'marketdata_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.analytics.service', 1, 'Serves pricing configuration: pricing models, engine types and product parameter mappings.',
        :'analytics_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.http.server', 1, 'Serves the REST API, projecting the services'' operations onto HTTP routes.',
        :'http_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.web.service', 1, 'Serves the web interface and its API, and reaches the services over the message bus on the browser''s behalf.',
        null)
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.storage.service', 1, 'Stores and serves files as objects over the message bus.',
        :'storage_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.inbox.service', 1, 'Serves approval requests and notifications: what waits on a person''s decision, and what a person is told.',
        :'inbox_service_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

insert into ores_telemetry_expected_services_tbl
    (service_name, replicas, description, service_account)
values ('ores.compute.wrapper', 5, 'Runs work units from the compute grid on one node and reports their results and logs.',
        :'compute_wrapper_user')
on conflict (service_name) do update set
    replicas = excluded.replicas,
    description = excluded.description,
    service_account = excluded.service_account;

-- Summary
select 'Expected Services' as entity, count(*) as count
from ores_telemetry_expected_services_tbl;
