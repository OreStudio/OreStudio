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

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.iam.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.refdata.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.workspace.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.dq.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.variability.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.assets.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.scheduler.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.reporting.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.telemetry.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.trading.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.compute.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.synthetic.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.workflow.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.ore.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.marketdata.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.analytics.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.http.server', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.web.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.storage.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.inbox.service', 1)
on conflict (service_name) do update set replicas = excluded.replicas;

insert into ores_telemetry_expected_services_tbl (service_name, replicas)
values ('ores.compute.wrapper', 5)
on conflict (service_name) do update set replicas = excluded.replicas;

-- Summary
select 'Expected Services' as entity, count(*) as count
from ores_telemetry_expected_services_tbl;
