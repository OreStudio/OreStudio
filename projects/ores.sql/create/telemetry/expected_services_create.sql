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
 * Expected Services
 *
 * The services the installation runs and how many instances of each it
 * expects. The rows are generated from the service registry
 * (projects/modeling/service_registry.org) into
 * populate/telemetry/telemetry_expected_services_populate.sql, so the registry
 * stays the one source. The services roster joins these rows with the last
 * heartbeat of each instance, so an expected instance that never reports
 * still has a row.
 *
 * Installation-level, like the heartbeat samples: no tenant owns a service.
 */
create table if not exists ores_telemetry_expected_services_tbl (
    "service_name"  text not null,
    "replicas"      integer not null,
    primary key (service_name),
    constraint expected_services_replicas_chk check (replicas > 0)
);
