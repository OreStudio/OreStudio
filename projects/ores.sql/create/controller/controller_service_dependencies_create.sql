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
 * Service Dependencies Table
 *
 * Records the startup dependencies between managed services. A row
 * (service_name, depends_on) means service_name must not be launched until
 * depends_on has signalled readiness (i.e. "Service ready" appears in its
 * log file).
 *
 * The process_supervisor reads this table at startup, builds a directed
 * acyclic graph, and starts services in topological order — waiting for each
 * service to become ready before launching services that depend on it.
 *
 * This is intentionally non-bitemporal: dependency relationships are static
 * configuration that does not require a full audit history.
 */

create table if not exists ores_controller_service_dependencies_tbl (
    "service_name" text not null,
    "depends_on"   text not null,
    primary key ("service_name", "depends_on")
);
