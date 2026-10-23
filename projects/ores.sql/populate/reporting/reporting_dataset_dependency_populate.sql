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
 * Reporting Component Dataset Dependencies
 *
 * The generic report definitions are published after the instrument conventions
 * they resolve, and the ore parameters after their value domains. The risk
 * report configurations are Acme's own datasets, and their dependencies are
 * declared with them.
 *
 * This script is idempotent.
 */

DO $$
BEGIN
    -- The canonical instrument conventions are party-scoped reference data a
    -- run resolves by id. The definitions run in party_reporting, after
    -- party_essentials, so the dependency is declared here even though the
    -- bundle display_order already puts them in the right order.
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.report_definitions', 'ore.conventions', 'convention_source');

    -- A parameter names the value domain of its value, and the insert checks
    -- the domain, so the domains publish first.
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.parameters', 'ore.parameter_value_domains', 'value_domains');
END $$;
