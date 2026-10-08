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
 * The risk report config publish resolves each artefact's report definition by
 * name for the target party, and it scopes the config to the party's root
 * portfolio, whose books the run expands through that scope. So the report
 * definitions and the organisation data -- business units, portfolios, books --
 * must be published before a configuration that names them.
 *
 * The four ORE configuration documents a risk run reads come from
 * `ore import-run`, not from a DQ dataset, so there is nothing to declare for
 * them here: the publish reads the bindings the import wrote and skips a
 * definition that does not carry all four.
 *
 * This script is idempotent.
 */

DO $$
BEGIN
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.risk_report_configs', 'ore.report_definitions', 'report_definition_source');

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.risk_report_configs', 'testdata.portfolios', 'root_portfolio_source');

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.risk_report_configs', 'testdata.books', 'book_population_source');
END $$;
