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

-- =============================================================================
-- ORE Sample Portfolios Dataset
--
-- The portfolios the ORE sample documents report their trades in: the names
-- every Envelope/PortfolioIds under external/ore/examples states, PF1 and PF2
-- (the inventory is recorded on the task that added this script). An ORE import
-- resolves each PortfolioId to a portfolio of the importing party by name, so
-- the party needs a portfolio of each name. The ore_samples bundle publishes
-- them as root portfolios of a sample sandbox it opens for the party, so no
-- sample portfolio sits in the party's official portfolio tree.
-- =============================================================================

\echo '--- ORE Sample Portfolios Methodology ---'

do $$
begin
    perform ores_dq_methodologies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ORE Sample Portfolio Inventory',
        'ORE sample data: portfolios named by the PortfolioIds of the ORE sample documents, inventoried from the samples'' trade envelopes',
        'https://github.com/OpenSourceRisk/Engine/tree/master/Examples',
        'Data Sourcing and Generation Steps:

    1. INVENTORY THE PORTFOLIO IDS
       Source: the trade envelopes of every portfolio under
       external/ore/examples, the ORE examples vendored at the engine commit
       recorded in external/ore/examples/manifest.json.
       Every distinct Envelope/PortfolioIds/PortfolioId is taken: PF1 and PF2.

    2. SYNTHESISE THE PORTFOLIOS
       One risk portfolio per name, active and not virtual, with no owner
       unit and no aggregation currency: ORE uses the names only to group
       trades in its reports. The names are ORE''s own and are kept as they
       are, because an import resolves them by name.

    3. PUBLISH
       The ore_samples bundle publishes the dataset to the party named by the
       publish parameters. The publish opens a shared sample sandbox for the
       party, anchored at its official top portfolio and owned by the
       publishing actor, and writes each name as a root portfolio of that
       sandbox. A name the sandbox holds already is skipped.'
    );
end $$;

\echo '--- ORE Sample Portfolios Dataset ---'

do $$
declare
    v_dataset_id uuid;
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.sample_portfolios',
        'ORE',
        'Organisation',
        'Reference Data',
        'NONE',
        'Derived',
        'Synthetic',
        'Raw',
        'ORE Sample Portfolio Inventory',
        'ORE Sample Portfolios',
        'ORE sample data: the portfolios the ORE sample documents report their trades in.',
        'ORE',
        'Portfolios for importing the ORE samples',
        '2026-10-04'::date,
        'Modified BSD License',
        'sandbox_portfolios'
    );

    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'ore.sample_portfolios'
      and valid_to = ores_utility_infinity_timestamp_fn();

    delete from ores_dq_portfolios_artefact_tbl where dataset_id = v_dataset_id;

    insert into ores_dq_portfolios_artefact_tbl (
        dataset_id, tenant_id, id, version, name, purpose_type, is_virtual
    )
    select v_dataset_id, ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0, p.name,
        'Risk', false
    from (values ('PF1'), ('PF2')) as p(name);
end $$;
