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
 * Currency Groups Seed Population Script
 *
 * Registers the refdata.currency_groups dataset and seeds its DQ artefact
 * table with the desk groupings a tenant needs: G11, Scandies,
 * Antipodeans, commodity currencies, Asians and Latams. A tenant gets its
 * own copy when its bundle publishes the dataset. The system tenant keeps
 * no live rows of its own, so this dataset is the only source.
 *
 * Which currencies belong to each group is a separate dataset,
 * refdata.currency_currency_groups.
 *
 * Execution order: no dependency on other populate scripts.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Catalog Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_catalogs_upsert_fn(ores_utility_system_tenant_id_fn(),
        'FX Market Conventions',
        'Curated FX market reference data: standard currency pairs and their conventions.',
        'OreStudio Development Team'
    );
END $$;

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_groups',
        'FX Market Conventions',
        'Currencies',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Currency Groups',
        'Desk groupings of currencies: G11, Scandies, Antipodeans, commodity currencies, Asians and Latams.',
        'ORESTUDIO',
        'Seed data for the currency groups Librarian bundle',
        current_date,
        'Internal Use Only',
        'currency_groups'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

DO $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_count integer := 0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.currency_groups'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.currency_groups';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_currency_groups_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_currency_groups_artefact_tbl (
        dataset_id, tenant_id, code, version, name, description, display_order
    )
    values
        (v_dataset_id, v_tenant_id, 'G11', 0, 'G11',
         'The eleven liquid majors that desks allocate by: EUR, USD, GBP, JPY, AUD, CAD, CHF, DKK, NOK, NZD and SEK', 1),
        (v_dataset_id, v_tenant_id, 'SCANDIES', 0, 'Scandies',
         'The Scandinavian currencies that desks often trade together: DKK, NOK and SEK', 2),
        (v_dataset_id, v_tenant_id, 'ANTIPODEANS', 0, 'Antipodeans',
         'The correlated Pacific currencies: AUD and NZD', 3),
        (v_dataset_id, v_tenant_id, 'COMMODITY', 0, 'Commodity currencies',
         'Currencies whose value follows a commodity export such as oil, metals or dairy', 4),
        (v_dataset_id, v_tenant_id, 'ASIANS', 0, 'Asians',
         'The Asian currencies that desks group by region, not by liquidity', 5),
        (v_dataset_id, v_tenant_id, 'LATAMS', 0, 'Latams',
         'The Latin American currencies that often share a desk and a price verification process', 6);

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % currency groups for dataset: refdata.currency_groups', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Currency Groups Summary ---'

select 'Total DQ Currency Groups' as metric, count(*) as count
from ores_dq_currency_groups_artefact_tbl;
