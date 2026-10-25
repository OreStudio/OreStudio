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
 * Commodities Seed Population Script
 *
 * Registers the refdata.commodities dataset and seeds its DQ artefact table.
 * Unlike the calendar types there is no generated populate script to keep in
 * lockstep with: the catalogue is published from this dataset rather than
 * loaded straight into the table, which is how the currencies and countries
 * arrive.
 *
 * The vocabulary is what the engine resolves, not what reads well. ORE's own
 * index corpus carries 25 commodity futures indices over five underlyings,
 * and the shipped example trades add two more:
 *
 *   ICE:B               external/ore/catalogue/index_corpus.jsonl
 *   NYMEX:CL            index_corpus.jsonl, and the example trades
 *   NYMEX:CSX           index_corpus.jsonl
 *   NYMEX:NG            index_corpus.jsonl, and the example trades
 *   XLME:AH             external/ore/examples/Products/Example_Trades
 *   PM:XAUUSD           external/ore/examples/Products/Example_Trades
 *
 * NYMEX:A7Q is the corpus's fifth underlying and is deliberately absent. The
 * corpus names it by code only and states no unit, so seeding it would mean
 * inventing one. It is added when its contract is confirmed.
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
        'Commodity Reference Data',
        'The commodities a trade may name, as the engine spells them, with the unit and the currency each is quoted in.',
        'OreStudio Development Team'
    );
END $$;

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.commodities',
        'Commodity Reference Data',
        'Commodities',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Commodities',
        'The commodity vocabulary a trade population draws from, with each commodity''s quotation unit and currency.',
        'ORESTUDIO',
        'Seed data for the commodities Librarian bundle',
        current_date,
        'Internal Use Only',
        'commodities'
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
      and code = 'refdata.commodities'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.commodities';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_commodities_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_commodities_artefact_tbl (
        dataset_id, tenant_id, code, version, name, description,
        unit_code, quotation_currency_code, display_order
    )
    values
        (v_dataset_id, v_tenant_id, 'ICE:B', 0, 'Brent Crude Oil',
         'North Sea Brent crude, the ICE benchmark crude contract.', 'bbl', 'USD', 1),
        (v_dataset_id, v_tenant_id, 'NYMEX:CL', 0, 'WTI Crude Oil',
         'Light sweet crude oil, the NYMEX benchmark crude contract.', 'bbl', 'USD', 2),
        (v_dataset_id, v_tenant_id, 'NYMEX:NG', 0, 'Henry Hub Natural Gas',
         'Natural gas delivered at Henry Hub, the NYMEX benchmark gas contract.', 'MMBtu', 'USD', 3),
        (v_dataset_id, v_tenant_id, 'NYMEX:CSX', 0, 'NYMEX CSX Coal',
         'The coal futures contract the engine carries as NYMEX:CSX.', 'short ton', 'USD', 4),
        (v_dataset_id, v_tenant_id, 'XLME:AH', 0, 'LME Aluminium',
         'Primary aluminium on the London Metal Exchange.', 't', 'USD', 5),
        (v_dataset_id, v_tenant_id, 'PM:XAUUSD', 0, 'Gold Spot',
         'Spot gold against the dollar.', 'oz', 'USD', 6);

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % commodities for dataset: refdata.commodities', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Commodities Summary ---'

select 'Total DQ Commodities' as metric, count(*) as count
from ores_dq_commodities_artefact_tbl;
