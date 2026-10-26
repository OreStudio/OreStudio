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
 * Registers the refdata.equity_underlyings dataset and seeds its DQ artefact table.
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
        'Equity Underlying Reference Data',
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
        'refdata.equity_underlyings',
        'Equity Underlying Reference Data',
        'Equity Underlyings',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Equity Underlyings',
        'The commodity vocabulary a trade population draws from, with each commodity''s quotation unit and currency.',
        'ORESTUDIO',
        'Seed data for the equity underlyings Librarian bundle',
        current_date,
        'Internal Use Only',
        'equity_underlyings'
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
      and code = 'refdata.equity_underlyings'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.equity_underlyings';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_equity_underlyings_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_equity_underlyings_artefact_tbl (
        dataset_id, tenant_id, code, version, name, description,
        instrument_type, resolves_to, currency_code, display_order
    ) values
        (v_dataset_id, v_tenant_id, '.SPX', 0, 'S&P 500 Index',
         'The S&P 500, the index the corpus prices most often.', 'index', null, 'USD', 0),
        (v_dataset_id, v_tenant_id, 'RIC:.SPX', 0, 'S&P 500 Index (RIC)',
         'The same index under its Reuters Instrument Code, as some documents spell it.', 'index', '.SPX', 'USD', 1),
        (v_dataset_id, v_tenant_id, '.STOXX50E', 0, 'Euro Stoxx 50',
         'The Euro Stoxx 50 index of euro area large caps.', 'index', null, 'EUR', 2),
        (v_dataset_id, v_tenant_id, 'RIC:.STOXX50E', 0, 'Euro Stoxx 50 (RIC)',
         'The same index under its Reuters Instrument Code.', 'index', '.STOXX50E', 'EUR', 3),
        (v_dataset_id, v_tenant_id, '.STOXX50', 0, 'Stoxx 50',
         'The Stoxx Europe 50 index.', 'index', null, 'EUR', 4),
        (v_dataset_id, v_tenant_id, '.NDX', 0, 'Nasdaq 100',
         'The Nasdaq 100 index of non-financial US listings.', 'index', null, 'USD', 5),
        (v_dataset_id, v_tenant_id, 'SP5E', 0, 'S&P 500 (alt)',
         'An S&P 500 spelling the corpus states beside the dotted form.', 'index', '.SPX', 'USD', 6),
        (v_dataset_id, v_tenant_id, 'GB0005405286', 0, 'HSBC Holdings',
         'A UK listed share, named in the corpus by its ISIN.', 'share', null, 'GBP', 7),
        (v_dataset_id, v_tenant_id, 'MSFT.O', 0, 'Microsoft Corporation',
         'A US listed share, named in the corpus by its exchange suffixed ticker.', 'share', null, 'USD', 8),
        (v_dataset_id, v_tenant_id, 'AMZN.O', 0, 'Amazon.com Inc',
         'A US listed share, named in the corpus by its exchange suffixed ticker.', 'share', null, 'USD', 9),
        (v_dataset_id, v_tenant_id, 'CR.N', 0, 'Crane Company',
         'A US listed share, named in the corpus by its exchange suffixed ticker.', 'share', null, 'USD', 10),
        (v_dataset_id, v_tenant_id, 'TR20H-EUR-SEK', 0, 'Total Return 20H EUR/SEK',
         'A total return index the corpus states only as a worst of basket constituent.', 'index', null, null, 11),
        (v_dataset_id, v_tenant_id, 'TR20H-USD-CAD', 0, 'Total Return 20H USD/CAD',
         'A total return index the corpus states only as a worst of basket constituent.', 'index', null, null, 12);

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % commodities for dataset: refdata.equity_underlyings', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Equity Underlyings Summary ---'

select 'Total DQ Equity Underlyings' as metric, count(*) as count
from ores_dq_equity_underlyings_artefact_tbl;
