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
 * Test Data Books Artefact Population Script
 *
 * Populates 20 trading books as leaf nodes of the portfolio tree.
 * Each book belongs to exactly one leaf portfolio.
 *
 * Portfolio -> Books mapping:
 *   GBP Rates       -> GBP Vanilla Swaps, GBP Rates Options
 *   EUR Rates       -> EUR Vanilla Swaps, EUR Exotic Rates
 *   IG Credit EMEA  -> IG CDS EMEA, IG Bonds EMEA
 *   G10 FX EMEA     -> G10 FX Spot Forward, G10 FX Options
 *   USD Rates       -> USD Vanilla Swaps, USD Rates Options
 *   CAD Rates       -> CAD Swaps
 *   IG Credit Amer  -> IG CDS Americas, IG Bonds Americas
 *   JPY Rates       -> JPY Vanilla Swaps, JPY Rates Options
 *   FX APAC         -> APAC FX Spot Forward, APAC FX NDF
 *   Regulatory Cap  -> Banking Book, Trading Book, CVA Book
 *
 * This script is idempotent.
 */

DO $$
declare
    v_dataset_id uuid;
    v_party_id uuid;
    v_count integer := 0;
begin
    -- Get the books dataset ID
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'testdata.books'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: testdata.books. Run dataset population first.';
    end if;

    -- Get the system party ID (legal entity for all test books)
    select id into v_party_id
    from ores_refdata_parties_tbl
    where short_code = 'system_party'
      and valid_to = ores_utility_infinity_timestamp_fn()
    limit 1;

    if v_party_id is null then
        raise exception 'System party not found. Run foundation population first.';
    end if;

    -- Clear existing data (idempotency)
    delete from ores_dq_books_artefact_tbl
    where dataset_id = v_dataset_id;

    raise notice 'Populating books for dataset: testdata.books';

    insert into ores_dq_books_artefact_tbl (
        dataset_id, tenant_id, id, version, legal_entity_id, name,
        parent_portfolio_id, ledger_ccy, gl_account_ref, cost_center,
        book_status, is_trading_book
    )
    values
        -- GBP Rates portfolio (20..004) books
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000001', 0, v_party_id,
         'GBP Vanilla Swaps',
         '20000000-0000-4000-a000-000000000004', 'GBP',
         'GL-RATES-001', 'CC-EMEA-RATES', 'Active', 1),

        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000002', 0, v_party_id,
         'GBP Rates Options',
         '20000000-0000-4000-a000-000000000004', 'GBP',
         'GL-RATES-002', 'CC-EMEA-RATES', 'Active', 1),

        -- EUR Rates portfolio (20..005) books
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000003', 0, v_party_id,
         'EUR Vanilla Swaps',
         '20000000-0000-4000-a000-000000000005', 'EUR',
         'GL-RATES-003', 'CC-EMEA-RATES', 'Active', 1),

        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000004', 0, v_party_id,
         'EUR Exotic Rates',
         '20000000-0000-4000-a000-000000000005', 'EUR',
         'GL-RATES-004', 'CC-EMEA-RATES', 'Active', 1),

        -- IG Credit EMEA portfolio (20..007) books
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000005', 0, v_party_id,
         'IG CDS EMEA',
         '20000000-0000-4000-a000-000000000007', 'EUR',
         'GL-CREDIT-001', 'CC-EMEA-CREDIT', 'Active', 1),

        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000006', 0, v_party_id,
         'IG Bonds EMEA',
         '20000000-0000-4000-a000-000000000007', 'EUR',
         'GL-CREDIT-002', 'CC-EMEA-CREDIT', 'Active', 0),

        -- G10 FX EMEA portfolio (20..009) books
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000007', 0, v_party_id,
         'G10 FX Spot Forward',
         '20000000-0000-4000-a000-000000000009', 'EUR',
         'GL-FX-001', 'CC-EMEA-FX', 'Active', 1),

        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000008', 0, v_party_id,
         'G10 FX Options',
         '20000000-0000-4000-a000-000000000009', 'EUR',
         'GL-FX-002', 'CC-EMEA-FX', 'Active', 1),

        -- USD Rates portfolio (20..00c) books
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000009', 0, v_party_id,
         'USD Vanilla Swaps',
         '20000000-0000-4000-a000-00000000000c', 'USD',
         'GL-RATES-005', 'CC-AMER-RATES', 'Active', 1),

        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-00000000000a', 0, v_party_id,
         'USD Rates Options',
         '20000000-0000-4000-a000-00000000000c', 'USD',
         'GL-RATES-006', 'CC-AMER-RATES', 'Active', 1),

        -- CAD Rates portfolio (20..00d) book
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-00000000000b', 0, v_party_id,
         'CAD Swaps',
         '20000000-0000-4000-a000-00000000000d', 'CAD',
         'GL-RATES-007', 'CC-AMER-RATES', 'Active', 1),

        -- IG Credit Americas portfolio (20..00f) books
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-00000000000c', 0, v_party_id,
         'IG CDS Americas',
         '20000000-0000-4000-a000-00000000000f', 'USD',
         'GL-CREDIT-003', 'CC-AMER-CREDIT', 'Active', 1),

        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-00000000000d', 0, v_party_id,
         'IG Bonds Americas',
         '20000000-0000-4000-a000-00000000000f', 'USD',
         'GL-CREDIT-004', 'CC-AMER-CREDIT', 'Active', 0),

        -- JPY Rates portfolio (20..012) books
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-00000000000e', 0, v_party_id,
         'JPY Vanilla Swaps',
         '20000000-0000-4000-a000-000000000012', 'JPY',
         'GL-RATES-008', 'CC-APAC-RATES', 'Active', 1),

        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-00000000000f', 0, v_party_id,
         'JPY Rates Options',
         '20000000-0000-4000-a000-000000000012', 'JPY',
         'GL-RATES-009', 'CC-APAC-RATES', 'Active', 1),

        -- FX APAC portfolio (20..013) books
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000010', 0, v_party_id,
         'APAC FX Spot Forward',
         '20000000-0000-4000-a000-000000000013', 'JPY',
         'GL-FX-003', 'CC-APAC-FX', 'Active', 1),

        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000011', 0, v_party_id,
         'APAC FX NDF',
         '20000000-0000-4000-a000-000000000013', 'JPY',
         'GL-FX-004', 'CC-APAC-FX', 'Active', 1),

        -- Regulatory Capital portfolio (20..014) books
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000012', 0, v_party_id,
         'Regulatory Banking Book',
         '20000000-0000-4000-a000-000000000014', 'USD',
         'GL-REG-001', 'CC-RISK', 'Active', 0),

        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000013', 0, v_party_id,
         'Regulatory Trading Book',
         '20000000-0000-4000-a000-000000000014', 'USD',
         'GL-REG-002', 'CC-RISK', 'Active', 1),

        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '30000000-0000-4000-a000-000000000014', 0, v_party_id,
         'Regulatory CVA Book',
         '20000000-0000-4000-a000-000000000014', 'USD',
         'GL-REG-003', 'CC-RISK', 'Closed', 1);

    get diagnostics v_count = row_count;
    raise notice 'Successfully populated % books', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Books Summary ---'

select 'Total DQ Books' as metric, count(*) as count
from ores_dq_books_artefact_tbl
union all
select 'Trading Books', count(*)
from ores_dq_books_artefact_tbl
where is_trading_book = 1
union all
select 'Banking Books', count(*)
from ores_dq_books_artefact_tbl
where is_trading_book = 0
union all
select 'Active Books', count(*)
from ores_dq_books_artefact_tbl
where book_status = 'Active'
union all
select 'Closed Books', count(*)
from ores_dq_books_artefact_tbl
where book_status = 'Closed';
