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
 * Populates 22 trading books as leaf nodes of the portfolio tree.
 * Each book belongs to exactly one leaf portfolio.
 *
 * Portfolio -> Books mapping:
 *   GBP Rates       -> GBP Nostro Sweep (is_sweepable), GBP Vanilla Swaps, GBP Rates Options
 *   EUR Rates       -> EUR Vanilla Swaps, EUR Exotic Rates
 *   IG Credit EMEA  -> IG CDS EMEA, IG Bonds EMEA
 *   G10 FX EMEA     -> G10 FX Spot Forward, G10 FX Options
 *   USD Rates       -> USD Cash Management Sweep (is_sweepable), USD Vanilla Swaps, USD Rates Options
 *   CAD Rates       -> CAD Swaps
 *   IG Credit Amer  -> IG CDS Americas, IG Bonds Americas
 *   JPY Rates       -> JPY Vanilla Swaps, JPY Rates Options
 *   FX APAC         -> APAC FX Spot Forward, APAC FX NDF
 *   Regulatory Cap  -> Banking Book, Trading Book, CVA Book
 *
 * Note: legal_entity_id is not stored in the artefact table. It is supplied
 * as a parameter to the publish function, allowing the same template
 * to be published to any number of parties/legal entities.
 *
 * This script is idempotent.
 */

DO $$
declare
    v_dataset_id uuid;
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

    -- Clear existing data (idempotency)
    delete from ores_dq_books_artefact_tbl
    where dataset_id = v_dataset_id;

    raise debug 'Populating books for dataset: testdata.books';

    insert into ores_dq_books_artefact_tbl (
        dataset_id, tenant_id, id, version, name,
        parent_portfolio_id, ledger_ccy, gl_account_ref, cost_center,
        book_status, regulatory_book_type, is_sweepable, rates_centre_code
    )
    values
        -- GBP Rates portfolio (20..004): Nostro sweep target -- collects
        -- GBP cash balances swept from other sweepable books (see the
        -- Spot and ledger sweep knowledge doc).
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '45811f01-f222-422f-8dcb-4b7b75b0e5cb', 0,
         'GBP Nostro Sweep',
         '9bf0f7d9-b808-4b62-aec2-bcc3c906c0bc', 'GBP',
         'GL-TREAS-001', 'CC-EMEA-TREASURY', 'Active', 'Trading', true, 'GBLO'),

        -- USD Rates portfolio (20..00c): Cash management sweep book.
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '81717504-de4d-43b2-b801-d872aa009624', 0,
         'USD Cash Management Sweep',
         'b7cc43eb-4063-4f21-b453-ccfc77aefba7', 'USD',
         'GL-TREAS-002', 'CC-AMER-TREASURY', 'Active', 'Trading', true, 'USNY'),

        -- GBP Rates portfolio (20..004) books
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '137a93e3-3341-4dea-86c2-7309b8ce3e8e', 0,
         'GBP Vanilla Swaps',
         '9bf0f7d9-b808-4b62-aec2-bcc3c906c0bc', 'GBP',
         'GL-RATES-001', 'CC-EMEA-RATES', 'Active', 'Trading', false, 'GBLO'),

        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '42430630-c8da-453e-a7b6-7cb3ad0f4ed0', 0,
         'GBP Rates Options',
         '9bf0f7d9-b808-4b62-aec2-bcc3c906c0bc', 'GBP',
         'GL-RATES-002', 'CC-EMEA-RATES', 'Active', 'Trading', false, 'GBLO'),

        -- EUR Rates portfolio (20..005) books
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'e54579c9-e0ba-41e4-a654-b70b8d9e7977', 0,
         'EUR Vanilla Swaps',
         '99eca864-434a-406d-a04f-0ae2d221f4de', 'EUR',
         'GL-RATES-003', 'CC-EMEA-RATES', 'Active', 'Trading', false, 'FRPA'),

        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '40a97f4d-e6eb-4039-ae4a-479aef3774d6', 0,
         'EUR Exotic Rates',
         '99eca864-434a-406d-a04f-0ae2d221f4de', 'EUR',
         'GL-RATES-004', 'CC-EMEA-RATES', 'Active', 'Trading', false, 'FRPA'),

        -- IG Credit EMEA portfolio (20..007) books
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '4d62fc95-b017-4746-bb03-e336cba850a1', 0,
         'IG CDS EMEA',
         'e28c5d02-a928-48b3-9ead-d444886d0989', 'EUR',
         'GL-CREDIT-001', 'CC-EMEA-CREDIT', 'Active', 'Trading', false, 'GBLO'),

        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '8b4e9466-00c7-45cb-bac0-3c28fffcfd46', 0,
         'IG Bonds EMEA',
         'e28c5d02-a928-48b3-9ead-d444886d0989', 'EUR',
         'GL-CREDIT-002', 'CC-EMEA-CREDIT', 'Active', 'Banking', false, 'GBLO'),

        -- G10 FX EMEA portfolio (20..009) books
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '180ec717-f41d-4fdd-a136-a0572602b456', 0,
         'G10 FX Spot Forward',
         'db06e9f0-36b1-46f9-b3f2-fcc4e1725c51', 'EUR',
         'GL-FX-001', 'CC-EMEA-FX', 'Active', 'Trading', false, 'GBLO'),

        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '622b12ab-63a1-4aff-a7fc-70cad5f3c4cd', 0,
         'G10 FX Options',
         'db06e9f0-36b1-46f9-b3f2-fcc4e1725c51', 'EUR',
         'GL-FX-002', 'CC-EMEA-FX', 'Active', 'Trading', false, 'GBLO'),

        -- USD Rates portfolio (20..00c) books
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '2fd7083d-686a-407f-855a-84e77122c3ac', 0,
         'USD Vanilla Swaps',
         'b7cc43eb-4063-4f21-b453-ccfc77aefba7', 'USD',
         'GL-RATES-005', 'CC-AMER-RATES', 'Active', 'Trading', false, 'USNY'),

        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '0e04ffc8-8342-496a-8b66-ae55f3738848', 0,
         'USD Rates Options',
         'b7cc43eb-4063-4f21-b453-ccfc77aefba7', 'USD',
         'GL-RATES-006', 'CC-AMER-RATES', 'Active', 'Trading', false, 'USNY'),

        -- CAD Rates portfolio (20..00d) book
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'a2f4d8ca-c2ef-466c-93b8-bc0674159741', 0,
         'CAD Swaps',
         '12734755-dd8d-4b01-b312-52055a67c235', 'CAD',
         'GL-RATES-007', 'CC-AMER-RATES', 'Active', 'Trading', false, 'CATO'),

        -- IG Credit Americas portfolio (20..00f) books
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'da8e93b9-2527-4401-b306-5be85d09b378', 0,
         'IG CDS Americas',
         'd2932457-de3c-4db6-9c33-fc9c5bd5b9b8', 'USD',
         'GL-CREDIT-003', 'CC-AMER-CREDIT', 'Active', 'Trading', false, 'USNY'),

        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '4f802c39-f3df-49d9-85a2-edb31fca4e78', 0,
         'IG Bonds Americas',
         'd2932457-de3c-4db6-9c33-fc9c5bd5b9b8', 'USD',
         'GL-CREDIT-004', 'CC-AMER-CREDIT', 'Active', 'Banking', false, 'USNY'),

        -- JPY Rates portfolio (20..012) books
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'c37dc90a-79ca-4837-9f9c-72ce7d71dd94', 0,
         'JPY Vanilla Swaps',
         'f2d6c228-3528-4ba0-bd0e-7a61eb4e7fcd', 'JPY',
         'GL-RATES-008', 'CC-APAC-RATES', 'Active', 'Trading', false, 'JPTO'),

        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '129951c6-cbac-415e-8611-8a40727e6c34', 0,
         'JPY Rates Options',
         'f2d6c228-3528-4ba0-bd0e-7a61eb4e7fcd', 'JPY',
         'GL-RATES-009', 'CC-APAC-RATES', 'Active', 'Trading', false, 'JPTO'),

        -- FX APAC portfolio (20..013) books
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '6c795ed4-b782-409b-a378-f5a7b21a20cf', 0,
         'APAC FX Spot Forward',
         '30741c42-ee10-4206-a62b-3bfb9cf1c29a', 'JPY',
         'GL-FX-003', 'CC-APAC-FX', 'Active', 'Trading', false, 'SGSI'),

        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'c4a6995f-bca0-4ab0-a627-87f27b430841', 0,
         'APAC FX NDF',
         '30741c42-ee10-4206-a62b-3bfb9cf1c29a', 'JPY',
         'GL-FX-004', 'CC-APAC-FX', 'Active', 'Trading', false, 'SGSI'),

        -- Regulatory Capital portfolio (20..014) books: group-level,
        -- region-agnostic. Rates centre left null -- the publish
        -- function fills it in with the publishing party's own
        -- business_center_code (e.g. Barclays Plc -> GBLO), so the same
        -- template produces the right rates centre for any party.
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '0f419e5d-9a05-4ac5-b020-cffc6d6d2afb', 0,
         'Regulatory Banking Book',
         '1f89906e-c6bc-49e9-97df-6ad8fc56a2b0', 'USD',
         'GL-REG-001', 'CC-RISK', 'Active', 'Banking', false, null),

        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'e155b3cb-55ec-4114-b8b8-e8cbb152e95a', 0,
         'Regulatory Trading Book',
         '1f89906e-c6bc-49e9-97df-6ad8fc56a2b0', 'USD',
         'GL-REG-002', 'CC-RISK', 'Active', 'Trading', false, null),

        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'd6a27f65-c064-43a1-9d81-be4edfee1b88', 0,
         'Regulatory CVA Book',
         '1f89906e-c6bc-49e9-97df-6ad8fc56a2b0', 'USD',
         'GL-REG-003', 'CC-RISK', 'Closed', 'Trading', false, null);

    get diagnostics v_count = row_count;
    raise debug 'Successfully populated % books', v_count;
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
where regulatory_book_type = 'Trading'
union all
select 'Banking Books', count(*)
from ores_dq_books_artefact_tbl
where regulatory_book_type = 'Banking'
union all
select 'Active Books', count(*)
from ores_dq_books_artefact_tbl
where book_status = 'Active'
union all
select 'Closed Books', count(*)
from ores_dq_books_artefact_tbl
where book_status = 'Closed'
union all
select 'Sweepable Books', count(*)
from ores_dq_books_artefact_tbl
where is_sweepable;
