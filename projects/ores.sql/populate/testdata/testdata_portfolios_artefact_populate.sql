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
 * Test Data Portfolios Artefact Population Script
 *
 * Populates 20 portfolios in a hierarchical tree structure:
 *
 * Global Portfolio (virtual, USD)
 * +-- EMEA Portfolio (virtual, EUR)
 * |   +-- Rates EMEA (virtual, EUR)
 * |   |   +-- GBP Rates (leaf, GBP)
 * |   |   +-- EUR Rates (leaf, EUR)
 * |   +-- Credit EMEA (virtual, EUR)
 * |   |   +-- IG Credit EMEA (leaf, EUR)
 * |   +-- FX EMEA (virtual, EUR)
 * |       +-- G10 FX EMEA (leaf, EUR)
 * +-- Americas Portfolio (virtual, USD)
 * |   +-- Rates Americas (virtual, USD)
 * |   |   +-- USD Rates (leaf, USD)
 * |   |   +-- CAD Rates (leaf, CAD)
 * |   +-- Credit Americas (virtual, USD)
 * |       +-- IG Credit Americas (leaf, USD)
 * +-- APAC Portfolio (virtual, JPY)
 * |   +-- Rates APAC (virtual, JPY)
 * |   |   +-- JPY Rates (leaf, JPY)
 * |   +-- FX APAC (leaf, JPY)
 * Regulatory Capital (leaf, USD)
 *
 * Leaf portfolios (is_virtual=0) are the parents of trading books.
 * Virtual portfolios (is_virtual=1) are aggregation nodes.
 *
 * This script is idempotent.
 */

DO $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the portfolios dataset ID
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'testdata.portfolios'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: testdata.portfolios. Run dataset population first.';
    end if;

    -- Clear existing data (idempotency)
    delete from ores_dq_portfolios_artefact_tbl
    where dataset_id = v_dataset_id;

    raise notice 'Populating portfolios for dataset: testdata.portfolios';

    insert into ores_dq_portfolios_artefact_tbl (
        dataset_id, tenant_id, id, version, name, parent_portfolio_id,
        owner_unit_id, purpose_type, aggregation_ccy, is_virtual
    )
    values
        -- Root: Global Portfolio
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000001', 0, 'Global Portfolio', null,
         '10000000-0000-4000-a000-000000000001', 'Risk', 'USD', 1),

        -- EMEA Portfolio (under Global)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000002', 0, 'EMEA Portfolio',
         '20000000-0000-4000-a000-000000000001',
         '10000000-0000-4000-a000-000000000002', 'Risk', 'EUR', 1),

        -- Rates EMEA (under EMEA)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000003', 0, 'Rates EMEA',
         '20000000-0000-4000-a000-000000000002',
         '10000000-0000-4000-a000-000000000003', 'Risk', 'EUR', 1),

        -- GBP Rates (leaf under Rates EMEA)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000004', 0, 'GBP Rates',
         '20000000-0000-4000-a000-000000000003',
         '10000000-0000-4000-a000-000000000003', 'Risk', 'GBP', 0),

        -- EUR Rates (leaf under Rates EMEA)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000005', 0, 'EUR Rates',
         '20000000-0000-4000-a000-000000000003',
         '10000000-0000-4000-a000-000000000003', 'Risk', 'EUR', 0),

        -- Credit EMEA (under EMEA)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000006', 0, 'Credit EMEA',
         '20000000-0000-4000-a000-000000000002',
         '10000000-0000-4000-a000-000000000004', 'Risk', 'EUR', 1),

        -- IG Credit EMEA (leaf under Credit EMEA)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000007', 0, 'IG Credit EMEA',
         '20000000-0000-4000-a000-000000000006',
         '10000000-0000-4000-a000-000000000004', 'Risk', 'EUR', 0),

        -- FX EMEA (under EMEA)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000008', 0, 'FX EMEA',
         '20000000-0000-4000-a000-000000000002',
         '10000000-0000-4000-a000-000000000005', 'Risk', 'EUR', 1),

        -- G10 FX EMEA (leaf under FX EMEA)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000009', 0, 'G10 FX EMEA',
         '20000000-0000-4000-a000-000000000008',
         '10000000-0000-4000-a000-000000000005', 'Risk', 'EUR', 0),

        -- Americas Portfolio (under Global)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-00000000000a', 0, 'Americas Portfolio',
         '20000000-0000-4000-a000-000000000001',
         '10000000-0000-4000-a000-000000000006', 'Risk', 'USD', 1),

        -- Rates Americas (under Americas)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-00000000000b', 0, 'Rates Americas',
         '20000000-0000-4000-a000-00000000000a',
         '10000000-0000-4000-a000-000000000007', 'Risk', 'USD', 1),

        -- USD Rates (leaf under Rates Americas)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-00000000000c', 0, 'USD Rates',
         '20000000-0000-4000-a000-00000000000b',
         '10000000-0000-4000-a000-000000000007', 'Risk', 'USD', 0),

        -- CAD Rates (leaf under Rates Americas)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-00000000000d', 0, 'CAD Rates',
         '20000000-0000-4000-a000-00000000000b',
         '10000000-0000-4000-a000-000000000007', 'Risk', 'CAD', 0),

        -- Credit Americas (under Americas)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-00000000000e', 0, 'Credit Americas',
         '20000000-0000-4000-a000-00000000000a',
         '10000000-0000-4000-a000-000000000008', 'Risk', 'USD', 1),

        -- IG Credit Americas (leaf under Credit Americas)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-00000000000f', 0, 'IG Credit Americas',
         '20000000-0000-4000-a000-00000000000e',
         '10000000-0000-4000-a000-000000000008', 'Risk', 'USD', 0),

        -- APAC Portfolio (under Global)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000010', 0, 'APAC Portfolio',
         '20000000-0000-4000-a000-000000000001',
         '10000000-0000-4000-a000-000000000009', 'Risk', 'JPY', 1),

        -- Rates APAC (under APAC)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000011', 0, 'Rates APAC',
         '20000000-0000-4000-a000-000000000010',
         '10000000-0000-4000-a000-00000000000a', 'Risk', 'JPY', 1),

        -- JPY Rates (leaf under Rates APAC)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000012', 0, 'JPY Rates',
         '20000000-0000-4000-a000-000000000011',
         '10000000-0000-4000-a000-00000000000a', 'Risk', 'JPY', 0),

        -- FX APAC (leaf under APAC)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000013', 0, 'FX APAC',
         '20000000-0000-4000-a000-000000000010',
         '10000000-0000-4000-a000-00000000000b', 'Risk', 'JPY', 0),

        -- Root: Regulatory Capital
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '20000000-0000-4000-a000-000000000014', 0, 'Regulatory Capital', null,
         '10000000-0000-4000-a000-00000000000c', 'Regulatory', 'USD', 0);

    get diagnostics v_count = row_count;
    raise notice 'Successfully populated % portfolios', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Portfolios Summary ---'

select 'Total DQ Portfolios' as metric, count(*) as count
from ores_dq_portfolios_artefact_tbl
union all
select 'Virtual Portfolios', count(*)
from ores_dq_portfolios_artefact_tbl
where is_virtual = 1
union all
select 'Leaf Portfolios', count(*)
from ores_dq_portfolios_artefact_tbl
where is_virtual = 0;
