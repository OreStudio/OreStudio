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

    raise debug 'Populating portfolios for dataset: testdata.portfolios';

    insert into ores_dq_portfolios_artefact_tbl (
        dataset_id, tenant_id, id, version, name, parent_portfolio_id,
        owner_unit_id, purpose_type, aggregation_ccy, is_virtual
    )
    values
        -- Root: Global Portfolio
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '0bed2cf6-1959-4ecf-85e4-4e4afefc2135', 0, 'Global Portfolio', null,
         '10000000-0000-4000-a000-000000000001', 'Risk', 'USD', 1),

        -- EMEA Portfolio (under Global)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '83d9be04-a3e7-4c44-8dbb-d8deca38f71f', 0, 'EMEA Portfolio',
         '0bed2cf6-1959-4ecf-85e4-4e4afefc2135',
         '10000000-0000-4000-a000-000000000002', 'Risk', 'EUR', 1),

        -- Rates EMEA (under EMEA)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '9f88c34f-7200-46f2-b726-7228e269241b', 0, 'Rates EMEA',
         '83d9be04-a3e7-4c44-8dbb-d8deca38f71f',
         '10000000-0000-4000-a000-000000000003', 'Risk', 'EUR', 1),

        -- GBP Rates (leaf under Rates EMEA)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '9bf0f7d9-b808-4b62-aec2-bcc3c906c0bc', 0, 'GBP Rates',
         '9f88c34f-7200-46f2-b726-7228e269241b',
         '10000000-0000-4000-a000-000000000003', 'Risk', 'GBP', 0),

        -- EUR Rates (leaf under Rates EMEA)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '99eca864-434a-406d-a04f-0ae2d221f4de', 0, 'EUR Rates',
         '9f88c34f-7200-46f2-b726-7228e269241b',
         '10000000-0000-4000-a000-000000000003', 'Risk', 'EUR', 0),

        -- Credit EMEA (under EMEA)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'bdf64a3e-6e13-48b9-9753-82dcde4ce735', 0, 'Credit EMEA',
         '83d9be04-a3e7-4c44-8dbb-d8deca38f71f',
         '10000000-0000-4000-a000-000000000004', 'Risk', 'EUR', 1),

        -- IG Credit EMEA (leaf under Credit EMEA)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'e28c5d02-a928-48b3-9ead-d444886d0989', 0, 'IG Credit EMEA',
         'bdf64a3e-6e13-48b9-9753-82dcde4ce735',
         '10000000-0000-4000-a000-000000000004', 'Risk', 'EUR', 0),

        -- FX EMEA (under EMEA)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'b78baf5e-5a5b-4380-9fed-629800766e91', 0, 'FX EMEA',
         '83d9be04-a3e7-4c44-8dbb-d8deca38f71f',
         '10000000-0000-4000-a000-000000000005', 'Risk', 'EUR', 1),

        -- G10 FX EMEA (leaf under FX EMEA)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'db06e9f0-36b1-46f9-b3f2-fcc4e1725c51', 0, 'G10 FX EMEA',
         'b78baf5e-5a5b-4380-9fed-629800766e91',
         '10000000-0000-4000-a000-000000000005', 'Risk', 'EUR', 0),

        -- Americas Portfolio (under Global)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'e9de460d-0b62-43e0-b78e-b9858ce58c10', 0, 'Americas Portfolio',
         '0bed2cf6-1959-4ecf-85e4-4e4afefc2135',
         '10000000-0000-4000-a000-000000000006', 'Risk', 'USD', 1),

        -- Rates Americas (under Americas)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'd2421c83-1357-4962-ae0b-86b8bb4a6fae', 0, 'Rates Americas',
         'e9de460d-0b62-43e0-b78e-b9858ce58c10',
         '10000000-0000-4000-a000-000000000007', 'Risk', 'USD', 1),

        -- USD Rates (leaf under Rates Americas)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'b7cc43eb-4063-4f21-b453-ccfc77aefba7', 0, 'USD Rates',
         'd2421c83-1357-4962-ae0b-86b8bb4a6fae',
         '10000000-0000-4000-a000-000000000007', 'Risk', 'USD', 0),

        -- CAD Rates (leaf under Rates Americas)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '12734755-dd8d-4b01-b312-52055a67c235', 0, 'CAD Rates',
         'd2421c83-1357-4962-ae0b-86b8bb4a6fae',
         '10000000-0000-4000-a000-000000000007', 'Risk', 'CAD', 0),

        -- Credit Americas (under Americas)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '63d9cccd-9582-40a0-9d09-fdec1834163b', 0, 'Credit Americas',
         'e9de460d-0b62-43e0-b78e-b9858ce58c10',
         '10000000-0000-4000-a000-000000000008', 'Risk', 'USD', 1),

        -- IG Credit Americas (leaf under Credit Americas)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'd2932457-de3c-4db6-9c33-fc9c5bd5b9b8', 0, 'IG Credit Americas',
         '63d9cccd-9582-40a0-9d09-fdec1834163b',
         '10000000-0000-4000-a000-000000000008', 'Risk', 'USD', 0),

        -- APAC Portfolio (under Global)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '5e78a956-6b92-4f69-9f35-7637204e37fd', 0, 'APAC Portfolio',
         '0bed2cf6-1959-4ecf-85e4-4e4afefc2135',
         '10000000-0000-4000-a000-000000000009', 'Risk', 'JPY', 1),

        -- Rates APAC (under APAC)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'c6f6014a-ca5e-4947-88cb-26783002c252', 0, 'Rates APAC',
         '5e78a956-6b92-4f69-9f35-7637204e37fd',
         '10000000-0000-4000-a000-00000000000a', 'Risk', 'JPY', 1),

        -- JPY Rates (leaf under Rates APAC)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         'f2d6c228-3528-4ba0-bd0e-7a61eb4e7fcd', 0, 'JPY Rates',
         'c6f6014a-ca5e-4947-88cb-26783002c252',
         '10000000-0000-4000-a000-00000000000a', 'Risk', 'JPY', 0),

        -- FX APAC (leaf under APAC)
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '30741c42-ee10-4206-a62b-3bfb9cf1c29a', 0, 'FX APAC',
         '5e78a956-6b92-4f69-9f35-7637204e37fd',
         '10000000-0000-4000-a000-00000000000b', 'Risk', 'JPY', 0),

        -- Root: Regulatory Capital
        (v_dataset_id, ores_utility_system_tenant_id_fn(),
         '1f89906e-c6bc-49e9-97df-6ad8fc56a2b0', 0, 'Regulatory Capital', null,
         '10000000-0000-4000-a000-00000000000c', 'Regulatory', 'USD', 0);

    get diagnostics v_count = row_count;
    raise debug 'Successfully populated % portfolios', v_count;
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
