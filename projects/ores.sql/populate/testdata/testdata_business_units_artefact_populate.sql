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
 * Test Data Business Units Artefact Population Script
 *
 * Populates 13 business units in a hierarchical structure representing
 * a fictitious global markets organisation:
 *
 * Global Markets
 * +-- EMEA Trading
 * |   +-- Rates Trading EMEA
 * |   +-- Credit Trading EMEA
 * |   +-- FX Trading EMEA
 * +-- Americas Trading
 * |   +-- Rates Trading Americas
 * |   +-- Credit Trading Americas
 * +-- APAC Trading
 *     +-- Rates Trading APAC
 *     +-- FX Trading APAC
 * Risk Management
 * +-- Counterparty Risk
 *
 * Note: party_id is not stored in the artefact table. It is supplied
 * as a parameter to the publish function, allowing the same template
 * to be published to any number of parties.
 *
 * This script is idempotent.
 */

DO $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the business units dataset ID
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'testdata.business_units'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: testdata.business_units. Run dataset population first.';
    end if;

    -- Clear existing data (idempotency)
    delete from ores_dq_business_units_artefact_tbl
    where dataset_id = v_dataset_id;

    raise notice 'Populating business units for dataset: testdata.business_units';

    insert into ores_dq_business_units_artefact_tbl (
        dataset_id, tenant_id, id, version, unit_name,
        parent_business_unit_id, unit_code, business_centre_code
    )
    values
        -- Top-level: Global Markets
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-000000000001', 0,
         'Global Markets', null, 'GLOB_MKT', 'GBLO'),

        -- EMEA Trading (under Global Markets)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-000000000002', 0,
         'EMEA Trading', '10000000-0000-4000-a000-000000000001', 'EMEA_TRD', 'GBLO'),

        -- Rates Trading EMEA (under EMEA Trading)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-000000000003', 0,
         'Rates Trading EMEA', '10000000-0000-4000-a000-000000000002', 'RATES_EMEA', 'GBLO'),

        -- Credit Trading EMEA (under EMEA Trading)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-000000000004', 0,
         'Credit Trading EMEA', '10000000-0000-4000-a000-000000000002', 'CREDIT_EMEA', 'GBLO'),

        -- FX Trading EMEA (under EMEA Trading)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-000000000005', 0,
         'FX Trading EMEA', '10000000-0000-4000-a000-000000000002', 'FX_EMEA', 'GBLO'),

        -- Americas Trading (under Global Markets)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-000000000006', 0,
         'Americas Trading', '10000000-0000-4000-a000-000000000001', 'AMER_TRD', 'USNY'),

        -- Rates Trading Americas (under Americas Trading)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-000000000007', 0,
         'Rates Trading Americas', '10000000-0000-4000-a000-000000000006', 'RATES_AMER', 'USNY'),

        -- Credit Trading Americas (under Americas Trading)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-000000000008', 0,
         'Credit Trading Americas', '10000000-0000-4000-a000-000000000006', 'CREDIT_AMER', 'USNY'),

        -- APAC Trading (under Global Markets)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-000000000009', 0,
         'APAC Trading', '10000000-0000-4000-a000-000000000001', 'APAC_TRD', 'JPTO'),

        -- Rates Trading APAC (under APAC Trading)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-00000000000a', 0,
         'Rates Trading APAC', '10000000-0000-4000-a000-000000000009', 'RATES_APAC', 'JPTO'),

        -- FX Trading APAC (under APAC Trading)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-00000000000b', 0,
         'FX Trading APAC', '10000000-0000-4000-a000-000000000009', 'FX_APAC', 'HKHK'),

        -- Top-level: Risk Management
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-00000000000c', 0,
         'Risk Management', null, 'RISK_MGMT', 'GBLO'),

        -- Counterparty Risk (under Risk Management)
        (v_dataset_id, ores_iam_system_tenant_id_fn(),
         '10000000-0000-4000-a000-00000000000d', 0,
         'Counterparty Risk', '10000000-0000-4000-a000-00000000000c', 'CPTY_RISK', 'GBLO');

    get diagnostics v_count = row_count;
    raise notice 'Successfully populated % business units', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Business Units Summary ---'

select 'Total DQ Business Units' as metric, count(*) as count
from ores_dq_business_units_artefact_tbl;
