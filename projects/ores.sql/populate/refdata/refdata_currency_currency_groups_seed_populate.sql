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
 * Currency Group Members Seed Population Script
 *
 * Registers the refdata.currency_currency_groups dataset and seeds its DQ
 * artefact table with the currencies in each desk group. A currency can
 * sit in several groups: NOK is in G11, Scandies and the commodity
 * currencies.
 *
 * The members dataset needs the groups dataset and the currencies. The
 * dependency edges make a bundle publish them first.
 *
 * Execution order: needs the refdata.currency_groups dataset, so it runs
 * after refdata_currency_groups_seed_populate.sql.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_currency_groups',
        'FX Market Conventions',
        'Currencies',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Currency Group Members',
        'The currencies in each desk group: G11, Scandies, Antipodeans, commodity currencies, Asians and Latams.',
        'ORESTUDIO',
        'Seed data for the currency group members Librarian bundle',
        current_date,
        'Internal Use Only',
        'currency_currency_groups'
    );

    -- Dependency edges: a member row names a group and a currency, so
    -- refdata.currency_groups and iso.currencies must publish first.
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_currency_groups',
        'refdata.currency_groups',
        'group_reference'
    );

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_currency_groups',
        'iso.currencies',
        'currency_reference'
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
      and code = 'refdata.currency_currency_groups'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.currency_currency_groups';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_currency_currency_groups_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_currency_currency_groups_artefact_tbl (
        dataset_id, tenant_id, currency_iso_code, currency_group_code, version
    )
    values
        (v_dataset_id, v_tenant_id, 'EUR', 'G11', 0),
        (v_dataset_id, v_tenant_id, 'USD', 'G11', 0),
        (v_dataset_id, v_tenant_id, 'GBP', 'G11', 0),
        (v_dataset_id, v_tenant_id, 'JPY', 'G11', 0),
        (v_dataset_id, v_tenant_id, 'AUD', 'G11', 0),
        (v_dataset_id, v_tenant_id, 'CAD', 'G11', 0),
        (v_dataset_id, v_tenant_id, 'CHF', 'G11', 0),
        (v_dataset_id, v_tenant_id, 'DKK', 'G11', 0),
        (v_dataset_id, v_tenant_id, 'NOK', 'G11', 0),
        (v_dataset_id, v_tenant_id, 'NZD', 'G11', 0),
        (v_dataset_id, v_tenant_id, 'SEK', 'G11', 0),
        (v_dataset_id, v_tenant_id, 'DKK', 'SCANDIES', 0),
        (v_dataset_id, v_tenant_id, 'NOK', 'SCANDIES', 0),
        (v_dataset_id, v_tenant_id, 'SEK', 'SCANDIES', 0),
        (v_dataset_id, v_tenant_id, 'AUD', 'ANTIPODEANS', 0),
        (v_dataset_id, v_tenant_id, 'NZD', 'ANTIPODEANS', 0),
        (v_dataset_id, v_tenant_id, 'AUD', 'COMMODITY', 0),
        (v_dataset_id, v_tenant_id, 'CAD', 'COMMODITY', 0),
        (v_dataset_id, v_tenant_id, 'NZD', 'COMMODITY', 0),
        (v_dataset_id, v_tenant_id, 'NOK', 'COMMODITY', 0),
        (v_dataset_id, v_tenant_id, 'ZAR', 'COMMODITY', 0),
        (v_dataset_id, v_tenant_id, 'BRL', 'COMMODITY', 0),
        (v_dataset_id, v_tenant_id, 'CLP', 'COMMODITY', 0),
        (v_dataset_id, v_tenant_id, 'JPY', 'ASIANS', 0),
        (v_dataset_id, v_tenant_id, 'CNY', 'ASIANS', 0),
        (v_dataset_id, v_tenant_id, 'HKD', 'ASIANS', 0),
        (v_dataset_id, v_tenant_id, 'SGD', 'ASIANS', 0),
        (v_dataset_id, v_tenant_id, 'KRW', 'ASIANS', 0),
        (v_dataset_id, v_tenant_id, 'INR', 'ASIANS', 0),
        (v_dataset_id, v_tenant_id, 'TWD', 'ASIANS', 0),
        (v_dataset_id, v_tenant_id, 'THB', 'ASIANS', 0),
        (v_dataset_id, v_tenant_id, 'IDR', 'ASIANS', 0),
        (v_dataset_id, v_tenant_id, 'MXN', 'LATAMS', 0),
        (v_dataset_id, v_tenant_id, 'BRL', 'LATAMS', 0),
        (v_dataset_id, v_tenant_id, 'ARS', 'LATAMS', 0),
        (v_dataset_id, v_tenant_id, 'CLP', 'LATAMS', 0),
        (v_dataset_id, v_tenant_id, 'COP', 'LATAMS', 0);

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % currency group members for dataset: refdata.currency_currency_groups', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Currency Group Members Summary ---'

select 'Total DQ Currency Group Members' as metric, count(*) as count
from ores_dq_currency_currency_groups_artefact_tbl;
