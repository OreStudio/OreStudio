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
 * Market Data Providers DQ Dataset
 *
 * Registers the synthetic.market_data_providers dataset and stages the
 * simulated market data providers a synthetic collection can stand for: the
 * main data vendors, exchanges and brokers, and the generic provider for a
 * system collection that stands for none in particular. The dataset is a
 * member of the system_core bundle, so the system provisioning step publishes
 * it into the system tenant through
 * ores_synthetic_publish_market_data_providers_from_dq_fn. Every provider is
 * simulated; its feeds' source names start synthetic.<code>.
 *
 * This script is idempotent: it replaces the dataset's artefact rows.
 */

\echo '--- DQ Dataset: Market Data Providers ---'

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.market_data_providers',
        'Synthetic Market Data',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Synthetic Market Data Providers',
        'The simulated market data providers the synthetic feeds stand for: Bloomberg, Refinitiv (LSEG), ICE, CME, LSE, Eurex, Tradeweb, and a generic provider.',
        'ORESTUDIO',
        'Market data providers for the synthetic feeds',
        current_date,
        'Internal Use Only',
        'market_data_providers'
    );
END $$;

\echo '--- DQ Artefact: Market Data Providers ---'

do $$
declare
    v_dataset_id uuid;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'synthetic.market_data_providers'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset synthetic.market_data_providers not found.';
    end if;

    delete from ores_dq_synthetic_market_data_providers_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_synthetic_market_data_providers_artefact_tbl (
        dataset_id, tenant_id, code, version, name, kind, description, display_order
    ) values
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'generic', 0, 'Generic', 'generic',
         'A provider that stands for no vendor or venue in particular, for system feeds that simulate the market as a whole.',
         1),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'bloomberg', 0, 'Bloomberg', 'data_vendor',
         'Data vendor publishing quotes across asset classes: FX, rates, credit, equities and commodities.',
         2),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'refinitiv', 0, 'Refinitiv (LSEG)', 'data_vendor',
         'Data vendor publishing quotes across asset classes, including FX spot and the WM/Refinitiv benchmark rates.',
         3),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'ice', 0, 'ICE Data Services', 'data_vendor',
         'Data vendor publishing evaluated prices, reference data and index data, with the ICE exchanges behind it.',
         4),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'cme', 0, 'CME Group', 'exchange',
         'Exchange group listing interest rate, FX, equity index and commodity futures and options.',
         5),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'lse', 0, 'London Stock Exchange', 'exchange',
         'Exchange listing equities, exchange traded funds and bonds.',
         6),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'eurex', 0, 'Eurex', 'exchange',
         'Exchange listing European interest rate, equity index and equity derivatives.',
         7),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'tradeweb', 0, 'Tradeweb', 'broker',
         'Electronic trading venue for rates, credit and money markets, publishing dealer composite prices.',
         8);
end $$;

-- Summary
select 'dq_synthetic_market_data_providers_artefact' as entity, count(*) as count
from ores_dq_synthetic_market_data_providers_artefact_tbl;
