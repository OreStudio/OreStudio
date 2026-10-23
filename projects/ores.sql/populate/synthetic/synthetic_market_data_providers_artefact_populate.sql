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
 * main data vendors, exchanges and brokers, and a fictional Demo provider for exercises. The dataset is a
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
        'The simulated market data providers the synthetic feeds stand for: Bloomberg, LSEG, ICE, CME, LSE, Eurex, Tradeweb, and a fictional Demo provider for exercises.',
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

    delete from ores_dq_market_data_providers_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_market_data_providers_artefact_tbl (
        dataset_id, tenant_id, code, version, name, kind, description, display_order
    ) values
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'demo', 0, 'Demo', 'reference',
         'A fictional provider for exercises and demonstrations: uniform, exaggerated volatility that is easy to eyeball on a chart.',
         1),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'bloomberg', 0, 'Bloomberg', 'data_vendor',
         'Simulates Bloomberg composite quotes: FX spots and overnight and swap rates for the major currencies.',
         2),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'lseg', 0, 'LSEG', 'data_vendor',
         'Simulates LSEG composite FX quotes, the WM benchmark rates and money-market rates.',
         3),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'ice', 0, 'ICE Data Services', 'data_vendor',
         'Simulates ICE evaluated prices and benchmark rates, and the curves they imply.',
         4),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'cme', 0, 'CME Group', 'exchange',
         'Simulates CME interest rate, FX and equity index futures, and the SOFR-linked curves they imply.',
         5),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'lse', 0, 'London Stock Exchange', 'exchange',
         'Simulates London Stock Exchange equity, exchange traded fund and bond quotes.',
         6),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'eurex', 0, 'Eurex', 'exchange',
         'Simulates Eurex European interest rate, equity index and equity derivative quotes.',
         7),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'tradeweb', 0, 'Tradeweb', 'broker',
         'Simulates Tradeweb dealer composite prices for rates, credit and money markets.',
         8);
end $$;

-- Summary
select 'dq_market_data_providers_artefact' as entity, count(*) as count
from ores_dq_market_data_providers_artefact_tbl;
