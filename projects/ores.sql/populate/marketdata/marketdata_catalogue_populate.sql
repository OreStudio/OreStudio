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
 * Market Data Catalogue Population Script
 *
 * Installs the providers the system names and the three sources of the provider
 * ore. The system tenant owns the rows and every tenant reads them. The source
 * ids are fixed, because the ORE import and the DQ publish name their source
 * by id: ore-import below is the id that the publish function and the C++
 * catalogue_ids header carry.
 *
 * Sources of the other providers are installed with their adapters.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO NOTHING.
 */

\echo '--- Market Data Providers ---'

insert into ores_marketdata_providers_tbl (
    tenant_id, code, version, name, kind, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'bloomberg', 0, 'Bloomberg', 'data_vendor',
     'Bloomberg L.P. identifies an instrument by a ticker and a yellow key.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of providers'),
    (ores_utility_system_tenant_id_fn(), 'lseg', 0, 'LSEG', 'data_vendor',
     'LSEG, known before as Reuters and Refinitiv, identifies an instrument by a RIC.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of providers'),
    (ores_utility_system_tenant_id_fn(), 'ice', 0, 'ICE', 'data_vendor',
     'ICE Data Services publishes pricing and reference data under its own symbology.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of providers'),
    (ores_utility_system_tenant_id_fn(), 'tradeweb', 0, 'Tradeweb', 'data_vendor',
     'Tradeweb publishes pricing for fixed income and other OTC markets.',
     4, current_user, current_user, 'system.initial_load', 'Initial population of providers'),
    (ores_utility_system_tenant_id_fn(), 'ore', 0, 'ORE', 'reference',
     'The Open Source Risk Engine. Its quote keys are an open notation.',
     5, current_user, current_user, 'system.initial_load', 'Initial population of providers')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

\echo '--- Market Data Sources ---'

insert into ores_marketdata_market_data_sources_tbl (
    id, tenant_id, version, name, provider_code, kind, is_default, description,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('26772cc0-57d6-4fac-be36-2ae75eb56b6b', ores_utility_system_tenant_id_fn(), 0,
     'ore-import', 'ore', 'replay', true,
     'The values of an ORE market data file, as the import and the DQ publish write them.',
     current_user, current_user, 'system.initial_load', 'Initial population of sources'),
    ('a94c20bb-09c4-48d3-b91a-d66322322d60', ores_utility_system_tenant_id_fn(), 0,
     'ore-simulated', 'ore', 'simulated', false,
     'The values that the simulator generates for the keys of an ORE market data file.',
     current_user, current_user, 'system.initial_load', 'Initial population of sources'),
    ('437b14be-6443-4492-b421-a51ce34f318a', ores_utility_system_tenant_id_fn(), 0,
     'ore-derived', 'ore', 'derived', false,
     'The values that the service computes from other series, such as a bootstrapped curve.',
     current_user, current_user, 'system.initial_load', 'Initial population of sources')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

\echo '--- Market Data Rate Cards ---'

-- Illustrative charges for the sources of the provider ore. A real contract replaces them with
-- its own components. The simulated source carries a card of the same shape, so a party that
-- runs on it sees the bill the same use would cost on a live source.
insert into ores_marketdata_rate_card_components_tbl (
    id, tenant_id, version, source_id, driver, licence_right, currency, charge_micros,
    effective_from, modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, s.source_id, c.driver,
       c.licence_right, 'USD', c.charge_micros, timestamptz '2026-01-01 00:00:00+00',
       current_user, current_user, 'system.initial_load', 'Initial population of rate cards'
from (values
    ('26772cc0-57d6-4fac-be36-2ae75eb56b6b'::uuid),
    ('a94c20bb-09c4-48d3-b91a-d66322322d60'::uuid)
) as s(source_id)
cross join (values
    ('fixed_per_period',   '',            100000000::bigint),
    ('per_instrument',     '',                50000::bigint),
    ('per_party',          '',             10000000::bigint),
    ('per_licence',        'non_display',   5000000::bigint),
    ('per_licence',        'derived',       8000000::bigint),
    ('per_thousand_ticks', '',                20000::bigint)
) as c(driver, licence_right, charge_micros)
where not exists (
    select 1 from ores_marketdata_rate_card_components_tbl x
    where x.tenant_id = ores_utility_system_tenant_id_fn()
      and x.source_id = s.source_id
      and x.driver = c.driver
      and x.licence_right = c.licence_right
      and x.valid_to = ores_utility_infinity_timestamp_fn()
);

-- Summary
select 'marketdata_providers' as entity, count(*) as count as entity, count(*) as count
from ores_marketdata_providers_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'marketdata_market_data_sources', count(*)
from ores_marketdata_market_data_sources_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
