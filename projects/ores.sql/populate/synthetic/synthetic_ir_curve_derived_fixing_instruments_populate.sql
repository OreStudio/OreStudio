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
 * Derived Term Index Fixing Instruments
 *
 * Installs the fixing address of each derived term index under the Derived
 * provider, and maps it to its oresmd item. The five indices are the ones the
 * derived feeds dataset publishes, and they are not among the fixings the
 * simulator states as a vendor would: the Derived provider owns them, so its
 * catalogue is written here rather than in the generated block of
 * marketdata_vendor_fixing_instruments_populate.sql, which writes only the ids
 * of the provider simulated-fixings.
 *
 * Each address is spelled as the derived notation writes it, so the runtime and
 * this seed name the same fixing. The id of each instrument follows the rule
 * every vendor instrument follows, a hash of its provider, scheme, address and
 * field, so a rerun writes the same row.
 *
 * This script is idempotent.
 */

\echo '--- Derived Term Index Fixing Instruments ---'

insert into ores_marketdata_market_data_vendor_instruments_tbl (
    id, tenant_id, version, provider_code, id_scheme, external_id, field, minted,
    deviation_note, modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select md5('derived' || '|' || 'oresmd' || '|' || d.item || '|' || 'value')::uuid,
       ores_utility_system_tenant_id_fn(), 0, 'derived', 'oresmd', d.item, 'value',
       false, '', current_user, current_user, 'system.initial_load',
       'The derived term indices the platform states',
       now(), ores_utility_infinity_timestamp_fn()
from (values
    ('oresmd://ir/USD?type=fixing&index=ibor&name=SOFR&tenor=1M'),
    ('oresmd://ir/COP?type=fixing&index=ibor&name=IBR&tenor=3M'),
    ('oresmd://ir/JPY?type=fixing&index=ibor&name=EYTIBOR&tenor=3M'),
    ('oresmd://ir/KRW?type=fixing&index=ibor&name=GENERIC&tenor=3M'),
    ('oresmd://ir/PLN?type=fixing&index=ibor&name=WIBOR&tenor=6M')
) as d(item)
where not exists (
    select 1 from ores_marketdata_market_data_vendor_instruments_tbl v
    where v.tenant_id = ores_utility_system_tenant_id_fn()
      and v.provider_code = 'derived'
      and v.external_id = d.item
      and v.field = 'value'
      and v.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_market_data_mappings_tbl (
    id, tenant_id, version, vendor_instrument_id, oresmd_uri, effective_from,
    modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, v.id, v.external_id,
       timestamptz '2026-01-01 00:00:00+00', current_user, current_user, 'system.initial_load',
       'The derived term indices the platform states',
       now(), ores_utility_infinity_timestamp_fn()
from ores_marketdata_market_data_vendor_instruments_tbl v
where v.tenant_id = ores_utility_system_tenant_id_fn()
  and v.provider_code = 'derived'
  and v.field = 'value'
  and v.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (
      select 1 from ores_marketdata_market_data_mappings_tbl m
      where m.vendor_instrument_id = v.id
        and m.valid_to = ores_utility_infinity_timestamp_fn()
  );

-- A route demands every item of its provider unless the feed is scoped, and the derived
-- provider serves the volatility points as well, so the fixings are scoped to the source that
-- states them: the simulator asks derived-a for those five and for nothing else.
insert into ores_marketdata_feed_scopes_tbl (
    id, tenant_id, version, feed_id, asset_class, item, vendor_instrument_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
       'ca365f97-bfe9-40fe-8e4d-b12fc9f075b2', 'interest_rates', m.oresmd_uri, v.id,
       current_user, current_user, 'system.initial_load',
       'The derived term index fixings the simulator states'
from ores_marketdata_market_data_vendor_instruments_tbl v
join ores_marketdata_market_data_mappings_tbl m
  on m.vendor_instrument_id = v.id
 and m.tenant_id = ores_utility_system_tenant_id_fn()
 and m.valid_to = ores_utility_infinity_timestamp_fn()
where v.tenant_id = ores_utility_system_tenant_id_fn()
  and v.provider_code = 'derived'
  and v.field = 'value'
  and v.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (
      select 1 from ores_marketdata_feed_scopes_tbl s
      where s.tenant_id = ores_utility_system_tenant_id_fn()
        and s.feed_id = 'ca365f97-bfe9-40fe-8e4d-b12fc9f075b2'
        and s.item = m.oresmd_uri
        and s.valid_to = ores_utility_infinity_timestamp_fn()
  );

-- Summary
select 'derived_fixing_instruments' as entity, count(*) as count
from ores_marketdata_market_data_vendor_instruments_tbl
where provider_code = 'derived'
  and valid_to = ores_utility_infinity_timestamp_fn();
