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
 * The Fixing Publishers' Instruments
 *
 * Every fixing item the simulator states is an instrument of the administrator
 * that owns it, written in that administrator's notation and mapped to the item
 * itself: =WMR1000NYC AUDJPY= for a WM/R cut, =USD LIBOR 3M= for a rate setting.
 * The item does not move; only the provider that states it does, which is what
 * makes the same fixing read as the administrator's rather than as the
 * simulator's.
 *
 * The items come from the instruments the catch-all publisher holds today, so
 * this script reads what is already mapped and writes the administrator beside
 * it. An index whose administrator publishes it already, such as SOFR at the New
 * York Fed, is left alone: its public publisher maps the item, and the
 * simulator's row for it is removed when the one-producer rule is applied.
 *
 * This script is idempotent.
 */

\echo '--- Fixing Publishers: Instruments and Mappings ---'

-- The administrator of each rate index the simulator states today. An index
-- whose administrator publishes it through an adapter is not here, because its
-- instrument already exists under that administrator.
create temporary table t_fixing_administrator (
    index_name text primary key,
    provider_code text not null
);

insert into t_fixing_administrator (index_name, provider_code) values
    ('LIBOR', 'iba'),
    ('EURIBOR', 'emmi'),
    ('HIBOR', 'hkab'),
    ('HONIA', 'hkab'),
    ('SARON', 'snb'),
    ('DESTR', 'danmarks-nationalbank'),
    ('HUFONIA', 'mnb'),
    ('KOFR', 'bok'),
    ('POLONIA', 'nbp'),
    ('NZONIA', 'rbnz'),
    ('ZARONIA', 'sarb'),
    ('SHIR', 'boi'),
    ('SHIBOR', 'nifc'),
    ('MIBOR', 'fbil'),
    ('TAIBOR', 'tba'),
    ('TLREF', 'bist'),
    ('IBR', 'banrep');

-- The items the catch-all publisher states today, split into their notation.
create temporary table t_fixing_inventory (
    provider_code text, id_scheme text, external_id text, field text, oresmd_uri text
);

insert into t_fixing_inventory (provider_code, id_scheme, external_id, field, oresmd_uri)
select distinct 'wmr', 'wmr_cut',
       substring(m.oresmd_uri from 'source=([A-Z0-9]+)') || ' ' ||
           split_part(split_part(m.oresmd_uri, '?', 1), '/', 4) || substring(m.oresmd_uri from 'ccy=([A-Z]{3})'),
       'value', m.oresmd_uri
from ores_marketdata_market_data_mappings_tbl m
join ores_marketdata_market_data_vendor_instruments_tbl v
  on v.id = m.vendor_instrument_id
 and v.valid_to = ores_utility_infinity_timestamp_fn()
where v.provider_code = 'simulated-fixings'
  and m.valid_to = ores_utility_infinity_timestamp_fn()
  and m.oresmd_uri like 'oresmd://fx/%'
  and m.oresmd_uri like '%type=fixing%'
  and substring(m.oresmd_uri from 'source=([A-Z0-9]+)') is not null
  and substring(m.oresmd_uri from 'ccy=([A-Z]{3})') is not null
union
select distinct a.provider_code, 'setting',
       split_part(split_part(m.oresmd_uri, '?', 1), '/', 4) || ' ' ||
           substring(m.oresmd_uri from 'name=([A-Za-z]+)') ||
           coalesce(' ' || substring(m.oresmd_uri from 'tenor=([0-9A-Z]+)'), ''),
       'value', m.oresmd_uri
from ores_marketdata_market_data_mappings_tbl m
join ores_marketdata_market_data_vendor_instruments_tbl v
  on v.id = m.vendor_instrument_id
 and v.valid_to = ores_utility_infinity_timestamp_fn()
join t_fixing_administrator a
  on a.index_name = substring(m.oresmd_uri from 'name=([A-Za-z]+)')
where v.provider_code = 'simulated-fixings'
  and m.valid_to = ores_utility_infinity_timestamp_fn()
  and m.oresmd_uri like 'oresmd://ir/%'
  and m.oresmd_uri like '%type=fixing%';

insert into ores_marketdata_market_data_vendor_instruments_tbl (
    id, tenant_id, version, provider_code, id_scheme, external_id, field, minted, deviation_note,
    modified_by, performed_by, change_reason_code, change_commentary
)
select md5(t.provider_code || '|' || t.id_scheme || '|' || t.external_id || '|' || t.field)::uuid,
       ores_utility_system_tenant_id_fn(), 0, t.provider_code, t.id_scheme, t.external_id, t.field,
       false, '', current_user, current_user, 'system.initial_load',
       'Initial population of the fixing publishers'' instruments'
from t_fixing_inventory t
where not exists (
    select 1 from ores_marketdata_market_data_vendor_instruments_tbl v
    where v.tenant_id = ores_utility_system_tenant_id_fn()
      and v.provider_code = t.provider_code
      and v.id_scheme = t.id_scheme
      and v.external_id = t.external_id
      and v.field = t.field
      and v.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_market_data_mappings_tbl (
    id, tenant_id, version, vendor_instrument_id, oresmd_uri, effective_from,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, v.id, t.oresmd_uri,
       timestamptz '2026-01-01 00:00:00+00', current_user, current_user, 'system.initial_load',
       'Initial population of the fixing publishers'' mappings'
from t_fixing_inventory t
join ores_marketdata_market_data_vendor_instruments_tbl v
  on v.tenant_id = ores_utility_system_tenant_id_fn()
 and v.provider_code = t.provider_code
 and v.id_scheme = t.id_scheme
 and v.external_id = t.external_id
 and v.field = t.field
 and v.valid_to = ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_marketdata_market_data_mappings_tbl m
    where m.tenant_id = ores_utility_system_tenant_id_fn()
      and m.vendor_instrument_id = v.id
      and m.oresmd_uri = t.oresmd_uri
      and m.valid_to = ores_utility_infinity_timestamp_fn()
);

-- Summary
select v.provider_code, v.id_scheme, count(*) as instruments
from ores_marketdata_market_data_vendor_instruments_tbl v
where v.tenant_id = ores_utility_system_tenant_id_fn()
  and v.valid_to = ores_utility_infinity_timestamp_fn()
  and v.provider_code in ('wmr', 'iba', 'emmi', 'hkab', 'gpw', 'banrep', 'snb',
                          'danmarks-nationalbank', 'mnb', 'bok', 'nbp', 'rbnz', 'sarb',
                          'boi', 'nifc', 'fbil', 'tba', 'bist')
group by 1, 2
order by 1;
