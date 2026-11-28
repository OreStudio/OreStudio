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
 * Computed Term Index Fixing Instruments
 *
 * Installs the fixing item of each term index the platform computes under the
 * administrator that owns the index: Term SOFR 1M under CME Group, IBR 3M under
 * Banco de la República, WIBOR 6M under GPW Benchmark. No provider named derived
 * states a fixing, because being computed is not a party: the index is the
 * administrator's, and the series' derivation_kind says the run computed the
 * value. The design is in
 * doc/knowledge/architecture/synthetic_generation_of_fixings.org.
 *
 * Each id is written in the administrator's own notation, as every other fixing
 * the run states is: =USD SOFR 1M= under the scheme =setting=, with the field
 * =value=. That is the notation the runtime writes, so the row this script seeds
 * and the id the simulator offers are the same, and the mapping beside it names
 * the oresmd item the series is stored against. The id follows the rule every
 * vendor instrument follows, a hash of its provider, scheme, id and field, so a
 * rerun writes the same row.
 *
 * The source needs no scope: the simulator offers the fixing of a derived index
 * from the index's own config, and the route that names the provider demands the
 * item the instrument maps. A scope here would narrow the administrator's route
 * to the term fixing alone, and drop the observed rates the same source states.
 *
 * This script is idempotent.
 */

\echo '--- Computed Term Index Fixing Instruments ---'

create temporary table t_derived_fixing_instrument (
    provider_code text not null,
    id_scheme text not null,
    external_id text not null,
    field text not null,
    oresmd_uri text primary key
);

insert into t_derived_fixing_instrument (provider_code, id_scheme, external_id, field, oresmd_uri)
select d.provider_code, 'setting',
       split_part(split_part(d.oresmd_uri, '?', 1), '/', 4) || ' ' ||
           substring(d.oresmd_uri from 'name=([A-Za-z]+)') ||
           coalesce(' ' || substring(d.oresmd_uri from 'tenor=([0-9A-Z]+)'), ''),
       'value', d.oresmd_uri
from (values
    ('cme',    'oresmd://ir/USD?type=fixing&index=ibor&name=SOFR&tenor=1M'),
    ('banrep', 'oresmd://ir/COP?type=fixing&index=ibor&name=IBR&tenor=3M'),
    ('gpw',    'oresmd://ir/PLN?type=fixing&index=ibor&name=WIBOR&tenor=6M')
) as d(provider_code, oresmd_uri);

insert into ores_marketdata_market_data_vendor_instruments_tbl (
    id, tenant_id, version, provider_code, id_scheme, external_id, field, minted,
    deviation_note, modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select md5(t.provider_code || '|' || t.id_scheme || '|' || t.external_id || '|' || t.field)::uuid,
       ores_utility_system_tenant_id_fn(), 0, t.provider_code, t.id_scheme, t.external_id, t.field,
       false, '', current_user, current_user, 'system.initial_load',
       'The term index ' || t.provider_code || ' owns, which the platform computes',
       now(), ores_utility_infinity_timestamp_fn()
from t_derived_fixing_instrument t
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
    modified_by, performed_by, change_reason_code, change_commentary, valid_from, valid_to
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, v.id, t.oresmd_uri,
       timestamptz '2026-01-01 00:00:00+00', current_user, current_user, 'system.initial_load',
       'The term index ' || t.provider_code || ' owns, which the platform computes',
       now(), ores_utility_infinity_timestamp_fn()
from t_derived_fixing_instrument t
join ores_marketdata_market_data_vendor_instruments_tbl v
  on v.tenant_id = ores_utility_system_tenant_id_fn()
 and v.provider_code = t.provider_code
 and v.id_scheme = t.id_scheme
 and v.external_id = t.external_id
 and v.field = t.field
 and v.valid_to = ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_marketdata_market_data_mappings_tbl m
    where m.vendor_instrument_id = v.id
      and m.valid_to = ores_utility_infinity_timestamp_fn()
);

drop table t_derived_fixing_instrument;

-- Summary
select 'computed_fixing_instruments' as entity, count(*) as count
from ores_marketdata_market_data_vendor_instruments_tbl
where provider_code in ('cme', 'banrep', 'gpw')
  and id_scheme = 'setting'
  and valid_to = ores_utility_infinity_timestamp_fn();
