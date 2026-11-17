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
 * The Derived Source's Quotes
 *
 * The vendor instruments the derived source serves, their mappings to fx_option items
 * and the feed's scopes. They are read from the published blueprints of the FX
 * volatility surfaces: every pillar a blueprint reads from the derived source is a quote
 * the source serves, so the ladder lives in one place, the blueprint dataset. This runs
 * after the refdata layer, which stages those blueprints.
 *
 * This script is idempotent.
 */

\echo '--- Derived Source: Vendor Instruments, Mappings and Scopes ---'

-- A vendor id names a pair, an expiry and a label, such as EURUSD/1Y/25RR, in the
-- derived source's own notation. Each maps to the fx_option quote of the pair in its
-- base/quote order. The quotes are the pillars the published blueprints read from the
-- derived source, so the ladder a pair is quoted at is the one its blueprint states.
create temporary table t_derived_inventory (
    external_id text, oresmd_uri text
) on commit drop;

insert into t_derived_inventory (external_id, oresmd_uri)
select distinct b.base_currency_code || b.quote_currency_code || '/' || p.expiry_code || '/'
           || p.strike_label,
       'oresmd://fx/' || b.base_currency_code
           || '?type=quote&instrument=fx_option&quote=rate_lnvol&ccy=' || b.quote_currency_code
           || '&expiry=' || p.expiry_code || '&strike_label=' || p.strike_label
from ores_dq_fx_vol_surface_pillars_artefact_tbl p
join ores_dq_fx_vol_surface_blueprints_artefact_tbl b
  on b.dataset_id = p.dataset_id and b.id = p.blueprint_id
join ores_dq_fx_vol_surface_pillar_sources_artefact_tbl s
  on s.dataset_id = p.dataset_id and s.pillar_id = p.id
where s.contributor_code = 'derived';

insert into ores_marketdata_market_data_vendor_instruments_tbl (
    id, tenant_id, version, provider_code, id_scheme, external_id, field, minted, deviation_note,
    modified_by, performed_by, change_reason_code, change_commentary
)
select md5('derived|derived_fx_vol|' || t.external_id || '|rate_lnvol')::uuid,
       ores_utility_system_tenant_id_fn(), 0, 'derived', 'derived_fx_vol', t.external_id,
       'rate_lnvol', false, '', current_user, current_user, 'system.initial_load',
       'Initial population of the derived source'
from t_derived_inventory t
where not exists (
    select 1 from ores_marketdata_market_data_vendor_instruments_tbl v
    where v.tenant_id = ores_utility_system_tenant_id_fn()
      and v.provider_code = 'derived'
      and v.id_scheme = 'derived_fx_vol'
      and v.external_id = t.external_id
      and v.field = 'rate_lnvol'
      and v.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_market_data_mappings_tbl (
    id, tenant_id, version, vendor_instrument_id, oresmd_uri, effective_from,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, v.id, t.oresmd_uri,
       timestamptz '2026-01-01 00:00:00+00', current_user, current_user, 'system.initial_load',
       'Initial population of the derived source'
from t_derived_inventory t
join ores_marketdata_market_data_vendor_instruments_tbl v
  on v.tenant_id = ores_utility_system_tenant_id_fn()
 and v.provider_code = 'derived'
 and v.id_scheme = 'derived_fx_vol'
 and v.external_id = t.external_id
 and v.field = 'rate_lnvol'
 and v.valid_to = ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_marketdata_market_data_mappings_tbl m
    where m.tenant_id = ores_utility_system_tenant_id_fn()
      and m.vendor_instrument_id = v.id
      and m.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_feed_scopes_tbl (
    id, tenant_id, version, feed_id, asset_class, item, vendor_instrument_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
       'afd34349-1c6c-56b2-a686-97b623ee051a', 'fx', m.oresmd_uri, v.id,
       current_user, current_user, 'system.initial_load', 'Initial population of the derived source'
from ores_marketdata_market_data_vendor_instruments_tbl v
join ores_marketdata_market_data_mappings_tbl m
  on m.vendor_instrument_id = v.id
 and m.tenant_id = ores_utility_system_tenant_id_fn()
 and m.valid_to = ores_utility_infinity_timestamp_fn()
where v.tenant_id = ores_utility_system_tenant_id_fn()
  and v.provider_code = 'derived'
  and v.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (
      select 1 from ores_marketdata_feed_scopes_tbl s
      where s.tenant_id = ores_utility_system_tenant_id_fn()
        and s.feed_id = 'afd34349-1c6c-56b2-a686-97b623ee051a'
        and s.item = m.oresmd_uri
        and s.valid_to = ores_utility_infinity_timestamp_fn()
  );
