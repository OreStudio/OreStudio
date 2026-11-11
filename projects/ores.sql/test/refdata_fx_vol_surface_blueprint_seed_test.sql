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
 * A fresh system tenant holds the EUR/USD surface blueprint, whole: every expiry can
 * be fitted, every pillar source names the series the derived source writes, and the
 * output series states a calibrated surface.
 *
 * Run with: pg_prove -d <database> test/refdata_fx_vol_surface_blueprint_seed_test.sql
 */

begin;

select plan(10);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    '{' || ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()) || '}', true);

create temporary view t_blueprint as
select * from ores_refdata_fx_vol_surface_blueprints_tbl
where name = 'EURUSD_SSVI' and valid_to = ores_utility_infinity_timestamp_fn();

create temporary view t_pillars as
select p.* from ores_refdata_fx_vol_surface_pillars_tbl p
join t_blueprint b on b.id = p.blueprint_id
where p.valid_to = ores_utility_infinity_timestamp_fn();

create temporary view t_sources as
select s.*, p.expiry_code, p.strike_label from ores_refdata_fx_vol_surface_pillar_sources_tbl s
join t_pillars p on p.id = s.pillar_id
where s.valid_to = ores_utility_infinity_timestamp_fn();

select is((select count(*)::int from t_blueprint), 1, 'one EUR/USD blueprint is seeded');

select is((select party_id from t_blueprint),
    ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn()),
    'the blueprint belongs to the system party');

select is((select smile_model from t_blueprint), 'ssvi', 'the blueprint fits SSVI');

select is((select count(*)::int from t_pillars), 35, 'the blueprint has every pillar of seven expiries');

select is(
    (select count(*)::int from (
        select expiry_code from t_pillars
        where strike_label in ('ATM', '25RR', '25BF')
        group by expiry_code having count(*) = 3) e),
    8,
    'each expiry has an ATM, a risk reversal and a butterfly at one delta');

select is(
    (select count(*)::int from t_pillars p
     where (select count(*) from t_sources s where s.pillar_id = p.id) <> 1),
    0,
    'each pillar has one source');

select is(
    (select count(*)::int from t_sources s
     join ores_marketdata_market_series_tbl m
       on m.tenant_id = s.tenant_id and m.id = s.market_series_id
      and m.valid_to = ores_utility_infinity_timestamp_fn()
     where m.oresmd_uri = 'oresmd://fx/EUR?type=series&instrument=fx_option&quote=rate_lnvol&ccy=USD'
       and m.provider_code = 'derived' and s.contributor_code = 'derived'),
    35,
    'every source names the series the derived source writes');

select is(
    (select m.oresmd_uri from ores_marketdata_market_series_tbl m
     join t_blueprint b on b.tenant_id = m.tenant_id and b.output_series_id = m.id
     where m.valid_to = ores_utility_infinity_timestamp_fn()),
    'oresmd://fx/EUR?type=series&instrument=fx_vol_surface&quote=rate_lnvol&ccy=USD&surface_id=EURUSD_SSVI',
    'the output series states a calibrated FX volatility surface');

select is(
    (select count(*)::int from ores_marketdata_market_series_tbl m
     where m.tenant_id = ores_utility_system_tenant_id_fn()
       and m.oresmd_uri = 'oresmd://fx/EUR?type=series&instrument=fx_option&quote=rate_lnvol&ccy=USD'
       and m.party_id = ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn())
       and m.valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'the derived feed has one series to find');

select is(
    (select count(*)::int from t_pillars p
     where not exists (
         select 1 from ores_marketdata_market_data_vendor_instruments_tbl v
         where v.tenant_id = p.tenant_id
           and v.provider_code = 'derived'
           and v.id_scheme = 'derived_fx_vol'
           and v.external_id = 'EURUSD/' || p.expiry_code || '/' || p.strike_label
           and v.valid_to = ores_utility_infinity_timestamp_fn())),
    0,
    'the derived source publishes the quote of every pillar');

select * from finish();

rollback;
