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
 * The refdata.fx_vol_surface_blueprints dataset publishes a whole blueprint for every
 * pair the derived source quotes: every expiry can be fitted, every pillar source names
 * the series the derived source writes, each output strike quotes the delta type of the
 * pair's FX option convention, and the output series states a calibrated surface. A
 * second publish writes nothing.
 *
 * Run with: pg_prove -d <database> test/refdata_fx_vol_surface_blueprints_publish_test.sql
 */

begin;

select plan(13);

create temporary table t_party on commit drop as
select id from ores_refdata_read_system_party_fn(ores_utility_system_tenant_id_fn());

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids', '{' || (select id::text from t_party) || '}', true);

create temporary table t_dataset on commit drop as
select id, code from ores_dq_datasets_tbl
where code in ('refdata.fx_vol_surface_blueprints', 'ore.conventions')
  and valid_to = ores_utility_infinity_timestamp_fn();

select lives_ok(
    format($$select * from ores_refdata_publish_fx_vol_surface_blueprints_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb)$$,
        (select id from t_dataset where code = 'refdata.fx_vol_surface_blueprints')),
    'the blueprints dataset publishes into the system tenant');

create temporary view t_derived_pairs as
select distinct substring(v.external_id from 1 for 3) as base,
                substring(v.external_id from 4 for 3) as quote
from ores_marketdata_market_data_vendor_instruments_tbl v
where v.tenant_id = ores_utility_system_tenant_id_fn()
  and v.provider_code = 'derived'
  and v.id_scheme = 'derived_fx_vol'
  and v.valid_to = ores_utility_infinity_timestamp_fn();

create temporary view t_blueprints as
select * from ores_refdata_fx_vol_surface_blueprints_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();

create temporary view t_pillars as
select p.* from ores_refdata_fx_vol_surface_pillars_tbl p
join t_blueprints b on b.id = p.blueprint_id
where p.valid_to = ores_utility_infinity_timestamp_fn();

create temporary view t_sources as
select s.*, p.blueprint_id, p.expiry_code, p.strike_label
from ores_refdata_fx_vol_surface_pillar_sources_tbl s
join t_pillars p on p.id = s.pillar_id
where s.valid_to = ores_utility_infinity_timestamp_fn();

select is((select count(*)::int from t_derived_pairs), 10, 'the derived source quotes ten pairs');

select set_eq(
    $$select base, quote from t_derived_pairs$$,
    $$select base_currency_code, quote_currency_code from t_blueprints$$,
    'every pair the derived source quotes has one blueprint, and no other pair has one');

select is(
    (select count(*)::int from t_blueprints
     where party_id <> (select id from t_party) or smile_model <> 'ssvi'
        or name <> base_currency_code || quote_currency_code || '_SSVI'),
    0,
    'every blueprint is an SSVI recipe of the system party, named for its pair');

select is(
    (select count(*)::int from t_blueprints b
     where (select count(*) from t_pillars p where p.blueprint_id = b.id) <> 35),
    0,
    'every blueprint has 35 pillars');

select is(
    (select count(*)::int from (
        select p.blueprint_id, p.expiry_code from t_pillars p
        group by 1, 2
        having count(*) filter (where p.strike_label in ('ATM', '25RR', '25BF', '10RR', '10BF')) <> 5) e),
    0,
    'each expiry of each blueprint has an ATM, and a risk reversal and a butterfly at both deltas');

select is(
    (select count(*)::int from t_pillars p
     where (select count(*) from t_sources s where s.pillar_id = p.id) <> 1),
    0,
    'each pillar has one source');

select is(
    (select count(*)::int from t_sources s
     join t_blueprints b on b.id = s.blueprint_id
     left join ores_marketdata_market_series_tbl m
       on m.tenant_id = s.tenant_id and m.id = s.market_series_id
      and m.valid_to = ores_utility_infinity_timestamp_fn()
     where s.contributor_code <> 'derived'
        or m.provider_code is distinct from 'derived'
        or m.oresmd_uri is distinct from 'oresmd://fx/' || b.base_currency_code
            || '?type=series&instrument=fx_option&quote=rate_lnvol&ccy=' || b.quote_currency_code),
    0,
    'every source names the series the derived source writes for its pair');

select is(
    (select count(*)::int from t_blueprints b
     left join ores_marketdata_market_series_tbl m
       on m.tenant_id = b.tenant_id and m.id = b.output_series_id
      and m.valid_to = ores_utility_infinity_timestamp_fn()
     where m.oresmd_uri is distinct from 'oresmd://fx/' || b.base_currency_code
            || '?type=series&instrument=fx_vol_surface&quote=rate_lnvol&ccy='
            || b.quote_currency_code || '&surface_id=' || b.name),
    0,
    'every output series states the calibrated surface of its blueprint');

select is(
    (select count(*)::int from t_blueprints b
     left join ores_dq_fx_option_conventions_artefact_tbl c
       on c.dataset_id = (select id from t_dataset where code = 'ore.conventions')
      and c.id = b.fx_option_convention_id
     where c.delta_type is null
        or b.output_strikes <> format('DEL/%1$s/Put/10,DEL/%1$s/Put/25,ATM,DEL/%1$s/Call/25,DEL/%1$s/Call/10',
                                      c.delta_type)),
    0,
    'every output strike quotes the delta type of the pair''s FX option convention in ore.conventions');

select is(
    (select count(*)::int from t_pillars p
     join t_blueprints b on b.id = p.blueprint_id
     where not exists (
         select 1 from ores_marketdata_market_data_vendor_instruments_tbl v
         where v.tenant_id = p.tenant_id
           and v.provider_code = 'derived'
           and v.id_scheme = 'derived_fx_vol'
           and v.external_id = b.base_currency_code || b.quote_currency_code || '/'
               || p.expiry_code || '/' || p.strike_label
           and v.valid_to = ores_utility_infinity_timestamp_fn())),
    0,
    'the derived source publishes the quote of every pillar');

select is(
    (select count(*)::int from t_derived_pairs d
     where (select count(*) from ores_marketdata_market_series_tbl m
            where m.tenant_id = ores_utility_system_tenant_id_fn()
              and m.party_id = (select id from t_party)
              and m.provider_code = 'derived'
              and m.oresmd_uri = 'oresmd://fx/' || d.base
                  || '?type=series&instrument=fx_option&quote=rate_lnvol&ccy=' || d.quote
              and m.valid_to = ores_utility_infinity_timestamp_fn()) <> 1),
    0,
    'the derived feed has one series to find for each pair');

select is(
    (select coalesce(sum(record_count), 0)::int
     from ores_refdata_publish_fx_vol_surface_blueprints_from_dq_fn(
         (select id from t_dataset where code = 'refdata.fx_vol_surface_blueprints'),
         ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb)
     where action = 'inserted'),
    0,
    'a second publish inserts nothing');

select * from finish();

rollback;
