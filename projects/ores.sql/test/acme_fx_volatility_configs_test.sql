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
 * Each ACME entity's staged curve configuration reads every surface the system calibrates
 * for a pair whose two currencies the entity builds a curve for, and the entity's
 * today's market names each one. Every FX volatility curve reads the published grid of
 * the pair's blueprint, under the pair's FX option convention, with the entity's own
 * overnight curves.
 *
 * The checks read the staged DQ rows, so they hold on a fresh database.
 *
 * Run with: pg_prove -d <database> test/acme_fx_volatility_configs_test.sql
 */

begin;

select plan(9);

create temporary view t_entity as
select substring(d.code from '^acme\.([a-z_]+)\.curve_configuration$') as entity,
       d.id as curve_dataset_id,
       (select t.id from ores_dq_datasets_tbl t
        where t.code = 'acme.' || substring(d.code from '^acme\.([a-z_]+)\.') || '.todays_market'
          and t.valid_to = ores_utility_infinity_timestamp_fn()) as market_dataset_id
from ores_dq_datasets_tbl d
where d.code ~ '^acme\.[a-z_]+\.curve_configuration$'
  and d.valid_to = ores_utility_infinity_timestamp_fn();

create temporary view t_blueprint as
select b.* from ores_dq_fx_vol_surface_blueprints_artefact_tbl b
join ores_dq_datasets_tbl d on d.id = b.dataset_id
 and d.code = 'refdata.fx_vol_surface_blueprints'
 and d.valid_to = ores_utility_infinity_timestamp_fn();

create temporary view t_volatility as
select e.entity, d.curve_id, c.*
from t_entity e
join ores_dq_curve_definitions_artefact_tbl d
  on d.dataset_id = e.curve_dataset_id and d.section_code = 'FXVolatilities'
join ores_dq_fx_volatility_configs_artefact_tbl c
  on c.dataset_id = e.curve_dataset_id and c.curve_definition_id = d.id;

create temporary view t_expected as
select e.entity, b.base_currency_code || b.quote_currency_code as curve_id
from t_entity e
cross join t_blueprint b
where exists (select 1 from ores_dq_curve_definitions_artefact_tbl y
              where y.dataset_id = e.curve_dataset_id and y.section_code = 'YieldCurves'
                and y.curve_id = b.base_currency_code || '1D')
  and exists (select 1 from ores_dq_curve_definitions_artefact_tbl y
              where y.dataset_id = e.curve_dataset_id and y.section_code = 'YieldCurves'
                and y.curve_id = b.quote_currency_code || '1D');

select is((select count(*)::int from t_entity), 4, 'four ACME entities stage a curve configuration');

select set_eq(
    $$select entity, curve_id from t_volatility$$,
    $$select entity, curve_id from t_expected$$,
    'each entity reads the surface of every quoted pair it builds both curves for, and no other');

select is(
    (select count(*)::int from t_expected), 22,
    'the entities read 22 surfaces: three, ten, six and three');

select is(
    (select count(*)::int from t_volatility v
     join t_blueprint b on b.base_currency_code || b.quote_currency_code = v.curve_id
     where v.dimension <> 'Smile'
        or v.expiries is distinct from b.output_expiries
        or v.conventions is distinct from b.fx_option_convention_id
        or v.fx_spot_id is distinct from 'FX/' || b.base_currency_code || '/' || b.quote_currency_code),
    0,
    'every curve reads the blueprint''s expiries under its convention, on the pair''s spot');

select is(
    (select count(*)::int from t_volatility v
     join t_blueprint b on b.base_currency_code || b.quote_currency_code = v.curve_id
     where v.deltas is distinct from (
         select string_agg(case when s = 'ATM' then 'ATM'
                                else split_part(s, '/', 4) || left(split_part(s, '/', 3), 1) end,
                           ',' order by n)
         from unnest(string_to_array(b.output_strikes, ',')) with ordinality as g (s, n))),
    0,
    'every curve reads one delta for each strike of the blueprint''s grid, in its order');

select is(
    (select count(*)::int from t_volatility v
     where (select count(*) from ores_dq_fx_volatility_smiles_artefact_tbl s
            where s.dataset_id = v.dataset_id and s.fx_volatility_config_id = v.id
              and s.smile_type = 'Delta') <> 1),
    0,
    'every curve has one smile, of smile type Delta');

select is(
    (select count(*)::int from t_volatility v
     where not exists (
         select 1 from ores_dq_fx_option_conventions_artefact_tbl c
         join ores_dq_datasets_tbl d on d.id = c.dataset_id and d.code = 'refdata.conventions'
          and d.valid_to = ores_utility_infinity_timestamp_fn()
         where c.id = v.conventions)),
    0,
    'every convention a curve names is in refdata.conventions');

select is(
    (select count(*)::int from t_volatility v
     join t_blueprint b on b.base_currency_code || b.quote_currency_code = v.curve_id
     where v.fx_foreign_curve_id is distinct from
               'Yield/' || b.base_currency_code || '/' || b.base_currency_code || '1D'
        or v.fx_domestic_curve_id is distinct from
               'Yield/' || b.quote_currency_code || '/' || b.quote_currency_code || '1D'),
    0,
    'every curve turns deltas into strikes on the entity''s own overnight curves');

select set_eq(
    $$select e.entity, x.key_value, x.target
      from t_entity e
      join ores_dq_todays_market_collections_artefact_tbl c
        on c.dataset_id = e.market_dataset_id and c.collection = 'FxVolatilities'
      join ores_dq_todays_market_entries_artefact_tbl x
        on x.dataset_id = c.dataset_id and x.todays_market_collection_id = c.id$$,
    $$select entity, curve_id,
             'FXVolatility/' || left(curve_id, 3) || '/' || right(curve_id, 3) || '/' || curve_id
      from t_volatility$$,
    'each entity''s today''s market names every curve under its pair, and nothing else');

select * from finish();

rollback;
