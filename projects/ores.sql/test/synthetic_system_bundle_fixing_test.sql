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
 * The system bundle installs the feeds that start from the public fixings.
 *
 * synthetic.feeds.bloomberg starts every pair from the newest public fixing of
 * the pair, so each config is fixing-priced and names no items of its own: the
 * FX calibration policy derives its candidates. A publish into the system tenant names no party: the
 * feeds belong to the system party, where the simulator, which reads as the
 * system tenant, finds them. The catalogue also holds the pair of provider and
 * source that the Acme profile routes its parties to.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/synthetic_system_bundle_fixing_test.sql
 */

begin;

select plan(5);

create temporary table t_dataset on commit drop as
select id, code from ores_dq_datasets_tbl
where code in ('synthetic.feeds.bloomberg', 'synthetic.market_data_providers')
  and valid_to = ores_utility_infinity_timestamp_fn();

create temporary table t_party on commit drop as
select id from ores_refdata_read_system_party_fn(ores_utility_system_tenant_id_fn());

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids', '{' || (select id::text from t_party) || '}', true);

create temporary table t_providers_published on commit drop as
select * from ores_synthetic_publish_market_data_providers_from_dq_fn(
    (select id from t_dataset where code = 'synthetic.market_data_providers'),
    ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

select lives_ok(
    format($$select * from ores_synthetic_publish_feeds_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb)$$,
        (select id from t_dataset where code = 'synthetic.feeds.bloomberg')),
    'the Bloomberg feeds publish into the system tenant'
);

select is(
    (select count(*)::int from ores_synthetic_fx_spot_generation_configs_tbl f
     join ores_synthetic_market_data_generation_configs_tbl c on c.id = f.config_id
     where c.provider_code = 'bloomberg'
       and f.price_source = 'fixing'
       and f.gmm_initial_price > 0
       and f.valid_to = ores_utility_infinity_timestamp_fn()
       and c.valid_to = ores_utility_infinity_timestamp_fn()),
    26,
    'every Bloomberg pair is fixing-priced and keeps a level to fall back to'
);

select is(
    (select p.window_days || ' ' || p.allow_inverse || ' ' || p.fallback_code || ' '
            || string_agg(o.provider_code, ',' order by o.rank)
     from ores_synthetic_calibration_policies_tbl p
     join ores_synthetic_calibration_provider_orders_tbl o on o.policy_id = p.id
     where p.asset_class_code = 'fx'
       and o.valid_to = ores_utility_infinity_timestamp_fn()
       and p.valid_to = ores_utility_infinity_timestamp_fn()
     group by p.window_days, p.allow_inverse, p.fallback_code),
    '90 true keep_fixed ecb,imf,boc,tcmb',
    'the FX policy reads ECB, IMF, Bank of Canada then CBRT over 90 days, allows an inverse, and keeps the fixed level'
);

select ok(
    not exists(select 1 from ores_synthetic_fx_spot_fixing_items_tbl i
               join ores_synthetic_fx_spot_generation_configs_tbl f on f.id = i.fx_spot_config_id
               join ores_synthetic_market_data_generation_configs_tbl c on c.id = f.config_id
               where c.provider_code = 'bloomberg'
                 and i.valid_to = ores_utility_infinity_timestamp_fn()
                 and f.valid_to = ores_utility_infinity_timestamp_fn()),
    'no Bloomberg feed names items of its own, so each derives its candidates from the policy'
);

select ok(
    exists(select 1 from ores_marketdata_feeds_tbl s
           where s.name = 'bloomberg-a' and s.provider_code = 'bloomberg'
             and s.valid_to = ores_utility_infinity_timestamp_fn()),
    'the catalogue holds the source bloomberg-a of the provider bloomberg that the Acme profile routes to'
);

select * from finish();

rollback;
