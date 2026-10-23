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
 * The system bundle installs the feeds that start from a vintage price.
 *
 * synthetic.feeds.bloomberg starts 13 of its pairs from the 2026-05-05 Fed H.10 vintage, so
 * the system bundle publishes the vintage observations with it. A publish into the system
 * tenant names no party: the observations belong to the system party, where the simulator,
 * which reads as the system tenant, finds them. The catalogue also holds the pair of
 * provider and source that the Acme profile routes its parties to.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/synthetic_system_bundle_vintage_test.sql
 */

begin;

select plan(5);

create temporary table t_dataset on commit drop as
select id, code from ores_dq_datasets_tbl
where code in ('marketdata.fx_driver_rates_2026', 'synthetic.feeds.bloomberg',
               'synthetic.market_data_providers')
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
    format($$select * from ores_marketdata_publish_market_data_observations_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb)$$,
        (select id from t_dataset where code = 'marketdata.fx_driver_rates_2026')),
    'the vintage observations publish into the system tenant with no party named'
);

select ok(
    exists(select 1 from ores_marketdata_market_observations_tbl o
           where o.tenant_id = ores_utility_system_tenant_id_fn()
             and o.party_id = (select id from t_party)),
    'the observations belong to the system party'
);

select lives_ok(
    format($$select * from ores_synthetic_publish_feeds_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb)$$,
        (select id from t_dataset where code = 'synthetic.feeds.bloomberg')),
    'the Bloomberg feeds publish into the system tenant after their vintage data'
);

select is(
    (select count(*)::int from ores_synthetic_fx_spot_generation_configs_tbl f
     join ores_synthetic_market_data_generation_configs_tbl c on c.id = f.config_id
     where c.provider_code = 'bloomberg'
       and f.price_source = 'vintage'
       and f.valid_to = ores_utility_infinity_timestamp_fn()
       and c.valid_to = ores_utility_infinity_timestamp_fn()),
    13,
    'the 13 vintage-priced pairs are installed in the system tenant'
);

select ok(
    exists(select 1 from ores_marketdata_market_data_sources_tbl s
           where s.name = 'ore-simulated' and s.provider_code = 'ore'
             and s.valid_to = ores_utility_infinity_timestamp_fn()),
    'the catalogue holds the source ore-simulated of the provider ore that the Acme profile routes to'
);

select * from finish();

rollback;
