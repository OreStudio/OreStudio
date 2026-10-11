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
 * The market data providers publish from DQ into the system tenant only.
 *
 * The providers are the installation's: the dataset is a member of the
 * system_core bundle, and the publish refuses any other target, so a tenant's
 * own setup never writes the catalogue. A publish over the catalogue updates
 * every provider the dataset stages.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/synthetic_market_data_providers_publish_test.sql
 */

begin;

select plan(3);

create temporary table t_dataset on commit drop as
select id from ores_dq_datasets_tbl
where code = 'synthetic.market_data_providers'
  and valid_to = ores_utility_infinity_timestamp_fn();

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- A database the system provisioning step has not run on holds no
-- providers yet, so the test publishes them once first.
create temporary table t_first_publish on commit drop as
select * from ores_synthetic_publish_market_data_providers_from_dq_fn(
    (select id from t_dataset), ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

select throws_ok(
    format($$select * from ores_synthetic_publish_market_data_providers_from_dq_fn(%L::uuid, gen_random_uuid(), 'upsert', '{}'::jsonb)$$,
        (select id from t_dataset)),
    'P0001',
    null,
    'the providers are not published into a tenant other than the system tenant'
);

select results_eq(
    format($$select action, record_count from ores_synthetic_publish_market_data_providers_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'insert_only', '{}'::jsonb)$$,
        (select id from t_dataset)),
    $$values ('skipped'::text, 8::bigint)$$,
    'an insert-only publish over the catalogue skips every provider it already holds'
);

select is(
    (select count(*)::int from ores_synthetic_market_data_providers_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    8,
    'the system tenant holds the eight providers the dataset stages'
);

select * from finish();

rollback;
