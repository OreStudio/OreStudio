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
 * The adapter and supplier catalogues publish from DQ into the system tenant.
 *
 * Both catalogues are installation data: their datasets are members of the
 * system_core bundle, and each publish refuses any other target, so a tenant's
 * own setup never writes them. A publish over a catalogue updates every row the
 * dataset stages.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/marketdata_catalogue_publish_test.sql
 */

begin;

select plan(9);

create temporary table t_adapters_dataset on commit drop as
select id from ores_dq_datasets_tbl
where code = 'marketdata.adapters'
  and valid_to = ores_utility_infinity_timestamp_fn();

create temporary table t_suppliers_dataset on commit drop as
select id from ores_dq_datasets_tbl
where code = 'marketdata.suppliers'
  and valid_to = ores_utility_infinity_timestamp_fn();

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- A database the system provisioning step has not run on holds no catalogue
-- rows yet, so the test publishes both once first.
select lives_ok(
    format($$select * from ores_marketdata_publish_adapters_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb)$$,
        (select id from t_adapters_dataset)),
    'the adapter catalogue publishes into the system tenant'
);

select lives_ok(
    format($$select * from ores_marketdata_publish_suppliers_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb)$$,
        (select id from t_suppliers_dataset)),
    'the supplier catalogue publishes into the system tenant'
);

select throws_ok(
    format($$select * from ores_marketdata_publish_adapters_from_dq_fn(%L::uuid, gen_random_uuid(), 'upsert', '{}'::jsonb)$$,
        (select id from t_adapters_dataset)),
    'P0001',
    null,
    'the adapter catalogue is not published into a tenant other than the system tenant'
);

select throws_ok(
    format($$select * from ores_marketdata_publish_suppliers_from_dq_fn(%L::uuid, gen_random_uuid(), 'upsert', '{}'::jsonb)$$,
        (select id from t_suppliers_dataset)),
    'P0001',
    null,
    'the supplier catalogue is not published into a tenant other than the system tenant'
);

select results_eq(
    format($$select action, record_count from ores_marketdata_publish_adapters_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'insert_only', '{}'::jsonb)$$,
        (select id from t_adapters_dataset)),
    format($$values ('skipped'::text, %s::bigint)$$,
        (select count(*) from ores_dq_adapters_artefact_tbl
         where dataset_id = (select id from t_adapters_dataset))),
    'an insert-only publish over the adapter catalogue skips every adapter the dataset stages'
);

select results_eq(
    format($$select action, record_count from ores_marketdata_publish_suppliers_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'insert_only', '{}'::jsonb)$$,
        (select id from t_suppliers_dataset)),
    format($$values ('skipped'::text, %s::bigint)$$,
        (select count(*) from ores_dq_suppliers_artefact_tbl
         where dataset_id = (select id from t_suppliers_dataset))),
    'an insert-only publish over the supplier catalogue skips every supplier the dataset stages'
);

select is(
    (select count(*)::int from (
        select code from ores_dq_adapters_artefact_tbl
        where dataset_id = (select id from t_adapters_dataset)
        except
        select code from ores_marketdata_adapters_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and valid_to = ores_utility_infinity_timestamp_fn()) missing),
    0,
    'the system tenant holds every adapter the dataset stages'
);

select is(
    (select count(*)::int from (
        select code from ores_dq_suppliers_artefact_tbl
        where dataset_id = (select id from t_suppliers_dataset)
        except
        select code from ores_marketdata_suppliers_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and valid_to = ores_utility_infinity_timestamp_fn()) missing),
    0,
    'the system tenant holds every supplier the dataset stages'
);

-- The catalogues were published earlier in this same transaction. A
-- replace_all closes those rows; a close stamped with the transaction's
-- frozen start time would fall before their valid_from.
select lives_ok(
    format($$select * from ores_marketdata_publish_adapters_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'replace_all', '{}'::jsonb)$$,
        (select id from t_adapters_dataset)),
    'a replace-all publish closes adapter rows written earlier in the same transaction'
);

select * from finish();

rollback;
