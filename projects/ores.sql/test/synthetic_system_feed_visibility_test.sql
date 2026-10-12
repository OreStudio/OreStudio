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
 * System feeds are read by every tenant and written by the system tenant only.
 *
 * A system collection belongs to the system tenant and its system party, and
 * so do its feeds and their mixtures. A tenant reads them; it reads no feed of
 * a party collection the system tenant holds, and it cannot change a system
 * feed.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/synthetic_system_feed_visibility_test.sql
 */

begin;

select plan(5);

create temporary table t_ids on commit drop as
select
    '7a000000-0000-4000-8000-000000000001'::uuid as system_collection,
    '7a000000-0000-4000-8000-000000000002'::uuid as party_collection,
    '7a000000-0000-4000-8000-000000000003'::uuid as system_feed,
    '7a000000-0000-4000-8000-000000000004'::uuid as party_feed,
    '7a000000-0000-4000-8000-000000000005'::uuid as system_component;

create temporary table t_other on commit drop as
select id from ores_iam_tenants_tbl
where id <> ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
  and status = 'active'
  and type <> 'automation'
order by id
limit 1;

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_system_party on commit drop as
select id from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and party_category = 'System'
  and valid_to = ores_utility_infinity_timestamp_fn()
limit 1;

insert into ores_synthetic_market_data_generation_configs_tbl (
    id, tenant_id, version, party_id, scope, binding_mode, name, description,
    enabled, dataset_id, provider_code, modified_by, performed_by,
    change_reason_code, change_commentary
)
select i.system_collection, ores_utility_system_tenant_id_fn(), 0, p.id, 'system',
       'sandboxed', 'visibility_test_system', 'test', false, null::uuid, 'generic',
       current_user, current_user, 'system.test', 'visibility test'
from t_ids i, t_system_party p
union all
select i.party_collection, ores_utility_system_tenant_id_fn(), 0, p.id, 'party',
       'sandboxed', 'visibility_test_party', 'test', false, null::uuid, null::text,
       current_user, current_user, 'system.test', 'visibility test'
from t_ids i, t_system_party p;

insert into ores_synthetic_fx_spot_generation_configs_tbl (
    id, tenant_id, version, party_id, config_id, base_currency_code,
    quote_currency_code, source_name, ore_key, price_source, gmm_initial_price,
    ticks_per_hour, process_type, enabled, auto_start, vintage_source,
    vintage_date, folder_id, modified_by, performed_by, change_reason_code,
    change_commentary
)
select i.system_feed, ores_utility_system_tenant_id_fn(), 0, p.id, i.system_collection,
       'EUR', 'USD', 'synthetic.generic.visibility.eurusd', 'FX/RATE/EUR/USD', 'fixed', 1.1,
       60, 'geometric', false, false, '', '', null::uuid,
       current_user, current_user, 'system.test', 'visibility test'
from t_ids i, t_system_party p
union all
select i.party_feed, ores_utility_system_tenant_id_fn(), 0, p.id, i.party_collection,
       'GBP', 'USD', 'synthetic.visibility.gbpusd', 'FX/RATE/GBP/USD', 'fixed', 1.3,
       60, 'geometric', false, false, '', '', null::uuid,
       current_user, current_user, 'system.test', 'visibility test'
from t_ids i, t_system_party p;

insert into ores_synthetic_gmm_components_tbl (
    id, tenant_id, version, party_id, fx_spot_config_id, component_index,
    description, mean, stdev, weight, modified_by, performed_by,
    change_reason_code, change_commentary
)
select i.system_component, ores_utility_system_tenant_id_fn(), 0, p.id, i.system_feed, 0,
       'calm', 0, 0.001, 1, current_user, current_user, 'system.test', 'visibility test'
from t_ids i, t_system_party p;

select set_config('app.current_tenant_id', coalesce((select id::text from t_other),
    ores_utility_system_tenant_id_fn()::text), true);

select case when not exists(select 1 from t_other)
    then skip('no other active tenant is provisioned', 5)
    else collect_tap(
        ok(exists(select 1 from ores_synthetic_market_data_generation_configs_tbl
                  where id = (select system_collection from t_ids)),
           'a tenant reads a system collection'),
        ok(exists(select 1 from ores_synthetic_fx_spot_generation_configs_tbl
                  where id = (select system_feed from t_ids)),
           'a tenant reads a system collection''s feed'),
        ok(exists(select 1 from ores_synthetic_gmm_components_tbl
                  where id = (select system_component from t_ids)),
           'a tenant reads a system feed''s mixture'),
        ok(not exists(select 1 from ores_synthetic_fx_spot_generation_configs_tbl
                      where id = (select party_feed from t_ids)),
           'a tenant reads no feed of a party collection the system tenant holds'),
        throws_ok(
            format($$insert into ores_synthetic_fx_spot_generation_configs_tbl (
                id, tenant_id, version, party_id, config_id, base_currency_code,
                quote_currency_code, source_name, ore_key, price_source, gmm_initial_price,
                ticks_per_hour, process_type, enabled, auto_start, vintage_source,
                vintage_date, folder_id, modified_by, performed_by, change_reason_code,
                change_commentary
            ) values (
                gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, %L::uuid, %L::uuid,
                'USD', 'JPY', 'synthetic.generic.visibility.usdjpy', 'FX/RATE/USD/JPY', 'fixed', 150,
                60, 'geometric', false, false, '', '', null::uuid,
                current_user, current_user, 'system.test', 'visibility test')$$,
                (select id from t_system_party), (select system_collection from t_ids)),
            -- Refused whichever check fires first: row-level security
            -- (42501), or the insert trigger, which resolves the row to the
            -- acting tenant, where the system collection does not exist
            -- (23503). The error code says which ran first, not whether the
            -- write is allowed.
            null,
            null,
            'a tenant cannot write a feed into a system collection'))
    end;

select * from finish();

rollback;
