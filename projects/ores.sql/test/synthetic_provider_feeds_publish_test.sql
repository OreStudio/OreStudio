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
 * A provider's feeds dataset publishes as a system collection.
 *
 * synthetic.feeds.refinitiv, published into the system tenant with no party named, becomes
 * a sandboxed system collection held by the system party, with its folders and its feeds.
 * Another tenant reads the collection, its folders and its feeds, and a dataset published
 * into a tenant with no party is refused.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/synthetic_provider_feeds_publish_test.sql
 */

begin;

select plan(9);

create temporary table t_dataset on commit drop as
select id, code from ores_dq_datasets_tbl
where code in ('synthetic.feeds.refinitiv', 'synthetic.market_data_providers')
  and valid_to = ores_utility_infinity_timestamp_fn();

create temporary table t_other on commit drop as
select id from ores_iam_tenants_tbl
where id <> ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
  and status = 'active'
  and type <> 'automation'
order by id
limit 1;

create temporary table t_system_party on commit drop as
select id from ores_refdata_read_system_party_fn(ores_utility_system_tenant_id_fn());

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids', '{' || (select id::text from t_system_party) || '}', true);

create temporary table t_providers_published on commit drop as
select * from ores_synthetic_publish_market_data_providers_from_dq_fn(
    (select id from t_dataset where code = 'synthetic.market_data_providers'),
    ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

select lives_ok(
    format($$select * from ores_synthetic_publish_feeds_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb)$$,
        (select id from t_dataset where code = 'synthetic.feeds.refinitiv')),
    'the Refinitiv feeds dataset publishes into the system tenant with no party named'
);

create temporary table t_collection on commit drop as
select c.id, c.scope, c.binding_mode, c.provider_code, c.name, c.party_id
from ores_synthetic_market_data_generation_configs_tbl c
where c.tenant_id = ores_utility_system_tenant_id_fn()
  and c.dataset_id = (select id from t_dataset where code = 'synthetic.feeds.refinitiv')
  and c.valid_to = ores_utility_infinity_timestamp_fn();

select results_eq(
    $$select scope, binding_mode, provider_code, name from t_collection$$,
    $$values ('system'::text, 'sandboxed'::text, 'refinitiv'::text, 'Refinitiv'::text)$$,
    'the dataset publishes one sandboxed system collection that stands for Refinitiv'
);

select is(
    (select party_id from t_collection),
    (select id from t_system_party),
    'the system party holds the collection'
);

select is(
    (select count(*)::int from ores_synthetic_fx_spot_generation_configs_tbl
     where config_id = (select id from t_collection)
       and source_name like 'synthetic.refinitiv.%'
       and asset_class_code = 'fx'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    14,
    'the collection holds the fourteen cross rates, named for Refinitiv'
);

select is(
    (select f.name || ' > ' || a.name || ' > ' || c.name || ' > ' || r.name
     from ores_synthetic_fx_spot_generation_configs_tbl x
     join ores_synthetic_folders_tbl f on f.id = x.folder_id and f.valid_to = ores_utility_infinity_timestamp_fn()
     join ores_synthetic_folders_tbl a on a.id = f.parent_id and a.valid_to = ores_utility_infinity_timestamp_fn()
     join ores_synthetic_folders_tbl c on c.id = a.parent_id and c.valid_to = ores_utility_infinity_timestamp_fn()
     join ores_synthetic_folders_tbl r on r.id = c.parent_id and r.valid_to = ores_utility_infinity_timestamp_fn()
     where x.source_name = 'synthetic.refinitiv.eurgbp'
       and x.valid_to = ores_utility_infinity_timestamp_fn()),
    'FX Rates > FX > Refinitiv > Synthetic',
    'a feed sits in the folders of its collection under the system root'
);

select throws_ok(
    format($$select * from ores_synthetic_publish_feeds_from_dq_fn(%L::uuid, %L::uuid, 'upsert', '{}'::jsonb)$$,
        (select id from t_dataset where code = 'synthetic.feeds.refinitiv'),
        (select id from t_other)),
    null,
    null,
    'a dataset published into a tenant with no party named is refused'
);

select set_config('app.current_tenant_id', (select id::text from t_other), true);

select ok(
    exists(select 1 from ores_synthetic_market_data_generation_configs_tbl
           where id = (select id from t_collection)),
    'another tenant reads the system collection'
);

select is(
    (select count(*)::int from ores_synthetic_fx_spot_generation_configs_tbl
     where config_id = (select id from t_collection)
       and valid_to = ores_utility_infinity_timestamp_fn()),
    14,
    'another tenant reads its feeds'
);

select is(
    (select count(*)::int from ores_synthetic_folders_tbl f
     where f.tenant_id = ores_utility_system_tenant_id_fn()
       and f.valid_to = ores_utility_infinity_timestamp_fn()
       and f.name in ('Synthetic', 'Refinitiv', 'FX', 'FX Rates')),
    4,
    'another tenant reads the folders of the system collection, the root above it included'
);

select * from finish();

rollback;
