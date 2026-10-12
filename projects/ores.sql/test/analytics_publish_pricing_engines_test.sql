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
 * pgTAP tests for the pricing engines publish.
 *
 * A dataset stages a pricing engines document as a header, a product and a
 * parameter. The publish writes it for the party a publish names:
 * - The three live tables and the reporting configuration receive their rows
 * - Every uuid is re-keyed for the party, and the rows of the document still
 *   point at each other
 * - The reporting configuration is the one the header names
 * - A repeat publish adds nothing
 * - The same staged id is a different live id for another party
 *
 * Run with: pg_prove -d <database> test/analytics_publish_pricing_engines_test.sql
 */

begin;

select plan(10);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       '00000000-0000-0000-0000-0000000d0001'::uuid as header_id,
       '00000000-0000-0000-0000-0000000d0002'::uuid as product_id,
       '00000000-0000-0000-0000-0000000d0003'::uuid as parameter_id,
       '00000000-0000-0000-0000-0000000d0004'::uuid as configuration_id;

-- The dataset the document is staged under.
do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.pricing_engines', 'ACME Corporation', 'Reporting', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'OreStudio Code Generation Methodology',
        'Test Pricing Engines', 'A pricing engines document staged for a test.',
        'ACMEBANK', 'pgTAP fixture', current_date, 'Internal Use Only', 'pricing_engines');
end $$;

create temp table t_dataset on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.pricing_engines' and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_dq_pricing_model_configs_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, name, description, config_variant,
    configuration_id)
select d.id, ores_utility_system_tenant_id_fn(), c.header_id, 0, c.party_id,
    'Test pricing engines', 'A pricing engines document', 'default', c.configuration_id
from t_dataset d, t_ctx c;

insert into ores_dq_pricing_model_products_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, pricing_model_config_id,
    pricing_engine_type_code, model, engine, position)
select d.id, ores_utility_system_tenant_id_fn(), c.product_id, 0, c.party_id, c.header_id,
    (select code from ores_analytics_pricing_engine_types_tbl
     where valid_to = ores_utility_infinity_timestamp_fn() order by code limit 1),
    'DiscountedCashflows', 'DiscountingSwapEngine', 0
from t_dataset d, t_ctx c;

insert into ores_dq_pricing_model_product_parameters_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, pricing_model_config_id,
    pricing_model_product_id, parameter_scope, parameter_name, parameter_value, position)
select d.id, ores_utility_system_tenant_id_fn(), c.parameter_id, 0, c.party_id, c.header_id,
    c.product_id, 'product', 'Discount', 'OIS', 0
from t_dataset d, t_ctx c;

create or replace function pg_temp.publish() returns table (action text, record_count bigint) as $$
    select p.action, p.record_count
    from t_dataset d, t_ctx c,
         ores_analytics_publish_pricing_engines_from_dq_fn(
             d.id, ores_utility_system_tenant_id_fn(),
             'upsert', jsonb_build_object('party_id', c.party_id::text)) p
    order by p.action;
$$ language sql;

select results_eq(
    $$select * from pg_temp.publish()$$,
    $$values ('inserted'::text, 3::bigint)$$,
    'the publish adds the header, the product and the parameter');

create temp view t_live_header as
select h.*
from ores_analytics_pricing_model_configs_tbl h, t_ctx c
where h.tenant_id = ores_utility_system_tenant_id_fn() and h.party_id = c.party_id
  and h.name = 'Test pricing engines' and h.valid_to = ores_utility_infinity_timestamp_fn();

select is((select count(*) from t_live_header), 1::bigint, 'the header is published once');

select is(
    (select h.id from t_live_header h),
    ores_dq_party_uuid_fn((select party_id from t_ctx), (select header_id from t_ctx)),
    'the header is re-keyed for the party');

select is(
    (select count(*) from ores_analytics_pricing_model_products_tbl p, t_live_header h
     where p.tenant_id = ores_utility_system_tenant_id_fn()
       and p.pricing_model_config_id = h.id
       and p.valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the product still points at the re-keyed header');

select is(
    (select count(*) from ores_analytics_pricing_model_product_parameters_tbl p,
         t_live_header h, ores_analytics_pricing_model_products_tbl pr
     where p.tenant_id = ores_utility_system_tenant_id_fn()
       and p.pricing_model_config_id = h.id
       and pr.id = p.pricing_model_product_id
       and pr.pricing_model_config_id = h.id
       and p.valid_to = ores_utility_infinity_timestamp_fn()
       and pr.valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the parameter still points at the re-keyed header and product');

select is(
    (select count(*) from ores_reporting_configurations_tbl r, t_live_header h
     where r.tenant_id = ores_utility_system_tenant_id_fn()
       and r.id = h.configuration_id
       and r.configuration_type_code = 'pricing_engines'
       and r.owning_component = 'ores.analytics'
       and r.name = 'Test pricing engines'
       and r.valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the reporting configuration is the one the header names');

select is(
    (select h.configuration_id from t_live_header h),
    ores_dq_party_uuid_fn((select party_id from t_ctx), (select configuration_id from t_ctx)),
    'the reporting configuration id is re-keyed for the party');

select results_eq(
    $$select * from pg_temp.publish()$$,
    $$values ('skipped'::text, 3::bigint)$$,
    'a repeat publish adds nothing');

select is(
    (select count(*) from t_live_header),
    1::bigint,
    'a repeat publish leaves one header');

select isnt(
    ores_dq_party_uuid_fn((select party_id from t_ctx), (select header_id from t_ctx)),
    ores_dq_party_uuid_fn('00000000-0000-0000-0000-00000000aaaa'::uuid,
                          (select header_id from t_ctx)),
    'the same staged id is a different live id for another party');

select * from finish();
rollback;
