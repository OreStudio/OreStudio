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
 * pgTAP tests for the today's market and curve configuration publishes.
 *
 * A document that references its header through two levels, a collection and an
 * entry, and a document that references it through a curve definition, publish
 * with every reference intact:
 * - The entry points at its collection and the collection at its header
 * - The curve points at its definition and the definition at its configuration
 * - Each document gets the reporting configuration its header names
 *
 * Run with: pg_prove -d <database> test/configuration_documents_publish_test.sql
 */

begin;

select plan(8);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       '00000000-0000-0000-0000-0000000e0001'::uuid as market_header,
       '00000000-0000-0000-0000-0000000e0002'::uuid as market_collection,
       '00000000-0000-0000-0000-0000000e0003'::uuid as market_entry,
       '00000000-0000-0000-0000-0000000e0004'::uuid as market_configuration,
       '00000000-0000-0000-0000-0000000e0011'::uuid as curve_header,
       '00000000-0000-0000-0000-0000000e0012'::uuid as curve_definition,
       '00000000-0000-0000-0000-0000000e0013'::uuid as curve_yield,
       '00000000-0000-0000-0000-0000000e0014'::uuid as market_reporting,
       '00000000-0000-0000-0000-0000000e0015'::uuid as curve_reporting;

do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.todays_market', 'ACME Corporation', 'Reporting', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'OreStudio Code Generation Methodology',
        'Test Todays Market', 'A today''s market document staged for a test.',
        'ACMEBANK', 'pgTAP fixture', current_date, 'Internal Use Only', 'todays_market');
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.curve_configuration', 'ACME Corporation', 'Reporting', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'OreStudio Code Generation Methodology',
        'Test Curve Configuration', 'A curve configuration staged for a test.',
        'ACMEBANK', 'pgTAP fixture', current_date, 'Internal Use Only', 'curve_configuration');
end $$;

create temp table t_market on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.todays_market' and valid_to = ores_utility_infinity_timestamp_fn();
create temp table t_curve on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.curve_configuration' and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_dq_todays_market_configs_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, name, description, config_variant, configuration_id)
select m.id, ores_utility_system_tenant_id_fn(), c.market_header, 0, c.party_id,
    'Test today''s market', 'A document', '', c.market_reporting
from t_market m, t_ctx c;

insert into ores_dq_todays_market_collections_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, todays_market_config_id, collection,
    collection_id, position)
select m.id, ores_utility_system_tenant_id_fn(), c.market_collection, 0, c.party_id,
    c.market_header, 'DiscountingCurves', 'ois', 0
from t_market m, t_ctx c;

insert into ores_dq_todays_market_entries_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, todays_market_config_id,
    todays_market_collection_id, key_value, key_value_2, target, discounting, position)
select m.id, ores_utility_system_tenant_id_fn(), c.market_entry, 0, c.party_id,
    c.market_header, c.market_collection, 'EUR', null, 'Yield/EUR/EUR1D', null, 0
from t_market m, t_ctx c;

insert into ores_dq_curve_configurations_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, name, description, configuration_id)
select k.id, ores_utility_system_tenant_id_fn(), c.curve_header, 0, c.party_id,
    'Test curve configuration', 'A document', c.curve_reporting
from t_curve k, t_ctx c;

insert into ores_dq_curve_definitions_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, curve_configuration_id, section_code,
    curve_id, description, position)
select k.id, ores_utility_system_tenant_id_fn(), c.curve_definition, 0, c.party_id,
    c.curve_header, 'YieldCurves', 'EUR1D', 'EUR overnight curve', 0
from t_curve k, t_ctx c;

insert into ores_dq_yield_curve_configs_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, curve_definition_id, currency, discount_curve,
    has_report)
select k.id, ores_utility_system_tenant_id_fn(), c.curve_yield, 0, c.party_id,
    c.curve_definition, 'EUR', '', false
from t_curve k, t_ctx c;

select results_eq(
    $$select p.action, p.record_count
      from t_market m, t_ctx c,
           ores_analytics_publish_todays_market_from_dq_fn(m.id,
               ores_utility_system_tenant_id_fn(), 'upsert',
               jsonb_build_object('party_id', c.party_id::text)) p$$,
    $$values ('inserted'::text, 3::bigint)$$,
    'the today''s market publish adds the header, the collection and the entry');

select results_eq(
    $$select p.action, p.record_count
      from t_curve k, t_ctx c,
           ores_refdata_publish_curve_configuration_from_dq_fn(k.id,
               ores_utility_system_tenant_id_fn(), 'upsert',
               jsonb_build_object('party_id', c.party_id::text)) p$$,
    $$values ('inserted'::text, 3::bigint)$$,
    'the curve configuration publish adds the header, the definition and the yield curve');

select is(
    (select count(*) from ores_analytics_todays_market_entries_tbl e,
         ores_analytics_todays_market_collections_tbl k, ores_analytics_todays_market_configs_tbl h,
         t_ctx c
     where e.tenant_id = ores_utility_system_tenant_id_fn()
       and e.todays_market_collection_id = k.id and k.todays_market_config_id = h.id
       and e.todays_market_config_id = h.id and h.party_id = c.party_id
       and h.name = 'Test today''s market'
       and e.valid_to = ores_utility_infinity_timestamp_fn()
       and k.valid_to = ores_utility_infinity_timestamp_fn()
       and h.valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the entry points at its collection and the collection at its header');

select is(
    (select count(*) from ores_refdata_yield_curve_configs_tbl y,
         ores_refdata_curve_definitions_tbl d, ores_refdata_curve_configurations_tbl h, t_ctx c
     where y.tenant_id = ores_utility_system_tenant_id_fn()
       and y.curve_definition_id = d.id and d.curve_configuration_id = h.id
       and h.party_id = c.party_id and h.name = 'Test curve configuration'
       and y.valid_to = ores_utility_infinity_timestamp_fn()
       and d.valid_to = ores_utility_infinity_timestamp_fn()
       and h.valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the yield curve points at its definition and the definition at its configuration');

select is(
    (select count(*) from ores_reporting_configurations_tbl r, t_ctx c
     where r.tenant_id = ores_utility_system_tenant_id_fn() and r.party_id = c.party_id
       and r.configuration_type_code = 'todays_market' and r.name = 'Test today''s market'
       and r.owning_component = 'ores.analytics'
       and r.valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the today''s market has its reporting configuration');

select is(
    (select count(*) from ores_reporting_configurations_tbl r, t_ctx c
     where r.tenant_id = ores_utility_system_tenant_id_fn() and r.party_id = c.party_id
       and r.configuration_type_code = 'curve_configuration'
       and r.name = 'Test curve configuration' and r.owning_component = 'ores.refdata'
       and r.valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the curve configuration has its reporting configuration');

select results_eq(
    $$select p.action, p.record_count
      from t_market m, t_ctx c,
           ores_analytics_publish_todays_market_from_dq_fn(m.id,
               ores_utility_system_tenant_id_fn(), 'upsert',
               jsonb_build_object('party_id', c.party_id::text)) p$$,
    $$values ('skipped'::text, 3::bigint)$$,
    'a repeat today''s market publish adds nothing');

select results_eq(
    $$select p.action, p.record_count
      from t_curve k, t_ctx c,
           ores_refdata_publish_curve_configuration_from_dq_fn(k.id,
               ores_utility_system_tenant_id_fn(), 'upsert',
               jsonb_build_object('party_id', c.party_id::text)) p$$,
    $$values ('skipped'::text, 3::bigint)$$,
    'a repeat curve configuration publish adds nothing');

select * from finish();
rollback;
