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
 * pgTAP tests for the binding of configuration documents to report definitions.
 *
 * A dataset stages report definitions and, beside them, the documents each binds
 * by name. The definitions publish resolves the names for the target party:
 * - A binding to a published document is written, with the document's id
 * - A binding to conventions creates the conventions configuration it names
 * - A binding to a document that does not exist is not written and is counted
 * - A repeat publish adds nothing
 *
 * Run with: pg_prove -d <database> test/reporting_publish_report_bindings_test.sql
 */

begin;

select plan(7);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       '00000000-0000-0000-0000-0000000f0001'::uuid as header_id,
       '00000000-0000-0000-0000-0000000f0002'::uuid as configuration_id;

do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.binding_documents', 'ACME Corporation', 'Reporting', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'OreStudio Code Generation Methodology',
        'Test Binding Documents', 'A document staged for a test.',
        'ACMEBANK', 'pgTAP fixture', current_date, 'Internal Use Only', 'pricing_engines');
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.binding_definitions', 'ACME Corporation', 'Reporting', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'OreStudio Code Generation Methodology',
        'Test Binding Definitions', 'Definitions staged for a test.',
        'ACMEBANK', 'pgTAP fixture', current_date, 'Internal Use Only', 'report_definitions');
end $$;

create temp table t_document on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.binding_documents' and valid_to = ores_utility_infinity_timestamp_fn();
create temp table t_definitions on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.binding_definitions' and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_dq_pricing_model_configs_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, name, description, config_variant,
    configuration_id)
select d.id, ores_utility_system_tenant_id_fn(), c.header_id, 0, c.party_id,
    'Test bound pricing engines', 'A document', '', c.configuration_id
from t_document d, t_ctx c;

insert into ores_dq_report_definitions_artefact_tbl (
    dataset_id, tenant_id, id, version, name, description, report_type, schedule_expression,
    concurrency_policy, display_order, tier, applicable_jurisdiction)
select d.id, ores_utility_system_tenant_id_fn(), gen_random_uuid(), 1, n.name, 'A report',
    'risk', '0 6 * * 1-5', 'skip', n.ord, 'trading', null
from t_definitions d, (values ('Test bound report', 1), ('Test unbound report', 2)) n(name, ord);

insert into ores_dq_report_configurations_artefact_tbl (
    dataset_id, tenant_id, version, report_name, configuration_type_code, configuration_name)
select d.id, ores_utility_system_tenant_id_fn(), 0, b.report_name, b.type_code, b.name
from t_definitions d, (values
    ('Test bound report', 'pricing_engines', 'Test bound pricing engines'),
    ('Test bound report', 'conventions', 'Test conventions'),
    ('Test unbound report', 'pricing_engines', 'No such document')) b(report_name, type_code, name);

select results_eq(
    $$select * from ores_analytics_publish_pricing_engines_from_dq_fn(
          (select id from t_document), ores_utility_system_tenant_id_fn(), 'upsert',
          jsonb_build_object('party_id', (select party_id::text from t_ctx)))$$,
    $$values ('inserted'::text, 1::bigint)$$,
    'the document is published before the definitions');

select results_eq(
    $$select p.action, p.record_count
      from ores_reporting_publish_report_definitions_from_dq_fn(
          (select id from t_definitions), ores_utility_system_tenant_id_fn(), 'upsert',
          jsonb_build_object('party_id', (select party_id::text from t_ctx))) p
      order by p.action$$,
    $$values ('bound'::text, 2::bigint), ('inserted'::text, 2::bigint), ('skipped'::text, 1::bigint)$$,
    'two definitions are published, two bindings are written and one is not');

create temp view t_bound as
select b.*
from ores_reporting_report_configurations_tbl b
join ores_reporting_report_definitions_tbl d on d.id = b.report_definition_id
 and d.valid_to = ores_utility_infinity_timestamp_fn()
where b.tenant_id = ores_utility_system_tenant_id_fn()
  and d.name = 'Test bound report'
  and b.valid_to = ores_utility_infinity_timestamp_fn();

select results_eq(
    $$select configuration_type_code from t_bound order by 1$$,
    $$values ('conventions'::text), ('pricing_engines'::text)$$,
    'the bound definition has a binding for each type it names');

select is(
    (select b.configuration_id from t_bound b where b.configuration_type_code = 'pricing_engines'),
    ores_dq_party_uuid_fn((select party_id from t_ctx), (select configuration_id from t_ctx)),
    'the pricing engines binding names the published document''s configuration');

select is(
    (select count(*) from ores_reporting_configurations_tbl c, t_bound b
     where c.id = b.configuration_id and c.configuration_type_code = 'conventions'
       and c.name = 'Test conventions' and c.owning_component = 'ores.refdata'
       and c.valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the conventions binding created the conventions configuration it names');

select is(
    (select count(*)
     from ores_reporting_report_configurations_tbl b
     join ores_reporting_report_definitions_tbl d on d.id = b.report_definition_id
      and d.valid_to = ores_utility_infinity_timestamp_fn()
     where b.tenant_id = ores_utility_system_tenant_id_fn() and d.name = 'Test unbound report'
       and b.valid_to = ores_utility_infinity_timestamp_fn()),
    0::bigint,
    'a binding to a document that does not exist is not written');

select results_eq(
    $$select p.action, p.record_count
      from ores_reporting_publish_report_definitions_from_dq_fn(
          (select id from t_definitions), ores_utility_system_tenant_id_fn(), 'upsert',
          jsonb_build_object('party_id', (select party_id::text from t_ctx))) p$$,
    $$values ('skipped'::text, 0::bigint)$$,
    'a repeat publish adds nothing');

select * from finish();
rollback;
