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
 * pgTAP tests for the cut selection a catalogue publishes.
 *
 * A report definition carries the kind of cut its catalogue entry states, and
 * the publish is the one act that puts it there:
 * - a definition published from a dataset carries the kind the dataset states
 * - a report the dataset states no kind for publishes with no selection
 * - a later dataset that states a kind writes it onto the definition the party
 *   already holds, as a new version of it, and changes nothing else
 * - the kind is refused when no active cut kind of that code stands
 *
 * Run with: pg_prove -d <database> test/reporting_publish_cut_selection_test.sql
 */

begin;

select plan(8);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id;

do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.cut_selection', 'ACME Corporation', 'Reporting', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'OreStudio Code Generation Methodology',
        'Test Cut Selection', 'Definitions staged with their cut selection.',
        'ACMEBANK', 'pgTAP fixture', current_date, 'Internal Use Only', 'report_definitions');
end $$;

create temp table t_dataset on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.cut_selection' and valid_to = ores_utility_infinity_timestamp_fn();

-- One report the catalogue marks as reading the live cut and one it does not
-- classify, which is the pair the Acme dataset states today.
insert into ores_dq_report_definitions_artefact_tbl (
    dataset_id, tenant_id, id, version, name, description, report_type, schedule_expression,
    concurrency_policy, display_order, tier, applicable_jurisdiction, cut_kind_code)
select d.id, ores_utility_system_tenant_id_fn(), gen_random_uuid(), 1, n.name, 'A report',
    'risk', '0 6 * * 1-5', 'skip', n.ord, 'trading', null, n.cut
from t_dataset d, (values ('Live cut report', 1, 'LIVE'), ('Unclassified report', 2, null))
    n(name, ord, cut);

select results_eq(
    $$select p.action, p.record_count
      from ores_reporting_publish_report_definitions_from_dq_fn(
          (select id from t_dataset), ores_utility_system_tenant_id_fn(), 'upsert',
          jsonb_build_object('party_id', (select party_id::text from t_ctx))) p$$,
    $$values ('inserted'::text, 2::bigint)$$,
    'both definitions are published');

select is(
    (select d.cut_kind_code from ores_reporting_report_definitions_tbl d
     where d.tenant_id = ores_utility_system_tenant_id_fn()
       and d.party_id = (select party_id from t_ctx)
       and d.name = 'Live cut report'
       and d.valid_to = ores_utility_infinity_timestamp_fn()),
    'LIVE',
    'the definition carries the kind of cut its catalogue entry states');

select is(
    (select d.cut_kind_code from ores_reporting_report_definitions_tbl d
     where d.tenant_id = ores_utility_system_tenant_id_fn()
       and d.party_id = (select party_id from t_ctx)
       and d.name = 'Unclassified report'
       and d.valid_to = ores_utility_infinity_timestamp_fn()),
    null::text,
    'a report the catalogue does not classify by cycle states no selection');

-- The catalogue moves: the report that read the close now reads the live cut.
update ores_dq_report_definitions_artefact_tbl
set cut_kind_code = 'EOD'
where dataset_id = (select id from t_dataset)
  and name = 'Live cut report';

create temp table t_before on commit drop as
select d.id, d.version
from ores_reporting_report_definitions_tbl d
where d.tenant_id = ores_utility_system_tenant_id_fn()
  and d.party_id = (select party_id from t_ctx)
  and d.name = 'Live cut report'
  and d.valid_to = ores_utility_infinity_timestamp_fn();

select results_eq(
    $$select p.action, p.record_count
      from ores_reporting_publish_report_definitions_from_dq_fn(
          (select id from t_dataset), ores_utility_system_tenant_id_fn(), 'upsert',
          jsonb_build_object('party_id', (select party_id::text from t_ctx))) p$$,
    $$values ('updated'::text, 1::bigint)$$,
    'the republished catalogue moves the one definition that changed');

select is(
    (select d.cut_kind_code from ores_reporting_report_definitions_tbl d
     where d.tenant_id = ores_utility_system_tenant_id_fn()
       and d.party_id = (select party_id from t_ctx)
       and d.name = 'Live cut report'
       and d.valid_to = ores_utility_infinity_timestamp_fn()),
    'EOD',
    'the definition now states the kind the later catalogue states');

select is(
    (select d.version from ores_reporting_report_definitions_tbl d
     where d.tenant_id = ores_utility_system_tenant_id_fn()
       and d.party_id = (select party_id from t_ctx)
       and d.name = 'Live cut report'
       and d.valid_to = ores_utility_infinity_timestamp_fn()),
    (select version + 1 from t_before),
    'the statement is a new version of the definition the party already held');

select is(
    (select d.id from ores_reporting_report_definitions_tbl d
     where d.tenant_id = ores_utility_system_tenant_id_fn()
       and d.party_id = (select party_id from t_ctx)
       and d.name = 'Live cut report'
       and d.valid_to = ores_utility_infinity_timestamp_fn()),
    (select id from t_before),
    'the definition keeps the identity it had, so its runs and bindings still name it');

select throws_ok(
    $$update ores_dq_report_definitions_artefact_tbl
      set cut_kind_code = 'NO SUCH KIND'
      where dataset_id = (select id from t_dataset) and name = 'Live cut report';
      select * from ores_reporting_publish_report_definitions_from_dq_fn(
          (select id from t_dataset), ores_utility_system_tenant_id_fn(), 'upsert',
          jsonb_build_object('party_id', (select party_id::text from t_ctx)))$$,
    '23503',
    null,
    'a kind no active cut kind holds is refused rather than published');

select * from finish();
rollback;
