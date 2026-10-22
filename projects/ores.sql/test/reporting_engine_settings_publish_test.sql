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
 * pgTAP tests for the engine settings publish.
 *
 * A dataset stages an engine settings configuration as a central row and the
 * parts it sets. The publish writes them for the party a publish names:
 * - The central row, the parts it stages and the reporting configuration it is
 *   registered as receive their rows, and the ids are re-keyed for the party
 * - A part the dataset does not stage gets no row, which means the engine default
 * - A part names the central row it belongs to
 * - A repeat publish adds nothing
 * - A write to a part makes the next version of the central row
 * - A value outside the range of a setting is refused
 * - A configuration has at most one row of a part
 *
 * Run with: pg_prove -d <database> test/reporting_engine_settings_publish_test.sql
 */

begin;

select plan(11);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       '00000000-0000-0000-0000-0000000f0001'::uuid as settings_id,
       '00000000-0000-0000-0000-0000000f0002'::uuid as configuration_id,
       '00000000-0000-0000-0000-0000000f0003'::uuid as fixings_id,
       '00000000-0000-0000-0000-0000000f0004'::uuid as execution_id;

do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.engine_settings', 'ACME Corporation', 'Reporting', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'OreStudio Code Generation Methodology',
        'Test Engine Settings', 'Engine settings staged for a test.',
        'ACMEBANK', 'pgTAP fixture', current_date, 'Internal Use Only', 'engine_settings');
end $$;

create temp table t_dataset on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.engine_settings' and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_dq_engine_settings_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, name, description, configuration_id)
select d.id, ores_utility_system_tenant_id_fn(), c.settings_id, 0, c.party_id,
    'Test engine settings', 'Engine settings for a test', c.configuration_id
from t_dataset d, t_ctx c;

insert into ores_dq_engine_settings_fixings_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, engine_settings_id, imply_todays_fixings)
select d.id, ores_utility_system_tenant_id_fn(), c.fixings_id, 0, c.party_id, c.settings_id, false
from t_dataset d, t_ctx c;

insert into ores_dq_engine_settings_execution_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, engine_settings_id, n_threads, log_mask)
select d.id, ores_utility_system_tenant_id_fn(), c.execution_id, 0, c.party_id, c.settings_id, 4, 31
from t_dataset d, t_ctx c;

create or replace function pg_temp.publish() returns table (action text, record_count bigint) as $$
    select p.action, p.record_count
    from t_dataset d, t_ctx c,
         ores_reporting_publish_engine_settings_from_dq_fn(
             d.id, ores_utility_system_tenant_id_fn(),
             'upsert', jsonb_build_object('party_id', c.party_id::text)) p
    order by p.action;
$$ language sql;

select results_eq(
    $$select * from pg_temp.publish()$$,
    $$values ('inserted'::text, 3::bigint)$$,
    'the publish adds the central row and the two parts it stages');

create temp view t_live as
select r.*
from ores_reporting_engine_settings_tbl r, t_ctx c
where r.tenant_id = ores_utility_system_tenant_id_fn() and r.party_id = c.party_id
  and r.name = 'Test engine settings' and r.valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select r.id from t_live r),
    ores_dq_party_uuid_fn((select party_id from t_ctx), (select settings_id from t_ctx)),
    'the central row is re-keyed for the party');

select is(
    (select count(*) from ores_reporting_configurations_tbl g, t_live r
     where g.tenant_id = ores_utility_system_tenant_id_fn()
       and g.id = r.configuration_id
       and g.configuration_type_code = 'engine_settings'
       and g.owning_component = 'ores.reporting'
       and g.name = 'Test engine settings'
       and g.valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the reporting configuration is registered under the engine settings type');

select results_eq(
    $$select f.imply_todays_fixings, f.enrich_index_fixings
      from ores_reporting_engine_settings_fixings_tbl f, t_live r
      where f.tenant_id = ores_utility_system_tenant_id_fn()
        and f.engine_settings_id = r.id
        and f.valid_to = ores_utility_infinity_timestamp_fn()$$,
    $$values (false, null::boolean)$$,
    'the fixings part keeps the setting it states and leaves the others unset');

select results_eq(
    $$select e.n_threads, e.log_mask, e.gzip_compression_level
      from ores_reporting_engine_settings_execution_tbl e, t_live r
      where e.tenant_id = ores_utility_system_tenant_id_fn()
        and e.engine_settings_id = r.id
        and e.valid_to = ores_utility_infinity_timestamp_fn()$$,
    $$values (4, 31, null::integer)$$,
    'the execution part belongs to the central row');

select is(
    (select count(*) from ores_reporting_engine_settings_building_tbl b, t_live r
     where b.tenant_id = ores_utility_system_tenant_id_fn()
       and b.engine_settings_id = r.id),
    0::bigint,
    'a part the dataset does not stage has no row');

select is_empty(
    $$select * from pg_temp.publish() where action = 'inserted'$$,
    'a repeat publish adds nothing');

create temp table t_before on commit drop as
select version from t_live;

insert into ores_reporting_engine_settings_execution_tbl (
    id, tenant_id, version, engine_settings_id, party_id, n_threads, log_mask,
    modified_by, performed_by, change_reason_code, change_commentary)
select e.id, e.tenant_id, e.version, e.engine_settings_id, e.party_id, 8, e.log_mask,
    current_user, current_user, 'system.test', 'pgTAP'
from ores_reporting_engine_settings_execution_tbl e, t_live r
where e.tenant_id = ores_utility_system_tenant_id_fn()
  and e.engine_settings_id = r.id
  and e.valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select r.version from t_live r),
    (select version + 1 from t_before),
    'a write to a part makes the next version of the central row');

select throws_ok(
    $$insert into ores_reporting_engine_settings_execution_tbl (
          id, tenant_id, version, engine_settings_id, party_id, n_threads,
          modified_by, performed_by, change_reason_code, change_commentary)
      select e.id, e.tenant_id, e.version, e.engine_settings_id, e.party_id, 0,
          current_user, current_user, 'system.test', 'pgTAP'
      from ores_reporting_engine_settings_execution_tbl e
      where e.valid_to = ores_utility_infinity_timestamp_fn()
        and e.engine_settings_id in (select id from t_live)$$,
    '23514',
    null,
    'zero threads is refused');

select throws_ok(
    $$insert into ores_reporting_engine_settings_building_tbl (
          id, tenant_id, version, engine_settings_id, party_id, observation_model,
          modified_by, performed_by, change_reason_code, change_commentary)
      select gen_random_uuid(), r.tenant_id, 0, r.id, r.party_id, 'Move',
          current_user, current_user, 'system.test', 'pgTAP'
      from t_live r$$,
    '23514',
    null,
    'an observation model that ORE does not read is refused');

select throws_ok(
    $$insert into ores_reporting_engine_settings_fixings_tbl (
          id, tenant_id, version, engine_settings_id, party_id,
          modified_by, performed_by, change_reason_code, change_commentary)
      select gen_random_uuid(), r.tenant_id, 0, r.id, r.party_id,
          current_user, current_user, 'system.test', 'pgTAP'
      from t_live r$$,
    '23505',
    null,
    'a second row of the same part for one configuration is refused');

select * from finish();
rollback;
