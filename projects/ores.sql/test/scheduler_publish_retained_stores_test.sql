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
 * pgTAP tests for publishing the retained store catalogue.
 *
 * A dataset stages the stores the installation keeps, and the publish writes them
 * into the system tenant:
 * - A first publish writes the rows; a repeat publish writes nothing
 * - The whole row is the platform's statement, so a changed period or a changed
 *   name reaches the catalogue
 * - insert_only refuses a row that exists, and replace_all closes the rows the
 *   dataset stages and writes them again
 * - The store refuses a bound that states a policy with no period, and a bound
 *   that states no policy while carrying one
 *
 * Run with: pg_prove -d <database> test/scheduler_publish_retained_stores_test.sql
 */

begin;

select plan(8);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.retained_stores', 'ORE', 'General', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'ORE Internal',
        'Test Retained Stores', 'Stores staged for a test.',
        'ORE_STUDIO', 'pgTAP fixture', current_date, 'Internal', 'retained_stores');
end $$;

create temp table t_stores on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.retained_stores' and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_dq_retained_stores_artefact_tbl (
    dataset_id, tenant_id, code, version, display_name, description,
    owner_component, scope, target_kind, target, time_column, measure_kind,
    bound_kind, default_action, default_retention_days, default_cadence_hours,
    default_reason)
select d.id, ores_utility_system_tenant_id_fn(), v.code, 0, v.display_name,
    v.description, 'scheduler', 'platform', v.target_kind, v.target, v.time_column,
    v.measure_kind, v.bound_kind, v.default_action, v.default_retention_days, 24,
    v.default_reason
from t_stores d, (values
    ('test.store_a', 'Test store A', 'A store kept for a quarter',
     'chunked_table', 'public.ores_scheduler_retention_runs_tbl', 'valid_from',
     'relation_rows', 'policy', 'drop_chunks', 90, 'Kept for the quarter a run is read'),
    ('test.store_b', 'Test store B', 'A store bounded by its keying',
     'directory', 'publish/run/packages', null,
     'files', 'keying', null, null, null)) v(code, display_name, description,
     target_kind, target, time_column, measure_kind, bound_kind, default_action,
     default_retention_days, default_reason);

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_retained_stores_from_dq_fn(
          (select id from t_stores), ores_utility_system_tenant_id_fn(), 'upsert')$$,
    $$values ('inserted'::text, 2::bigint)$$,
    'a first publish writes the stores the dataset stages');

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_retained_stores_from_dq_fn(
          (select id from t_stores), ores_utility_system_tenant_id_fn(), 'upsert')$$,
    $$values ('skipped'::text, 2::bigint)$$,
    'a repeat publish writes nothing');

update ores_dq_retained_stores_artefact_tbl
set display_name = 'Test store A, restated',
    default_retention_days = 120
where dataset_id = (select id from t_stores)
  and code = 'test.store_a';

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_retained_stores_from_dq_fn(
          (select id from t_stores), ores_utility_system_tenant_id_fn(), 'upsert') p
      order by action$$,
    $$values ('skipped'::text, 1::bigint), ('updated'::text, 1::bigint)$$,
    'a changed name and period reach the catalogue, and the store it did not change is skipped');

select is(
    (select default_retention_days from ores_scheduler_retained_stores_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and code = 'test.store_a'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    120,
    'the default period the platform states reaches the catalogue');

update ores_dq_retained_stores_artefact_tbl
set description = 'A store bounded by its keying, restated'
where dataset_id = (select id from t_stores)
  and code = 'test.store_b';

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_retained_stores_from_dq_fn(
          (select id from t_stores), ores_utility_system_tenant_id_fn(), 'insert_only')$$,
    $$values ('skipped'::text, 2::bigint)$$,
    'insert_only writes nothing over a store that exists, even where the dataset changed it');

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_retained_stores_from_dq_fn(
          (select id from t_stores), ores_utility_system_tenant_id_fn(), 'replace_all') p
      order by action$$,
    $$values ('deleted'::text, 2::bigint), ('inserted'::text, 2::bigint)$$,
    'replace_all closes the stores the dataset stages and writes them again');

select throws_ok(
    $$insert into ores_scheduler_retained_stores_tbl (
          code, tenant_id, version, display_name, description, owner_component,
          scope, target_kind, target, time_column, measure_kind, bound_kind,
          default_action, default_retention_days, default_cadence_hours,
          modified_by, performed_by, change_reason_code, change_commentary)
      values ('test.store_c', ores_utility_system_tenant_id_fn(), 0, 'Test store C',
          'A policy bound with no period', 'scheduler', 'platform', 'table',
          'public.ores_scheduler_retained_stores_tbl', 'valid_from', 'relation_rows',
          'policy', 'drop_chunks', null, 24, current_user, current_user,
          'system.admin_reset', '')$$,
    null,
    'the store refuses a bound that states a policy with no period');

select throws_ok(
    $$insert into ores_scheduler_retained_stores_tbl (
          code, tenant_id, version, display_name, description, owner_component,
          scope, target_kind, target, time_column, measure_kind, bound_kind,
          default_action, default_retention_days, default_cadence_hours,
          modified_by, performed_by, change_reason_code, change_commentary)
      values ('test.store_d', ores_utility_system_tenant_id_fn(), 0, 'Test store D',
          'A keyed store carrying a period', 'scheduler', 'platform', 'directory',
          'publish/run/packages', null, 'files',
          'keying', null, 90, 24, current_user, current_user,
          'system.admin_reset', '')$$,
    null,
    'the store refuses a bound that states no policy while carrying a period');

select * from finish();

rollback;
