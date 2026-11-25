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
 * pgTAP tests for publishing the housekeeping data.
 *
 * A dataset stages the job definitions the platform declares and the retention
 * policies that bound each store, and the publish writes them into the system
 * tenant:
 * - A first publish writes the rows; a repeat publish writes nothing
 * - The contract is reconciled: a changed command or description reaches the
 *   target, and the installation's own values survive it
 * - A job's schedule and state, and a policy's period, cadence and state, are
 *   written only when the row does not exist
 * - insert_only refuses a row that exists, and replace_all closes the rows the
 *   dataset stages and writes them again, still leaving the installation's own
 *   values alone
 *
 * Run with: pg_prove -d <database> test/scheduler_publish_housekeeping_test.sql
 */

begin;

select plan(15);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.housekeeping_policies', 'ORE', 'General', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'ORE Internal',
        'Test Housekeeping Policies', 'Policies staged for a test.',
        'ORE_STUDIO', 'pgTAP fixture', current_date, 'Internal', 'retention_policies');
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.housekeeping_jobs', 'ORE', 'General', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'ORE Internal',
        'Test Housekeeping Jobs', 'Jobs staged for a test.',
        'ORE_STUDIO', 'pgTAP fixture', current_date, 'Internal', 'job_definitions');
end $$;

create temp table t_policies on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.housekeeping_policies' and valid_to = ores_utility_infinity_timestamp_fn();

create temp table t_jobs on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.housekeeping_jobs' and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_dq_retention_policies_artefact_tbl (
    dataset_id, tenant_id, code, version, target_kind, target, action,
    retention_days, cadence_hours, enabled, description)
select d.id, ores_utility_system_tenant_id_fn(), v.code, 0, 'chunked_table',
    'public.ores_scheduler_retention_runs_tbl', 'drop_chunks', v.days, 24, true, v.description
from t_policies d, (values
    ('test_store_a_90d', 90, 'A store kept for a quarter'),
    ('test_store_b_30d', 30, 'A store kept for a month')) v(code, days, description);

insert into ores_dq_job_definitions_artefact_tbl (
    dataset_id, tenant_id, job_name, description, command,
    schedule_expression, action_type, action_payload, is_active)
select d.id, ores_utility_system_tenant_id_fn(), 'test.sweep_job',
    'A job staged for a test', 'select 1', '0 3 * * *', 'execute_sql', '{}'::jsonb, true
from t_jobs d;

-- =============================================================================
-- The retention policies
-- =============================================================================

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_retention_policies_from_dq_fn(
          (select id from t_policies), ores_utility_system_tenant_id_fn(), 'upsert')$$,
    $$values ('inserted'::text, 2::bigint)$$,
    'a first publish writes the policies the dataset stages');

-- A person changes a period. The store holds a new version, which is what the
-- policy screen does.
insert into ores_scheduler_retention_policies_tbl (
    code, tenant_id, version, target_kind, target, action, retention_days,
    cadence_hours, enabled, description,
    modified_by, performed_by, change_reason_code, change_commentary)
select r.code, r.tenant_id, r.version, r.target_kind, r.target, r.action, 365,
    r.cadence_hours, r.enabled, r.description,
    'system', 'system', 'system.admin_reset', ''
from ores_scheduler_retention_policies_tbl r
where r.tenant_id = ores_utility_system_tenant_id_fn()
  and r.code = 'test_store_a_90d'
  and r.valid_to = ores_utility_infinity_timestamp_fn();

update ores_dq_retention_policies_artefact_tbl
set description = 'A store kept for a quarter, restated'
where dataset_id = (select id from t_policies)
  and code = 'test_store_a_90d';

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_retention_policies_from_dq_fn(
          (select id from t_policies), ores_utility_system_tenant_id_fn(), 'upsert') p
      order by action$$,
    $$values ('skipped'::text, 1::bigint), ('updated'::text, 1::bigint)$$,
    'a changed description is reconciled, and the policy it did not change is skipped');

select is(
    (select retention_days from ores_scheduler_retention_policies_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and code = 'test_store_a_90d'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    365,
    'the period the person chose survives the republish');

select is(
    (select description from ores_scheduler_retention_policies_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and code = 'test_store_a_90d'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'A store kept for a quarter, restated',
    'the description the platform states reaches the policy');

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_retention_policies_from_dq_fn(
          (select id from t_policies), ores_utility_system_tenant_id_fn(), 'upsert')$$,
    $$values ('skipped'::text, 2::bigint)$$,
    'a repeat publish writes nothing');

update ores_dq_retention_policies_artefact_tbl
set action = 'delete_rows'
where dataset_id = (select id from t_policies)
  and code = 'test_store_b_30d';

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_retention_policies_from_dq_fn(
          (select id from t_policies), ores_utility_system_tenant_id_fn(), 'insert_only')$$,
    $$values ('skipped'::text, 2::bigint)$$,
    'insert_only writes nothing over a policy that exists');

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_retention_policies_from_dq_fn(
          (select id from t_policies), ores_utility_system_tenant_id_fn(), 'replace_all') p
      order by action$$,
    $$values ('deleted'::text, 2::bigint), ('inserted'::text, 2::bigint)$$,
    'replace_all closes the policies the dataset stages and writes them again');

select is(
    (select retention_days from ores_scheduler_retention_policies_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and code = 'test_store_a_90d'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    365,
    'the period the person chose survives a replace that closed the row first');

select is(
    (select action from ores_scheduler_retention_policies_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and code = 'test_store_b_30d'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'delete_rows',
    'the action the platform states reaches the policy it was changed on');

-- =============================================================================
-- The job definitions
-- =============================================================================

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_job_definitions_from_dq_fn(
          (select id from t_jobs), ores_utility_system_tenant_id_fn(), 'upsert')$$,
    $$values ('inserted'::text, 1::bigint)$$,
    'a first publish writes the job the dataset stages');

-- A person changes when the job fires. The store holds a new version.
insert into ores_scheduler_job_definitions_tbl (
    id, tenant_id, version, job_name, party_id, description, command,
    schedule_expression, action_type, action_payload, is_active,
    modified_by, performed_by, change_reason_code, change_commentary)
select r.id, r.tenant_id, r.version, r.job_name, r.party_id, r.description, r.command,
    '* * * * *', r.action_type, r.action_payload, r.is_active,
    'system', 'system', 'system.admin_reset', ''
from ores_scheduler_job_definitions_tbl r
where r.tenant_id = ores_utility_system_tenant_id_fn()
  and r.job_name = 'test.sweep_job'
  and r.valid_to = ores_utility_infinity_timestamp_fn();

update ores_dq_job_definitions_artefact_tbl
set command = 'select 2'
where dataset_id = (select id from t_jobs) and job_name = 'test.sweep_job';

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_job_definitions_from_dq_fn(
          (select id from t_jobs), ores_utility_system_tenant_id_fn(), 'upsert')$$,
    $$values ('updated'::text, 1::bigint)$$,
    'a changed command is reconciled');

select is(
    (select schedule_expression from ores_scheduler_job_definitions_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and job_name = 'test.sweep_job'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    '* * * * *',
    'the schedule the person chose survives the republish');

select is(
    (select command from ores_scheduler_job_definitions_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and job_name = 'test.sweep_job'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'select 2',
    'the command the platform states reaches the job');

select is(
    (select count(*) from ores_scheduler_job_definitions_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and job_name = 'test.sweep_job'),
    3::bigint,
    'the job holds one version per write, and no more');

select results_eq(
    $$select action, record_count
      from ores_scheduler_publish_job_definitions_from_dq_fn(
          (select id from t_jobs), ores_utility_system_tenant_id_fn(), 'upsert')$$,
    $$values ('skipped'::text, 1::bigint)$$,
    'a repeat publish writes nothing over the job');

select * from finish();

rollback;
