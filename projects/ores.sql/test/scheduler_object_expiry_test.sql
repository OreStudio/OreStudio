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
 * pgTAP tests for the SQL side of object expiry.
 *
 * The service half calls the store; this half decides which policies are due
 * and records what a run did:
 * - an object policy with no ledger row is due, and names its prefix and period
 * - a policy the ledger recorded within its cadence is not due
 * - a table policy is not due here: the table sweep applies it
 * - a disabled object policy is not due
 * - a recorded run states the policy, the prefix, the removal and the outcome
 *
 * Run with: pg_prove -d <database> test/scheduler_object_expiry_test.sql
 */

begin;

select plan(9);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.current_service', 'ores_solid_dirac_scheduler_service', true);

-- The ledger is the fixture. A policy is due once its cadence has passed since
-- the last row the ledger holds for it, so an installation that has swept before
-- is one this suite must clear first to state anything about due-ness.
delete from ores_scheduler_retention_runs_tbl
where policy_code in ('job_inputs_7d', 'job_outputs_90d', 'ticks_90d');

select is(
    (select target from ores_scheduler_due_object_policies_fn()
     where policy_code = 'job_inputs_7d'),
    'compute/input/',
    'an object policy with no run behind it is due, and names its prefix');

select is(
    (select retention_days from ores_scheduler_due_object_policies_fn()
     where policy_code = 'job_inputs_7d'),
    7,
    'the due policy carries the period the catalogue states');

select is(
    (select count(*)::bigint from ores_scheduler_due_object_policies_fn()
     where policy_code = 'ticks_90d'),
    0::bigint,
    'a table policy is not due here: the table sweep applies it');

select is(
    (select count(*)::bigint from ores_scheduler_due_object_policies_fn()
     where policy_code = 'node_scratch_1d'),
    0::bigint,
    'a directory store is not an object prefix');

select ores_scheduler_record_object_run_fn('job_inputs_7d', 'compute/input/', 3, 'applied', 'probe');

select is(
    (select count(*)::bigint from ores_scheduler_due_object_policies_fn()
     where policy_code = 'job_inputs_7d'),
    0::bigint,
    'a run recorded within the cadence makes the policy not due');

select is(
    (select removed || ' ' || outcome
     from ores_scheduler_retention_runs_tbl
     where policy_code = 'job_inputs_7d'
     order by ran_at desc
     limit 1),
    '3 applied',
    'the ledger states what the run removed and its outcome');

select is(
    (select action || ' ' || target
     from ores_scheduler_retention_runs_tbl
     where policy_code = 'job_inputs_7d'
     order by ran_at desc
     limit 1),
    'expire_prefix compute/input/',
    'the ledger states the action and the prefix it acted on');

select ores_scheduler_record_object_run_fn('job_outputs_90d', 'compute/output/', 0, 'planned', 'dry run');

select is(
    (select outcome from ores_scheduler_retention_runs_tbl
     where policy_code = 'job_outputs_90d'
     order by ran_at desc
     limit 1),
    'planned',
    'a dry run is recorded as a plan, and not as an application');

select is(
    (select count(*)::bigint
     from ores_scheduler_retention_runs_tbl
     where policy_code in ('job_inputs_7d', 'job_outputs_90d')),
    2::bigint,
    'each call writes one row, so the ledger keeps the history of a prefix');

select * from finish();
rollback;
