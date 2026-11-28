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
 * pgTAP tests for the retention sweep.
 *
 * The sweep reads the catalogue for the store behind each policy and applies the
 * action the policy states, where the action is one a database can perform:
 * - a row delete removes what is older than the period and keeps the rest
 * - a chunk drop still reports applied on a chunked store
 * - an action the sweep cannot perform is skipped, and the detail names who owns
 *   it, so a store is never reported bounded by a mechanism that did not run
 * - the ledger records the run under the policy's tenant
 * - the bounds view says which mechanism bounds the store, and whether it is
 *   bounded now
 *
 * Run with: pg_prove -d <database> test/scheduler_retention_sweep_test.sql
 */

begin;

select plan(9);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp table t_actor on commit drop as
select coalesce(
    (select username from ores_iam_accounts_tbl
     where valid_to = ores_utility_infinity_timestamp_fn()
     order by username
     limit 1),
    'system') as username;

-- The row delete is proved on the delivery counts, the store the story names: the
-- catalogue and the policy for it are the ones the installation ships. The second
-- store is the one whose action the sweep cannot perform.
insert into ores_scheduler_retained_stores_tbl (
    code, tenant_id, version, display_name, description, owner_component, scope,
    target_kind, target, time_column, measure_kind, bound_kind, default_action,
    default_retention_days, default_cadence_hours,
    modified_by, performed_by, change_reason_code, change_commentary)
values
    ('test.sweep_prefix', ores_utility_system_tenant_id_fn(), 0, 'Sweep test prefix',
     'A store whose bound is an object expiry', 'scheduler', 'platform',
     'object_prefix', 'compute/input/', null, 'objects', 'policy', 'expire_prefix',
     7, 24, (select username from t_actor), (select username from t_actor),
     'system.admin_reset', '');

insert into ores_scheduler_retention_policies_tbl (
    code, tenant_id, version, store_code, target_kind, target, action, retention_days,
    cadence_hours, enabled, description,
    modified_by, performed_by, change_reason_code, change_commentary)
values
    ('test_sweep_prefix_7d', ores_utility_system_tenant_id_fn(), 0, 'test.sweep_prefix',
     'object_prefix', 'compute/input/', 'expire_prefix', 7, 24, true, 'Archives older than a week',
     (select username from t_actor), (select username from t_actor),
     'system.admin_reset', '');

-- Every policy is due: the ledger is what decides that, and a store the
-- installation already swept would be skipped rather than applied.
delete from ores_scheduler_retention_runs_tbl;

-- The delivery counts are the store the story names as the row delete's proof.
-- The rows are the test's own: two outside the month and one inside it.
insert into ores_marketdata_delivery_counts_tbl (
    party_id, provider_code, feed_id, oresmd_uri, period_start, tenant_id, id,
    flushed_at, deliveries)
values
    (gen_random_uuid(), 'test', gen_random_uuid(), 'oresmd://test/row-1',
     now() - interval '40 days', ores_utility_system_tenant_id_fn(),
     gen_random_uuid(), now(), 10),
    (gen_random_uuid(), 'test', gen_random_uuid(), 'oresmd://test/row-2',
     now() - interval '35 days', ores_utility_system_tenant_id_fn(),
     gen_random_uuid(), now(), 20),
    (gen_random_uuid(), 'test', gen_random_uuid(), 'oresmd://test/row-3',
     now() - interval '2 days', ores_utility_system_tenant_id_fn(),
     gen_random_uuid(), now(), 30);

create temp table t_run on commit drop as
select * from ores_scheduler_apply_retention_policies_fn();

select is(
    (select removed from t_run where policy_code = 'delivery_counts_30d'),
    2::bigint,
    'the row delete removes the rows older than the period');

select is(
    (select count(*) from ores_marketdata_delivery_counts_tbl
     where oresmd_uri like 'oresmd://test/row-%'),
    1::bigint,
    'the row delete keeps what is inside the period');

select is(
    (select outcome from t_run where policy_code = 'delivery_counts_30d'),
    'applied',
    'the sweep reports the row delete as applied');

select is(
    (select outcome from t_run where policy_code = 'report_cashflows_90d'),
    'applied',
    'the sweep still performs a chunk drop on a chunked store');

select is(
    (select count(*)::bigint
     from ores_scheduler_retention_runs_tbl
     where policy_code = 'test_sweep_prefix_7d'),
    0::bigint,
    'the sweep writes no row for an action another mechanism owns');

select is(
    (select target from ores_scheduler_due_object_policies_fn()
     where policy_code = 'test_sweep_prefix_7d'),
    'compute/input/',
    'the object expiry states the policy the sweep left to it');

select is(
    (select tenant_id from ores_scheduler_retention_runs_tbl
     where policy_code = 'delivery_counts_30d'
     order by ran_at desc
     limit 1),
    ores_utility_system_tenant_id_fn(),
    'the ledger records the run under the policy''s tenant');

select is(
    (select mechanism || ':' || bounded_now::text from ores_scheduler_store_bounds_vw
     where code = 'marketdata.delivery_counts'),
    'sweep:true',
    'the view says the sweep bounds the store it deleted from');

select is(
    (select mechanism || ':' || bounded_now::text from ores_scheduler_store_bounds_vw
     where code = 'test.sweep_prefix'),
    'sweep:true',
    'the view says the sweep applies an object prefix''s policy');

select * from finish();

rollback;
