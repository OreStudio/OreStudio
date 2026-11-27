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
 * pgTAP tests for the housekeeping notices.
 *
 * The pass reads the bounds view and the ledger and tells the person who can act:
 * - one notice per store nothing effective bounds, and none for a store its
 *   keying bounds
 * - the notice names the store, links to its page, and carries no size, rate or
 *   period
 * - only a person is told, never a service account
 * - a second pass tells nobody again, so a count of open notices is the count of
 *   unbounded stores
 * - a store that comes back under a bound has its notice cleared
 * - one sweep that leaves policies unapplied raises one notice, and a later
 *   sweep with none clears it
 *
 * Run with: pg_prove -d <database> test/scheduler_housekeeping_alerts_test.sql
 */

begin;

select plan(15);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.current_service', 'ores_solid_dirac_scheduler_service', true);


-- A person who can act. The audience is the holder of the write a bound needs,
-- and a freshly created installation holds no person at all: the person who will
-- administer it does not exist until the first-run setup makes one.
insert into ores_iam_accounts_tbl (
    id, tenant_id, version, account_type, username, email, shared,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-0000000000bb'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'user', 'housekeeping_probe', 'probe@system.ores', false,
    current_user, current_user, 'system.test', 'Housekeeping alert probe'
);

insert into ores_iam_account_roles_tbl (
    tenant_id, account_id, role_id, version,
    modified_by, performed_by, change_reason_code, change_commentary
)
select ores_utility_system_tenant_id_fn(),
    'a0000000-0000-0000-0000-0000000000bb'::uuid, r.id, 0,
    current_user, current_user, 'system.test', 'Housekeeping alert probe'
from ores_iam_roles_tbl r
where r.name = 'SuperAdmin'
  and r.valid_to = ores_utility_infinity_timestamp_fn();

create temp table t_actor on commit drop as
select coalesce(
    (select username from ores_iam_accounts_tbl
     where valid_to = ores_utility_infinity_timestamp_fn()
       and account_type = 'user'
     order by username
     limit 1),
    'system') as username;

-- The pass is idempotent against what an installation already told people, so
-- the test starts from no open notice: an earlier run of the job in the same
-- database would otherwise be the reason the first assertion saw nothing.
delete from ores_inbox_notification_recipients_tbl
where notification_id in (
    select id from ores_inbox_notifications_tbl where kind_code like 'scheduler.%');

-- The ledger is what the sweep case reads, and a run this test did not write
-- would decide which sweep is the newest.
delete from ores_scheduler_retention_runs_tbl;

-- =============================================================================
-- A store nothing effective bounds
-- =============================================================================

-- The notices the pass raises are the ones this run added: an installation that
-- already told somebody about these stores holds notices too, and each of them
-- names the same store.
create temp table t_before on commit drop as
select id from ores_inbox_notifications_tbl;

select is(
    (select raised from ores_scheduler_raise_housekeeping_alerts_fn()
     where case_name = 'store'),
    4::bigint,
    'the pass raises one notice per store nothing effective bounds');

create temp table t_raised on commit drop as
select id, kind_code, link_id from ores_inbox_notifications_tbl
where id not in (select id from t_before);

select is(
    (select link_id from t_raised
     where kind_code = 'scheduler.store_unbounded'
     order by link_id
     limit 1),
    'compute.job_inputs',
    'the notice links to the store it is about');

select is(
    (select a.value
     from ores_inbox_notification_arguments_tbl a
     where a.notification_id in (
         select id from t_raised where link_id = 'refdata.calendar_dates')
       and a.name = 'store'
       and a.valid_to = ores_utility_infinity_timestamp_fn()),
    'Calendar dates',
    'the notice names the store by the name a person reads');

select is(
    (select count(*)
     from ores_inbox_notification_arguments_tbl a
     where a.notification_id in (select id from t_raised)
       and a.valid_to = ores_utility_infinity_timestamp_fn()
       and a.name in ('size', 'size_bytes', 'rate', 'growth', 'period', 'retention_days')),
    0::bigint,
    'the notice carries no size, no rate and no period');

select is(
    (select count(*)
     from ores_inbox_notification_recipients_tbl r
     join ores_iam_accounts_tbl a on a.id = r.account_id
     where r.notification_id in (select id from t_raised)
       and r.valid_to = ores_utility_infinity_timestamp_fn()
       and a.account_type <> 'user'),
    0::bigint,
    'only a person is told, never a service account');

select is(
    (select count(*)
     from ores_inbox_notification_recipients_tbl r
     where r.notification_id in (select id from t_raised)
       and r.valid_to = ores_utility_infinity_timestamp_fn()
       and r.cleared_at is null),
    4::bigint,
    'each notice is open for one person, which is what the inbox counts');

select is(
    (select sum(raised)::bigint from ores_scheduler_raise_housekeeping_alerts_fn()),
    0::bigint,
    'a second pass tells nobody again');

-- A store bounded by its keying ships no policy, and the pass leaves it alone.
select is(
    (select count(*) from t_raised where link_id = 'compute.package_cache'),
    0::bigint,
    'a store its own keying bounds is not told about');

-- =============================================================================
-- A store that comes back under a bound
-- =============================================================================

-- Turning the delivery counts' policy off is the case the screen reports as a
-- decision somebody made: the store is then kept for ever, and the person who
-- can act is told. The write states the version it read, as every versioned row
-- does.
insert into ores_scheduler_retention_policies_tbl (
    code, tenant_id, version, store_code, target_kind, target, action, retention_days,
    cadence_hours, enabled, description,
    modified_by, performed_by, change_reason_code, change_commentary)
select code, tenant_id, version, store_code, target_kind, target, action, retention_days,
    cadence_hours, false, description,
    (select username from t_actor), (select username from t_actor),
    'system.admin_reset', ''
from ores_scheduler_retention_policies_tbl
where code = 'delivery_counts_30d'
  and valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select raised from ores_scheduler_raise_housekeeping_alerts_fn()
     where case_name = 'store'),
    1::bigint,
    'a policy that is turned off raises a notice for its store');

select is(
    (select count(distinct n.id)
     from ores_inbox_notifications_tbl n
     join ores_inbox_notification_recipients_tbl r
       on r.tenant_id = n.tenant_id and r.notification_id = n.id
     where n.kind_code = 'scheduler.store_unbounded'
       and n.link_id = 'marketdata.delivery_counts'
       and n.id not in (select id from t_raised)
       and n.valid_to = ores_utility_infinity_timestamp_fn()
       and r.valid_to = ores_utility_infinity_timestamp_fn()
       and r.cleared_at is null),
    1::bigint,
    'the store that was turned off is told about once');

insert into ores_scheduler_retention_policies_tbl (
    code, tenant_id, version, store_code, target_kind, target, action, retention_days,
    cadence_hours, enabled, description,
    modified_by, performed_by, change_reason_code, change_commentary)
select code, tenant_id, version, store_code, target_kind, target, action, retention_days,
    cadence_hours, true, description,
    (select username from t_actor), (select username from t_actor),
    'system.admin_reset', ''
from ores_scheduler_retention_policies_tbl
where code = 'delivery_counts_30d'
  and valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select resolved from ores_scheduler_raise_housekeeping_alerts_fn()
     where case_name = 'store'),
    1::bigint,
    'a store that comes back under a bound has its notice cleared');

select is(
    (select count(distinct n.id)
     from ores_inbox_notifications_tbl n
     join ores_inbox_notification_recipients_tbl r
       on r.tenant_id = n.tenant_id and r.notification_id = n.id
     where n.kind_code = 'scheduler.store_unbounded'
       and n.link_id = 'marketdata.delivery_counts'
       and n.id not in (select id from t_raised)
       and r.valid_to = ores_utility_infinity_timestamp_fn()
       and r.cleared_at is null),
    0::bigint,
    'the cleared notice is no longer open for anybody');

-- =============================================================================
-- A sweep that leaves policies unapplied
-- =============================================================================

-- One sweep is one ran_at: the sweep writes now(), the transaction's instant, so
-- the ledger's rows for one sweep share it.
insert into ores_scheduler_retention_runs_tbl (
    id, tenant_id, policy_code, action, target, ran_at, removed, outcome, detail)
values
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'delivery_counts_30d',
     'delete_rows', 'public.ores_marketdata_delivery_counts_tbl', '2026-11-27 03:00:00+00',
     0, 'skipped', 'the store names no column'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'job_inputs_7d',
     'expire_prefix', 'compute/input/', '2026-11-27 03:00:00+00',
     0, 'skipped', 'the storage lifecycle expires an object prefix');

select is(
    (select raised from ores_scheduler_raise_housekeeping_alerts_fn()
     where case_name = 'sweep'),
    1::bigint,
    'one sweep that leaves two policies unapplied raises one notice');

select is(
    (select a.value
     from ores_inbox_notification_arguments_tbl a
     join ores_inbox_notifications_tbl n
       on n.tenant_id = a.tenant_id and n.id = a.notification_id
     where n.kind_code = 'scheduler.sweep_skipped'
       and a.name = 'count'
       and a.valid_to = ores_utility_infinity_timestamp_fn()),
    '2',
    'the notice states how many policies the sweep left');

insert into ores_scheduler_retention_runs_tbl (
    id, tenant_id, policy_code, action, target, ran_at, removed, outcome, detail)
values
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'ticks_90d',
     'drop_chunks', 'public.ores_marketdata_ticks_tbl', '2026-11-28 03:00:00+00',
     0, 'applied', '');

select is(
    (select resolved from ores_scheduler_raise_housekeeping_alerts_fn()
     where case_name = 'sweep'),
    1::bigint,
    'a later sweep with nothing skipped clears the notice');

select * from finish();
rollback;
