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
 * Tells the administrator a store has no bound, and stops telling them once it
 * has one.
 *
 * The housekeeping screen is pull: a person reads it when a backup takes longer
 * or an audit asks how long a store is kept. A store fills a disk while nobody
 * is looking, so the same facts are pushed: the pass reads the bounds view and
 * the ledger, raises a notification for each store nothing effective bounds, and
 * clears the notification of a store that has come back under a bound. Clearing
 * is what makes the count worth reading — the open notifications of a tenant
 * follow the unbounded stores of its catalogue, so the inbox's number is the
 * work left.
 *
 * A store takes one of two kinds and never both: a store nothing bounds reads as
 * one notice, and a store whose last sweep failed reads as another, because a
 * failure is an incident and a person acts on it differently. A sweep that
 * leaves policies unapplied is one notice for the sweep, not one per policy: the
 * ledger's ran_at is the transaction's instant, so every policy one sweep
 * applied shares it, and that instant is what the notice groups by.
 *
 * The audience is the permission to act — the write the catalogue's own bound
 * needs — resolved in the tenant that holds the store. The pass is the definer's
 * because it reads the stores of every tenant and writes each one's inbox; the
 * raise itself writes in the tenant of the caller, so the pass states the store's
 * tenant around the call and puts the session's own back afterwards.
 *
 * The notice names the store and links to its page. It carries no size, no rate
 * and no period: those are what the page is for, and a notice that repeated them
 * would state a number the page disagrees with by the time it is read.
 */

-- =============================================================================
-- The audience
-- =============================================================================

/**
 * The people of one tenant who hold a permission, or any role that covers it.
 *
 * A role may grant the permission by name, by its component's wildcard, or by the
 * wildcard that grants everything. The IAM's own effective-permission function is
 * the authority on what an account holds, so a rule added there reaches this
 * pass; a second copy of the matching rule would be a second answer.
 *
 * Only a person is told. A service account holds the component's wildcard and
 * therefore the write this audience is about, and telling a service is telling
 * nobody: nobody signs in as it, and the notice would sit unread in a count a
 * person is meant to trust.
 */
create or replace function ores_scheduler_permission_holders_fn(
    p_tenant_id uuid,
    p_permission_code text
) returns uuid[]
language sql
stable
security definer
as $$
    select coalesce(array_agg(a."id" order by a."username"), '{}'::uuid[])
    from "ores_iam_accounts_tbl" a
    where a."tenant_id" = p_tenant_id
      and a."account_type" = 'user'
      and a."valid_to" = ores_utility_infinity_timestamp_fn()
      and exists (
          select 1
          from ores_iam_get_effective_permissions_fn(a."id") p
          where p.code in (
              p_permission_code,
              split_part(p_permission_code, '::', 1) || '::*',
              '*')
      );
$$;

comment on function ores_scheduler_permission_holders_fn(uuid, text) is
'The people of a tenant who hold a permission, directly or through a role that
 covers it. Empty when nobody in the tenant holds it, which is also the answer for
 a tenant with no account.';

-- =============================================================================
-- The one notice a store has open
-- =============================================================================

/**
 * The notification of one store that is still open for a person to read, if any.
 *
 * Open means a recipient row that is neither read nor cleared: a notice somebody
 * read and cleared is a notice that was dealt with, and the pass raises again
 * only when the store falls out of bounds again.
 */
create or replace function ores_scheduler_open_store_notice_fn(
    p_tenant_id uuid,
    p_store_code text,
    p_kinds text[]
) returns uuid
language sql
stable
security definer
as $$
    select n."id"
    from "ores_inbox_notifications_tbl" n
    join "ores_inbox_notification_recipients_tbl" r
      on r."tenant_id" = n."tenant_id"
     and r."notification_id" = n."id"
     and r."valid_to" = ores_utility_infinity_timestamp_fn()
     and r."cleared_at" is null
    where n."tenant_id" = p_tenant_id
      and n."valid_to" = ores_utility_infinity_timestamp_fn()
      and n."kind_code" = any (p_kinds)
      and n."link_id" = p_store_code
    order by n."raised_at" desc
    limit 1;
$$;

-- =============================================================================
-- Clearing a notice
-- =============================================================================

/**
 * Clears a notice for every person it was raised for.
 *
 * A new version of each recipient row says when and why it went: the row is
 * closed, not deleted, so a person's history keeps what they were told and when
 * it stopped applying. The actor is the pass, so the row states that the platform
 * withdrew the notice rather than the person clearing it.
 */
create or replace function ores_scheduler_clear_notice_fn(
    p_tenant_id uuid,
    p_notification_id uuid,
    p_actor text
) returns bigint
language plpgsql
security definer
as $$
declare
    v_count bigint;
begin
    insert into "ores_inbox_notification_recipients_tbl" (
        "tenant_id", "notification_id", "account_id", "version", "read_at", "cleared_at",
        "modified_by", "performed_by", "change_reason_code", "change_commentary")
    select r."tenant_id", r."notification_id", r."account_id", r."version",
        r."read_at", clock_timestamp(),
        coalesce(nullif(p_actor, ''), current_user), current_user,
        'system.update', 'The store is bounded again'
    from "ores_inbox_notification_recipients_tbl" r
    where r."tenant_id" = p_tenant_id
      and r."notification_id" = p_notification_id
      and r."valid_to" = ores_utility_infinity_timestamp_fn()
      and r."cleared_at" is null;

    get diagnostics v_count = row_count;
    return v_count;
end;
$$;

-- =============================================================================
-- The pass
-- =============================================================================

/**
 * Raises what is true and clears what is no longer true, one row per case.
 *
 * Three cases, in the order the screen states them: a store nothing bounds, a
 * store whose last sweep failed, and a sweep that left policies unapplied.
 */
create or replace function ores_scheduler_raise_housekeeping_alerts_fn()
returns table (
    "case_name" text,
    "raised" bigint,
    "resolved" bigint
)
language plpgsql
security definer
as $$
declare
    c_permission constant text := 'scheduler::retention_policies:write';
    c_store_kinds constant text[] :=
        array['scheduler.store_unbounded', 'scheduler.store_sweep_failed'];
    c_unbounded constant text := 'scheduler.store_unbounded';
    c_failed constant text := 'scheduler.store_sweep_failed';
    c_skipped constant text := 'scheduler.sweep_skipped';
    c_store_case constant text := 'store';
    c_sweep_case constant text := 'sweep';
    v_actor text := coalesce(nullif(ores_iam_current_service_fn(), ''), current_user);
    v_raiser uuid;
    v_home text := current_setting('app.current_tenant_id', true);
    v_raised_store bigint := 0;
    v_resolved_store bigint := 0;
    v_raised_sweep bigint := 0;
    v_resolved_sweep bigint := 0;
    v_want text;
    v_open uuid;
    v_open_kind text;
    v_holders uuid[];
    v_store record;
    v_last_sweep timestamptz;
    v_skips bigint;
    v_notice uuid;
    v_at text;
begin
    /*
     * The raiser is the service the job runs as. A job run by hand has no
     * service, and then the actor names the account; a pass with neither raises
     * nothing, because a notification nobody raised is a row the inbox cannot
     * explain.
     */
    select a."id" into v_raiser
    from "ores_iam_accounts_tbl" a
    where a."valid_to" = ores_utility_infinity_timestamp_fn()
      and a."username" in (coalesce(ores_iam_current_service_fn(), ''),
                           coalesce(ores_iam_current_actor_fn(), ''))
    order by (a."username" = ores_iam_current_service_fn()) desc, a."username"
    limit 1;

    if v_raiser is null then
        raise warning 'No account raises the housekeeping notices, so none was raised.';
        return query select c_store_case, 0::bigint, 0::bigint
        union all select c_sweep_case, 0::bigint, 0::bigint;
        return;
    end if;

    -- Every store the catalogue holds, in its own tenant, with the bound the
    -- view states for it. A store bounded by its keying needs no notice.
    for v_store in
        select v.*
        from "ores_scheduler_store_bounds_vw" v
        where v."bound_kind" <> 'keying'
        order by v."tenant_id", v."display_name"
    loop
        v_want := case
            when v_store."bounded_now" then null
            when v_store."last_outcome" = 'failed' then c_failed
            else c_unbounded
        end;

        v_open := ores_scheduler_open_store_notice_fn(
            v_store."tenant_id", v_store."code", c_store_kinds);

        if v_open is not null then
            select n."kind_code" into v_open_kind
            from "ores_inbox_notifications_tbl" n
            where n."tenant_id" = v_store."tenant_id"
              and n."id" = v_open
              and n."valid_to" = ores_utility_infinity_timestamp_fn();
        else
            v_open_kind := null;
        end if;

        if v_want is null then
            if v_open is not null then
                v_resolved_store := v_resolved_store +
                    ores_scheduler_clear_notice_fn(
                        v_store."tenant_id", v_open, v_actor);
            end if;
            continue;
        end if;

        -- The store still wants telling, and it is already told the same thing.
        if v_open_kind = v_want then
            continue;
        end if;

        -- A store that changed case is told the case it is in now, so the notice
        -- a person reads matches the row the screen states.
        if v_open is not null then
            v_resolved_store := v_resolved_store +
                ores_scheduler_clear_notice_fn(v_store."tenant_id", v_open, v_actor);
        end if;

        v_holders := ores_scheduler_permission_holders_fn(v_store."tenant_id", c_permission);
        if cardinality(v_holders) = 0 then
            continue;
        end if;

        perform set_config('app.current_tenant_id', v_store."tenant_id"::text, true);
        perform ores_inbox_raise_notification_fn(
            v_want,
            'operations/housekeeping',
            v_store."code",
            c_permission,
            v_raiser,
            v_actor,
            array['store'],
            array[v_store."display_name"],
            v_holders);
        v_raised_store := v_raised_store + 1;
    end loop;

    /*
     * The sweep case. The ledger states one row per policy applied, and every
     * row one sweep wrote shares its instant, so the newest instant that holds a
     * skip is the sweep to tell about. A notice already open for that instant is
     * the notice a person has, and a later sweep with no skips closes it.
     */
    select max(r."ran_at") into v_last_sweep
    from "ores_scheduler_retention_runs_tbl" r;

    select count(*) into v_skips
    from "ores_scheduler_retention_runs_tbl" r
    where r."ran_at" = v_last_sweep
      and r."outcome" = 'skipped';

    v_at := case when v_last_sweep is null then ''
                 else to_char(v_last_sweep at time zone 'UTC',
                              'YYYY-MM-DD HH24:MI:SS.US') || 'Z' end;

    select n."id" into v_notice
    from "ores_inbox_notifications_tbl" n
    join "ores_inbox_notification_recipients_tbl" r
      on r."tenant_id" = n."tenant_id"
     and r."notification_id" = n."id"
     and r."valid_to" = ores_utility_infinity_timestamp_fn()
     and r."cleared_at" is null
    join "ores_inbox_notification_arguments_tbl" a
      on a."tenant_id" = n."tenant_id"
     and a."notification_id" = n."id"
     and a."valid_to" = ores_utility_infinity_timestamp_fn()
    where n."tenant_id" = ores_utility_system_tenant_id_fn()
      and n."valid_to" = ores_utility_infinity_timestamp_fn()
      and n."kind_code" = c_skipped
      and a."name" = 'ran_at'
      and a."value" = v_at
    limit 1;

    if v_skips > 0 then
        if v_notice is null then
            v_holders := ores_scheduler_permission_holders_fn(
                ores_utility_system_tenant_id_fn(), c_permission);
            if cardinality(v_holders) > 0 then
                perform set_config(
                    'app.current_tenant_id',
                    ores_utility_system_tenant_id_fn()::text,
                    true);
                perform ores_inbox_raise_notification_fn(
                    c_skipped,
                    'operations/housekeeping',
                    '',
                    c_permission,
                    v_raiser,
                    v_actor,
                    array['count', 'ran_at'],
                    array[v_skips::text, v_at],
                    v_holders);
                v_raised_sweep := v_raised_sweep + 1;
            end if;
        end if;
    else
        -- No skip on the newest sweep, so every open skip notice is stale.
        for v_notice in
            select n."id"
            from "ores_inbox_notifications_tbl" n
            join "ores_inbox_notification_recipients_tbl" r
              on r."tenant_id" = n."tenant_id"
             and r."notification_id" = n."id"
             and r."valid_to" = ores_utility_infinity_timestamp_fn()
             and r."cleared_at" is null
            where n."tenant_id" = ores_utility_system_tenant_id_fn()
              and n."valid_to" = ores_utility_infinity_timestamp_fn()
              and n."kind_code" = c_skipped
            group by n."id"
        loop
            v_resolved_sweep := v_resolved_sweep +
                ores_scheduler_clear_notice_fn(
                    ores_utility_system_tenant_id_fn(), v_notice, v_actor);
        end loop;
    end if;

    -- The session belongs to the job, not to this pass: put back the tenant the
    -- job opened with, so whatever it does next does it in its own tenant.
    perform set_config('app.current_tenant_id', coalesce(v_home, ''), true);

    return query select c_store_case, v_raised_store, v_resolved_store
    union all select c_sweep_case, v_raised_sweep, v_resolved_sweep;
end;
$$;

comment on function ores_scheduler_raise_housekeeping_alerts_fn() is
'Raises a notice per store that nothing effective bounds and one per sweep that
 left policies unapplied, and clears the notices of stores that came back under a
 bound. One row per case states how many notices it raised and how many it
 cleared. The notice names the store and links to its page, and carries no size,
 no rate and no period.';
