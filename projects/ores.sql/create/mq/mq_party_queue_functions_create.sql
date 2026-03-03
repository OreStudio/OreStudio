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

-- =============================================================================
-- Party-scoped queue access functions.
--
-- pgmq tables carry no tenant_id column and are therefore outside the normal
-- RLS model.  Access control is enforced here via the naming convention
-- established for party codenames:
--
--   queue name = {codename}_{suffix}
--
-- Access rules (evaluated in order):
--
--   1. Actor has the 'SuperAdmin' role  → all queues visible.
--   2. app.visible_party_ids is non-empty → only queues whose name starts with
--      the codename of a party in the visible set are returned.
--   3. Otherwise (no party selected, not super-admin) → no queues returned.
--
-- All three functions use SECURITY DEFINER so they can read the IAM and
-- refdata tables regardless of the caller's role.
-- =============================================================================

-- ---------------------------------------------------------------------------
-- Internal helper: returns true when the current actor has the SuperAdmin role.
-- ---------------------------------------------------------------------------
create or replace function ores_mq_actor_is_super_admin_fn()
returns boolean
language plpgsql stable security definer
as $$
begin
    return exists (
        select 1
        from ores_iam_accounts_tbl a
        join ores_iam_account_roles_tbl ar on ar.account_id = a.id
          and ar.valid_to = ores_utility_infinity_timestamp_fn()
        join ores_iam_roles_tbl r on r.id = ar.role_id
        where a.username = ores_iam_current_actor_fn()
          and r.name = 'SuperAdmin'
    );
end;
$$;

-- ---------------------------------------------------------------------------
-- ores_mq_list_party_queues_fn
--
-- Party-scoped replacement for pgmq.list_queues().
-- ---------------------------------------------------------------------------
create or replace function ores_mq_list_party_queues_fn()
returns table(
    queue_name     text,
    created_at     timestamp with time zone,
    is_unlogged    boolean,
    is_partitioned boolean
)
language plpgsql stable security definer
as $$
declare
    v_visible_ids uuid[];
begin
    -- SuperAdmin sees all queues regardless of party selection.
    if ores_mq_actor_is_super_admin_fn() then
        return query
            select q.queue_name::text, q.created_at, q.is_unlogged, q.is_partitioned
            from pgmq.list_queues() q;
        return;
    end if;

    v_visible_ids := ores_iam_visible_party_ids_fn();

    -- Party context: return only queues prefixed by the codename of a visible party.
    if v_visible_ids is not null and array_length(v_visible_ids, 1) is not null then
        return query
            select q.queue_name::text, q.created_at, q.is_unlogged, q.is_partitioned
            from pgmq.list_queues() q
            where exists (
                select 1
                from ores_refdata_parties_tbl p
                where p.id = any(v_visible_ids)
                  and p.valid_to = ores_utility_infinity_timestamp_fn()
                  and q.queue_name like p.codename || '_%'
            );
        return;
    end if;

    -- No party selected and not super-admin: return nothing.
end;
$$;

-- ---------------------------------------------------------------------------
-- ores_mq_metrics_party_fn
--
-- Party-scoped replacement for pgmq.metrics_all().
-- ---------------------------------------------------------------------------
create or replace function ores_mq_metrics_party_fn()
returns table(
    queue_name         text,
    queue_length       bigint,
    newest_msg_age_sec integer,
    oldest_msg_age_sec integer,
    total_messages     bigint,
    scrape_time        timestamp with time zone
)
language plpgsql stable security definer
as $$
declare
    v_visible_ids uuid[];
begin
    -- SuperAdmin sees all queue metrics regardless of party selection.
    if ores_mq_actor_is_super_admin_fn() then
        return query
            select m.queue_name::text, m.queue_length, m.newest_msg_age_sec,
                   m.oldest_msg_age_sec, m.total_messages, m.scrape_time
            from pgmq.metrics_all() m;
        return;
    end if;

    v_visible_ids := ores_iam_visible_party_ids_fn();

    -- Party context: return only metrics for queues belonging to visible parties.
    if v_visible_ids is not null and array_length(v_visible_ids, 1) is not null then
        return query
            select m.queue_name::text, m.queue_length, m.newest_msg_age_sec,
                   m.oldest_msg_age_sec, m.total_messages, m.scrape_time
            from pgmq.metrics_all() m
            where exists (
                select 1
                from ores_refdata_parties_tbl p
                where p.id = any(v_visible_ids)
                  and p.valid_to = ores_utility_infinity_timestamp_fn()
                  and m.queue_name like p.codename || '_%'
            );
        return;
    end if;

    -- No party selected and not super-admin: return nothing.
end;
$$;

-- ---------------------------------------------------------------------------
-- ores_mq_metric_samples_fn
--
-- Party-scoped replacement for the inline samples query against
-- ores_mq_metrics_samples_tbl.  Silently returns no rows when the
-- requested queue is not visible to the caller (access denied without error).
--
-- Pass empty string ('') for p_from / p_to to omit time-window filtering;
-- callers should use NULLIF($n, '')::timestamptz on the wire.
-- ---------------------------------------------------------------------------
create or replace function ores_mq_metric_samples_fn(
    p_queue_name text,
    p_from       timestamp with time zone default null,
    p_to         timestamp with time zone default null
)
returns table(
    sample_time    timestamp with time zone,
    queue_length   bigint,
    total_messages bigint
)
language plpgsql stable security definer
as $$
declare
    v_visible_ids uuid[];
begin
    -- SuperAdmin can access samples for any queue.
    if not ores_mq_actor_is_super_admin_fn() then
        v_visible_ids := ores_iam_visible_party_ids_fn();

        -- No party selected and not super-admin: return nothing.
        if v_visible_ids is null or array_length(v_visible_ids, 1) is null then
            return;
        end if;

        -- Party context: verify the requested queue belongs to a visible party.
        if not exists (
            select 1
            from ores_refdata_parties_tbl p
            where p.id = any(v_visible_ids)
              and p.valid_to = ores_utility_infinity_timestamp_fn()
              and p_queue_name like p.codename || '_%'
        ) then
            return; -- silently return no rows
        end if;
    end if;

    return query
        select s.sample_time, s.queue_length, s.total_messages
        from ores_mq_metrics_samples_tbl s
        where s.queue_name = p_queue_name
          and (p_from is null or s.sample_time >= p_from)
          and (p_to   is null or s.sample_time <= p_to  )
        order by s.sample_time asc
        limit 10000;
end;
$$;
