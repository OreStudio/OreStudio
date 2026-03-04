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
-- MQ task queue messages table.
-- Regular (non-hypertable) table for pending and in-flight task messages.
-- =============================================================================

create table if not exists ores_mq_messages_tbl (
    id            bigserial    primary key,
    queue_id      uuid         not null references ores_mq_queues_tbl(id),
    tenant_id     uuid,
    party_id      uuid,
    message_type  text         not null default '',
    payload_type  text         not null default 'json'
                  check (payload_type in ('json','binary')),
    payload       jsonb,
    raw_payload   bytea,
    status        text         not null default 'pending'
                  check (status in ('pending','processing','done','failed')),
    visible_after timestamptz  not null default now(),
    created_at    timestamptz  not null default now(),
    updated_at    timestamptz  not null default now(),
    read_count    int          not null default 0,
    error_message text         not null default ''
);

create index if not exists ores_mq_messages_queue_status_idx
    on ores_mq_messages_tbl (queue_id, status, visible_after)
    where status in ('pending','processing');

-- ---------------------------------------------------------------------------
-- Row-level security
-- ---------------------------------------------------------------------------

alter table ores_mq_messages_tbl enable row level security;

create policy ores_mq_messages_read_policy on ores_mq_messages_tbl for select using (
    (tenant_id is null and party_id is null)  -- system scope
    or (tenant_id = ores_iam_current_tenant_id_fn() and party_id is null)  -- tenant scope
    or (tenant_id = ores_iam_current_tenant_id_fn() and party_id = any(ores_iam_visible_party_ids_fn()))  -- party scope
);

create policy ores_mq_messages_write_policy on ores_mq_messages_tbl for all using (
    (tenant_id is null and party_id is null)
    or (tenant_id = ores_iam_current_tenant_id_fn() and party_id is null)
    or (tenant_id = ores_iam_current_tenant_id_fn() and party_id = any(ores_iam_visible_party_ids_fn()))
);

-- ---------------------------------------------------------------------------
-- Core message queue functions (security definer to bypass RLS internally)
-- ---------------------------------------------------------------------------

-- Atomic read with visibility timeout (SKIP LOCKED)
create or replace function ores_mq_messages_read_fn(
    p_queue_id    uuid,
    p_batch       int  default 1,
    p_vt_seconds  int  default 30
) returns setof ores_mq_messages_tbl
language sql security definer
as $$
    update ores_mq_messages_tbl
    set status = 'processing',
        visible_after = now() + (p_vt_seconds || ' seconds')::interval,
        read_count = read_count + 1,
        updated_at = now()
    where id in (
        select id from ores_mq_messages_tbl
        where queue_id = p_queue_id
          and status = 'pending'
          and visible_after <= now()
        order by id
        limit p_batch
        for update skip locked
    )
    returning *;
$$;

-- Acknowledge -> move to archive
create or replace function ores_mq_messages_ack_fn(p_message_ids bigint[])
returns void
language plpgsql security definer
as $$
begin
    insert into ores_mq_message_archive_tbl (
        id, queue_id, tenant_id, party_id, message_type, payload_type,
        payload, raw_payload, final_status, created_at, read_count, error_message
    )
    select id, queue_id, tenant_id, party_id, message_type, payload_type,
           payload, raw_payload, 'done', created_at, read_count, error_message
    from ores_mq_messages_tbl
    where id = any(p_message_ids);

    delete from ores_mq_messages_tbl where id = any(p_message_ids);
end;
$$;

-- Nack / mark failed
create or replace function ores_mq_messages_nack_fn(
    p_message_id  bigint,
    p_error       text default ''
) returns void
language sql security definer
as $$
    update ores_mq_messages_tbl
    set status = 'failed',
        error_message = p_error,
        updated_at = now()
    where id = p_message_id;
$$;

-- Send (JSON)
create or replace function ores_mq_messages_send_fn(
    p_queue_id      uuid,
    p_type          text,
    p_payload       jsonb,
    p_delay_seconds int default 0
) returns bigint
language sql security definer
as $$
    insert into ores_mq_messages_tbl (queue_id, tenant_id, party_id, message_type, payload_type, payload, visible_after)
    select p_queue_id, q.tenant_id, q.party_id, p_type, 'json', p_payload,
           now() + (p_delay_seconds || ' seconds')::interval
    from ores_mq_queues_tbl q where q.id = p_queue_id
    returning id;
$$;

-- Send (binary)
create or replace function ores_mq_messages_send_binary_fn(
    p_queue_id      uuid,
    p_type          text,
    p_raw_payload   bytea,
    p_delay_seconds int default 0
) returns bigint
language sql security definer
as $$
    insert into ores_mq_messages_tbl (queue_id, tenant_id, party_id, message_type, payload_type, raw_payload, visible_after)
    select p_queue_id, q.tenant_id, q.party_id, p_type, 'binary', p_raw_payload,
           now() + (p_delay_seconds || ' seconds')::interval
    from ores_mq_queues_tbl q where q.id = p_queue_id
    returning id;
$$;
