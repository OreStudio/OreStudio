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
-- MQ queue definitions table.
-- Supports party, tenant and system scope; task and channel queue types.
-- =============================================================================

create table if not exists ores_mq_queues_tbl (
    id          uuid        not null default gen_random_uuid() primary key,
    tenant_id   uuid,                           -- NULL for system scope
    party_id    uuid,                           -- NULL for tenant/system scope
    scope_type  text        not null default 'party'
                check (scope_type in ('party','tenant','system')),
    queue_type  text        not null default 'task'
                check (queue_type in ('task','channel')),
    name        text        not null,
    description text        not null default '',
    created_at  timestamptz not null default now(),
    modified_at timestamptz not null default now(),
    modified_by text        not null default '',
    is_active   boolean     not null default true,
    constraint ores_mq_queues_scope_check check (
        (scope_type = 'party'  and tenant_id is not null and party_id is not null) or
        (scope_type = 'tenant' and tenant_id is not null and party_id is null) or
        (scope_type = 'system' and tenant_id is null     and party_id is null)
    )
);

-- Unique indexes per scope level
create unique index if not exists ores_mq_queues_party_name_uniq_idx
    on ores_mq_queues_tbl (tenant_id, party_id, name)
    where is_active = true and scope_type = 'party';
create unique index if not exists ores_mq_queues_tenant_name_uniq_idx
    on ores_mq_queues_tbl (tenant_id, name)
    where is_active = true and scope_type = 'tenant';
create unique index if not exists ores_mq_queues_system_name_uniq_idx
    on ores_mq_queues_tbl (name)
    where is_active = true and scope_type = 'system';

-- ---------------------------------------------------------------------------
-- Row-level security
-- ---------------------------------------------------------------------------

alter table ores_mq_queues_tbl enable row level security;

create policy ores_mq_queues_read_policy on ores_mq_queues_tbl for select using (
    scope_type = 'system'
    or (scope_type = 'tenant' and tenant_id = ores_iam_current_tenant_id_fn())
    or (scope_type = 'party'  and tenant_id = ores_iam_current_tenant_id_fn()
                               and party_id = any(ores_iam_visible_party_ids_fn()))
);

-- ---------------------------------------------------------------------------
-- NOTIFY trigger
-- ---------------------------------------------------------------------------

create or replace function ores_mq_queues_notify_fn()
returns trigger
language plpgsql
as $$
begin
    perform pg_notify('ores_mq_queues', row_to_json(NEW)::text);
    return NEW;
end;
$$;

create trigger ores_mq_queues_notify_trg
after insert or update on ores_mq_queues_tbl
for each row execute function ores_mq_queues_notify_fn();

-- ---------------------------------------------------------------------------
-- Helper: idempotent queue creation
-- ---------------------------------------------------------------------------

create or replace function ores_mq_queues_create_fn(
    p_tenant_id   uuid,
    p_party_id    uuid,
    p_scope       text,
    p_type        text,
    p_name        text,
    p_description text,
    p_modified_by text
) returns uuid
language plpgsql security definer
as $$
declare
    v_id uuid;
begin
    select id into v_id
    from ores_mq_queues_tbl
    where name = p_name
      and (tenant_id = p_tenant_id or (p_tenant_id is null and tenant_id is null))
      and (party_id = p_party_id or (p_party_id is null and party_id is null))
      and scope_type = p_scope
      and is_active = true;

    if found then
        return v_id;
    end if;

    insert into ores_mq_queues_tbl (tenant_id, party_id, scope_type, queue_type, name, description, modified_by)
    values (p_tenant_id, p_party_id, p_scope, p_type, p_name, p_description, p_modified_by)
    returning id into v_id;

    return v_id;
end;
$$;
