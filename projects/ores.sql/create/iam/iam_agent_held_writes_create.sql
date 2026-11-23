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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_create.mustache
 * To modify, update the template and regenerate.
 *
 * Agent Held Write Table
 *
 * A row is the pending change of an iam.write_by_agent approval request: the call
 * an agent made, kept as it was made, so that the approval replays it as the
 * person with the agent as actor. The replay then meets the controls the person's
 * own write would meet, which is the rest of the six eyes. The row's state says
 * whether the call waits, is being replayed, or has been applied or failed.
 *
 * The table holds one row per request and is rewritten in place as its state
 * moves, so the model states :current_state:. Its tenant_id is a text column
 * isolated by a policy in iam_rls_policies_create.sql, and only ores.http writes
 * it, in the write's own tenant.
 */

create table if not exists "ores_iam_agent_held_writes_tbl" (
    "request_id" text not null,
    "held_at" timestamp with time zone not null,
    "tenant_id" text not null default '',
    "party_id" text not null default '',
    "grant_id" text not null default '',
    "grantor_account_id" text not null default '',
    "method" text not null default '',
    "route" text not null default '',
    "target" text not null default '',
    "content_type" text not null default '',
    "body" text not null default '',
    "state" text not null default '',
    "outcome" text not null default '',
    "status" integer not null default 0,
    primary key (request_id),
    check ("request_id" <> '')
);



create index if not exists agent_held_writes_grantor_held_idx
on "ores_iam_agent_held_writes_tbl" (grantor_account_id, held_at desc);

create or replace function ores_iam_agent_held_writes_insert_fn()
returns trigger as $$
declare
begin


    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_iam_agent_held_writes_insert_trg
before insert on "ores_iam_agent_held_writes_tbl"
for each row execute function ores_iam_agent_held_writes_insert_fn();

