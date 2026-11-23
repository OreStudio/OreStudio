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
 * Agent Call Table
 *
 * A row says that an LLM agent, holding a grant, called one route of ores.http
 * for its person, with which method, how it was answered and how long it took.
 * Reads are logged as writes are, because a person who wants to know what an
 * agent did wants to know what it looked at. It never holds a key, a token or a
 * request body. A person reads their agents' rows; the record each write changed
 * carries the agent in its own acting_agent stamp.
 *
 * The table is an audit log like the agent token issue log, and follows it: its
 * tenant_id is an ordinary text column isolated by a policy in
 * iam_rls_policies_create.sql, and only ores.http writes it, after it answers
 * the call, each row through a session of the call's own tenant. No retention
 * policy is set.
 *
 * The table is internal, so only the data layer and the SQL schema are generated.
 * The reads stay hand-written in agent_call_log.
 */

create table if not exists "ores_iam_agent_calls_tbl" (
    "id" text not null,
    "called_at" timestamp with time zone not null,
    "tenant_id" text not null default '',
    "party_id" text not null default '',
    "grant_id" text not null default '',
    "session_id" text not null default '',
    "grantor_account_id" text not null default '',
    "method" text not null default '',
    "route" text not null default '',
    "status" integer not null default 0,
    "duration_ms" integer not null default 0,
    primary key (id, called_at),
    check ("id" <> '')
);



create index if not exists agent_calls_grantor_time_idx
on "ores_iam_agent_calls_tbl" (grantor_account_id, called_at desc);

create index if not exists agent_calls_grant_time_idx
on "ores_iam_agent_calls_tbl" (grant_id, called_at desc);

create index if not exists agent_calls_tenant_time_idx
on "ores_iam_agent_calls_tbl" (tenant_id, called_at desc);

create or replace function ores_iam_agent_calls_insert_fn()
returns trigger as $$
declare
begin


    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_iam_agent_calls_insert_trg
before insert on "ores_iam_agent_calls_tbl"
for each row execute function ores_iam_agent_calls_insert_fn();

do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        raise notice 'TimescaleDB detected - creating hypertable (1 day chunks)';

        perform public.create_hypertable(
            '"ores_iam_agent_calls_tbl"',
            'called_at',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;
