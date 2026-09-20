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
 * Session Table
 *
 * A user session: one login or service-account connection, recorded for
 * analytics, session listing and end-time tracking. The table is a
 * TimescaleDB hypertable partitioned by start_time with 7-day chunks (see
 * projects/ores.sql/create/iam/iam_sessions_create.sql).
 *
 * The table has no valid_from/valid_to, no GIST exclusion, no version
 * column and no audit tail, and it keys on (id, start_time) rather than a
 * single surrogate key -- the partition column must sit in the primary key
 * of a hypertable. The :current_state: and :hypertable: flags in the
 * * SQL ** Flags drawer select exactly that shape. The compound key is
 * what makes this the first entity in the estate whose key carries a
 * timestamp; the mapper, entity and generator templates gained the
 * timestamp key-column branches they were missing.
 *
 * end_time is text not null default '', not a nullable timestamp, and
 * client_ip is text, not inet. The model mirrors each column's real
 * type; the domain projection is the column's type too, so a session's
 * end_time is an empty string while the session is active and
 * client_ip is a string. The richer readings (an instant, an IP address)
 * belong to the consumer that needs them.
 *
 * The hand-written domain struct also carried party_id,
 * visible_party_ids and username. No column backs any of them; they are
 * the denormalised, party-scoped shape of a session, and the rule that an
 * entity describes its table puts them on a message instead. They are
 * declared as message fields on session_view in
 * ores.iam.session_messages.
 *
 * The entity's CRUD handler and sub-registrar are switched off below: the
 * hand-written session_handler already owns the iam.v1.sessions.*
 * subjects that ores.iam.session_messages declares, and the generated
 * session_handler.hpp would overwrite it. The generated
 * session_protocol.hpp is suppressed by the same one-owner gate that
 * the operation model already satisfies; only the competing handler is
 * switched off here.
 */

create table if not exists "ores_iam_sessions_tbl" (
    "id" uuid not null,
    "start_time" timestamp with time zone not null,
    "tenant_id" uuid not null,
    "account_id" uuid not null,
    "end_time" text not null default '',
    "client_ip" text not null,
    "client_identifier" text not null default '',
    "client_version_major" smallint not null default 0,
    "client_version_minor" smallint not null default 0,
    "bytes_sent" bigint not null default 0,
    "bytes_received" bigint not null default 0,
    "country_code" text not null default '',
    "protocol" text not null default 'binary',
    primary key (id, start_time)
);



create index if not exists sessions_tenant_idx
on "ores_iam_sessions_tbl" (tenant_id, start_time desc);

create index if not exists sessions_account_id_idx
on "ores_iam_sessions_tbl" (account_id, start_time desc);

create index if not exists sessions_active_idx
on "ores_iam_sessions_tbl" (account_id)
where end_time = '';

create index if not exists sessions_country_idx
on "ores_iam_sessions_tbl" (country_code, start_time desc)
where country_code != '';

create index if not exists sessions_protocol_idx
on "ores_iam_sessions_tbl" (protocol, start_time desc);

create or replace function ores_iam_sessions_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_iam_sessions_insert_trg
before insert on "ores_iam_sessions_tbl"
for each row execute function ores_iam_sessions_insert_fn();

do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        raise notice 'TimescaleDB detected - creating hypertable (7 days chunks)';

        perform public.create_hypertable(
            '"ores_iam_sessions_tbl"',
            'start_time',
            chunk_time_interval => interval '7 days',
            if_not_exists => true
        );
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;
