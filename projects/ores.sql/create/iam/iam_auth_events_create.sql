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
 * Auth Event Table
 *
 * One row per authentication event. The rows are append-only, so the model states
 * :current_state:: an event is a fact about an instant, and the bi-temporal pair
 * has no meaning on it.
 *
 * The table is the audit log of who signed in and who failed to. Its tenant_id
 * is an ordinary text column and not the generated kind, because an event can
 * happen before a tenant is known, and it is then the system tenant's. The read in
 * ores.iam.auth_event_operations scopes on that column itself. The model binds
 * no variability profile: readonly-timeseries fixes has_tenant_id to true, and
 * this table states it false for the reason above.
 *
 * The table is internal: its protocol, handler and shell commands are the
 * hand-written operation model's, so only the data layer and the SQL schema are
 * generated here.
 *
 * The table is isolated by tenant like the rest of IAM, by a policy in
 * iam_rls_policies_create.sql. An event that names no tenant is the system
 * tenant's. The hourly and daily continuous aggregates it once had are gone:
 * nothing read them, and TimescaleDB does not build a continuous aggregate over a
 * table with row-level security.
 */

create table if not exists "ores_iam_auth_events_tbl" (
    "id" text not null,
    "event_time" timestamp with time zone not null,
    "tenant_id" text not null default '',
    "account_id" text not null default '',
    "event_type" text not null,
    "username" text not null default '',
    "session_id" text not null default '',
    "party_id" text not null default '',
    "error_detail" text not null default '',
    primary key (id, event_time),
    check ("id" <> '')
);



create index if not exists auth_events_tenant_time_idx
on "ores_iam_auth_events_tbl" (tenant_id, event_time desc);

create index if not exists auth_events_event_type_idx
on "ores_iam_auth_events_tbl" (event_type, event_time desc);

create or replace function ores_iam_auth_events_insert_fn()
returns trigger as $$
declare
begin


    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_iam_auth_events_insert_trg
before insert on "ores_iam_auth_events_tbl"
for each row execute function ores_iam_auth_events_insert_fn();

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
            '"ores_iam_auth_events_tbl"',
            'event_time',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
        -- A hypertable grows without bound unless a retention policy drops the
        -- chunks that have aged out. The licence decides whether the policy can
        -- be set: the community edition has no retention job, so the model's
        -- interval is reported rather than silently ignored.
        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;
            if current_license = 'timescale' then
                perform public.add_retention_policy(
                    '"ores_iam_auth_events_tbl"',
                    drop_after => interval '90 days',
                    if_not_exists => true
                );
            else
                raise notice 'TimescaleDB community edition - no retention policy is set for ores_iam_auth_events_tbl';
            end if;
        end;
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;
