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
 * Session Sample Table
 *
 * One sample of a session's traffic: the bytes it sent and received and the latency
 * of the heartbeat. The rows are append-only, so the model states :current_state:.
 *
 * The table is tenant-scoped. TimescaleDB columnstore compression is not enabled on
 * it, because it is incompatible with row-level security, which the table needs for
 * tenant isolation.
 *
 * The table is internal, so only the data layer and the SQL schema are generated.
 * The wire message that carries samples is ores.iam.session_samples_messages.
 */

create table if not exists "ores_iam_session_samples_tbl" (
    "session_id" uuid not null,
    "sample_time" timestamp with time zone not null,
    "tenant_id" uuid not null,
    "bytes_sent" bigint not null default 0,
    "bytes_received" bigint not null default 0,
    "latency_ms" bigint not null default 0,
    primary key (session_id, sample_time)
);



create index if not exists session_samples_session_idx
on "ores_iam_session_samples_tbl" (session_id, sample_time desc);

create or replace function ores_iam_session_samples_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_iam_session_samples_insert_trg
before insert on "ores_iam_session_samples_tbl"
for each row execute function ores_iam_session_samples_insert_fn();

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
            '"ores_iam_session_samples_tbl"',
            'sample_time',
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
                    '"ores_iam_session_samples_tbl"',
                    drop_after => interval '30 days',
                    if_not_exists => true
                );
            else
                raise notice 'TimescaleDB community edition - no retention policy is set for ores_iam_session_samples_tbl';
            end if;
        end;
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;

-- =============================================================================
-- Row-level security: tenant isolation for Session Sample
-- =============================================================================
alter table ores_iam_session_samples_tbl enable row level security;

drop policy if exists session_samples_tbl_tenant_isolation_policy
    on ores_iam_session_samples_tbl;

create policy session_samples_tbl_tenant_isolation_policy
on ores_iam_session_samples_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
