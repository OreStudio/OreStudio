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
-- Scheduler Job Instances Table
-- Execution history for scheduler jobs.
-- Designed for TimescaleDB hypertable; degrades gracefully to a plain table.
-- Partitioned by triggered_at.
-- =============================================================================

create table if not exists ores_scheduler_job_instances_tbl (
    id                bigserial   not null,
    tenant_id         uuid,
    party_id          uuid,
    job_definition_id uuid        not null,
    action_type       text        not null default 'execute_sql',
    status            text        not null check (status in ('started','succeeded','failed')),
    triggered_at      timestamptz not null,
    started_at        timestamptz not null default now(),
    completed_at      timestamptz,
    duration_ms       bigint,
    error_message     text        not null default '',
    primary key (id, triggered_at)
);

do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        raise notice '=========================================';
        raise notice 'TimescaleDB detected - creating hypertable';
        raise notice '=========================================';

        perform public.create_hypertable(
            'ores_scheduler_job_instances_tbl',
            'triggered_at',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
        raise notice 'Created hypertable with 1-day chunks';

        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;

            if current_license = 'timescale' then
                perform public.add_retention_policy(
                    'ores_scheduler_job_instances_tbl',
                    drop_after => interval '30 days',
                    if_not_exists => true
                );
                raise notice 'Enabled retention policy (30 days)';
            else
                raise notice 'TimescaleDB Apache license - retention policy skipped';
                raise notice 'Set timescaledb.license = ''timescale'' for full features';
            end if;
        end;

        raise notice 'TimescaleDB setup complete for ores_scheduler_job_instances_tbl';
    else
        raise notice '================================================';
        raise notice 'TimescaleDB NOT available - using regular table';
        raise notice '================================================';
        raise notice 'Note: Manual cleanup of old instance data will be required';
    end if;
end $$;

alter table ores_scheduler_job_instances_tbl enable row level security;

create policy ores_scheduler_job_instances_read_policy
on ores_scheduler_job_instances_tbl for select using (
    tenant_id is null  -- system jobs visible to all
    or tenant_id = ores_iam_current_tenant_id_fn()
);
