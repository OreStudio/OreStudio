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
-- MQ message archive table.
-- Stores acknowledged messages. TimescaleDB hypertable on archived_at when
-- available; degrades gracefully to a plain table.
-- Partitioned by archived_at with 1-day chunks and 90-day retention.
-- =============================================================================

create table if not exists ores_mq_message_archive_tbl (
    id            bigint      not null,
    queue_id      uuid        not null,
    tenant_id     uuid,
    party_id      uuid,
    message_type  text        not null default '',
    payload_type  text        not null default 'json',
    payload       jsonb,
    raw_payload   bytea,
    final_status  text        not null default 'done',
    created_at    timestamptz not null default now(),
    archived_at   timestamptz not null default now(),
    read_count    int         not null default 0,
    error_message text        not null default '',
    primary key (id, archived_at)
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
            'ores_mq_message_archive_tbl',
            'archived_at',
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
                    'ores_mq_message_archive_tbl',
                    drop_after => interval '90 days',
                    if_not_exists => true
                );
                raise notice 'Enabled retention policy (90 days)';
            else
                raise notice 'TimescaleDB Apache license - retention policy skipped';
                raise notice 'Set timescaledb.license = ''timescale'' for full features';
            end if;
        end;

        raise notice 'TimescaleDB setup complete for ores_mq_message_archive_tbl';
    else
        raise notice '================================================';
        raise notice 'TimescaleDB NOT available - using regular table';
        raise notice '================================================';
        raise notice 'Note: Manual cleanup of old archived data will be required';
    end if;
end $$;

-- ---------------------------------------------------------------------------
-- Row-level security
-- ---------------------------------------------------------------------------

alter table ores_mq_message_archive_tbl enable row level security;

create policy ores_mq_message_archive_read_policy on ores_mq_message_archive_tbl for select using (
    (tenant_id is null and party_id is null)
    or (tenant_id = ores_iam_current_tenant_id_fn() and party_id is null)
    or (tenant_id = ores_iam_current_tenant_id_fn() and party_id = any(ores_iam_visible_party_ids_fn()))
);
