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
-- MQ queue statistics time-series table.
-- Populated by ores_mq_queue_stats_scrape_fn() via a pg_cron job.
-- TimescaleDB hypertable when available; degrades gracefully to a plain table.
-- Partitioned by recorded_at with 1-day chunks and 30-day retention.
-- =============================================================================

create table if not exists ores_mq_queue_stats_tbl (
    recorded_at       timestamptz not null,
    queue_id          uuid        not null,
    tenant_id         uuid,
    party_id          uuid,
    pending_count     bigint      not null default 0,
    processing_count  bigint      not null default 0,
    total_archived    bigint      not null default 0,
    primary key (queue_id, recorded_at)
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
            'ores_mq_queue_stats_tbl',
            'recorded_at',
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
                    'ores_mq_queue_stats_tbl',
                    drop_after => interval '30 days',
                    if_not_exists => true
                );
                raise notice 'Enabled retention policy (30 days)';
            else
                raise notice 'TimescaleDB Apache license - retention policy skipped';
                raise notice 'Set timescaledb.license = ''timescale'' for full features';
            end if;
        end;

        raise notice 'TimescaleDB setup complete for ores_mq_queue_stats_tbl';
    else
        raise notice '================================================';
        raise notice 'TimescaleDB NOT available - using regular table';
        raise notice '================================================';
        raise notice 'Note: Manual cleanup of old stats data will be required';
    end if;
end $$;

-- ---------------------------------------------------------------------------
-- Scrape function: point-in-time snapshot of all active queue depths
-- ---------------------------------------------------------------------------

create or replace function ores_mq_queue_stats_scrape_fn() returns void
language sql security definer
as $$
    insert into ores_mq_queue_stats_tbl (
        recorded_at, queue_id, tenant_id, party_id,
        pending_count, processing_count, total_archived
    )
    select now(), q.id, q.tenant_id, q.party_id,
        count(*) filter (where m.status = 'pending'),
        count(*) filter (where m.status = 'processing'),
        (select count(*) from ores_mq_message_archive_tbl a where a.queue_id = q.id)
    from ores_mq_queues_tbl q
    left join ores_mq_messages_tbl m on m.queue_id = q.id
    where q.is_active = true
    group by q.id, q.tenant_id, q.party_id;
$$;
