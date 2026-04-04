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
-- Service lifecycle events.
-- Kubernetes-style Event log: one row per significant state transition for a
-- service instance (started, stopped, crashed, restarted, etc.).
-- Designed as a TimescaleDB hypertable partitioned by occurred_at for
-- efficient time-range queries and automatic data retention.
-- =============================================================================

create table if not exists ores_controller_service_events_tbl (
    "occurred_at"    timestamp with time zone not null,
    "event_id"       uuid not null default gen_random_uuid(),
    "service_name"   text not null,
    "instance_id"    uuid null,
    "replica_index"  integer null,
    "event_type"     text not null,
    "message"        text not null default '',
    primary key (occurred_at, event_id),
    check ("event_id" <> '00000000-0000-0000-0000-000000000000'::uuid)
);

create index if not exists ores_controller_service_events_name_idx
    on ores_controller_service_events_tbl (service_name, occurred_at desc);

create index if not exists ores_controller_service_events_instance_idx
    on ores_controller_service_events_tbl (instance_id, occurred_at desc)
    where instance_id is not null;

do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        perform public.create_hypertable(
            'ores_controller_service_events_tbl',
            'occurred_at',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );

        declare
            current_license text;
        begin
            select current_setting('timescaledb.license', true) into current_license;
            if current_license = 'timescale' then
                perform public.add_retention_policy(
                    'ores_controller_service_events_tbl',
                    drop_after => interval '90 days',
                    if_not_exists => true
                );
            end if;
        end;
    end if;
end $$;
