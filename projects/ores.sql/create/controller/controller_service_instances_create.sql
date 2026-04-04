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
 * Template: sql_schema_non_temporal_create.mustache
 * To modify, update the template and regenerate.
 *
 *  Table
 *
 * Represents a Kubernetes-style Pod: one concrete running instance of a
 * service definition. Tracks the OS process ID, lifecycle phase, start/stop
 * times, and how many times the instance has been restarted. Non-temporal:
 * only the current state is kept here; history is captured in service_events.
 */

create table if not exists "ores_controller_service_instances_tbl" (
    "id" uuid not null,
    "service_name" text not null,
    "replica_index" integer not null default 0,
    "pid" integer null,
    "phase" text not null default 'pending',
    "started_at" timestamp with time zone null,
    "stopped_at" timestamp with time zone null,
    "restart_count" integer not null default 0,
    "created_at" timestamp with time zone not null default current_timestamp,
    primary key (id),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid)
);

-- Only one replica per index per service at a time.
create unique index if not exists ores_controller_service_instances_name_replica_uniq_idx
on ores_controller_service_instances_tbl (service_name, replica_index);

create index if not exists ores_controller_service_instances_service_name_idx
on ores_controller_service_instances_tbl (service_name);
