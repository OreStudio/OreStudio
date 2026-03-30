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
 * Tracks the lifecycle of a workflow execution, including its type, status,
 * the serialised request that triggered it, and any result or error produced.
 * Instances are append-mostly; status transitions are the primary mutation.
 */

create table if not exists "ores_workflow_workflow_instances_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "type" text not null,
    "status" text not null default 'pending',
    "request_json" jsonb not null,
    "result_json" jsonb null,
    "error" text null,
    "correlation_id" text null,
    "created_by" text not null,
    "completed_at" timestamptz null,
    "created_at" timestamp with time zone not null default current_timestamp,
    primary key (id),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid)
);
