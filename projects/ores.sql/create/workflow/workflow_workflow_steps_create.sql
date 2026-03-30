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
 * Records the execution of one step in a saga workflow, including the step
 * index, name, status, request/response payloads, and timing. Steps reference
 * their parent workflow instance via workflow_id.
 */

create table if not exists "ores_workflow_workflow_steps_tbl" (
    "id" uuid not null,
    "workflow_id" uuid not null,
    "step_index" integer not null,
    "name" text not null,
    "status" text not null default 'pending',
    "request_json" jsonb not null,
    "response_json" jsonb null,
    "error" text null,
    "started_at" timestamptz null,
    "completed_at" timestamptz null,
    "created_at" timestamp with time zone not null default current_timestamp,
    primary key (id),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid)
);
