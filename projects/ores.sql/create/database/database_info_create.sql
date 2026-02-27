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
 * Stores the schema version, git commit, and build environment inserted at
 * database creation time. Used to correlate service behaviour with the
 * database schema in use. Contains exactly one row.
 */

create table if not exists "ores_database_info_tbl" (
    "id" uuid not null,
    "schema_version" text not null,
    "build_environment" text not null,
    "git_commit" text not null,
    "git_date" text not null,
    "created_at" timestamp with time zone not null default current_timestamp,
    primary key (id),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid)
);
