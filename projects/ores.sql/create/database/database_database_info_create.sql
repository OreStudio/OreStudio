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
 * Database Info Table
 *
 * Stores the schema version, git commit and build environment recorded when
 * the database was created or recreated. Used to correlate service behaviour
 * with the schema in use. Holds exactly one row.
 *
 * Deliberately not a temporal table and not a codegen entity. It records the
 * checkout that built the database before any service is running, so it
 * cannot depend on the audit, history or eventing stacks a generated entity
 * carries. `compass db recreate` inserts the row and `compass bearings` reads
 * the newest one.
 */

create table if not exists "ores_database_info_tbl" (
    "id" uuid not null,
    "schema_version" text not null,
    "build_environment" text not null,
    "git_commit" text not null,
    "git_date" text not null,
    "created_at" timestamp with time zone not null default current_timestamp,
    primary key (id),
    check ("id" <> ores_utility_nil_uuid_fn())
);
