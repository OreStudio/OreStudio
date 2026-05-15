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
-- ores_database_info_fn
-- =============================================================================
-- Returns the single row from ores_database_info_tbl describing when and how
-- this database was created: schema version, git commit, git date, build
-- environment, and creation timestamp.
-- =============================================================================

create or replace function ores_database_info_fn()
returns table (
    schema_version    text,
    build_environment text,
    git_commit        text,
    git_date          text,
    created_at        timestamp with time zone
)
language sql
stable
as $$
    select schema_version,
           build_environment,
           git_commit,
           git_date,
           created_at
    from ores_database_info_tbl
    limit 1;
$$;
