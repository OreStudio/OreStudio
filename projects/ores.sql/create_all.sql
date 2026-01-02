/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
 * DEPRECATED: This file is kept for backwards compatibility.
 *
 * The SQL scripts have been reorganized. Please use:
 *
 *   - setup_template.sql      : Create the template database (one-time setup)
 *   - create_instance.sql     : Create instance from template (fast, recommended)
 *   - create_database_direct.sql : Create database without template (slower)
 *
 * This file now delegates to the new structure.
 */

\echo ''
\echo 'WARNING: create_all.sql is deprecated.'
\echo 'Please use the new organized scripts instead.'
\echo ''
\echo 'See: setup_template.sql, create_instance.sql, create_database_direct.sql'
\echo ''

-- Include schema creation (for backwards compatibility when run in existing database)
\ir ./template/create_schema.sql

-- Include instance initialization
\ir ./instance/init_instance.sql
