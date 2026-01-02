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
 * Creates a database named 'ores' using the direct creation method.
 *
 * For new usage, please use:
 *   - create_instance.sql     : Create from template with whimsical name (fast)
 *   - create_database_direct.sql : Create without template (specify any name)
 */

\echo ''
\echo 'WARNING: create_database.sql is deprecated.'
\echo 'Using create_database_direct.sql with default name "ores"'
\echo ''

\set db_name 'ores'
\ir ./create_database_direct.sql
