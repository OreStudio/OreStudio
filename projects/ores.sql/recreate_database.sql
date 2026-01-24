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
 * Recreate Database from Scratch
 *
 * Full wipe and rebuild for development environments. Tears down all ORES
 * components and recreates them from scratch.
 *
 * USAGE:
 *   -- Basic (no instance databases to drop):
 *   psql -U postgres -v skip_validation='off' -f recreate_database.sql
 *
 *   -- With instance databases to drop:
 *   psql -U postgres -v skip_validation='off' -v db_list="db1,db2" -f recreate_database.sql
 *
 * Variables:
 *   :skip_validation - 'on' to skip input validation in seed functions (faster)
 *   :db_list         - Comma-separated list of instance databases to drop (optional)
 */
\pset pager off
\pset tuples_only on
\timing off

-- Set session variable for seed function validation control
-- This can be checked via current_setting('ores.skip_validation', true)
select set_config('ores.skip_validation', :'skip_validation', false);

\ir teardown_all.sql
\ir setup_user.sql
\ir admin/setup_admin.sql
\ir setup_template.sql
\ir create_instance.sql
\ir populate/populate.sql
