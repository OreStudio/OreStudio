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
-- ORE Tables
-- =============================================================================
-- The description of ORE itself: the files of an ORE run, the groups they fall
-- in and the contents of each file as an ORE release ships it.

\ir ./ore_ore_file_categories_create.sql
\ir ./ore_ore_file_categories_notify_trigger_create.sql
\ir ./ore_ore_files_create.sql
\ir ./ore_ore_files_notify_trigger_create.sql
\ir ./ore_ore_file_versions_create.sql
\ir ./ore_ore_file_versions_notify_trigger_create.sql
