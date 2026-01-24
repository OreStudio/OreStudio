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

-- Drop functions first (depend on views)
drop function if exists admin_generate_cleanup_ores_instance_databases_sql_fn();
drop function if exists admin_generate_cleanup_ores_databases_sql_fn();
drop function if exists admin_generate_cleanup_test_databases_sql_fn();
drop function if exists admin_list_ores_instance_databases_fn();
drop function if exists admin_list_ores_databases_fn();
drop function if exists admin_list_test_databases_fn();

-- Drop views
drop view if exists admin_ores_instance_databases_view;
drop view if exists admin_ores_databases_view;
drop view if exists admin_test_databases_view;
