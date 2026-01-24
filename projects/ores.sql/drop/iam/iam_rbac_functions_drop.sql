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
set schema 'ores';

drop function if exists ores.iam_account_has_permission_by_id_fn;
drop function if exists ores.iam_account_has_permission_fn;
drop function if exists ores.iam_get_account_roles_with_permissions_fn;
drop function if exists ores.iam_get_roles_by_ids_fn;
drop function if exists ores.iam_get_role_permission_codes_fn;
drop function if exists ores.iam_get_all_role_permission_codes_fn;
drop function if exists ores.iam_get_effective_permissions_fn;
