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
 * Drop IAM Population Functions
 *
 * Drops all helper functions used by IAM population scripts.
 * This script is safe to run even if functions don't exist (uses IF EXISTS).
 */

set schema 'ores';

-- Permissions
drop function if exists ores.upsert_permission(text, text);

-- Roles
drop function if exists ores.upsert_role(text, text, text);
drop function if exists ores.assign_permission_to_role(text, text, text);
