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
-- Drop Row-Level Security Policies for Assets Tables
-- =============================================================================
-- Must be dropped before the corresponding tables are dropped.

-- Image Tags
drop policy if exists ores_assets_image_tags_tbl_tenant_isolation_policy on "ores_assets_image_tags_tbl";

-- Tags
drop policy if exists ores_assets_tags_tbl_tenant_isolation_policy on "ores_assets_tags_tbl";

-- Images
drop policy if exists ores_assets_images_tbl_tenant_isolation_policy on "ores_assets_images_tbl";
