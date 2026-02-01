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

-- =============================================================================
-- Row-Level Security Policies for Assets Tables
-- =============================================================================
-- These policies enforce per-tenant isolation for asset data.
-- Each tenant can only see and modify their own images, tags, and image-tag
-- associations.

-- -----------------------------------------------------------------------------
-- Images
-- -----------------------------------------------------------------------------
alter table ores_assets_images_tbl enable row level security;

create policy ores_assets_images_tbl_tenant_isolation_policy on ores_assets_images_tbl
for all using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Tags
-- -----------------------------------------------------------------------------
alter table ores_assets_tags_tbl enable row level security;

create policy ores_assets_tags_tbl_tenant_isolation_policy on ores_assets_tags_tbl
for all using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Image Tags (many-to-many)
-- -----------------------------------------------------------------------------
alter table ores_assets_image_tags_tbl enable row level security;

create policy ores_assets_image_tags_tbl_tenant_isolation_policy on ores_assets_image_tags_tbl
for all using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());
