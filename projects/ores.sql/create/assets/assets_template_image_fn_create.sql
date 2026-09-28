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

-- Hand-written: a security-definer accessor, so it belongs outside the files
-- the image model regenerates. A service reads the system tenant's template
-- image by code across the tenant-isolation policy: owned by the table owner,
-- it runs with owner privileges whatever the caller's own tenant context is.
create or replace function ores_assets_get_template_image_fn(p_code text)
returns table("description" text, "mime_type" text, "data" text)
language sql
security definer
set search_path = public
as $$
    select "description", "mime_type", "data"
    from "ores_assets_images_tbl"
    where tenant_id = ores_utility_system_tenant_id_fn()
    and code = p_code
    and valid_to = ores_utility_infinity_timestamp_fn();
$$;

-- PostgreSQL grants EXECUTE on a new function to PUBLIC. This one reads across
-- the tenant-isolation policy, so it belongs to the two services that call it
-- and to nothing else: ores.iam.service for the provisioner's logo copy, and
-- ores.refdata.service for the LEI party publish. Their grants are named in
-- projects/modeling/service_registry.org and generated into the service grants.
revoke execute on function ores_assets_get_template_image_fn(text) from public;
