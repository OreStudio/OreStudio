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
-- System Settings Access Function
-- =============================================================================

-- SECURITY DEFINER: called from service contexts (e.g. IAM) that hold no direct
-- SELECT grant on ores_variability_system_settings_tbl. The function owner (DDL
-- user) performs the read; the caller needs only EXECUTE on this function.

-- Tenant-scoped variant: used by IAM when handling per-tenant requests.
create or replace function ores_variability_get_system_settings_fn(
    p_tenant_id uuid
) returns table(setting_name text, setting_value text) as $$
    select name, value
    from ores_variability_system_settings_tbl
    where tenant_id = p_tenant_id
      and valid_to = ores_utility_infinity_timestamp_fn()
    order by name;
$$ language sql stable security definer set search_path = public, pg_temp;

-- No-argument variant: used at service startup before a tenant_id is known
-- (e.g. auth_handler, bootstrap_handler token/mode checks). Reads the system
-- tenant's settings, which hold the global configuration values.
create or replace function ores_variability_get_system_settings_fn()
returns table(setting_name text, setting_value text) as $$
    select name, value
    from ores_variability_system_settings_tbl
    where tenant_id = ores_iam_system_tenant_id_fn()
      and valid_to = ores_utility_infinity_timestamp_fn()
    order by name;
$$ language sql stable security definer set search_path = public, pg_temp;
