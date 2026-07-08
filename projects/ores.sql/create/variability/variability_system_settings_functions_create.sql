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

-- Reads the settings visible to one scope: the tenant's system party for
-- system/tenant-level flags, or a specific party for party-level flags
-- like onboarding.party. Unambiguous: exactly one row per setting name for
-- a given (tenant_id, party_id). p_party_id defaults to the tenant's
-- system party when omitted, mirroring the insert trigger's write-time
-- default (see ores_variability_resolve_system_party_fn).
create or replace function ores_variability_get_system_settings_fn(
    p_tenant_id uuid,
    p_party_id uuid default null
) returns table(setting_name text, setting_value text) as $$
    select name, value
    from ores_variability_system_settings_tbl
    where tenant_id = p_tenant_id
      and party_id = coalesce(p_party_id, ores_variability_resolve_system_party_fn(p_tenant_id))
      and valid_to = ores_utility_infinity_timestamp_fn()
    order by name;
$$ language sql stable security definer set search_path = public, pg_temp;

