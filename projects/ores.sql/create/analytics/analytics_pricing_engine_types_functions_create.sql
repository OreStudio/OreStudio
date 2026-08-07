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

-- Validate instrument_type_code references a known instrument code (soft FK).
-- Validates against system tenant's instrument codes (shared reference data).
-- The NONE sentinel is a valid value for engine types with no direct mapping.
--
-- SECURITY DEFINER: called from INSERT triggers in analytics service, which
-- does not hold SELECT on ores_refdata_instrument_codes_tbl.
-- Running as the DDL owner satisfies the read.
create or replace function ores_analytics_validate_pricing_engine_instrument_type_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    -- Pass-through during bootstrap (refdata table not yet populated)
    if not exists (select 1 from ores_refdata_instrument_codes_tbl limit 1) then
        return p_value;
    end if;

    -- Validate against system tenant's instrument codes (shared reference data)
    if not exists (
        select 1 from ores_refdata_instrument_codes_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid instrument_type_code: %. Must reference a valid instrument code.',
            p_value using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql
    stable
    security definer
    set search_path = public, pg_temp;
