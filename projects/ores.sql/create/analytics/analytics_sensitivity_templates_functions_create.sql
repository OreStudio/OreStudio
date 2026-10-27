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

-- The two validators a sensitivity template's lookups are checked by. They live
-- here rather than in a generated file because each is a rule across tables: the
-- method's validator reads the analytics methods table, and the class's validator
-- delegates to the reference data's own function and holds a null to mean "no
-- class", which that function refuses.
--
-- SECURITY DEFINER: the insert triggers run as the analytics service, which does
-- not hold SELECT on the tables these read. Running as the DDL owner satisfies it.

create or replace function ores_analytics_validate_sensitivity_method_code_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    -- Allow pass-through during bootstrap (the methods are not yet seeded).
    if not exists (
        select 1 from ores_analytics_sensitivity_methods_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    if not exists (
        select 1 from ores_analytics_sensitivity_methods_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid method_code: %. Must be one of: %', p_value, (
            select string_agg(code::text, ', ' order by display_order)
            from ores_analytics_sensitivity_methods_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql
    stable
    security definer
    set search_path = public, pg_temp;

create or replace function ores_analytics_validate_sensitivity_asset_class_code_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    -- No class is a statement, not a mistake: the trade-level method is the one
    -- ORE defines without a class. The reference data's function refuses a null,
    -- so the null is answered here and everything else is its decision.
    if p_value is null or p_value = '' then
        return null;
    end if;

    return ores_refdata_validate_asset_class_code_fn(p_tenant_id, p_value);
end;
$$ language plpgsql
    stable
    security definer
    set search_path = public, pg_temp;
