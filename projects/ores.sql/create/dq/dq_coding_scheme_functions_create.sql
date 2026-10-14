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
-- Validate that a coding_scheme_code names a known coding scheme (a required
-- soft FK). Coding schemes are shared reference data, so the check reads the
-- system tenant's rows whatever the caller's tenant.
--
-- SECURITY DEFINER: called from the insert triggers of other components,
-- whose service roles do not hold SELECT on ores_dq_coding_schemes_tbl.
create or replace function ores_dq_validate_coding_scheme_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    if p_value is null or p_value = '' then
        raise exception 'coding_scheme_code is required.' using errcode = '23502';
    end if;

    -- Bootstrap writes coded rows before any coding scheme exists.
    if not exists (select 1 from ores_dq_coding_schemes_tbl limit 1) then
        return p_value;
    end if;

    if not exists (
        select 1 from ores_dq_coding_schemes_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid coding_scheme_code: %. Must reference a valid coding scheme.',
            p_value using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql
    stable
    security definer
    set search_path = public, pg_temp;
