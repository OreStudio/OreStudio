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
 * Change Reason Functions
 *
 * Shared functions for validating and defaulting change_reason_code values
 * across all bitemporal tables.
 */

set schema 'ores';

/**
 * Validates and defaults a change_reason_code value.
 *
 * - If the code is NULL or empty, returns 'system.new_record' (for new records)
 * - If change_reasons table is empty, skips validation (allows tests without seeded data)
 * - If the code is non-empty and table has data, validates it exists
 * - Raises an exception if an explicitly provided code doesn't exist
 *
 * Usage in triggers:
 *   NEW.change_reason_code := ores.validate_and_default_change_reason(NEW.change_reason_code);
 */
create or replace function ores.validate_and_default_change_reason(
    p_change_reason_code text
) returns text as $$
begin
    -- Default empty/null to system.new_record for new records
    if p_change_reason_code is null or p_change_reason_code = '' then
        return 'system.new_record';
    end if;

    -- Skip validation if change_reasons table is empty (test databases)
    if not exists (select 1 from ores.change_reasons limit 1) then
        return p_change_reason_code;
    end if;

    -- Validate that explicitly provided codes exist in change_reasons
    if not exists (
        select 1 from ores.change_reasons
        where code = p_change_reason_code
        and valid_to = ores.infinity_timestamp()
    ) then
        raise exception 'Invalid change_reason_code: %. Reason must exist in change_reasons table.',
            p_change_reason_code
            using errcode = '23503';  -- foreign_key_violation
    end if;

    return p_change_reason_code;
end;
$$ language plpgsql;
