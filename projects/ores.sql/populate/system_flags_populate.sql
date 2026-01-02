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
 * System Flags Population Script
 *
 * Seeds the feature_flags table with well-known system flags.
 * This script is idempotent - running it multiple times will not create
 * duplicate entries or overwrite existing values.
 *
 * System flags use the "system." prefix followed by the flag name.
 *
 * Predefined System Flags:
 * - system.bootstrap_mode: Indicates system is waiting for initial admin account
 * - system.user_signups: Controls whether user self-registration is allowed
 * - system.signup_requires_authorization: New signups require admin approval
 * - system.disable_password_validation: Disables strict password validation (dev only)
 */

set schema 'ores';

-- Helper function to create a system flag if it doesn't exist
create or replace function ores.upsert_system_flag(
    p_name text,
    p_enabled boolean,
    p_description text
) returns void as $$
begin
    -- Only insert if flag doesn't exist (preserve existing values)
    if not exists (
        select 1 from ores.feature_flags
        where name = p_name and valid_to = ores.infinity_timestamp()
    ) then
        -- The update_feature_flags trigger will set version and valid_from/valid_to
        -- Cast boolean to integer for the enabled column
        insert into ores.feature_flags (name, enabled, description, modified_by, valid_from, valid_to)
        values (p_name, p_enabled::int, p_description, 'system',
                current_timestamp, ores.infinity_timestamp());
        raise notice 'Created system flag: % (default: %)', p_name, p_enabled;
    else
        raise notice 'System flag already exists: %', p_name;
    end if;
end;
$$ language plpgsql;

-- Seed system flags with their default values
select ores.upsert_system_flag(
    'system.bootstrap_mode',
    true,
    'Indicates whether the system is in bootstrap mode (waiting for initial admin account).'
);

select ores.upsert_system_flag(
    'system.user_signups',
    false,
    'Controls whether user self-registration is allowed.'
);

select ores.upsert_system_flag(
    'system.signup_requires_authorization',
    false,
    'Controls whether new signups require admin authorization. NOT YET IMPLEMENTED - enabling will cause signup to fail.'
);

select ores.upsert_system_flag(
    'system.disable_password_validation',
    false,
    'When enabled, disables strict password validation. FOR TESTING/DEVELOPMENT ONLY.'
);

-- Clean up helper function
drop function ores.upsert_system_flag(text, boolean, text);

-- Show summary
select name, enabled, description
from ores.feature_flags
where name like 'system.%' and valid_to = ores.infinity_timestamp()
order by name;
