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
 * Password Validation Disable Flag Setup Script
 *
 * This script creates the system.disable_password_validation feature flag,
 * which allows disabling strict password validation for TESTING/DEVELOPMENT
 * environments.
 *
 * IMPORTANT: This flag should be enabled=0 (false) in production environments.
 * Only enable it (set to 1) for testing/development purposes.
 *
 * The script is idempotent and can be safely run multiple times.
 */

set schema 'ores';

-- Insert the password validation disable flag if it doesn't already exist
do $$
declare
    flag_exists boolean;
begin
    -- Check if the flag already exists
    select exists(
        select 1 from ores.feature_flags
        where name = 'system.disable_password_validation'
          and valid_to = '9999-12-31 23:59:59'::timestamptz
    ) into flag_exists;

    if not flag_exists then
        -- Insert the flag with enabled=0 (validation ENABLED by default for security)
        insert into ores.feature_flags (name, enabled, description, modified_by, valid_from, valid_to)
        values (
            'system.disable_password_validation',
            0,  -- 0 = false = password validation ENABLED (secure default)
            'When enabled (1), disables strict password validation. FOR TESTING/DEVELOPMENT ONLY.',
            'system',
            current_timestamp,
            '9999-12-31 23:59:59'::timestamptz
        );

        raise notice 'Created system.disable_password_validation flag with enabled=0 (password validation ENABLED)';
    else
        raise notice 'Password validation flag already exists, skipping initialization';
    end if;
end;
$$ language plpgsql;

-- Query to check current state
select name, enabled, description, modified_by
from ores.feature_flags
where name = 'system.disable_password_validation'
  and valid_to = '9999-12-31 23:59:59'::timestamptz;
