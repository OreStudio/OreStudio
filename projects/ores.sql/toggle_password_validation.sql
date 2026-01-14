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
 * Toggle Password Validation Script
 *
 * Helper script to enable or disable password validation for testing.
 *
 * Usage:
 *   -- To DISABLE password validation (allow weak passwords for testing):
 *   \set new_value 1
 *   \i projects/ores.sql/toggle_password_validation.sql
 *
 *   -- To ENABLE password validation (enforce strong passwords):
 *   \set new_value 0
 *   \i projects/ores.sql/toggle_password_validation.sql
 */

set schema 'ores';

-- Default to enabled (0) if not specified
\if :{?new_value}
    -- Variable is set, use it
\else
    \set new_value 0
\endif

-- Update the flag using bitemporal pattern
insert into ores.variability_feature_flags_tbl (name, enabled, description, modified_by, valid_from, valid_to)
values (
    'system.disable_password_validation',
    true,
    'When enabled (1), disables strict password validation. FOR TESTING/DEVELOPMENT ONLY.',
    current_user,
    current_timestamp,
    ores.utility_infinity_timestamp_fn()
);

-- Show the updated state
select name, enabled, description, modified_by, valid_from
from ores.variability_feature_flags_tbl
where name = 'system.disable_password_validation'
  and valid_to = ores.utility_infinity_timestamp_fn();

\if :new_value
    \echo 'Password validation is now DISABLED (weak passwords allowed)'
\else
    \echo 'Password validation is now ENABLED (strong passwords required)'
\endif
