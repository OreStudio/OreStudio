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

set schema 'production';

-- Seed system flags with their default values
select production.variability_feature_flags_upsert_fn(
    'system.bootstrap_mode',
    true,
    'Indicates whether the system is in bootstrap mode (waiting for initial admin account).'
);

select production.variability_feature_flags_upsert_fn(
    'system.user_signups',
    false,
    'Controls whether user self-registration is allowed.'
);

select production.variability_feature_flags_upsert_fn(
    'system.signup_requires_authorization',
    false,
    'Controls whether new signups require admin authorization. NOT YET IMPLEMENTED - enabling will cause signup to fail.'
);

select production.variability_feature_flags_upsert_fn(
    'system.disable_password_validation',
    false,
    'When enabled, disables strict password validation. FOR TESTING/DEVELOPMENT ONLY.'
);

select production.variability_feature_flags_upsert_fn(
    'system.synthetic_data_generation',
    false,
    'Enables synthetic test data generation in the UI. FOR TESTING/DEVELOPMENT ONLY.'
);

-- Show summary
select name, enabled, description
from production.variability_feature_flags_tbl
where name like 'system.%' and valid_to = public.utility_infinity_timestamp_fn()
order by name;
