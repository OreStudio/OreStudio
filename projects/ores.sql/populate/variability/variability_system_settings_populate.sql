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

/**
 * System Settings Population Script
 *
 * Seeds the system_settings table with well-known system settings.
 * This script is idempotent - running it multiple times will not create
 * duplicate entries or overwrite existing values.
 */

-- -----------------------------------------------------------------------------
-- Seed system settings with their default values (insert-if-absent)
-- -----------------------------------------------------------------------------
select ores_variability_system_settings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'system.bootstrap_mode',
    'true',
    'boolean',
    'Indicates whether the system is in bootstrap mode (waiting for initial admin account).'
);

select ores_variability_system_settings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'system.user_signups',
    'false',
    'boolean',
    'Controls whether user self-registration is allowed.'
);

select ores_variability_system_settings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'system.signup_requires_authorization',
    'false',
    'boolean',
    'Controls whether new signups require admin authorization. NOT YET IMPLEMENTED - enabling will cause signup to fail.'
);

select ores_variability_system_settings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'system.disable_password_validation',
    'false',
    'boolean',
    'When enabled, disables strict password validation. FOR TESTING/DEVELOPMENT ONLY.'
);

select ores_variability_system_settings_upsert_fn(ores_iam_system_tenant_id_fn(),
    'system.synthetic_data_generation',
    'false',
    'boolean',
    'Enables synthetic test data generation in the UI. FOR TESTING/DEVELOPMENT ONLY.'
);

-- Show summary
select name, data_type, value, description
from ores_variability_system_settings_tbl
where name like 'system.%' and valid_to = ores_utility_infinity_timestamp_fn()
order by name;
