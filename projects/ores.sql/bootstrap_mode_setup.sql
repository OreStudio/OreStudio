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
 * Bootstrap Mode Setup Script
 *
 * This script initializes the system.bootstrap_mode feature flag, which controls
 * whether the system is in bootstrap mode (waiting for initial admin account) or
 * secure mode (admin account exists, full authentication required).
 *
 * The script is idempotent and can be safely run multiple times. It will:
 * 1. Check if any admin accounts exist
 * 2. Create the bootstrap flag if it doesn't exist
 * 3. Set the flag appropriately based on whether admin accounts exist:
 *    - enabled=1 (true): No admin accounts, system in bootstrap mode
 *    - enabled=0 (false): Admin accounts exist, system in secure mode
 */

set schema 'ores';

-- Insert the bootstrap mode feature flag if it doesn't already exist
-- The enabled state depends on whether admin accounts exist
do $$
declare
    admin_count integer;
    flag_exists boolean;
begin
    -- Check if the flag already exists
    select exists(
        select 1 from ores.variability_feature_flags_tbl
        where name = 'system.bootstrap_mode'
          and valid_to = ores.utility_infinity_timestamp_fn()
    ) into flag_exists;

    if not flag_exists then
        -- Count accounts with Admin role assigned via RBAC
        select count(*) into admin_count
        from ores.iam_account_roles_tbl ar
        join ores.iam_roles_tbl r on ar.role_id = r.id
        where r.name = 'Admin'
          and ar.valid_to = ores.utility_infinity_timestamp_fn()
          and r.valid_to = ores.utility_infinity_timestamp_fn();

        -- Insert the flag with appropriate enabled state
        -- enabled=1 (true) if no admin accounts exist (bootstrap mode)
        -- enabled=0 (false) if admin accounts exist (secure mode)
        insert into ores.variability_feature_flags_tbl (name, enabled, description, modified_by, valid_from, valid_to)
        values (
            'system.bootstrap_mode',
            case when admin_count = 0 then 1 else 0 end,
            'Indicates whether the system is in bootstrap mode (waiting for initial admin account)',
            'system',
            current_timestamp,
            ores.utility_infinity_timestamp_fn()
        );

        raise notice 'Created bootstrap flag with enabled=%', case when admin_count = 0 then '1 (bootstrap mode)' else '0 (secure mode)' end;
    else
        raise notice 'Bootstrap flag already exists, skipping initialization';
    end if;
end;
$$ language plpgsql;
