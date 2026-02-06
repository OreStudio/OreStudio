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
 * System Tenant Population Script
 *
 * Creates the system (platform) tenant.
 * This tenant owns all shared governance data (DQ catalogs, datasets, etc.)
 * and is used for platform administration.
 *
 * The system tenant uses the max UUID (RFC 9562 sentinel value) to prevent
 * confusion with uninitialized UUIDs (nil UUID is default-constructed value).
 * This script is idempotent.
 */

\echo '--- System Tenant ---'

-- Helper function for idempotent system tenant creation
create or replace function ores_seed_system_tenant_fn()
returns void as $$
begin
    if not exists (
        select 1 from ores_iam_tenants_tbl
        where id = ores_iam_system_tenant_id_fn()
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        -- Note: tenant_id is set automatically by the trigger to system_tenant_id
        insert into ores_iam_tenants_tbl (
            id,
            version,
            type,
            code,
            name,
            description,
            hostname,
            status,
            modified_by,
            performed_by,
            change_reason_code,
            change_commentary
        ) values (
            'ffffffff-ffff-ffff-ffff-ffffffffffff'::uuid,
            0,
            'platform',
            'system',
            'System',
            'Platform-level system tenant for shared governance data and administration',
            'localhost',
            'active',
            current_user,
            current_user,
            'system.initial_load',
            'Initial system tenant creation'
        );
        raise notice 'Created system tenant';
    else
        raise notice 'System tenant already exists';
    end if;
end;
$$ language plpgsql;

select ores_seed_system_tenant_fn();

drop function ores_seed_system_tenant_fn();

-- Summary
select 'Tenants' as entity, count(*) as count
from ores_iam_tenants_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
