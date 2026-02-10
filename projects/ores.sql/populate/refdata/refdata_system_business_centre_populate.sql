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
 * System Business Centre Population Script
 *
 * Seeds the 'WRLD' (World) business centre for the system tenant. This global
 * business centre is used by system parties and other entities not tied to a
 * specific geographic location. Follows the FPML 4-character code convention.
 *
 * Must run after: coding schemes (NONE scheme must exist), system tenant.
 *
 * This script is idempotent.
 */

\echo '--- System Business Centre ---'

-- Use a helper function for idempotent creation
create or replace function ores_seed_system_business_centre_fn()
returns void as $$
begin
    if not exists (
        select 1 from ores_refdata_business_centres_tbl
        where tenant_id = ores_iam_system_tenant_id_fn()
        and code = 'WRLD'
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_refdata_business_centres_tbl (
            code, tenant_id, version, coding_scheme_code,
            description, modified_by, performed_by,
            change_reason_code, change_commentary
        ) values (
            'WRLD', ores_iam_system_tenant_id_fn(), 0, 'NONE',
            'World. Global business centre for entities not tied to a specific geographic location.',
            current_user, current_user,
            'system.initial_load', 'System business centre for platform tenant'
        );
        raise notice 'Created WRLD business centre for system tenant';
    else
        raise notice 'WRLD business centre already exists for system tenant';
    end if;
end;
$$ language plpgsql;

select ores_seed_system_business_centre_fn();

drop function ores_seed_system_business_centre_fn();

-- Summary
select 'refdata_business_centres (system)' as entity, count(*) as count
from ores_refdata_business_centres_tbl
where tenant_id = ores_iam_system_tenant_id_fn()
and valid_to = ores_utility_infinity_timestamp_fn();
