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
 * System Party Population Script
 *
 * Creates the system party for the system tenant. Every tenant has exactly one
 * system party (party_category='system') representing the organisation itself.
 * This party serves as the root of the party hierarchy (parent_party_id=NULL).
 *
 * Must run after: party_categories, party_types, party_statuses, change_reasons.
 *
 * This script is idempotent.
 */

\echo '--- System Party ---'

-- Use a helper function for idempotent creation
create or replace function ores_seed_system_party_fn()
returns void as $$
begin
    if not exists (
        select 1 from ores_refdata_parties_tbl
        where tenant_id = ores_iam_system_tenant_id_fn()
        and party_category = 'system'
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_refdata_parties_tbl (
            id, tenant_id, full_name, short_code, party_category,
            party_type, business_center_code, parent_party_id, status,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), ores_iam_system_tenant_id_fn(),
            'System Party', 'system', 'system',
            'Internal', 'WRLD', null, 'Active',
            current_user, current_user, 'system.initial_load',
            'Root system party for the platform tenant'
        );
        raise notice 'Created system party for system tenant';
    else
        raise notice 'System party already exists for system tenant';
    end if;
end;
$$ language plpgsql;

select ores_seed_system_party_fn();

drop function ores_seed_system_party_fn();

-- Summary
select 'refdata_parties (system)' as entity, count(*) as count
from ores_refdata_parties_tbl
where tenant_id = ores_iam_system_tenant_id_fn()
and valid_to = ores_utility_infinity_timestamp_fn();
