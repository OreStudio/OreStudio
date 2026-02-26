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
 * Business Unit Types Population Script
 *
 * 1. Adds the ORES-ORG coding scheme for internal organisational hierarchy.
 * 2. Seeds the 5 standard business unit types under ORES-ORG.
 *
 * This script is idempotent.
 */

\echo '--- Business Unit Types: coding scheme ---'

select ores_dq_coding_schemes_upsert_fn(ores_iam_system_tenant_id_fn(),
    'ORES-ORG',
    'ORE Studio Organisational Hierarchy',
    'internal',
    'Parties',
    'Reference Data',
    '',
    'Internal coding scheme for ORE Studio organisational unit classification.'
);

\echo '--- Business Unit Types: seed types ---'

-- Helper: insert one business unit type, skipping if the natural key already exists.
do $$
declare
    v_tenant_id uuid := ores_iam_system_tenant_id_fn();
    v_scheme    text := 'ORES-ORG';

    type_rows record;
begin
    for type_rows in (
        select *
        from (values
            (gen_random_uuid(), 'DIVISION',      'Division',      0, 'Top-level functional grouping within a party.'),
            (gen_random_uuid(), 'BRANCH',         'Branch',        0, 'Top-level geographic grouping within a party.'),
            (gen_random_uuid(), 'BUSINESS_AREA',  'Business Area', 1, 'Cohesive set of business activities within a division.'),
            (gen_random_uuid(), 'DESK',           'Trading Desk',  2, 'Operational trading or risk desk; direct owner of books.'),
            (gen_random_uuid(), 'COST_CENTRE',    'Cost Centre',   2, 'Finance/accounting unit, leaf of the hierarchy.')
        ) as t(id, code, name, level, description)
    ) loop
        if not exists (
            select 1
            from ores_refdata_business_unit_types_tbl
            where tenant_id = v_tenant_id
              and coding_scheme_code = v_scheme
              and code = type_rows.code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            insert into ores_refdata_business_unit_types_tbl (
                id, tenant_id, version, coding_scheme_code, code, name,
                level, description, modified_by, performed_by,
                change_reason_code, change_commentary
            ) values (
                type_rows.id, v_tenant_id, 0, v_scheme,
                type_rows.code, type_rows.name,
                type_rows.level, type_rows.description,
                current_user, current_user,
                'system.initial_load', 'Initial population of business unit types'
            );
        end if;
    end loop;
end;
$$;

-- Summary
select 'refdata_business_unit_types' as entity, count(*) as count
from ores_refdata_business_unit_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
