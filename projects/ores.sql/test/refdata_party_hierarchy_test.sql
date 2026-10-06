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
 * pgTAP tests for the party hierarchy function's parent-cycle guard.
 *
 * A parent cycle is a ring with no root, so neither the walk up to the root
 * nor the walk down from it terminates on its own. The function carries the
 * path it has walked and stops when a row repeats.
 *
 * Run with: pg_prove -d ores_dev_local1 test/refdata_party_hierarchy_test.sql
 */

begin;

select plan(2);

-- A root, then its child, then a second version of the root that points at the
-- child. The two rows now point at each other, and no row is a root.
insert into ores_refdata_parties_tbl (
    id, tenant_id, version, full_name, short_code, party_category, party_type,
    parent_party_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'b0000000-0000-0000-0000-000000000001'::uuid,
    ores_utility_system_tenant_id_fn(), 0, 'Cycle Root', 'CYCR', 'Operational', 'Bank',
    NULL, 'WRLD', 'Active',
    current_user, current_user, 'system.test', 'Cycle guard test'
);

insert into ores_refdata_parties_tbl (
    id, tenant_id, version, full_name, short_code, party_category, party_type,
    parent_party_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'b0000000-0000-0000-0000-000000000002'::uuid,
    ores_utility_system_tenant_id_fn(), 0, 'Cycle Child', 'CYCC', 'Operational', 'Bank',
    'b0000000-0000-0000-0000-000000000001'::uuid, 'WRLD', 'Active',
    current_user, current_user, 'system.test', 'Cycle guard test'
);

-- The version states the row this write replaces, so this closes the root's
-- first version and inserts the version that points at its own child.
insert into ores_refdata_parties_tbl (
    id, tenant_id, version, full_name, short_code, party_category, party_type,
    parent_party_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'b0000000-0000-0000-0000-000000000001'::uuid,
    ores_utility_system_tenant_id_fn(), 1, 'Cycle Root', 'CYCR', 'Operational', 'Bank',
    'b0000000-0000-0000-0000-000000000002'::uuid, 'WRLD', 'Active',
    current_user, current_user, 'system.test', 'Cycle guard test'
);

select is(
    (select count(*)::integer from ores_refdata_parties_hierarchy_fn(
        ores_utility_system_tenant_id_fn(),
        'b0000000-0000-0000-0000-000000000001'::uuid,
        false)),
    2,
    'party hierarchy: a cycle yields every row and the walk terminates'
);

select is(
    (select count(*)::integer from ores_refdata_parties_hierarchy_fn(
        ores_utility_system_tenant_id_fn(),
        'b0000000-0000-0000-0000-000000000001'::uuid,
        true)),
    2,
    'party hierarchy: a cycle with no root falls back to the given node'
);

select * from finish();

rollback;
