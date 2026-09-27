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
 * pgTAP tests for the system tenant's immovability.
 *
 * The dedicated lifecycle functions refuse the system tenant, but the generic
 * paths do not go through them: the generated delete rule turns a delete into
 * an UPDATE that sets status = 'terminated', and the generated save path writes
 * whatever status the caller states. A table check states that the row whose id
 * is the system tenant stays active, so all three paths are refused, and the
 * hard delete, which disables the rule and issues a real DELETE, carries its
 * own guard because a check does not see a DELETE.
 *
 * Run with: pg_prove -d ores_dev_local1 test/iam_tenant_delete_test.sql
 */

begin;

select plan(3);

-- =============================================================================
-- Test: the generic delete cannot terminate the system tenant
-- =============================================================================

select throws_ok(
    $$delete from ores_iam_tenants_tbl
      where id = ores_utility_system_tenant_id_fn()$$,
    '23514',
    NULL,
    'generic delete: the system tenant cannot be terminated'
);

-- =============================================================================
-- Test: the generic save path cannot terminate it either
-- =============================================================================

select throws_ok(
    $$update ores_iam_tenants_tbl
      set status = 'terminated'
      where id = ores_utility_system_tenant_id_fn()
        and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '23514',
    NULL,
    'generic update: the system tenant cannot be terminated'
);

-- =============================================================================
-- Test: the hard delete carries its own guard
-- =============================================================================

select throws_ok(
    $$select ores_iam_hard_delete_tenants_fn(
          array[ores_utility_system_tenant_id_fn()]::uuid[])$$,
    '42501',
    NULL,
    'hard delete: the system tenant cannot be hard deleted'
);

select * from finish();

rollback;
