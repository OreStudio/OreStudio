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
 * The product type catalogue is one copy, held in the system tenant.
 *
 * A session of any tenant reads it: the three tables carry the
 * own-or-system-tenant policy, so a tenant reads its own rows and the system
 * tenant's and nobody else's. No tenant holds a copy, and the write half of the
 * policy keeps it that way: a tenant may write only its own rows, and the
 * catalogue it reads is not its own.
 *
 * The session tenant is set to an id no tenant table states, because the policy
 * compares the row against the session's tenant rather than against a row: a
 * made-up id proves the read reaches the system rows without needing a
 * provisioned tenant to exist.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/trading_product_type_catalogue_visibility_test.sql
 */

begin;

select plan(6);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    (select count(*)::int from ores_trading_product_types_tbl
     where valid_to = ores_utility_infinity_timestamp_fn()),
    121,
    'the system tenant holds the 121 product types');

select is(
    (select count(*)::int from ores_trading_product_types_tbl
     where tenant_id <> ores_utility_system_tenant_id_fn()),
    0,
    'no tenant holds a copy of the product type catalogue');

-- A session of a tenant that never held catalogue rows reads the system ones.
select set_config('app.current_tenant_id', '11111111-1111-4111-8111-111111111111', true);

select is(
    (select count(*)::int from ores_trading_product_types_tbl
     where valid_to = ores_utility_infinity_timestamp_fn()),
    121,
    'a tenant session reads the whole product type catalogue');

select is(
    (select count(*)::int from ores_trading_product_type_asset_classes_tbl
     where valid_to = ores_utility_infinity_timestamp_fn()),
    121,
    'a tenant session reads the markets the catalogue references');

select is(
    (select count(*)::int from ores_refdata_product_family_codes_tbl
     where valid_to = ores_utility_infinity_timestamp_fn()),
    8,
    'a tenant session reads the product families');

-- The write half of the policy keeps the catalogue the system tenant's: a
-- session reads the system rows, and may write only rows of its own tenant.
select ok(
    (select with_check from pg_policies
     where tablename = 'ores_trading_product_types_tbl'
       and policyname = 'product_types_tbl_tenant_isolation_policy')
        not like '%ores_utility_system_tenant_id_fn%',
    'the write half of the product type policy refuses a row the session does not own');

rollback;
