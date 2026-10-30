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
 * Each catalogue read has its permission, and the Viewer role holds it, so the
 * Connectivity screen can read publishers, suppliers, adapters, sessions and
 * feeds without a write grant.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/marketdata_catalogue_read_permissions_test.sql
 */

begin;

select plan(6);

select is(
    (select count(*)::int from ores_iam_permissions_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and code in (
           'marketdata::providers:read',
           'marketdata::suppliers:read',
           'marketdata::adapters:read',
           'marketdata::sessions:read',
           'marketdata::feeds:read')
       and valid_to = ores_utility_infinity_timestamp_fn()),
    5,
    'the five catalogue read permissions are seeded'
);

select is(
    (select count(*)::int from ores_iam_role_permissions_tbl rp
     join ores_iam_roles_tbl r on r.id = rp.role_id
     join ores_iam_permissions_tbl p on p.id = rp.permission_id
     where r.tenant_id = ores_utility_system_tenant_id_fn()
       and r.name = 'Viewer'
       and p.code in (
           'marketdata::providers:read',
           'marketdata::suppliers:read',
           'marketdata::adapters:read',
           'marketdata::sessions:read',
           'marketdata::feeds:read')
       and rp.valid_to = ores_utility_infinity_timestamp_fn()
       and p.valid_to = ores_utility_infinity_timestamp_fn()),
    5,
    'the Viewer role holds all five reads'
);

select ok(
    exists (select 1 from ores_marketdata_providers_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and valid_to = ores_utility_infinity_timestamp_fn()),
    'a publisher can be listed'
);

select ok(
    exists (select 1 from ores_marketdata_sessions_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and valid_to = ores_utility_infinity_timestamp_fn()),
    'a session can be listed'
);

select ok(
    exists (select 1 from ores_marketdata_feeds_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and valid_to = ores_utility_infinity_timestamp_fn()),
    'a feed can be listed'
);

select ok(
    exists (select 1 from ores_marketdata_feed_scopes_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and valid_to = ores_utility_infinity_timestamp_fn()),
    'a feed scope can be listed'
);

select * from finish();

rollback;
