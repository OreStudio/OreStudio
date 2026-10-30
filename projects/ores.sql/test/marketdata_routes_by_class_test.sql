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
 * A route is (party, publisher, class), and the feed it names serves both the
 * publisher and the class.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/marketdata_routes_by_class_test.sql
 */

begin;

select plan(5);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    (select count(*)::int from ores_marketdata_routes_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    5,
    'the seed holds five routes, four FX and one IR'
);

create temporary table t_ecb on commit drop as
select r.party_id, r.publisher_code, r.asset_class, r.feed_id
from ores_marketdata_routes_tbl r
where r.publisher_code = 'ecb'
  and r.tenant_id = ores_utility_system_tenant_id_fn()
  and r.valid_to = ores_utility_infinity_timestamp_fn()
limit 1;

select throws_ok(
    format($$
        insert into ores_marketdata_routes_tbl (
            id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, %L, %L, %L, %L,
            current_user, current_user, 'system.initial_load', 'pgTAP duplicate route'
        )$$, (select party_id from t_ecb), (select publisher_code from t_ecb),
        (select asset_class from t_ecb), (select feed_id from t_ecb)),
    '23505',
    null,
    'a party has one route for a publisher and class'
);

select throws_ok(
    format($$
        insert into ores_marketdata_routes_tbl (
            id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, %L, %L, 'ir', %L,
            current_user, current_user, 'system.initial_load', 'pgTAP wrong class'
        )$$, (select party_id from t_ecb), (select publisher_code from t_ecb),
        (select feed_id from t_ecb)),
    '23514',
    null,
    'a feed that serves FX is refused for an IR route'
);

select throws_ok(
    format($$
        insert into ores_marketdata_routes_tbl (
            id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, %L, 'imf', 'fx', %L,
            current_user, current_user, 'system.initial_load', 'pgTAP wrong publisher'
        )$$, (select party_id from t_ecb), (select feed_id from t_ecb)),
    '23514',
    null,
    'a feed of another publisher is refused'
);

select is(
    (select count(*)::int
     from ores_marketdata_routes_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and asset_class = 'fx'
       and publisher_code in ('ecb', 'imf', 'boc', 'tcmb')
       and valid_to = ores_utility_infinity_timestamp_fn()),
    4,
    'the four public publishers route at FX'
);

select * from finish();

rollback;
