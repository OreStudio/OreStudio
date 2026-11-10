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
 * The feed table carries the publisher, session, service and provider key, and
 * one feed of a publisher is the default.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/marketdata_feeds_test.sql
 */

begin;

select plan(6);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create or replace function pg_temp.feed_insert(
    p_name text,
    p_publisher text,
    p_session text,
    p_default boolean,
    p_service text default 'end_of_day',
    p_key text default '')
returns void as $$
begin
    insert into ores_marketdata_feeds_tbl (
        id, tenant_id, version, name, publisher_code, session_code, service,
        provider_key, is_default, description,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, p_name,
        p_publisher, p_session, p_service, p_key, p_default,
        'pgTAP feed fixture',
        current_user, current_user, 'system.initial_load', 'pgTAP feed fixture'
    );
end;
$$ language plpgsql;

-- The number of feeds the seed installs is not stated here: a feed the
-- catalogue gains is not a fault. What the seed must get right is that every
-- feed it installs names a publisher the catalogue holds and a session that is
-- enabled, so a feed nobody can read through cannot be seeded.
select is(
    (select count(*)::int from ores_marketdata_feeds_tbl f
     where f.tenant_id = ores_utility_system_tenant_id_fn()
       and f.valid_to = ores_utility_infinity_timestamp_fn()
       and (not exists (
                select 1 from ores_marketdata_providers_tbl p
                where p.code = f.publisher_code
                  and p.valid_to = ores_utility_infinity_timestamp_fn())
            or not exists (
                select 1 from ores_marketdata_sessions_tbl s
                where s.code = f.session_code
                  and s.tenant_id = f.tenant_id
                  and s.enabled
                  and s.valid_to = ores_utility_infinity_timestamp_fn()))),
    0,
    'every feed names a catalogued publisher and an enabled session'
);

select is(
    (select count(*)::int from ores_marketdata_feeds_tbl f
     where f.tenant_id = ores_utility_system_tenant_id_fn()
       and f.name in ('ecb-frankfurter', 'boc-frankfurter', 'imf-frankfurter', 'tcmb-frankfurter')
       and f.valid_to = ores_utility_infinity_timestamp_fn()
       and (f.session_code <> 'frankfurter-eu'
            or f.service <> 'end_of_day'
            or f.provider_key = '')),
    0,
    'every Frankfurter feed reads through the Frankfurter session at end of day'
);

select is(
    (select count(*)::int from (
        select publisher_code
        from ores_marketdata_feeds_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and is_default
          and valid_to = ores_utility_infinity_timestamp_fn()
        group by publisher_code
        having count(*) > 1
    ) duplicated),
    0,
    'no publisher has two default feeds'
);

select throws_ok(
    $$select pg_temp.feed_insert('ecb-second-default', 'ecb', 'frankfurter-eu', true)$$,
    '23505',
    null,
    'a second default feed for one publisher is refused'
);

select throws_ok(
    $$select pg_temp.feed_insert('feed-no-session', 'ecb', 'no-such-session', false)$$,
    '23503',
    null,
    'a feed naming a session outside the catalogue is refused'
);

select is(
    (select f.session_code
     from ores_marketdata_feeds_tbl f
     where f.tenant_id = ores_utility_system_tenant_id_fn()
       and f.name = 'ecb-frankfurter'
       and f.valid_to = ores_utility_infinity_timestamp_fn()),
    'frankfurter-eu',
    'the ECB feed reads back with its session'
);

select * from finish();

rollback;
