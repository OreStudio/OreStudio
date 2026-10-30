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
 * A feed's scope names its items, and no item belongs to two feeds of one
 * publisher.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/marketdata_feed_scopes_test.sql
 */

begin;

select plan(6);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    (select count(*)::int
     from ores_marketdata_feed_scopes_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select count(*)::int
     from ores_marketdata_market_data_mappings_tbl m
     join ores_marketdata_market_data_vendor_instruments_tbl v
       on v.id = m.vendor_instrument_id
     where m.tenant_id = ores_utility_system_tenant_id_fn()
       and v.provider_code in ('ecb', 'boc', 'imf', 'tcmb')
       and m.valid_to = ores_utility_infinity_timestamp_fn()
       and v.valid_to = ores_utility_infinity_timestamp_fn()),
    'every public feed scope row is a mapping of its publisher'
);

select is(
    (select count(distinct f.name)::int
     from ores_marketdata_feed_scopes_tbl s
     join ores_marketdata_feeds_tbl f on f.id = s.feed_id
     where s.tenant_id = ores_utility_system_tenant_id_fn()
       and s.valid_to = ores_utility_infinity_timestamp_fn()),
    4,
    'the four public feeds carry a scope'
);

create temporary table t_ecb on commit drop as
select s.feed_id, s.item
from ores_marketdata_feed_scopes_tbl s
join ores_marketdata_feeds_tbl f on f.id = s.feed_id
where f.name = 'ecb-frankfurter'
  and s.valid_to = ores_utility_infinity_timestamp_fn()
limit 1;

-- A publisher with two feeds: the same item may not sit in both.
insert into ores_marketdata_feeds_tbl (
    id, tenant_id, version, name, publisher_code, session_code, service,
    provider_key, is_default, description,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, 'ecb-second',
    'ecb', 'frankfurter-eu', 'real_time', 'ECB', false, 'pgTAP second ecb feed',
    current_user, current_user, 'system.initial_load', 'pgTAP second ecb feed'
);

select throws_ok(
    format($$
        insert into ores_marketdata_feed_scopes_tbl (
            id, tenant_id, version, feed_id, asset_class, item, vendor_instrument_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
            (select id from ores_marketdata_feeds_tbl where name = 'ecb-second' and valid_to = ores_utility_infinity_timestamp_fn()),
            'fx', %L, null,
            current_user, current_user, 'system.initial_load', 'pgTAP scope overlap'
        )$$, (select item from t_ecb)),
    '23514',
    null,
    'an item of one feed is refused for another feed of the same publisher'
);

select lives_ok(
    format($$
        insert into ores_marketdata_feed_scopes_tbl (
            id, tenant_id, version, feed_id, asset_class, item, vendor_instrument_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
            (select id from ores_marketdata_feeds_tbl where name = 'demo-a' and valid_to = ores_utility_infinity_timestamp_fn()),
            'fx', %L, null,
            current_user, current_user, 'system.initial_load', 'pgTAP scope other publisher'
        )$$, (select item from t_ecb)),
    'the same item under another publisher is allowed'
);

select is(
    (select count(*)::int
     from ores_marketdata_feed_scopes_tbl s
     join ores_marketdata_feeds_tbl f on f.id = s.feed_id
     where f.name = 'ecb-frankfurter'
       and s.valid_to = ores_utility_infinity_timestamp_fn()),
    (select count(*)::int
     from ores_marketdata_feed_scopes_tbl s
     join ores_marketdata_feeds_tbl f on f.id = s.feed_id
     where f.name = 'ecb-frankfurter'
       and s.item like 'oresmd://fx/%'
       and s.valid_to = ores_utility_infinity_timestamp_fn()),
    'the ECB feed reads back the items its scope names'
);

select is(
    (select count(*)::int from (
        select s.item
        from ores_marketdata_feed_scopes_tbl s
        join ores_marketdata_feeds_tbl f on f.id = s.feed_id
        where s.tenant_id = ores_utility_system_tenant_id_fn()
          and s.valid_to = ores_utility_infinity_timestamp_fn()
        group by f.publisher_code, s.item
        having count(distinct s.feed_id) > 1
    ) overlapping),
    0,
    'no item is held by two feeds of one publisher'
);

select * from finish();

rollback;
