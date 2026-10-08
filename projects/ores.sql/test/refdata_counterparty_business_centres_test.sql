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
 * pgTAP tests for the business centres a counterparty deals through.
 *
 * A counterparty used to carry one business_center_code column, which could
 * only ever name one centre. It now holds a set, through the
 * counterparty_business_centres junction, and the column is gone.
 *
 * The junction is the ordinary bitemporal one, so the same centre written
 * twice is an upsert rather than a duplicate.
 *
 * Run with: pg_prove -d <database> test/refdata_counterparty_business_centres_test.sql
 */

begin;

select plan(5);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- One counterparty, so the junction has an owner. The suite rolls it back.
insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version, full_name, short_code, party_type, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'b0000000-0000-0000-0000-0000000000c1'::uuid,
    ores_utility_system_tenant_id_fn(), 0, 'Business Centre Test Counterparty',
    'BCTC', 'Corporate', 'Active',
    current_user, current_user, 'system.test', 'Business centre fixture'
);

create or replace function pg_temp.add_centre(p_counterparty uuid, p_code text)
returns void as $$
    insert into ores_refdata_counterparty_business_centres_tbl (
        tenant_id, counterparty_id, business_centre_code, version,
        modified_by, performed_by, change_reason_code, change_commentary)
    values (ores_utility_system_tenant_id_fn(), p_counterparty, p_code, 0,
        current_user, current_user, 'system.new_record', 'test');
$$ language sql;

create or replace function pg_temp.live_centre_count(p_counterparty uuid)
returns int as $$
    select count(*)::int
    from ores_refdata_counterparty_business_centres_tbl
    where counterparty_id = p_counterparty
      and valid_to = ores_utility_infinity_timestamp_fn();
$$ language sql;

-- 1. The counterparty deals through a centre.
select lives_ok(
    $$select pg_temp.add_centre(
        'b0000000-0000-0000-0000-0000000000c1'::uuid, 'WRLD')$$,
    'a counterparty deals through a centre');

-- 2. And through a second one, which the single column could not name.
select lives_ok(
    $$select pg_temp.add_centre(
        'b0000000-0000-0000-0000-0000000000c1'::uuid, 'GBLO')$$,
    'the same counterparty deals through a second centre');

-- 3. Both are live, so a counterparty-side read returns the set.
select is(
    pg_temp.live_centre_count('b0000000-0000-0000-0000-0000000000c1'::uuid),
    2,
    'the counterparty holds two live centres');

-- 4. The same centre written again is an upsert, not a third row.
select lives_ok(
    $$select pg_temp.add_centre(
        'b0000000-0000-0000-0000-0000000000c1'::uuid, 'WRLD')$$,
    'writing a centre the counterparty already deals through upserts it');

-- 5. The single column is gone from the counterparty.
select is(
    (select count(*)::int from information_schema.columns
     where table_name = 'ores_refdata_counterparties_tbl'
       and column_name = 'business_center_code'),
    0,
    'the counterparty no longer carries a single business centre column');

select * from finish();

rollback;
