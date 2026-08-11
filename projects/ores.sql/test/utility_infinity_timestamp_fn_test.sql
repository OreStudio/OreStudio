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
 * pgTAP tests for ores_utility_infinity_timestamp_fn() TimeZone independence.
 *
 * The function returns the bitemporal sentinel instant and is IMMUTABLE.
 * It must fold to the same instant in every session TimeZone; the naive
 * literal '9999-12-31 23:59:59'::timestamptz must NOT fold to that
 * instant in a non-UTC session, or the planner could prove the app's
 * naive-literal condition equivalent to the partial-index predicates
 * and drop the valid_to filter (task 3828BF82).
 *
 * Run with: pg_prove -d ores_dev_local1 test/utility_infinity_timestamp_fn_test.sql
 */

begin;

select plan(6);

-- =============================================================================
-- Test: The sentinel is the same instant in every session TimeZone
-- =============================================================================

-- Test 1: UTC session renders the sentinel as 23:59:59 UTC
select set_config('TimeZone', 'UTC', false);

select is(
    ores_utility_infinity_timestamp_fn() at time zone 'UTC',
    '9999-12-31 23:59:59'::timestamp,
    'sentinel: same instant in UTC session'
);

-- Test 2: America/New_York session still yields the same instant
select set_config('TimeZone', 'America/New_York', false);

select is(
    ores_utility_infinity_timestamp_fn() at time zone 'UTC',
    '9999-12-31 23:59:59'::timestamp,
    'sentinel: same instant in America/New_York session'
);

-- Test 3: Asia/Tokyo session still yields the same instant
select set_config('TimeZone', 'Asia/Tokyo', false);

select is(
    ores_utility_infinity_timestamp_fn() at time zone 'UTC',
    '9999-12-31 23:59:59'::timestamp,
    'sentinel: same instant in Asia/Tokyo session'
);

-- =============================================================================
-- Test: Fold regression -- naive literal vs sentinel function
-- =============================================================================

-- Test 4: In UTC the naive literal folds to the sentinel instant
select set_config('TimeZone', 'UTC', false);

select is(
    ores_utility_infinity_timestamp_fn() = '9999-12-31 23:59:59'::timestamptz,
    true,
    'fold: naive literal equals sentinel in UTC session'
);

-- Test 5: In America/New_York the naive literal folds away from the sentinel
select set_config('TimeZone', 'America/New_York', false);

select is(
    ores_utility_infinity_timestamp_fn() = '9999-12-31 23:59:59'::timestamptz,
    false,
    'fold: naive literal folds away from sentinel in America/New_York session'
);

-- Test 6: The explicit +00 literal equals the sentinel in any session
select is(
    ores_utility_infinity_timestamp_fn() = '9999-12-31 23:59:59+00'::timestamptz,
    true,
    'fold: explicit +00 literal equals sentinel in America/New_York session'
);

select * from finish();

rollback;
