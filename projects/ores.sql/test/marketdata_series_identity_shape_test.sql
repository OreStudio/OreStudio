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
 * pgTAP tests for the stated identity shape of a market series family.
 *
 * A family names the fields its identity must carry and the ones it must not,
 * so an identity is never a bag of columns that happens to hold a value. The
 * projection refuses a series that contradicts its family; this test states the
 * same fact about the rows already written:
 *
 *     select count(*) from ores_marketdata_market_series_identity_tbl ...
 *
 * The scan is asserted to read some identities before it is asserted to find no
 * fault, because a broken join would otherwise report a clean database.
 *
 * An IBOR fixing keys on its currency, its index and the index name; a swap
 * fixing keys on its currency and its index and names the tenor instead. The two
 * share a class and an index family and differ only by that name, which is why
 * the family is stated rather than inferred from the columns a row fills.
 *
 * A tenant holds no copy of the shapes. It reads the system tenant's rows beside
 * its own, so the shapes a new tenant sees are asserted against a tenant this
 * test provisions and rolls back.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/marketdata_series_identity_shape_test.sql
 */

begin;

select plan(9);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- One row per identity a shape speaks for. The shape is joined by the same key
-- the projection uses: the class and the index family from the identity row, and
-- the subclass from the series that row belongs to.
--
-- The shape is read from the caller's rows and the system tenant's, because that
-- is what the writer reads: the catalogue is shared, and the generated read carries
-- =tenant_id == tid || tenant_id == sys=. A join on the identity's own tenant would
-- describe a copy no tenant holds.
--
-- What this scan can see is what the session can see. Row-level security bounds it
-- to one tenant, so the identities it holds to the shapes are that tenant's: on a
-- suite run that is the system tenant's, which is the tenant whose families the
-- catalogue states. A tenant's own identities are guarded by the same code, and the
-- chain is proved in two places that compose: this suite asserts a provisioned
-- tenant reads the system tenant's shapes and holds none of its own, and the guard's
-- unit test asserts that whatever shapes it is given are enforced.
create temporary view shape_scan as
select st.asset_class, st.series_subclass, st.index_family, st.field, st.requirement,
       i.tenant_id as identity_tenant,
       coalesce(to_jsonb(i) ->> st.field, '') as held
from ores_marketdata_market_series_identity_tbl i
join ores_marketdata_market_series_tbl s
  on s.id = i.series_id
 and s.tenant_id = i.tenant_id
 and s.valid_to = ores_utility_infinity_timestamp_fn()
join ores_marketdata_series_identity_shapes_tbl st
  on st.tenant_id in (i.tenant_id, ores_utility_system_tenant_id_fn())
 and st.asset_class = to_jsonb(i) ->> 'asset_class'
 and st.series_subclass = s.series_subclass
 and st.index_family = coalesce(to_jsonb(i) ->> 'index', '')
 and st.valid_to = ores_utility_infinity_timestamp_fn();

-- On a database that has not imported a market series yet there is nothing to
-- contradict, so this count reads 0 whatever the shapes say. The claims about
-- the shapes themselves, which follow, hold on a database of any age.
select is(
    (select count(*)::int
     from shape_scan
     where (requirement = 'required' and held = '')
        or (requirement = 'forbidden' and held <> '')),
    0,
    'no identity contradicts the shape its family states'
);

-- A fixing is keyed by its index family and by nothing else, so one rule speaks
-- for every family. Two rules would leave the classifier reading whichever row
-- the table happened to return last.
select is(
    (select count(*)::int
     from ores_marketdata_series_classification_rules_tbl
     where key_kind = 'index'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'the tenant holds one rule for the index keys, and every fixing reads it'
);

select results_eq(
    $$select field || ' ' || requirement
      from ores_marketdata_series_identity_shapes_tbl
      where asset_class = 'ir'
        and series_subclass = 'index_fixing'
        and index_family = 'ibor'
        and valid_to = ores_utility_infinity_timestamp_fn()
      order by field$$,
    $$values ('ccy required'), ('index required'), ('index_name required'),
             ('unit_ccy forbidden')$$,
    'an IBOR fixing states the three fields it keys on and forbids a unit'
);

select is(
    (select count(*)::int
     from ores_marketdata_market_series_identity_tbl i
     join ores_marketdata_market_series_tbl s
       on s.id = i.series_id
      and s.tenant_id = i.tenant_id
      and s.valid_to = ores_utility_infinity_timestamp_fn()
     where i.asset_class = 'ir'
       and i.index = 'ibor'
       and s.series_subclass = 'index_fixing'
       and coalesce(i.index_name, '') = ''),
    0,
    'every IBOR fixing carries the index name that separates it from a swap fixing'
);

-- The writer reads the shapes the session can see, so a tenant that could not
-- see the system's rows would write identities unguarded. Provision one here and
-- let the transaction take it away, so the suite owns its fixture.
--
-- The system tenant's rows go into a temporary table first, which row-level
-- security does not bind, so the two sets can be compared side by side.
create temporary table t_system_shapes on commit drop as
select asset_class, series_subclass, index_family, field, requirement
from ores_marketdata_series_identity_shapes_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

create temporary table t_other on commit drop as
select tenant_id as id from ores_iam_provision_tenant_fn(
    'production', 'test_identity_shape', 'Identity Shape Fixture',
    'test_identity_shape');

select cmp_ok((select count(*) from t_system_shapes), '>', 0::bigint,
    'the system tenant states some shape rows');

select set_config('app.current_tenant_id', (select id::text from t_other), true);

select is(
    (select count(*)::int from ores_marketdata_series_identity_shapes_tbl
     where valid_to = ores_utility_infinity_timestamp_fn()),
    (select count(*)::int from t_system_shapes),
    'a provisioned tenant reads as many shapes as the system tenant states'
);

select is(
    (select count(*)::int from (
        (select asset_class, series_subclass, index_family, field, requirement
         from t_system_shapes
         except
         select asset_class, series_subclass, index_family, field, requirement
         from ores_marketdata_series_identity_shapes_tbl
         where valid_to = ores_utility_infinity_timestamp_fn())
        union all
        (select asset_class, series_subclass, index_family, field, requirement
         from ores_marketdata_series_identity_shapes_tbl
         where valid_to = ores_utility_infinity_timestamp_fn()
         except
         select asset_class, series_subclass, index_family, field, requirement
         from t_system_shapes)) seen),
    0,
    'a provisioned tenant reads the same shape rows and no others'
);

select is(
    (select count(*)::int from ores_marketdata_series_identity_shapes_tbl
     where tenant_id = (select id from t_other)),
    0,
    'a provisioned tenant holds no copy of the shapes'
);

select is(
    (select count(*)::int
     from ores_marketdata_series_classification_rules_tbl
     where key_kind = 'index'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'a provisioned tenant holds one rule for the index keys, so its fixings classify'
);

select * from finish();

rollback;
