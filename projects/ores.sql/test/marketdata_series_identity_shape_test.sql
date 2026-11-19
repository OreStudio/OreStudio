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

select plan(16);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- A shape names a family by code, and its entries name the fields. Every claim
-- below is about a family and a field, so the two are read joined, from the rows
-- the session can see: its own and the system tenant's.
create temporary view stated_entries as
select e.tenant_id, sh.asset_class, sh.series_subclass, sh.index_family,
       e.field, e.requirement
from ores_marketdata_series_identity_shape_entries_tbl e
join ores_marketdata_series_identity_shapes_tbl sh
  on sh.code = e.shape_code
 and sh.tenant_id in (e.tenant_id, ores_utility_system_tenant_id_fn())
 and sh.valid_to = ores_utility_infinity_timestamp_fn()
where e.valid_to = ores_utility_infinity_timestamp_fn();

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
join stated_entries st
  on st.tenant_id in (i.tenant_id, ores_utility_system_tenant_id_fn())
 and st.asset_class = to_jsonb(i) ->> 'asset_class'
 and st.series_subclass = s.series_subclass
 and st.index_family = coalesce(to_jsonb(i) ->> 'index', '');

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

-- The writer reads an entry whose shape is gone as speaking for no family, so a
-- catalogue that holds one has lost a rule without a word. The seed holds none.
select is(
    (select string_agg(e.shape_code || '/' || e.field, ', ' order by e.shape_code, e.field)
     from ores_marketdata_series_identity_shape_entries_tbl e
     where e.valid_to = ores_utility_infinity_timestamp_fn()
       and not exists (
           select 1 from ores_marketdata_series_identity_shapes_tbl sh
           where sh.code = e.shape_code
             and sh.tenant_id in (e.tenant_id, ores_utility_system_tenant_id_fn())
             and sh.valid_to = ores_utility_infinity_timestamp_fn())),
    null,
    'every entry names a shape that is stated'
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
      from stated_entries
      where asset_class = 'ir'
        and series_subclass = 'index_fixing'
        and index_family = 'ibor'
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

-- A family no shape row states is written unguarded, by design, so the catalogue
-- can grow one family at a time. This names each family the stored identities use
-- that no shape states, keyed as the guard keys it, so a family the taxonomy gains
-- cannot go unguarded without this suite failing. An identity the codec could not
-- read has no class and is no family.
--
-- A family left unguarded on purpose goes in t_unguarded_on_purpose with its
-- reason. There is none today.
create temporary table t_unguarded_on_purpose (
    asset_class text not null,
    series_subclass text not null,
    index_family text not null,
    reason text not null
) on commit drop;

-- The shapes are copied so the cases below can take a family away without touching
-- the catalogue. The copy reads what the writer reads: a shape states its family
-- whether or not it has entries yet.
create temporary table t_stated_shapes on commit drop as
select distinct asset_class, series_subclass, index_family
from ores_marketdata_series_identity_shapes_tbl
where tenant_id in (ores_iam_current_tenant_id_fn(), ores_utility_system_tenant_id_fn())
  and valid_to = ores_utility_infinity_timestamp_fn();

create temporary view unstated_families as
select distinct i.asset_class, s.series_subclass, coalesce(i.index, '') as index_family
from ores_marketdata_market_series_identity_tbl i
join ores_marketdata_market_series_tbl s
  on s.id = i.series_id
 and s.tenant_id = i.tenant_id
 and s.valid_to = ores_utility_infinity_timestamp_fn()
where i.asset_class is not null
  and not exists (
      select 1 from t_stated_shapes st
      where st.asset_class = i.asset_class
        and st.series_subclass = s.series_subclass
        and st.index_family = coalesce(i.index, ''))
  and not exists (
      select 1 from t_unguarded_on_purpose g
      where g.asset_class = i.asset_class
        and g.series_subclass = s.series_subclass
        and g.index_family = coalesce(i.index, ''));

select is(
    (select string_agg(asset_class || '/' || series_subclass || '/' || index_family, ', '
                       order by asset_class, series_subclass, index_family)
     from unstated_families),
    null,
    'every family in use has a shape or is unguarded on purpose'
);

-- The check must be able to fail. Take one stated family that is in use out of the
-- copy, and it is named; list it as unguarded on purpose, and the check passes. A
-- database with no family in use yet has nothing to take away, so both cases skip.
create temporary table t_taken on commit drop as
select st.asset_class, st.series_subclass, st.index_family
from t_stated_shapes st
where exists (
    select 1
    from ores_marketdata_market_series_identity_tbl i
    join ores_marketdata_market_series_tbl s
      on s.id = i.series_id
     and s.tenant_id = i.tenant_id
     and s.valid_to = ores_utility_infinity_timestamp_fn()
    where i.asset_class = st.asset_class
      and s.series_subclass = st.series_subclass
      and coalesce(i.index, '') = st.index_family)
order by 1, 2, 3
limit 1;

delete from t_stated_shapes st
using t_taken tk
where st.asset_class = tk.asset_class
  and st.series_subclass = tk.series_subclass
  and st.index_family = tk.index_family;

select case when exists (select 1 from t_taken) then
    is((select string_agg(asset_class || '/' || series_subclass || '/' || index_family, ', ')
        from unstated_families),
       (select asset_class || '/' || series_subclass || '/' || index_family from t_taken),
       'a family in use whose shape is taken away is named')
else
    skip('no family is in use yet', 1)
end;

insert into t_unguarded_on_purpose
select asset_class, series_subclass, index_family,
       'A case of this suite, to show a listed gap passes.'
from t_taken;

select case when exists (select 1 from t_taken) then
    is((select count(*)::int from unstated_families), 0,
       'a family listed as unguarded on purpose passes')
else
    skip('no family is in use yet', 1)
end;

-- The writer reads the shapes the session can see, so a tenant that could not
-- see the system's rows would write identities unguarded. Provision one here and
-- let the transaction take it away, so the suite owns its fixture.
--
-- The system tenant's rows go into a temporary table first, which row-level
-- security does not bind, so the two sets can be compared side by side.
create temporary table t_system_shapes on commit drop as
select asset_class, series_subclass, index_family, field, requirement
from stated_entries;

create temporary table t_other on commit drop as
select tenant_id as id from ores_iam_provision_tenant_fn(
    'production', 'test_identity_shape', 'Identity Shape Fixture',
    'test_identity_shape');

select cmp_ok((select count(*) from t_system_shapes), '>', 0::bigint,
    'the system tenant states some shape rows');

select set_config('app.current_tenant_id', (select id::text from t_other), true);

select is(
    (select count(*)::int from stated_entries),
    (select count(*)::int from t_system_shapes),
    'a provisioned tenant reads as many shapes as the system tenant states'
);

select is(
    (select count(*)::int from (
        (select asset_class, series_subclass, index_family, field, requirement
         from t_system_shapes
         except
         select asset_class, series_subclass, index_family, field, requirement
         from stated_entries)
        union all
        (select asset_class, series_subclass, index_family, field, requirement
         from stated_entries
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
    (select count(*)::int from ores_marketdata_series_identity_shape_entries_tbl
     where tenant_id = (select id from t_other)),
    0,
    'a provisioned tenant holds no copy of the shape entries'
);

select is(
    (select count(*)::int
     from ores_marketdata_series_classification_rules_tbl
     where key_kind = 'index'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'a provisioned tenant holds one rule for the index keys, so its fixings classify'
);

-- A code is the family's parts joined by dots, so a person who reads a code reads
-- the family. A shape whose code names another family is refused.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select throws_ok(
    $$insert into ores_marketdata_series_identity_shapes_tbl (
          tenant_id, code, asset_class, series_subclass, index_family, description,
          version, modified_by, performed_by, change_reason_code, change_commentary)
      values (ores_utility_system_tenant_id_fn(), 'ir.yield', 'fx', 'spot', '',
              'A code that names another family.',
              0, current_user, current_user, 'system.initial_load', 'test')$$,
    '23514',
    null,
    'a shape whose code names another family is refused'
);

select throws_ok(
    $$insert into ores_marketdata_series_identity_shape_entries_tbl (
          tenant_id, id, shape_code, field, requirement, description,
          version, modified_by, performed_by, change_reason_code, change_commentary)
      values (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 'ir.no_such_shape', 'ccy', 'required',
              'An entry of a shape nobody stated.',
              0, current_user, current_user, 'system.initial_load', 'test')$$,
    null,
    'Invalid shape_code: ir.no_such_shape. No active identity shape has this code.',
    'an entry that names no stated shape is refused'
);

select * from finish();

rollback;
