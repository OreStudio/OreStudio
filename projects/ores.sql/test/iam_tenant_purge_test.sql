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
 * pgTAP tests for the tenant purge.
 *
 * A purge deletes every row a tenant holds, in every table that carries a
 * tenant_id, and leaves every other tenant's rows alone. The suite asserts the
 * whole set rather than a list of tables: it counts the rows the tenant holds
 * across the schema before the purge and after it, so a table this function
 * does not name is covered without the test knowing it exists. The book and the
 * product types it allows are asserted by name, because they were the rows the
 * purge used to leave behind.
 *
 * The tenant's party is made visible to the assertions on both sides of the
 * purge: a book is read only by a session whose visible parties name its own,
 * and a test that could not see the rows would pass on an empty read.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/iam_tenant_purge_test.sql
 */

begin;

select plan(7);

-- The rows the tenant holds, in every public table that carries a tenant_id.
-- The session tenant is the tenant being counted, so the policies that filter a
-- row to its own tenant show exactly the rows this asks about.
create or replace function pg_temp.tenant_rows(p_tenant uuid) returns bigint as $$
declare
    r record;
    v_rows bigint;
    v_total bigint := 0;
begin
    for r in
        select t.tablename
        from pg_tables t
        where t.schemaname = 'public'
          and exists (
              select 1 from information_schema.columns c
              where c.table_schema = t.schemaname
                and c.table_name = t.tablename
                and c.column_name = 'tenant_id')
    loop
        execute format('select count(*) from public.%I where tenant_id::text = $1', r.tablename)
            into v_rows using p_tenant::text;
        v_total := v_total + v_rows;
    end loop;
    return v_total;
end;
$$ language plpgsql;

-- A tenant that people set up, with a book that states the product types it
-- allows.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_tenant on commit drop as
select tenant_id as id from ores_iam_provision_tenant_fn(
    'production', 'test_tenant_purge', 'Tenant Purge Fixture',
    'test_tenant_purge');

select set_config('app.current_tenant_id', (select id::text from t_tenant), true);

create temporary table t_party on commit drop as
select id, short_code from ores_refdata_parties_tbl
where tenant_id = (select id from t_tenant)
  and valid_to = ores_utility_infinity_timestamp_fn();

create temporary table t_book on commit drop as
select gen_random_uuid() as portfolio_id, gen_random_uuid() as book_id;

-- A book is read only by a session whose visible parties name its own, and the
-- book's own trigger reads it back, so the party is in view from here on.
select set_config('app.visible_party_ids',
    '{' || (select id::text from t_party) || '}', true);

insert into ores_refdata_portfolios_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id, purpose_type,
    is_virtual, status, modified_by, performed_by, change_reason_code, change_commentary
)
select b.portfolio_id, (select id from t_tenant), 0, p.id,
    'Purge Fixture Portfolio', null, 'Risk', false, 'Active',
    current_user, current_user, 'system.test', 'Tenant purge pgTAP fixture'
from t_book b, t_party p;

insert into ores_refdata_books_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id,
    functional_currency, book_status, regulatory_book_type, book_purpose_type,
    ledger_feed_type, is_sweepable, restricts_product_types, rates_centre_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
select b.book_id, (select id from t_tenant), 0, p.id, 'Purge Fixture Book',
    b.portfolio_id, 'GBP', 'Active', 'Trading', 'Trading', 'Automatic',
    false, true, 'WRLD',
    current_user, current_user, 'system.test', 'Tenant purge pgTAP fixture'
from t_book b, t_party p;

insert into ores_refdata_book_product_types_tbl (
    book_id, tenant_id, product_type_code, version,
    modified_by, change_reason_code, change_commentary
)
select b.book_id, (select id from t_tenant), c.code, 0,
    current_user, 'system.test', 'Tenant purge pgTAP fixture'
from t_book b
cross join ores_trading_product_types_tbl c
where c.tenant_id = ores_utility_system_tenant_id_fn()
  and c.valid_to = ores_utility_infinity_timestamp_fn();

select cmp_ok(pg_temp.tenant_rows((select id from t_tenant)), '>', 0::bigint,
    'a provisioned tenant holds rows');

select is((select count(*) from ores_refdata_books_tbl
           where tenant_id = (select id from t_tenant)
             and valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the fixture tenant holds the book it was given');

select cmp_ok((select count(*) from ores_refdata_book_product_types_tbl
               where tenant_id = (select id from t_tenant)
                 and valid_to = ores_utility_infinity_timestamp_fn()),
    '>', 0::bigint,
    'the book states the product types it allows');

-- The purge, and the hard delete of the tenant record that follows it.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select ores_iam_purge_tenant_fn((select id from t_tenant));
select ores_iam_hard_delete_tenants_fn((select array_agg(id) from t_tenant));

-- The assertions read as the tenant itself, with its party in view, so a
-- surviving row cannot hide behind a policy.
select set_config('app.current_tenant_id', (select id::text from t_tenant), true);
select set_config('app.visible_party_ids',
    '{' || (select id::text from t_party) || '}', true);

select is(pg_temp.tenant_rows((select id from t_tenant)), 0::bigint,
    'the purge leaves no row of the tenant in any table that carries one');

select is((select count(*) from ores_refdata_books_tbl
           where tenant_id = (select id from t_tenant)),
    0::bigint,
    'the purge deletes the tenant''s books');

select is((select count(*) from ores_refdata_book_product_types_tbl
           where tenant_id = (select id from t_tenant)),
    0::bigint,
    'the purge deletes the product types the tenant''s books allowed');

select is((select count(*) from ores_iam_tenants_tbl
           where id = (select id from t_tenant)),
    0::bigint,
    'the hard delete removes the tenant record');

select * from finish();

rollback;
