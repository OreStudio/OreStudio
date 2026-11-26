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
 * pgTAP tests for the seed of the two junctions that say what allows a product
 * type.
 *
 * - A book published from a curated dataset states exactly the product types the
 *   dataset names for it, and the desk and the party above it state sets that
 *   hold what it carries.
 * - A book a tenant creates carries no rows and allows everything, while a book
 *   that restricts with no rows allows nothing: the flag tells them apart.
 * - A provisioned tenant's roles all permit the whole catalogue, because the
 *   junction is the grant and a role with no rows permits nothing.
 *
 * The books are published here rather than assumed: publication is a runtime
 * act, so a rebuilt database holds the artefact and no book. The transaction
 * rolls every fixture back.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/refdata_book_product_types_seed_test.sql
 */

begin;

select plan(14);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_ctx on commit drop as
select gen_random_uuid() as party_id,
       gen_random_uuid() as portfolio_id,
       (select id from ores_dq_datasets_tbl
        where code = 'acme.acme_group.books'
          and valid_to = ores_utility_infinity_timestamp_fn()) as books_dataset_id,
       (select id from ores_dq_datasets_tbl
        where code = 'acme.acme_group.portfolios'
          and valid_to = ores_utility_infinity_timestamp_fn()) as portfolios_dataset_id,
       (select username from ores_iam_accounts_tbl
        where account_type = 'service'
          and valid_to = ores_utility_infinity_timestamp_fn()
        order by username limit 1) as actor;

-- A party to publish to, and the portfolio the group book hangs from. The
-- publish matches a book's portfolio by name, so the fixture repeats the name
-- the artefact states. A party and a book both name a business centre, and the
-- bring-up publishes the FpML set into the system tenant, so the fixture
-- publishes nothing and reads the catalogue where it lies.

insert into ores_refdata_parties_tbl (
    id, tenant_id, version, short_code, full_name, codename,
    party_category, party_type, parent_party_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
)
select party_id, ores_utility_system_tenant_id_fn(), 0,
    'TEST_BOOK_TYPES', 'Book Types Fixture Party', '',
    'Operational', 'Corporate', null, 'GBLO', 'Active',
    actor, actor, 'system.test', 'Book product type seed pgTAP fixture'
from t_ctx;

-- A book is read only by a session whose visible parties name its own, so the
-- assertions about the books published below see them.
select set_config('app.visible_party_ids',
    '{' || (select party_id::text from t_ctx) || '}', true);

insert into ores_refdata_business_units_tbl (
    id, tenant_id, version, party_id, unit_name, status,
    modified_by, performed_by, change_reason_code, change_commentary
)
select '00000000-0000-0000-0000-0000000f2001', ores_utility_system_tenant_id_fn(),
    0, party_id, 'Group Treasury Desk', 'Active',
    actor, actor, 'system.test', 'Book product type seed pgTAP fixture'
from t_ctx;

insert into ores_refdata_portfolios_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id, owner_unit_id,
    purpose_type, is_virtual, status, modified_by, performed_by,
    change_reason_code, change_commentary
)
select portfolio_id, ores_utility_system_tenant_id_fn(), 0, party_id,
    a.name, null, '00000000-0000-0000-0000-0000000f2001',
    'Risk', false, 'Active',
    actor, actor, 'system.test', 'Book product type seed pgTAP fixture'
from t_ctx c
cross join ores_dq_portfolios_artefact_tbl a
where a.dataset_id = c.portfolios_dataset_id;

select results_eq(
    $$select p.action, p.record_count
      from t_ctx c,
           ores_refdata_publish_books_from_dq_fn(
               c.books_dataset_id, ores_utility_system_tenant_id_fn(),
               'upsert', jsonb_build_object('party_id', c.party_id::text)) p$$,
    $$values ('inserted'::text, 2::bigint)$$,
    'the publish writes the two books of the group dataset');

create temporary table t_book on commit drop as
select b.id, b.name, b.restricts_product_types
from ores_refdata_books_tbl b, t_ctx c
where b.tenant_id = ores_utility_system_tenant_id_fn()
  and b.party_id = c.party_id
  and b.valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select count(*) from t_book where restricts_product_types),
    2::bigint,
    'a published book states the product types it allows rather than leaving the question open');

select is(
    (select count(*) from t_book b
     where exists (
         select a.product_type_code
         from ores_dq_book_product_types_artefact_tbl a, t_ctx c
         where a.dataset_id = c.books_dataset_id
           and a.book_id = b.id
         except
         select j.product_type_code
         from ores_refdata_book_product_types_tbl j
         where j.tenant_id = ores_utility_system_tenant_id_fn()
           and j.book_id = b.id
           and j.valid_to = ores_utility_infinity_timestamp_fn())
        or exists (
         select j.product_type_code
         from ores_refdata_book_product_types_tbl j
         where j.tenant_id = ores_utility_system_tenant_id_fn()
           and j.book_id = b.id
           and j.valid_to = ores_utility_infinity_timestamp_fn()
         except
         select a.product_type_code
         from ores_dq_book_product_types_artefact_tbl a, t_ctx c
         where a.dataset_id = c.books_dataset_id
           and a.book_id = b.id)),
    0::bigint,
    'a published book states exactly the product types the dataset names for it, and no other');

select is(
    (select count(*) from ores_refdata_business_units_tbl u, t_ctx c
     where u.id = '00000000-0000-0000-0000-0000000f2001'
       and u.valid_to = ores_utility_infinity_timestamp_fn()
       and u.restricts_product_types
       and exists (
           select j.product_type_code
           from ores_refdata_business_unit_product_types_tbl j
           where j.business_unit_id = u.id
             and j.valid_to = ores_utility_infinity_timestamp_fn()
           except
           select bj.product_type_code
           from ores_refdata_book_product_types_tbl bj
           join ores_refdata_books_tbl b
               on b.id = bj.book_id
               and b.valid_to = ores_utility_infinity_timestamp_fn()
           where b.owner_unit_id = u.id
             and bj.valid_to = ores_utility_infinity_timestamp_fn())),
    0::bigint,
    'the desk the books hang from states a set that holds every type they carry');

select is(
    (select count(*) from ores_refdata_parties_tbl p, t_ctx c
     where p.id = c.party_id
       and p.valid_to = ores_utility_infinity_timestamp_fn()
       and p.restricts_product_types
       and exists (
           select bj.product_type_code
           from ores_refdata_book_product_types_tbl bj
           join ores_refdata_books_tbl b
               on b.id = bj.book_id
               and b.valid_to = ores_utility_infinity_timestamp_fn()
           where b.party_id = p.id
             and bj.valid_to = ores_utility_infinity_timestamp_fn()
           except
           select j.product_type_code
           from ores_refdata_party_product_types_tbl j
           where j.party_id = p.id
             and j.valid_to = ores_utility_infinity_timestamp_fn())),
    0::bigint,
    'the party states a set that holds what every one of its books carries');

select is(
    (select count(*) from ores_refdata_party_product_types_tbl j, t_ctx c
     where j.party_id = c.party_id
       and j.valid_to = ores_utility_infinity_timestamp_fn()
       and not exists (
           select 1 from ores_trading_product_types_tbl t
           where t.tenant_id = ores_utility_system_tenant_id_fn()
             and t.code = j.product_type_code
             and t.valid_to = ores_utility_infinity_timestamp_fn())),
    0::bigint,
    'every type the party states is one the catalogue states');

select is(
    (select count(*) from ores_refdata_book_product_types_tbl j
     where j.tenant_id = ores_utility_system_tenant_id_fn()
       and j.valid_to = ores_utility_infinity_timestamp_fn()
       and not exists (
           select 1 from ores_trading_product_types_tbl c
           where c.tenant_id = ores_utility_system_tenant_id_fn()
             and c.code = j.product_type_code
             and c.valid_to = ores_utility_infinity_timestamp_fn())),
    0::bigint,
    'every allowed code is one the system catalogue states');

-- Two books with opposite meanings and the same absence of rows.
insert into ores_refdata_books_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id,
    functional_currency, book_status, regulatory_book_type, book_purpose_type,
    ledger_feed_type, is_sweepable, restricts_product_types, rates_centre_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
select b.id, ores_utility_system_tenant_id_fn(), 0, c.party_id, b.name,
    c.portfolio_id, 'GBP', 'Active', 'Banking', 'Trading', 'Automatic',
    false, b.restricts_product_types, 'GBLO',
    c.actor, c.actor, 'system.test', 'Book product type seed pgTAP fixture'
from t_ctx c
cross join (values
    ('00000000-0000-0000-0000-0000000f1001'::uuid, 'Unrestricted Book', false),
    ('00000000-0000-0000-0000-0000000f1002'::uuid, 'Empty Restricted Book', true)
) as b(id, name, restricts_product_types);

select results_eq(
    $$select b.name, b.restricts_product_types,
             (select count(*) from ores_refdata_book_product_types_tbl j
              where j.tenant_id = ores_utility_system_tenant_id_fn()
                and j.book_id = b.id
                and j.valid_to = ores_utility_infinity_timestamp_fn())
      from ores_refdata_books_tbl b
      where b.tenant_id = ores_utility_system_tenant_id_fn()
        and b.party_id = (select party_id from t_ctx)
        and b.name in ('Unrestricted Book', 'Empty Restricted Book')
        and b.valid_to = ores_utility_infinity_timestamp_fn()
      order by b.name$$,
    $$values ('Empty Restricted Book'::text, true, 0::bigint),
             ('Unrestricted Book'::text, false, 0::bigint)$$,
    'a book that allows everything and one that allows nothing both hold no rows, and the flag tells them apart');

-- The read a choosing caller asks: the resolution composed over several books.
select is(
    (select a.book_id
     from ores_refdata_book_product_approvals_fn(
         array['00000000-0000-0000-0000-0000000f1002'::uuid,
               (select id from t_book order by name limit 1)]::uuid[]) a
     limit 1),
    '00000000-0000-0000-0000-0000000f1002'::uuid,
    'the read answers the books in the order the request named them');

select is(
    (select count(*)
     from ores_refdata_book_product_approvals_fn(
         array[(select id from t_book order by name limit 1)]::uuid[]) a
     where not a.permits_every_type
       and a.product_type_code is not null),
    (select count(*)
     from ores_refdata_book_product_types_tbl j
     where j.book_id = (select id from t_book order by name limit 1)
       and j.valid_to = ores_utility_infinity_timestamp_fn()),
    'a restricted book answers one row for every type it permits');

select results_eq(
    $$select a.book_id, a.permits_every_type, a.product_type_code
      from ores_refdata_book_product_approvals_fn(
          array['00000000-0000-0000-0000-0000000f1002'::uuid]) a$$,
    $$values ('00000000-0000-0000-0000-0000000f1002'::uuid, false, null::text)$$,
    'a book that permits nothing answers one row naming no type, and says it permits nothing');

-- A tenant that people set up starts with roles that permit the catalogue.
create temporary table t_tenant on commit drop as
select tenant_id as id from ores_iam_provision_tenant_fn(
    'production', 'test_book_product_types', 'Book Types Fixture',
    'test_book_product_types');

select set_config('app.current_tenant_id', (select id::text from t_tenant), true);

select is(
    (select count(*) from ores_iam_roles_tbl r
     where r.tenant_id = (select id from t_tenant)
       and r.valid_to = ores_utility_infinity_timestamp_fn()
       and (select count(*) from ores_iam_role_product_types_tbl j
            where j.tenant_id = r.tenant_id
              and j.role_id = r.id
              and j.valid_to = ores_utility_infinity_timestamp_fn()) <> 121),
    0::bigint,
    'every role a provisioned tenant starts with permits all 121 product types');

select is(
    (select count(*) from ores_iam_role_product_types_tbl j
     where j.tenant_id = (select id from t_tenant)
       and j.valid_to = ores_utility_infinity_timestamp_fn()
       and not exists (
           select 1 from ores_trading_product_types_tbl c
           where c.tenant_id = ores_utility_system_tenant_id_fn()
             and c.code = j.product_type_code
             and c.valid_to = ores_utility_infinity_timestamp_fn())),
    0::bigint,
    'a provisioned role permits no code the system catalogue does not state');

select is(
    (select count(*) from ores_iam_role_product_types_tbl j
     where j.tenant_id = (select id from t_tenant)
       and j.valid_to = ores_utility_infinity_timestamp_fn()),
    (select count(*) * 121 from ores_iam_roles_tbl r
     where r.tenant_id = (select id from t_tenant)
       and r.valid_to = ores_utility_infinity_timestamp_fn()),
    'the provisioned tenant holds one grant for every role and every code');

select * from finish();

rollback;
