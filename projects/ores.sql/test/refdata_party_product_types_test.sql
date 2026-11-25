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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * pgTAP tests for the product approval matrices.
 *
 * - A party's set resolves the types it permits, and a party that states none
 *   resolves nothing at all rather than every type.
 * - A book that states no set carries its party's, and a book that states one
 *   carries only that set.
 * - A book may not name a type its party does not permit, a party may not drop
 *   a type a book of the party carries, and a party may not start restricting
 *   while one of its books carries a type outside the set it would state.
 *
 * The fixtures are written here rather than assumed: no provision leaves a
 * party with a product approval matrix. The transaction rolls them back.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/refdata_party_product_types_test.sql
 */

begin;

select plan(14);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_ctx on commit drop as
select gen_random_uuid() as party_id,
       gen_random_uuid() as party_narrow_id,
       gen_random_uuid() as party_open_id,
       gen_random_uuid() as portfolio_id,
       (select username from ores_iam_accounts_tbl
        where account_type = 'service'
          and valid_to = ores_utility_infinity_timestamp_fn()
        order by username limit 1) as actor;

insert into ores_refdata_parties_tbl (
    id, tenant_id, version, short_code, full_name, codename,
    party_category, party_type, parent_party_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
)
select p.id, ores_utility_system_tenant_id_fn(), 0,
    p.short_code, p.full_name, '',
    'Operational', 'Corporate',
    (select id from ores_refdata_parties_tbl
     where party_category = 'System'
       and tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'GBLO', 'Active',
    c.actor, c.actor, 'system.test', 'Party product type pgTAP fixture'
from t_ctx c
cross join lateral (values
    (c.party_id, 'APPROVAL_TEST', 'Approval Test Party'),
    (c.party_narrow_id, 'APPROVAL_NARROW', 'Approval Narrow Party'),
    (c.party_open_id, 'APPROVAL_OPEN', 'Approval Open Party')
) as p(id, short_code, full_name);

select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}'
     from ores_refdata_parties_tbl
     where short_code like 'APPROVAL%'
       and valid_to = ores_utility_infinity_timestamp_fn()), true);

insert into ores_refdata_portfolios_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id, purpose_type,
    is_virtual, status, modified_by, performed_by, change_reason_code, change_commentary
)
select portfolio_id, ores_utility_system_tenant_id_fn(), 0, party_id,
    'Approval Test Portfolio', null, 'Risk', false, 'Active',
    actor, actor, 'system.test', 'Party product type pgTAP fixture'
from t_ctx;

-- The party states a set, and only then strips the books: a party that begins
-- restricting while a book is already carrying a type outside the set is a
-- refusal of its own, asserted below.
insert into ores_refdata_party_product_types_tbl (
    party_id, tenant_id, product_type_code, version,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select c.party_id, ores_utility_system_tenant_id_fn(), t.code, 0,
    c.actor, c.actor, 'system.test', 'Party product type pgTAP fixture',
    now(), ores_utility_infinity_timestamp_fn()
from t_ctx c
cross join (values ('Swap'), ('FxForward')) as t(code);

update ores_refdata_parties_tbl p
set restricts_product_types = true
from t_ctx c
where p.id = c.party_id
  and p.valid_to = ores_utility_infinity_timestamp_fn();

select is(
    ores_refdata_party_allowed_product_types_fn((select party_id from t_ctx)),
    array['FxForward', 'Swap']::text[],
    'a restricted party resolves the product types it permits');

insert into ores_refdata_books_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id,
    functional_currency, book_status, regulatory_book_type, book_purpose_type,
    ledger_feed_type, is_sweepable, restricts_product_types, rates_centre_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
select b.id, ores_utility_system_tenant_id_fn(), 0, c.party_id, b.name,
    c.portfolio_id, 'GBP', 'Active', 'Banking', 'Trading', 'Automatic',
    false, b.restricts_product_types, 'GBLO',
    c.actor, c.actor, 'system.test', 'Party product type pgTAP fixture'
from t_ctx c
cross join (values
    ('00000000-0000-0000-0000-0000000ba001'::uuid, 'Inheriting Book', false),
    ('00000000-0000-0000-0000-0000000ba002'::uuid, 'Narrowing Book', true)
) as b(id, name, restricts_product_types);

select is(
    ores_refdata_book_allowed_product_types_fn('00000000-0000-0000-0000-0000000ba001'),
    array['FxForward', 'Swap']::text[],
    'a book that states no set carries its party''s set');

select lives_ok(
    $$insert into ores_refdata_book_product_types_tbl (
          book_id, tenant_id, product_type_code, version,
          modified_by, performed_by, change_reason_code, change_commentary,
          valid_from, valid_to)
      values ('00000000-0000-0000-0000-0000000ba002',
          ores_utility_system_tenant_id_fn(), 'Swap', 0,
          'system.test', 'system.test', 'system.test', 'pgTAP fixture',
          now(), ores_utility_infinity_timestamp_fn())$$,
    'a book states a type its party permits');

select throws_ok(
    $$insert into ores_refdata_book_product_types_tbl (
          book_id, tenant_id, product_type_code, version,
          modified_by, performed_by, change_reason_code, change_commentary,
          valid_from, valid_to)
      values ('00000000-0000-0000-0000-0000000ba002',
          ores_utility_system_tenant_id_fn(), 'Bond', 0,
          'system.test', 'system.test', 'system.test', 'pgTAP fixture',
          now(), ores_utility_infinity_timestamp_fn())$$,
    '23514', null,
    'a book may not state a type its party does not permit');

select is(
    ores_refdata_book_allowed_product_types_fn('00000000-0000-0000-0000-0000000ba002'),
    array['Swap']::text[],
    'a book that states a set carries only that set');

select throws_ok(
    $$delete from ores_refdata_party_product_types_tbl
      where party_id = (select party_id from t_ctx)
        and product_type_code = 'Swap'
        and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '23514', null,
    'a party may not drop a type a book of the party carries');

select lives_ok(
    $$delete from ores_refdata_party_product_types_tbl
      where party_id = (select party_id from t_ctx)
        and product_type_code = 'FxForward'
        and valid_to = ores_utility_infinity_timestamp_fn()$$,
    'a party drops a type no book of the party carries');

select is(
    ores_refdata_party_allowed_product_types_fn((select party_id from t_ctx)),
    array['Swap']::text[],
    'the party resolves the narrowed set after the drop');

select is(
    ores_refdata_book_allowed_product_types_fn('00000000-0000-0000-0000-0000000ba001'),
    array['Swap']::text[],
    'an inheriting book follows the party when the party narrows');

insert into ores_refdata_books_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id,
    functional_currency, book_status, regulatory_book_type, book_purpose_type,
    ledger_feed_type, is_sweepable, restricts_product_types, rates_centre_code,
    modified_by, performed_by, change_reason_code, change_commentary
)
select '00000000-0000-0000-0000-0000000ba003', ores_utility_system_tenant_id_fn(), 0,
    c.party_narrow_id, 'Outside Book', c.portfolio_id, 'GBP', 'Active', 'Banking',
    'Trading', 'Automatic', false, true, 'GBLO',
    c.actor, c.actor, 'system.test', 'Party product type pgTAP fixture'
from t_ctx c;

insert into ores_refdata_book_product_types_tbl (
    book_id, tenant_id, product_type_code, version,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
values ('00000000-0000-0000-0000-0000000ba003', ores_utility_system_tenant_id_fn(),
    'Bond', 0, 'system.test', 'system.test', 'system.test', 'pgTAP fixture',
    now(), ores_utility_infinity_timestamp_fn());

select throws_ok(
    $$update ores_refdata_parties_tbl
      set restricts_product_types = true
      where id = (select party_narrow_id from t_ctx)
        and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '23514', null,
    'a party may not start restricting while a book carries a type it would not permit');

select is(
    ores_refdata_party_allowed_product_types_fn((select party_open_id from t_ctx)),
    null::text[],
    'a party that states no set permits every type');

select is(
    ores_refdata_book_allowed_product_types_fn('00000000-0000-0000-0000-0000000ba003'),
    array['Bond']::text[],
    'a book of a party that states no set carries the set it states');

select is(
    ores_refdata_party_allowed_product_types_fn(gen_random_uuid()),
    '{}'::text[],
    'a party the caller cannot see permits nothing');

select is(
    ores_refdata_book_allowed_product_types_fn(gen_random_uuid()),
    '{}'::text[],
    'a book the caller cannot see carries nothing');

select * from finish();

rollback;
