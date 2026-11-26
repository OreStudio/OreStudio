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
 * pgTAP tests for the desk, the middle level of the product approval chain.
 *
 * - A desk that states no set permits what its party permits, and a desk that
 *   states one resolves only that set.
 * - A book under a desk carries the desk's set where the desk states one, and
 *   the party's set where it does not.
 * - A desk may not permit a type its party does not permit, a book may not
 *   carry a type its desk does not permit, and a book may not sit under a desk
 *   of another party.
 * - Neither a desk nor a party may drop a type a level below it still carries,
 *   nor start restricting while a level below it carries a type outside the set
 *   it would state. A version replacement is not a drop, so it stays allowed.
 *
 * The fixtures are written here rather than assumed: no provision leaves a
 * party, a desk or a book with a product approval matrix. The transaction rolls
 * them back.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/refdata_business_unit_product_types_test.sql
 */

begin;

select plan(30);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_ctx on commit drop as
select gen_random_uuid() as party_id,
       gen_random_uuid() as party_open_id,
       coalesce(
           (select username from ores_iam_accounts_tbl
            where valid_to = ores_utility_infinity_timestamp_fn()
            order by account_type, username limit 1),
           current_user) as actor;

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
    c.actor, c.actor, 'system.test', 'Desk approval pgTAP fixture'
from t_ctx c
cross join lateral (values
    (c.party_id, 'DESK_APPROVAL', 'Desk Approval Party'),
    (c.party_open_id, 'DESK_APPROVAL_OPEN', 'Desk Approval Open Party')
) as p(id, short_code, full_name);

select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}'
     from ores_refdata_parties_tbl
     where short_code like 'DESK_APPROVAL%'
       and valid_to = ores_utility_infinity_timestamp_fn()), true);

-- The party states a set, and only then strips the desks: a party that begins
-- restricting while a desk is already carrying a type outside the set is a
-- refusal of its own, asserted below.
insert into ores_refdata_party_product_types_tbl (
    party_id, tenant_id, product_type_code, version,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select c.party_id, ores_utility_system_tenant_id_fn(), t.code, 0,
    c.actor, c.actor, 'system.test', 'Desk approval pgTAP fixture',
    now(), ores_utility_infinity_timestamp_fn()
from t_ctx c
cross join (values ('Swap'), ('FxForward')) as t(code);

update ores_refdata_parties_tbl p
set restricts_product_types = true
from t_ctx c
where p.id = c.party_id
  and p.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_refdata_business_units_tbl (
    id, tenant_id, version, party_id, unit_name, status, restricts_product_types,
    modified_by, performed_by, change_reason_code, change_commentary
)
select u.id, ores_utility_system_tenant_id_fn(), 0, u.party_id, u.unit_name,
    'Active', u.restricts_product_types,
    c.actor, c.actor, 'system.test', 'Desk approval pgTAP fixture'
from t_ctx c
cross join lateral (values
    ('00000000-0000-0000-0000-0000000bd001'::uuid, c.party_id, 'Inheriting Desk', false),
    ('00000000-0000-0000-0000-0000000bd002'::uuid, c.party_id, 'Narrow Desk', false),
    ('00000000-0000-0000-0000-0000000bd003'::uuid, c.party_open_id, 'Open Desk', true),
    ('00000000-0000-0000-0000-0000000bd004'::uuid, c.party_open_id, 'Free Desk', false),
    ('00000000-0000-0000-0000-0000000bd005'::uuid, c.party_id, 'Late Desk', false)
) as u(id, party_id, unit_name, restricts_product_types);

-- A book may only be owned by the unit its own portfolio names, or by a unit
-- named above it, so each desk that owns a book gets a portfolio that names it.
insert into ores_refdata_portfolios_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id, owner_unit_id,
    purpose_type, is_virtual, status,
    modified_by, performed_by, change_reason_code, change_commentary
)
select p.id, ores_utility_system_tenant_id_fn(), 0, p.party_id, p.name, null,
    p.owner_unit_id, 'Risk', false, 'Active',
    c.actor, c.actor, 'system.test', 'Desk approval pgTAP fixture'
from t_ctx c
cross join lateral (values
    ('00000000-0000-0000-0000-0000000bc001'::uuid, c.party_id, 'Inheriting Desk Portfolio',
     '00000000-0000-0000-0000-0000000bd001'::uuid),
    ('00000000-0000-0000-0000-0000000bc002'::uuid, c.party_id, 'Narrow Desk Portfolio',
     '00000000-0000-0000-0000-0000000bd002'::uuid),
    ('00000000-0000-0000-0000-0000000bc003'::uuid, c.party_id, 'Late Desk Portfolio',
     '00000000-0000-0000-0000-0000000bd005'::uuid),
    ('00000000-0000-0000-0000-0000000bc004'::uuid, c.party_open_id, 'Free Desk Portfolio',
     '00000000-0000-0000-0000-0000000bd004'::uuid),
    ('00000000-0000-0000-0000-0000000bc005'::uuid, c.party_open_id, 'Open Desk Portfolio',
     '00000000-0000-0000-0000-0000000bd003'::uuid),
    ('00000000-0000-0000-0000-0000000bc006'::uuid, c.party_id, 'Deskless Portfolio',
     null)
) as p(id, party_id, name, owner_unit_id);

insert into ores_refdata_books_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id,
    functional_currency, book_status, regulatory_book_type, book_purpose_type,
    ledger_feed_type, is_sweepable, restricts_product_types, rates_centre_code,
    owner_unit_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
select b.id, ores_utility_system_tenant_id_fn(), 0, b.party_id, b.name,
    b.portfolio_id, 'GBP', 'Active', 'Banking', 'Trading', 'Automatic',
    false, b.restricts_product_types, 'GBLO', b.owner_unit_id,
    c.actor, c.actor, 'system.test', 'Desk approval pgTAP fixture'
from t_ctx c
cross join lateral (values
    ('00000000-0000-0000-0000-0000000bb001'::uuid, c.party_id, 'Inheriting Desk Book', false,
     '00000000-0000-0000-0000-0000000bd001'::uuid, '00000000-0000-0000-0000-0000000bc001'::uuid),
    ('00000000-0000-0000-0000-0000000bb002'::uuid, c.party_id, 'Narrow Desk Book', false,
     '00000000-0000-0000-0000-0000000bd002'::uuid, '00000000-0000-0000-0000-0000000bc002'::uuid),
    ('00000000-0000-0000-0000-0000000bb003'::uuid, c.party_id, 'Narrow Desk Own Book', true,
     '00000000-0000-0000-0000-0000000bd002'::uuid, '00000000-0000-0000-0000-0000000bc002'::uuid),
    ('00000000-0000-0000-0000-0000000bb004'::uuid, c.party_id, 'Late Desk Book', true,
     '00000000-0000-0000-0000-0000000bd005'::uuid, '00000000-0000-0000-0000-0000000bc003'::uuid),
    ('00000000-0000-0000-0000-0000000bb005'::uuid, c.party_open_id, 'Free Desk Book', false,
     '00000000-0000-0000-0000-0000000bd004'::uuid, '00000000-0000-0000-0000-0000000bc004'::uuid),
    ('00000000-0000-0000-0000-0000000bb006'::uuid, c.party_open_id, 'Open Desk Book', false,
     '00000000-0000-0000-0000-0000000bd003'::uuid, '00000000-0000-0000-0000-0000000bc005'::uuid),
    ('00000000-0000-0000-0000-0000000bb007'::uuid, c.party_id, 'Deskless Book', false,
     null, '00000000-0000-0000-0000-0000000bc006'::uuid)
) as b(id, party_id, name, restricts_product_types, owner_unit_id, portfolio_id);

select is(
    ores_refdata_business_unit_allowed_product_types_fn('00000000-0000-0000-0000-0000000bd001'),
    array['FxForward', 'Swap']::text[],
    'a desk that states no set permits what its party permits');

select is(
    ores_refdata_book_allowed_product_types_fn('00000000-0000-0000-0000-0000000bb001'),
    array['FxForward', 'Swap']::text[],
    'a book under such a desk carries the party''s set');

select lives_ok(
    $$insert into ores_refdata_business_unit_product_types_tbl (
          business_unit_id, tenant_id, product_type_code, version,
          modified_by, performed_by, change_reason_code, change_commentary,
          valid_from, valid_to)
      values ('00000000-0000-0000-0000-0000000bd002',
          ores_utility_system_tenant_id_fn(), 'Swap', 0,
          (select actor from t_ctx), (select actor from t_ctx), 'system.test', 'pgTAP fixture',
          now(), ores_utility_infinity_timestamp_fn())$$,
    'a desk states a type its party permits');

select throws_ok(
    $$insert into ores_refdata_business_unit_product_types_tbl (
          business_unit_id, tenant_id, product_type_code, version,
          modified_by, performed_by, change_reason_code, change_commentary,
          valid_from, valid_to)
      values ('00000000-0000-0000-0000-0000000bd002',
          ores_utility_system_tenant_id_fn(), 'Bond', 0,
          (select actor from t_ctx), (select actor from t_ctx), 'system.test', 'pgTAP fixture',
          now(), ores_utility_infinity_timestamp_fn())$$,
    '23514', null,
    'a desk may not state a type its party does not permit');

select lives_ok(
    $$update ores_refdata_business_units_tbl
      set restricts_product_types = true
      where id = '00000000-0000-0000-0000-0000000bd002'
        and valid_to = ores_utility_infinity_timestamp_fn()$$,
    'a desk starts restricting within the set its party permits');

select is(
    ores_refdata_business_unit_allowed_product_types_fn('00000000-0000-0000-0000-0000000bd002'),
    array['Swap']::text[],
    'a desk that states a set carries only that set');

select is(
    ores_refdata_book_allowed_product_types_fn('00000000-0000-0000-0000-0000000bb002'),
    array['Swap']::text[],
    'a book that states no set carries its desk''s set');

select lives_ok(
    $$insert into ores_refdata_book_product_types_tbl (
          book_id, tenant_id, product_type_code, version,
          modified_by, performed_by, change_reason_code, change_commentary,
          valid_from, valid_to)
      values ('00000000-0000-0000-0000-0000000bb003',
          ores_utility_system_tenant_id_fn(), 'Swap', 0,
          (select actor from t_ctx), (select actor from t_ctx), 'system.test', 'pgTAP fixture',
          now(), ores_utility_infinity_timestamp_fn())$$,
    'a book states a type its desk permits');

select throws_ok(
    $$insert into ores_refdata_book_product_types_tbl (
          book_id, tenant_id, product_type_code, version,
          modified_by, performed_by, change_reason_code, change_commentary,
          valid_from, valid_to)
      values ('00000000-0000-0000-0000-0000000bb003',
          ores_utility_system_tenant_id_fn(), 'FxForward', 0,
          (select actor from t_ctx), (select actor from t_ctx), 'system.test', 'pgTAP fixture',
          now(), ores_utility_infinity_timestamp_fn())$$,
    '23514', null,
    'a book may not state a type its desk does not permit');

select is(
    ores_refdata_book_allowed_product_types_fn('00000000-0000-0000-0000-0000000bb003'),
    array['Swap']::text[],
    'a book that states a set carries only that set');

select throws_ok(
    $$delete from ores_refdata_business_unit_product_types_tbl
      where business_unit_id = '00000000-0000-0000-0000-0000000bd002'
        and product_type_code = 'Swap'
        and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '23514', null,
    'a desk may not drop a type a book of the desk carries');

select lives_ok(
    $$insert into ores_refdata_business_unit_product_types_tbl (
          business_unit_id, tenant_id, product_type_code, version,
          modified_by, performed_by, change_reason_code, change_commentary,
          valid_from, valid_to)
      values ('00000000-0000-0000-0000-0000000bd002',
          ores_utility_system_tenant_id_fn(), 'Swap', 1,
          (select actor from t_ctx), (select actor from t_ctx), 'system.test', 'pgTAP fixture',
          now(), ores_utility_infinity_timestamp_fn())$$,
    'a desk re-states a type a book carries without dropping it');

select is(
    ores_refdata_business_unit_allowed_product_types_fn('00000000-0000-0000-0000-0000000bd002'),
    array['Swap']::text[],
    'the desk resolves the same set after the replacement');

select throws_ok(
    $$delete from ores_refdata_party_product_types_tbl
      where party_id = (select party_id from t_ctx)
        and product_type_code = 'Swap'
        and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '23514', null,
    'a party may not drop a type a desk of the party permits');

select is(
    ores_refdata_book_allowed_product_types_fn('00000000-0000-0000-0000-0000000bb007'),
    array['FxForward', 'Swap']::text[],
    'a book with no desk carries its party''s set');

select lives_ok(
    $$insert into ores_refdata_business_unit_product_types_tbl (
          business_unit_id, tenant_id, product_type_code, version,
          modified_by, performed_by, change_reason_code, change_commentary,
          valid_from, valid_to)
      values ('00000000-0000-0000-0000-0000000bd003',
          ores_utility_system_tenant_id_fn(), 'Bond', 0,
          (select actor from t_ctx), (select actor from t_ctx), 'system.test', 'pgTAP fixture',
          now(), ores_utility_infinity_timestamp_fn())$$,
    'a desk of a party that states no set may state any set');

select is(
    ores_refdata_business_unit_allowed_product_types_fn('00000000-0000-0000-0000-0000000bd003'),
    array['Bond']::text[],
    'the open desk carries only the set it states');

select is(
    ores_refdata_book_allowed_product_types_fn('00000000-0000-0000-0000-0000000bb006'),
    array['Bond']::text[],
    'a book under the open desk carries the desk''s set');

select is(
    ores_refdata_business_unit_allowed_product_types_fn('00000000-0000-0000-0000-0000000bd004'),
    null::text[],
    'a desk of a party that states no set and states none itself permits every type');

select is(
    ores_refdata_book_allowed_product_types_fn('00000000-0000-0000-0000-0000000bb005'),
    null::text[],
    'a book under such a desk permits every type');

insert into ores_refdata_book_product_types_tbl (
    book_id, tenant_id, product_type_code, version,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
values ('00000000-0000-0000-0000-0000000bb004', ores_utility_system_tenant_id_fn(),
    'FxForward', 0, (select actor from t_ctx), (select actor from t_ctx), 'system.test', 'pgTAP fixture',
    now(), ores_utility_infinity_timestamp_fn());

select throws_like(
    $$update ores_refdata_business_units_tbl
      set restricts_product_types = true
      where id = '00000000-0000-0000-0000-0000000bd005'
        and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '%may not start restricting: a book carries%',
    'a desk may not start restricting while a book carries a type it would not permit');

select throws_ok(
    $$insert into ores_refdata_books_tbl (
          id, tenant_id, version, party_id, name, parent_portfolio_id,
          functional_currency, book_status, regulatory_book_type, book_purpose_type,
          ledger_feed_type, is_sweepable, restricts_product_types, rates_centre_code,
          owner_unit_id,
          modified_by, performed_by, change_reason_code, change_commentary)
      select '00000000-0000-0000-0000-0000000bb008',
          ores_utility_system_tenant_id_fn(), 0, (select party_open_id from t_ctx),
          'Wrong Party Book', '00000000-0000-0000-0000-0000000bc002',
          'GBP', 'Active', 'Banking', 'Trading', 'Automatic', false, false, 'GBLO',
          '00000000-0000-0000-0000-0000000bd002',
          (select actor from t_ctx), (select actor from t_ctx), 'system.test', 'pgTAP fixture'$$,
    '23514', null,
    'a book may not belong to a desk of another party');

insert into ores_refdata_party_product_types_tbl (
    party_id, tenant_id, product_type_code, version,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select c.party_open_id, ores_utility_system_tenant_id_fn(), 'Swap', 0,
    c.actor, c.actor, 'system.test', 'Desk approval pgTAP fixture',
    now(), ores_utility_infinity_timestamp_fn()
from t_ctx c;

select throws_like(
    $$update ores_refdata_parties_tbl
      set restricts_product_types = true
      where id = (select party_open_id from t_ctx)
        and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '%may not start restricting: a desk permits%',
    'a party may not start restricting while a desk permits a type it would not permit');

-- The entities are version-inserted, so a flip reaches the table as an INSERT
-- that closes the live row. The refusals are pinned by their message because the
-- code alone cannot tell these guards apart from the ancestry and status ones.
select throws_like(
    $$insert into ores_refdata_parties_tbl (
          id, tenant_id, version, short_code, full_name, codename,
          party_category, party_type, parent_party_id, business_center_code,
          status, restricts_product_types,
          modified_by, performed_by, change_reason_code, change_commentary)
      select p.id, ores_utility_system_tenant_id_fn(), p.version, p.short_code,
          p.full_name, p.codename, p.party_category, p.party_type, p.parent_party_id,
          p.business_center_code, p.status, true,
          p.modified_by, p.performed_by, p.change_reason_code, p.change_commentary
      from ores_refdata_parties_tbl p
      where p.id = (select party_open_id from t_ctx)
        and p.valid_to = ores_utility_infinity_timestamp_fn()$$,
    '%may not start restricting: a desk permits%',
    'a party that starts restricting in a new version is refused, not only one that is updated');

select throws_like(
    $$insert into ores_refdata_business_units_tbl (
          id, tenant_id, version, party_id, unit_name, status,
          restricts_product_types,
          modified_by, performed_by, change_reason_code, change_commentary)
      select u.id, ores_utility_system_tenant_id_fn(), u.version, u.party_id,
          u.unit_name, u.status, true,
          u.modified_by, u.performed_by, u.change_reason_code, u.change_commentary
      from ores_refdata_business_units_tbl u
      where u.id = '00000000-0000-0000-0000-0000000bd005'
        and u.valid_to = ores_utility_infinity_timestamp_fn()$$,
    '%may not start restricting: a book carries%',
    'a desk that starts restricting in a new version is refused, not only one that is updated');

select throws_like(
    $$insert into ores_refdata_books_tbl (
          id, tenant_id, version, party_id, name, parent_portfolio_id,
          functional_currency, book_status, regulatory_book_type, book_purpose_type,
          ledger_feed_type, is_sweepable, restricts_product_types, rates_centre_code,
          owner_unit_id,
          modified_by, performed_by, change_reason_code, change_commentary)
      select b.id, ores_utility_system_tenant_id_fn(), b.version, b.party_id, b.name,
          '00000000-0000-0000-0000-0000000bc002', b.functional_currency, b.book_status,
          b.regulatory_book_type, b.book_purpose_type, b.ledger_feed_type, b.is_sweepable,
          b.restricts_product_types, b.rates_centre_code,
          '00000000-0000-0000-0000-0000000bd002',
          b.modified_by, b.performed_by, b.change_reason_code, b.change_commentary
      from ores_refdata_books_tbl b
      where b.id = '00000000-0000-0000-0000-0000000bb004'
        and b.valid_to = ores_utility_infinity_timestamp_fn()$$,
    '%may not move to business unit%',
    'a book may not move to a desk that does not permit a type the book carries');

select lives_ok(
    $$insert into ores_refdata_books_tbl (
          id, tenant_id, version, party_id, name, parent_portfolio_id,
          functional_currency, book_status, regulatory_book_type, book_purpose_type,
          ledger_feed_type, is_sweepable, restricts_product_types, rates_centre_code,
          owner_unit_id,
          modified_by, performed_by, change_reason_code, change_commentary)
      select b.id, ores_utility_system_tenant_id_fn(), b.version, b.party_id, b.name,
          '00000000-0000-0000-0000-0000000bc001', b.functional_currency, b.book_status,
          b.regulatory_book_type, b.book_purpose_type, b.ledger_feed_type, b.is_sweepable,
          b.restricts_product_types, b.rates_centre_code,
          '00000000-0000-0000-0000-0000000bd001',
          b.modified_by, b.performed_by, b.change_reason_code, b.change_commentary
      from ores_refdata_books_tbl b
      where b.id = '00000000-0000-0000-0000-0000000bb003'
        and b.valid_to = ores_utility_infinity_timestamp_fn()$$,
    'a book moves to a desk whose set holds every type it carries');

select is(
    ores_refdata_book_allowed_product_types_fn('00000000-0000-0000-0000-0000000bb003'),
    array['Swap']::text[],
    'the moved book still carries the set it states');

select is(
    ores_refdata_business_unit_allowed_product_types_fn(gen_random_uuid()),
    '{}'::text[],
    'a desk the caller cannot see permits nothing');

select is(
    ores_refdata_party_allowed_product_types_fn((select party_id from t_ctx)),
    array['FxForward', 'Swap']::text[],
    'the party still permits every type it started with');

select * from finish();

rollback;
