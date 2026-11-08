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
 * pgTAP tests for the division of product types into flow and non-flow work.
 *
 * - The seed states a class for every code, and the eight plain trades are the
 *   only ones it calls flow.
 * - A type nobody has classified may be null, which is the state the queue reads
 *   as non-flow.
 * - A class outside the two is refused.
 * - A party may state its own class for a type, once, and the row reads back.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/trading_product_type_deal_class_test.sql
 */

begin;

select plan(7);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_flow on commit drop as
select array[
    'CashPosition', 'CapFloor', 'FxAverageForward', 'FxDigitalOption',
    'FxForward', 'FxOption', 'FxSwap', 'Swap']::text[] as codes;

select is(
    (select count(*)::int from ores_trading_product_types_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()
       and deal_class is null),
    0,
    'the seed states a class for every product type');

select is(
    (select count(*)::int from ores_trading_product_types_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()
       and code = any(array(select unnest(codes) from t_flow))
       and deal_class = 'flow'),
    8,
    'the eight plain trades are flow');

select is(
    (select count(*)::int from ores_trading_product_types_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()
       and deal_class = 'flow'
       and code <> all(array(select unnest(codes) from t_flow))),
    0,
    'no other product type the catalogue states is flow');

-- A type added later has no class until somebody gives it one, and the column
-- allows that state.
insert into ores_trading_product_types_tbl (
    code, tenant_id, version, name, construction,
    has_options, has_extension, display_order,
    modified_by, change_reason_code, change_commentary
) values (
    'UnclassifiedProduct', ores_utility_system_tenant_id_fn(), 0,
    'Unclassified Product', 'native', false, false, 999,
    current_user, 'system.test', 'Deal class pgTAP fixture');

select is(
    (select deal_class from ores_trading_product_types_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and code = 'UnclassifiedProduct'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    null,
    'a product type nobody has classified states no class');

select throws_ok(
    $$update ores_trading_product_types_tbl
      set deal_class = 'structured-ish'
      where tenant_id = ores_utility_system_tenant_id_fn()
        and code = 'Swap'
        and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '23514',
    null,
    'a class outside the two the store states is refused');

create temporary table t_party on commit drop as
select id from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and short_code = 'system_party'
  and valid_to = ores_utility_infinity_timestamp_fn();

-- The class a party states is read only by a session that can see the party.
select set_config('app.visible_party_ids',
    '{' || (select id::text from t_party) || '}', true);

insert into ores_refdata_party_product_type_deal_classes_tbl (
    id, tenant_id, version, party_id, product_type_code, deal_class,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, id,
    'Swaption', 'flow',
    current_user, current_user, 'system.test', 'Deal class pgTAP fixture'
from t_party;

select is(
    (select deal_class
     from ores_refdata_party_product_type_deal_classes_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and party_id = (select id from t_party)
       and product_type_code = 'Swaption'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'flow',
    'a party states its own class for a type and reads it back');

select throws_ok(
    format($$
        insert into ores_refdata_party_product_type_deal_classes_tbl (
            id, tenant_id, version, party_id, product_type_code, deal_class,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, %L,
            'Swaption', 'non_flow',
            current_user, current_user, 'system.test', 'Deal class pgTAP fixture'
        )$$, (select id from t_party)),
    '23505',
    null,
    'a party states one class for a type, not two');

select * from finish();

rollback;
