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
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * pgTAP tests for the trade structures.
 *
 * Tests cover:
 * - A deal the customer sees on its own is written
 * - A deal that is a leg of a larger one is written
 * - A structure nests one level at most, and is not its own parent
 * - The kind and the template name entries in their catalogues
 *
 * Run with: pg_prove -d <database> test/trading_structures_test.sql
 */

begin;

select plan(10);

-- Row-level security applies to the test user too: state the tenant before
-- writing anything. The tenant comes first because the fixture below writes
-- rows the policies then filter.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- A recreated database carries the system party and nothing else, so the
-- fixture writes the second party and the counterparty this suite names. The
-- suite rolls them back.
insert into ores_refdata_parties_tbl (
    id, tenant_id, full_name, short_code, party_category, party_type,
    business_center_code, parent_party_id, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    '00000000-0000-0000-0000-0000000cf004'::uuid, ores_utility_system_tenant_id_fn(),
    'Structures Other Party', 'TST-OP', 'Operational', 'Corporate',
    'WRLD', null, 'Active', current_user, current_user,
    'system.test', 'Structures pgTAP fixture');

insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version, full_name, short_code, party_type,
    parent_counterparty_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    '00000000-0000-0000-0000-0000000cf104'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'Structures Counterparty', 'TST-CP', 'Corporate',
    null, 'WRLD', 'Active', current_user, current_user,
    'system.test', 'Structures pgTAP fixture');

select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       (select id from ores_refdata_counterparties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'TST-CP'
          and valid_to = ores_utility_infinity_timestamp_fn()) as counterparty_id;

create or replace function pg_temp.structure(p_id uuid, p_parent uuid default null,
    p_kind text default 'Strategy', p_template text default 'Straddle')
returns void as $$
    insert into ores_trading_structures_tbl (id, tenant_id, party_id, counterparty_id,
        kind, template_code, parent_structure_id)
    select p_id, ores_utility_system_tenant_id_fn(), party_id, counterparty_id,
        p_kind, p_template, p_parent
    from t_ctx;
$$ language sql;

-- =============================================================================
-- The deal and its legs
-- =============================================================================

select lives_ok(
    $$select pg_temp.structure('00000000-0000-0000-0000-0000000cb001')$$,
    'a deal the customer sees on its own is written with no parent');

select lives_ok(
    $$select pg_temp.structure('00000000-0000-0000-0000-0000000cb002',
        '00000000-0000-0000-0000-0000000cb001')$$,
    'a deal that is a leg of a larger one names its parent');

select throws_ok(
    $$select pg_temp.structure('00000000-0000-0000-0000-0000000cb003',
        '00000000-0000-0000-0000-0000000cb002')$$,
    '23514', null,
    'a leg of a leg is refused, because structures nest one level at most');

select throws_ok(
    $$select pg_temp.structure('00000000-0000-0000-0000-0000000cb004',
        '00000000-0000-0000-0000-0000000cb004')$$,
    '23514', null,
    'a structure cannot be its own parent');

select throws_ok(
    $$select pg_temp.structure('00000000-0000-0000-0000-0000000cb005', null,
        'Nonsense')$$,
    '23503', null,
    'a structure naming an unknown kind is refused');

select throws_ok(
    $$select pg_temp.structure('00000000-0000-0000-0000-0000000cb006', null,
        'Strategy', 'Nonsense')$$,
    '23503', null,
    'a structure naming an unknown template is refused');

select throws_ok(
    $$select pg_temp.structure('00000000-0000-0000-0000-0000000cb008',
        '00000000-0000-0000-0000-0000000cb0ff')$$,
    '23503', null,
    'a structure naming an unknown parent is refused');

select lives_ok(
    $$select pg_temp.structure('00000000-0000-0000-0000-0000000cb009', null,
        'Package', null)$$,
    'a structure resting on no template is written, as a package does');

-- The row is immutable, so the table refuses both. The state is 55000 rather
-- than a constraint violation: the store will not put the row in the state the
-- write asks for, which is the immutability rule itself and not a bad value.
select throws_ok(
    $$update ores_trading_structures_tbl set kind = 'Typed'
      where id = '00000000-0000-0000-0000-0000000cb001'$$,
    '55000', null,
    'a structure is never updated');

select throws_ok(
    $$delete from ores_trading_structures_tbl
      where id = '00000000-0000-0000-0000-0000000cb001'$$,
    '55000', null,
    'a structure is never deleted');

select * from finish();

rollback;
