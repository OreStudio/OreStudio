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
 * pgTAP tests for the trade booking and trade state components.
 *
 * Tests cover:
 * - A booking and a state reference an existing anchor
 * - The booking's party and counterparty are the anchor's
 * - The booking's book belongs to the trade's party
 * - The netting set belongs to the trade's counterparty and party
 * - The state follows the trade_status machine
 * - A virtual book holds only drafts, tests and hypotheticals
 *
 * Run with: pg_prove -d <database> test/trading_trade_components_test.sql
 */

begin;

select plan(19);

-- Row-level security applies to the test user too: state the tenant before
-- writing anything. The tenant comes first because the fixture below writes
-- rows the policies then filter.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- A recreated database carries the system party and nothing else, so the
-- fixture writes the second party and the two counterparties this suite
-- names. The suite rolls them back.
-- A recreated database carries the WRLD business centre alone, so the fixture
-- writes the GBLO one this suite names. The suite rolls it back.
insert into ores_refdata_business_centres_tbl (
    code, tenant_id, version, coding_scheme_code, source, description,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'GBLO', ores_utility_system_tenant_id_fn(), 0, 'NONE', 'Internal',
    'London. Trading pgTAP fixture.',
    current_user, current_user, 'system.test', 'Trading pgTAP fixture');

insert into ores_refdata_parties_tbl (
    id, tenant_id, full_name, short_code, party_category, party_type,
    business_center_code, parent_party_id, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    '00000000-0000-0000-0000-0000000cf002'::uuid, ores_utility_system_tenant_id_fn(),
    'Trading Components Other Party', 'TTC-OP', 'Operational', 'Corporate',
    'WRLD', null, 'Active', current_user, current_user,
    'system.test', 'Trading pgTAP fixture');

insert into ores_refdata_counterparties_tbl (
    id, tenant_id, version, full_name, short_code, party_type,
    parent_counterparty_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    ('00000000-0000-0000-0000-0000000cf102'::uuid, ores_utility_system_tenant_id_fn(), 0,
     'Trading Components Counterparty', 'TTC-CP1', 'Corporate',
     null, 'WRLD', 'Active', current_user, current_user,
     'system.test', 'Trading pgTAP fixture'),
    ('00000000-0000-0000-0000-0000000cf103'::uuid, ores_utility_system_tenant_id_fn(), 0,
     'Trading Components Other Counterparty', 'TTC-CP2', 'Corporate',
     null, 'WRLD', 'Active', current_user, current_user,
     'system.test', 'Trading pgTAP fixture');

select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'TTC-OP'
          and valid_to = ores_utility_infinity_timestamp_fn()) as other_party_id,
       (select id from ores_refdata_counterparties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'TTC-CP1'
          and valid_to = ores_utility_infinity_timestamp_fn()) as counterparty_id,
       (select id from ores_refdata_counterparties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'TTC-CP2'
          and valid_to = ores_utility_infinity_timestamp_fn()) as other_counterparty_id,
       (select id from ores_iam_accounts_tbl
        where account_type = 'service' and valid_to = ores_utility_infinity_timestamp_fn()
        order by username limit 1) as owner_id,
       (select username from ores_iam_accounts_tbl
        where account_type = 'service' and valid_to = ores_utility_infinity_timestamp_fn()
        order by username limit 1) as owner_name;

select set_config('app.current_actor', (select owner_name from t_ctx), true);

create or replace function pg_temp.activity(p_party uuid, p_type text default 'new_booking')
returns uuid as $$
    insert into ores_trading_trade_activities_tbl (id, tenant_id, party_id,
        activity_type_code, actor, occurred_at, comment)
    values (gen_random_uuid(), ores_utility_system_tenant_id_fn(), p_party, p_type, 'test',
        now(), 'test')
    returning id;
$$ language sql;

create or replace function pg_temp.portfolio(p_id uuid, p_party uuid, p_sandbox uuid)
returns void as $$
    insert into ores_refdata_portfolios_tbl (id, tenant_id, version, party_id, name,
        parent_portfolio_id, purpose_type, is_virtual, sandbox_id, status,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_id, ores_utility_system_tenant_id_fn(), 0, p_party, 'TCTEST-' || p_id::text,
        null, 'Risk', false, p_sandbox, 'Active', owner_name, owner_name,
        'system.new_record', 'test'
    from t_ctx;
$$ language sql;

create or replace function pg_temp.book(p_id uuid, p_party uuid, p_portfolio uuid,
    p_sandbox uuid)
returns void as $$
    insert into ores_refdata_books_tbl (id, tenant_id, version, party_id, name,
        parent_portfolio_id, functional_currency, book_status, regulatory_book_type,
        is_sweepable, rates_centre_code, sandbox_id,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_id, ores_utility_system_tenant_id_fn(), 0, p_party, 'TCTEST-' || p_id::text,
        p_portfolio, 'USD', 'Active', 'Trading', false, 'GBLO', p_sandbox,
        owner_name, owner_name, 'system.new_record', 'test'
    from t_ctx;
$$ language sql;

create or replace function pg_temp.anchor(p_id uuid, p_counterparty uuid, p_scope text,
    p_nature text)
returns void as $$
    insert into ores_trading_trades_tbl (id, tenant_id, party_id, counterparty_id,
        trade_type, counterparty_scope, booking_nature, entry_channel)
    select p_id, ores_utility_system_tenant_id_fn(), party_id, p_counterparty, 'Swap',
        p_scope, p_nature, 'manual'
    from t_ctx;
$$ language sql;

create or replace function pg_temp.booking(p_trade uuid, p_book uuid, p_version int,
    p_counterparty uuid default null, p_netting_set uuid default null,
    p_party uuid default null)
returns void as $$
    insert into ores_trading_trade_bookings_tbl (trade_id, trade_activity_id, tenant_id, version,
        party_id, counterparty_id, book_id, netting_set_id, trade_date,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_trade, pg_temp.activity(coalesce(p_party, party_id)),
        ores_utility_system_tenant_id_fn(), p_version,
        coalesce(p_party, party_id), p_counterparty, p_book, p_netting_set, current_date,
        owner_name, owner_name, 'system.new_record', 'test'
    from t_ctx;
$$ language sql;

create or replace function pg_temp.state(p_trade uuid, p_activity text, p_version int)
returns void as $$
    insert into ores_trading_trade_states_tbl (trade_id, trade_activity_id, tenant_id, version,
        party_id, status_id,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_trade, pg_temp.activity(party_id, p_activity), ores_utility_system_tenant_id_fn(),
        p_version, party_id, ores_utility_nil_uuid_fn(),
        owner_name, owner_name, 'system.new_record', 'test'
    from t_ctx;
$$ language sql;

create or replace function pg_temp.status_name(p_trade uuid)
returns text as $$
    select s.name
    from ores_trading_trade_states_tbl t
    join ores_dq_fsm_states_tbl s
      on s.id = t.status_id and s.valid_to = ores_utility_infinity_timestamp_fn()
    where t.trade_id = p_trade
      and t.valid_to = ores_utility_infinity_timestamp_fn();
$$ language sql;

-- A real book of the trade's party, a real book of another party, and a
-- virtual book inside a sandbox anchored at the trade party's portfolio.
select pg_temp.portfolio('00000000-0000-0000-0000-0000000bc000', (select party_id from t_ctx), null);
select pg_temp.portfolio('00000000-0000-0000-0000-0000000bc001', (select other_party_id from t_ctx), null);
select pg_temp.book('00000000-0000-0000-0000-0000000bb000', (select party_id from t_ctx),
    '00000000-0000-0000-0000-0000000bc000', null);
select pg_temp.book('00000000-0000-0000-0000-0000000bb001', (select other_party_id from t_ctx),
    '00000000-0000-0000-0000-0000000bc001', null);

insert into ores_refdata_portfolio_rights_tbl (id, tenant_id, version, account_id,
    portfolio_id, right_code, modified_by, performed_by, change_reason_code,
    change_commentary)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, owner_id,
    '00000000-0000-0000-0000-0000000bc000', 'open_sandbox', owner_name, owner_name,
    'system.new_record', 'test'
from t_ctx;

insert into ores_refdata_sandboxes_tbl (id, tenant_id, version, name, purpose,
    anchor_portfolio_id, owner_account_id, visibility, status, review_date,
    modified_by, performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-0000000bd000', ores_utility_system_tenant_id_fn(), 0,
    'TCTEST-sandbox', 'experiment', '00000000-0000-0000-0000-0000000bc000', owner_id,
    'private', 'open', current_date + 90, owner_name, owner_name, 'system.new_record', 'test'
from t_ctx;

select pg_temp.portfolio('00000000-0000-0000-0000-0000000bc002', (select party_id from t_ctx),
    '00000000-0000-0000-0000-0000000bd000');
select pg_temp.book('00000000-0000-0000-0000-0000000bb002', (select party_id from t_ctx),
    '00000000-0000-0000-0000-0000000bc002', '00000000-0000-0000-0000-0000000bd000');

-- Netting sets of the trade's counterparty and of another counterparty.
insert into ores_refdata_netting_sets_tbl (id, tenant_id, version, code, counterparty_id,
    party_id, modified_by, performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-0000000be000'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'TCTEST-NS-0', counterparty_id, party_id, owner_name, owner_name, 'system.new_record', 'test'
from t_ctx
union all
select '00000000-0000-0000-0000-0000000be001'::uuid, ores_utility_system_tenant_id_fn(), 0,
    'TCTEST-NS-1', other_counterparty_id, party_id, owner_name, owner_name, 'system.new_record', 'test'
from t_ctx;

-- Trades: an intra-entity actual, an external actual, a hypothetical.
select pg_temp.anchor('00000000-0000-0000-0000-0000000ba001', null, 'intra_entity', 'actual');
select pg_temp.anchor('00000000-0000-0000-0000-0000000ba002', (select counterparty_id from t_ctx),
    'external', 'actual');
select pg_temp.anchor('00000000-0000-0000-0000-0000000ba003', null, 'intra_entity', 'hypothetical');

-- =============================================================================
-- What a booking may name
-- =============================================================================

select lives_ok(
    $$select pg_temp.booking('00000000-0000-0000-0000-0000000ba001',
        '00000000-0000-0000-0000-0000000bb000', 0)$$,
    'a booking of an existing trade into a real book of its party is written');

select throws_ok(
    $$select pg_temp.booking('00000000-0000-0000-0000-0000000ba0ff',
        '00000000-0000-0000-0000-0000000bb000', 0)$$,
    '23503', null,
    'a booking of an unknown trade is refused');

select throws_ok(
    $$select pg_temp.booking('00000000-0000-0000-0000-0000000ba002',
        '00000000-0000-0000-0000-0000000bb001', 0, (select counterparty_id from t_ctx), null,
        (select other_party_id from t_ctx))$$,
    '23503', null,
    'a booking cannot name a party other than the trade''s');

select throws_ok(
    $$select pg_temp.booking('00000000-0000-0000-0000-0000000ba002',
        '00000000-0000-0000-0000-0000000bb000', 0, (select other_counterparty_id from t_ctx))$$,
    '23503', null,
    'a booking cannot name a counterparty other than the trade''s');

select throws_ok(
    $$select pg_temp.booking('00000000-0000-0000-0000-0000000ba002',
        '00000000-0000-0000-0000-0000000bb001', 0, (select counterparty_id from t_ctx))$$,
    '23503', null,
    'a booking cannot name a book of another party');

select throws_ok(
    $$select pg_temp.booking('00000000-0000-0000-0000-0000000ba002',
        '00000000-0000-0000-0000-0000000bb000', 0, (select counterparty_id from t_ctx),
        '00000000-0000-0000-0000-0000000be001')$$,
    '23503', null,
    'a booking cannot file the trade in another counterparty''s netting set');

select lives_ok(
    $$select pg_temp.booking('00000000-0000-0000-0000-0000000ba002',
        '00000000-0000-0000-0000-0000000bb000', 0, (select counterparty_id from t_ctx),
        '00000000-0000-0000-0000-0000000be000')$$,
    'a booking files the trade in its counterparty''s netting set');

-- =============================================================================
-- The state follows the machine
-- =============================================================================

select lives_ok(
    $$select pg_temp.state('00000000-0000-0000-0000-0000000ba001', 'new_booking', 0)$$,
    'a new booking starts the machine');

select is(pg_temp.status_name('00000000-0000-0000-0000-0000000ba001'), 'live',
    'a new booking makes the trade live');

select throws_ok(
    $$select pg_temp.state('00000000-0000-0000-0000-0000000ba002', 'execution', 0)$$,
    '23514', null,
    'a transition that leaves a state cannot book a trade');

select lives_ok(
    $$select pg_temp.state('00000000-0000-0000-0000-0000000ba001', 'amendment', 1)$$,
    'an activity with no transition versions the state');

select is(pg_temp.status_name('00000000-0000-0000-0000-0000000ba001'), 'live',
    'an activity with no transition leaves the status where it was');

-- =============================================================================
-- Virtual books
-- =============================================================================

select throws_ok(
    $$select pg_temp.booking('00000000-0000-0000-0000-0000000ba001',
        '00000000-0000-0000-0000-0000000bb002', 1)$$,
    '23514', null,
    'a live actual trade cannot move into a virtual book');

select lives_ok(
    $$select pg_temp.state('00000000-0000-0000-0000-0000000ba002', 'draft_capture', 0);
      select pg_temp.booking('00000000-0000-0000-0000-0000000ba002',
        '00000000-0000-0000-0000-0000000bb002', 1, (select counterparty_id from t_ctx))$$,
    'a draft may sit in a virtual book');

select throws_ok(
    $$select pg_temp.state('00000000-0000-0000-0000-0000000ba002', 'execution', 1)$$,
    '23514', null,
    'a draft in a virtual book cannot go live');

select lives_ok(
    $$select pg_temp.booking('00000000-0000-0000-0000-0000000ba002',
        '00000000-0000-0000-0000-0000000bb000', 2, (select counterparty_id from t_ctx));
      select pg_temp.state('00000000-0000-0000-0000-0000000ba002', 'execution', 1)$$,
    'a draft goes live once the same write books it into a real book');

select is(pg_temp.status_name('00000000-0000-0000-0000-0000000ba002'), 'live',
    'the executed draft is live');

select lives_ok(
    $$select pg_temp.booking('00000000-0000-0000-0000-0000000ba003',
        '00000000-0000-0000-0000-0000000bb002', 0)$$,
    'a hypothetical may sit in a virtual book');

select lives_ok(
    $$select pg_temp.state('00000000-0000-0000-0000-0000000ba003', 'new_booking', 0)$$,
    'a hypothetical in a virtual book may be live');

select * from finish();

rollback;
