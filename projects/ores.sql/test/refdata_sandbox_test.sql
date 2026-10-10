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
 * pgTAP tests for sandboxes, virtual books and the rights at portfolio nodes.
 *
 * Tests cover:
 * - A right held at a node applies below it and nowhere else
 * - Opening a sandbox needs open_sandbox at an official anchor
 * - A portfolio or book shares its parent's sandbox, or both have none
 * - A portfolio's sandbox is fixed
 * - A virtual book has no ledger reference and is not a sweep target
 * - Who may see a sandbox: owner, readers of the anchor, members
 * - Row-level security hides a sandbox's portfolios and books from others
 *
 * Run with: pg_prove -d <database> test/refdata_sandbox_test.sql
 */

begin;

select plan(24);

-- =============================================================================
-- Setup: two accounts, an official desk with a sub-desk, a sibling desk
-- =============================================================================

-- Row-level security applies to the test user too: state the tenant and the
-- visible parties before writing anything, because the fixture below reads
-- the parties table and the policies then filter.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

-- A recreated database carries the system party and no book, and the context
-- below reads a book. The suite rolls the fixture back.
insert into ores_refdata_portfolios_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id, purpose_type,
    is_virtual, status, modified_by, performed_by, change_reason_code, change_commentary
)
select '00000000-0000-0000-0000-0000000cf201'::uuid, ores_utility_system_tenant_id_fn(), 0,
    id, 'SANDBOX-FIXTURE-PORTFOLIO', null, 'Risk', false, 'Active',
    current_user, current_user, 'system.test', 'Sandbox pgTAP fixture'
from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
  and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_refdata_books_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id, functional_currency,
    book_status, regulatory_book_type, is_sweepable, rates_centre_code, sandbox_id,
    book_purpose_type, ledger_feed_type,
    modified_by, performed_by, change_reason_code, change_commentary
)
select '00000000-0000-0000-0000-0000000cf202'::uuid, ores_utility_system_tenant_id_fn(), 0,
    party_id, 'SANDBOX-FIXTURE-BOOK', '00000000-0000-0000-0000-0000000cf201'::uuid,
    'USD', 'Active', 'Trading', false, 'WRLD', null,
    'Test', 'None',
    current_user, current_user, 'system.test', 'Sandbox pgTAP fixture'
from ores_refdata_portfolios_tbl
where id = '00000000-0000-0000-0000-0000000cf201'::uuid
  and valid_to = ores_utility_infinity_timestamp_fn();

create temp table t_ctx on commit drop as
select (select tenant_id from ores_refdata_books_tbl where valid_to = ores_utility_infinity_timestamp_fn() order by id limit 1) as tenant_id, (select party_id from ores_refdata_books_tbl where valid_to = ores_utility_infinity_timestamp_fn() order by id limit 1) as party_id, (select functional_currency from ores_refdata_books_tbl where valid_to = ores_utility_infinity_timestamp_fn() order by id limit 1) as ccy, (select regulatory_book_type from ores_refdata_books_tbl where valid_to = ores_utility_infinity_timestamp_fn() order by id limit 1) as rbt, (select rates_centre_code from ores_refdata_books_tbl where valid_to = ores_utility_infinity_timestamp_fn() order by id limit 1) as rc,
       (select id from ores_iam_accounts_tbl
        where account_type = 'service' and valid_to = ores_utility_infinity_timestamp_fn()
        order by username limit 1) as owner_id,
       (select username from ores_iam_accounts_tbl
        where account_type = 'service' and valid_to = ores_utility_infinity_timestamp_fn()
        order by username limit 1) as owner_name,
       (select id from ores_iam_accounts_tbl
        where account_type = 'service' and valid_to = ores_utility_infinity_timestamp_fn()
        order by username offset 1 limit 1) as other_id,
       (select username from ores_iam_accounts_tbl
        where account_type = 'service' and valid_to = ores_utility_infinity_timestamp_fn()
        order by username offset 1 limit 1) as other_name,
       (select id from ores_iam_accounts_tbl
        where account_type = 'service' and valid_to = ores_utility_infinity_timestamp_fn()
        order by username offset 2 limit 1) as member_id;


create or replace function pg_temp.portfolio(p_id uuid, p_parent uuid, p_sandbox uuid)
returns void as $$
    insert into ores_refdata_portfolios_tbl (id, tenant_id, version, party_id, name,
        parent_portfolio_id, purpose_type, is_virtual, sandbox_id, status,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_id, tenant_id, 0, party_id, 'SBTEST-' || p_id::text, p_parent, 'Risk',
        false, p_sandbox, 'Active', owner_name, owner_name, 'system.new_record', 'test'
    from t_ctx;
$$ language sql;

create or replace function pg_temp.book(p_id uuid, p_parent uuid, p_sandbox uuid,
    p_gl text default null, p_sweep boolean default false)
returns void as $$
    insert into ores_refdata_books_tbl (id, tenant_id, version, party_id, name,
        parent_portfolio_id, functional_currency, gl_account_ref, book_status,
        regulatory_book_type, is_sweepable, rates_centre_code, sandbox_id,
        book_purpose_type, ledger_feed_type,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_id, tenant_id, 0, party_id, 'SBTEST-' || p_id::text, p_parent, ccy, p_gl,
        'Active', rbt, p_sweep, rc, p_sandbox, 'Test', 'None', owner_name, owner_name,
        'system.new_record', 'test'
    from t_ctx;
$$ language sql;

create or replace function pg_temp.sandbox(p_id uuid, p_anchor uuid, p_version int,
    p_visibility text default 'private', p_status text default 'open')
returns void as $$
    insert into ores_refdata_sandboxes_tbl (id, tenant_id, version, name, purpose,
        anchor_portfolio_id, owner_account_id, visibility, status, review_date,
        modified_by, performed_by, change_reason_code, change_commentary)
    select p_id, tenant_id, p_version, 'SBTEST-' || p_id::text, 'experiment', p_anchor,
        owner_id, p_visibility, p_status, current_date + 90, owner_name, owner_name,
        'system.new_record', 'test'
    from t_ctx;
$$ language sql;

create or replace function pg_temp.grant_right(p_account uuid, p_portfolio uuid, p_right text)
returns void as $$
    insert into ores_refdata_portfolio_rights_tbl (id, tenant_id, version, account_id,
        portfolio_id, right_code, modified_by, performed_by, change_reason_code,
        change_commentary)
    select gen_random_uuid(), tenant_id, 0, p_account, p_portfolio, p_right,
        owner_name, owner_name, 'system.new_record', 'test'
    from t_ctx;
$$ language sql;

-- The test acts as the sandbox owner: row-level security applies to the test
-- user, so a sandbox's portfolios are written by someone who may see them.
select set_config('app.current_actor', (select owner_name from t_ctx), true);

select pg_temp.portfolio('00000000-0000-0000-0000-00000000ab00', null, null);
select pg_temp.portfolio('00000000-0000-0000-0000-00000000ab01', '00000000-0000-0000-0000-00000000ab00', null);
select pg_temp.portfolio('00000000-0000-0000-0000-00000000ab02', null, null);
select pg_temp.grant_right((select owner_id from t_ctx), '00000000-0000-0000-0000-00000000ab00', 'open_sandbox');

-- =============================================================================
-- Rights at portfolio nodes
-- =============================================================================

select ok(ores_refdata_account_holds_portfolio_right_fn(
    (select tenant_id from t_ctx), (select owner_id from t_ctx), '00000000-0000-0000-0000-00000000ab01', 'open_sandbox'),
    'a right held at a desk applies to its sub-desk');
select ok(not ores_refdata_account_holds_portfolio_right_fn(
    (select tenant_id from t_ctx), (select owner_id from t_ctx), '00000000-0000-0000-0000-00000000ab02', 'open_sandbox'),
    'a right held at a desk does not apply to a sibling desk');
select ok(not ores_refdata_account_holds_portfolio_right_fn(
    (select tenant_id from t_ctx), (select owner_id from t_ctx), '00000000-0000-0000-0000-00000000ab01', 'read'),
    'a right is held only under its own name');
select throws_ok($$select pg_temp.grant_right((select owner_id from t_ctx), '00000000-0000-0000-0000-00000000ab02', 'trade')$$,
    '23514', null, 'an unknown right is refused');

-- =============================================================================
-- Opening a sandbox and its tree
-- =============================================================================

select lives_ok($$select pg_temp.sandbox('00000000-0000-0000-0000-00000000ab20', '00000000-0000-0000-0000-00000000ab01', 0)$$,
    'the holder of open_sandbox opens a sandbox below the node');
select throws_ok($$select pg_temp.sandbox(gen_random_uuid(), '00000000-0000-0000-0000-00000000ab02', 0)$$,
    '42501', null, 'opening a sandbox without the right is refused');
select lives_ok($$select pg_temp.portfolio('00000000-0000-0000-0000-00000000ab10', null, '00000000-0000-0000-0000-00000000ab20')$$,
    'a sandbox root portfolio is accepted');
select throws_ok($$select pg_temp.portfolio(gen_random_uuid(), '00000000-0000-0000-0000-00000000ab10', null)$$,
    '23514', null, 'an official portfolio under a sandbox portfolio is refused');
select throws_ok($$select pg_temp.portfolio(gen_random_uuid(), '00000000-0000-0000-0000-00000000ab00', '00000000-0000-0000-0000-00000000ab20')$$,
    '23514', null, 'a sandbox portfolio under an official portfolio is refused');
select throws_ok($$select pg_temp.sandbox(gen_random_uuid(), '00000000-0000-0000-0000-00000000ab10', 0)$$,
    '23514', null, 'a sandbox anchored at a sandbox portfolio is refused');
select throws_ok(
    $$insert into ores_refdata_portfolios_tbl (id, tenant_id, version, party_id, name,
        parent_portfolio_id, purpose_type, is_virtual, sandbox_id, status, modified_by,
        performed_by, change_reason_code, change_commentary)
      select '00000000-0000-0000-0000-00000000ab10', tenant_id, 1, party_id, 'SBTEST-moved', null, 'Risk', false, null,
        'Active', owner_name, owner_name, 'system.new_record', 'test' from t_ctx$$,
    '23514', null, 'a portfolio cannot leave its sandbox');

-- =============================================================================
-- Virtual books
-- =============================================================================

select lives_ok($$select pg_temp.book(gen_random_uuid(), '00000000-0000-0000-0000-00000000ab10', '00000000-0000-0000-0000-00000000ab20')$$,
    'a virtual book in its sandbox tree is accepted');
select throws_ok($$select pg_temp.book(gen_random_uuid(), '00000000-0000-0000-0000-00000000ab10', '00000000-0000-0000-0000-00000000ab20', 'GL-1')$$,
    '23514', null, 'a virtual book with a ledger reference is refused');
select throws_ok($$select pg_temp.book(gen_random_uuid(), '00000000-0000-0000-0000-00000000ab10', '00000000-0000-0000-0000-00000000ab20', null, true)$$,
    '23514', null, 'a virtual book as a sweep target is refused');
select throws_ok($$select pg_temp.book(gen_random_uuid(), '00000000-0000-0000-0000-00000000ab10', null)$$,
    '23514', null, 'a real book under a sandbox portfolio is refused');
select throws_ok($$select pg_temp.book(gen_random_uuid(), '00000000-0000-0000-0000-00000000ab00', '00000000-0000-0000-0000-00000000ab20')$$,
    '23514', null, 'a virtual book under an official portfolio is refused');

-- =============================================================================
-- Who may see a sandbox
-- =============================================================================

select ok(ores_refdata_account_sees_sandbox_fn(
    (select tenant_id from t_ctx), (select owner_id from t_ctx), '00000000-0000-0000-0000-00000000ab20'),
    'the owner sees a private sandbox');
select ok(not ores_refdata_account_sees_sandbox_fn(
    (select tenant_id from t_ctx), (select other_id from t_ctx), '00000000-0000-0000-0000-00000000ab20'),
    'another account does not see a private sandbox');
select pg_temp.sandbox('00000000-0000-0000-0000-00000000ab20', '00000000-0000-0000-0000-00000000ab01', 1, 'shared');
select pg_temp.grant_right((select other_id from t_ctx), '00000000-0000-0000-0000-00000000ab00', 'read');
select ok(ores_refdata_account_sees_sandbox_fn(
    (select tenant_id from t_ctx), (select other_id from t_ctx), '00000000-0000-0000-0000-00000000ab20'),
    'a reader of the anchor sees a shared sandbox');
select pg_temp.sandbox('00000000-0000-0000-0000-00000000ab20', '00000000-0000-0000-0000-00000000ab01', 2, 'members');
insert into ores_refdata_sandbox_members_tbl (id, tenant_id, version, sandbox_id,
    account_id, modified_by, performed_by, change_reason_code, change_commentary)
select gen_random_uuid(), tenant_id, 0, '00000000-0000-0000-0000-00000000ab20', member_id, owner_name, owner_name,
    'system.new_record', 'test' from t_ctx;
select ok(ores_refdata_account_sees_sandbox_fn(
    (select tenant_id from t_ctx), (select member_id from t_ctx), '00000000-0000-0000-0000-00000000ab20'),
    'a member sees a members sandbox');

-- =============================================================================
-- Row-level security: what each actor sees
-- =============================================================================

select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

select set_config('app.current_actor', (select owner_name from t_ctx), true);
select is((select count(*) from ores_refdata_portfolios_tbl where id = '00000000-0000-0000-0000-00000000ab10'
    and valid_to = ores_utility_infinity_timestamp_fn())::int, 1,
    'the owner sees the sandbox portfolio');

select set_config('app.current_actor', (select other_name from t_ctx), true);
select is((select count(*) from ores_refdata_portfolios_tbl where id = '00000000-0000-0000-0000-00000000ab10'
    and valid_to = ores_utility_infinity_timestamp_fn())::int, 0,
    'an account that may not see the sandbox sees none of its portfolios');

select set_config('app.current_actor', '', true);
select is((select count(*) from ores_refdata_books_tbl where sandbox_id = '00000000-0000-0000-0000-00000000ab20')::int, 0,
    'a session with no actor sees no virtual book');

select set_config('app.current_actor', (select owner_name from t_ctx), true);
select pg_temp.sandbox('00000000-0000-0000-0000-00000000ab20', '00000000-0000-0000-0000-00000000ab01', 3, 'members', 'archived');
select throws_ok($$select pg_temp.portfolio(gen_random_uuid(), '00000000-0000-0000-0000-00000000ab10', '00000000-0000-0000-0000-00000000ab20')$$,
    '23514', null, 'nothing is written into an archived sandbox');

select * from finish();
rollback;
