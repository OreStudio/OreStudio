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
 * pgTAP tests for official reports reading no sandbox.
 *
 * Tests cover:
 * - Report definitions are official unless set otherwise
 * - An official report's scope refuses a virtual book and a sandbox portfolio
 * - A report that is not official may scope sandbox inputs
 * - A definition cannot become official while its scope holds sandbox inputs
 * - An official report resolves its official books; one that is not official
 *   resolves its virtual books
 *
 * Run with: pg_prove -d <database> test/reporting_official_reports_test.sql
 */

begin;

select plan(9);

-- =============================================================================
-- Setup: a sandbox with a virtual book, an official book, and a report
-- =============================================================================

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select b.tenant_id, b.party_id, b.functional_currency as ccy,
       b.regulatory_book_type as rbt, b.rates_centre_code as rc,
       (select id from ores_iam_accounts_tbl where account_type = 'service'
        and valid_to = ores_utility_infinity_timestamp_fn() order by username limit 1) as owner_id,
       (select username from ores_iam_accounts_tbl where account_type = 'service'
        and valid_to = ores_utility_infinity_timestamp_fn() order by username limit 1) as owner_name,
       (select c.id from ores_reporting_risk_report_configs_tbl c
        where c.tenant_id = b.tenant_id and c.valid_to = ores_utility_infinity_timestamp_fn()
        order by c.id limit 1) as config_id
from ores_refdata_books_tbl b
where b.valid_to = ores_utility_infinity_timestamp_fn() and b.tenant_id = ores_utility_system_tenant_id_fn()
order by b.id limit 1;

select set_config('app.current_actor', (select owner_name from t_ctx), true);

create or replace function pg_temp.definition_id()
returns uuid as $$
    select report_definition_id from ores_reporting_risk_report_configs_tbl
    where id = (select config_id from t_ctx) and valid_to = ores_utility_infinity_timestamp_fn();
$$ language sql;

create or replace function pg_temp.set_official(p_flag boolean)
returns void as $$
declare
    r ores_reporting_report_definitions_tbl;
begin
    select * into r from ores_reporting_report_definitions_tbl
    where id = pg_temp.definition_id() and valid_to = ores_utility_infinity_timestamp_fn();
    r.is_official := p_flag;
    insert into ores_reporting_report_definitions_tbl select (r).*;
end;
$$ language plpgsql;

create or replace function pg_temp.scope_book(p_book uuid)
returns void as $$
    insert into ores_reporting_risk_report_config_books_tbl (tenant_id,
        risk_report_config_id, book_id, valid_from, valid_to)
    select tenant_id, config_id, p_book, now(), ores_utility_infinity_timestamp_fn() from t_ctx;
$$ language sql;

create or replace function pg_temp.scope_portfolio(p_portfolio uuid)
returns void as $$
    insert into ores_reporting_risk_report_config_portfolios_tbl (tenant_id,
        risk_report_config_id, portfolio_id, valid_from, valid_to)
    select tenant_id, config_id, p_portfolio, now(), ores_utility_infinity_timestamp_fn() from t_ctx;
$$ language sql;

create or replace function pg_temp.resolved()
returns setof uuid as $$
    select ores_reporting_resolve_book_ids_for_config_fn(tenant_id, config_id) from t_ctx;
$$ language sql;

insert into ores_refdata_portfolios_tbl (id, tenant_id, version, party_id, name,
    parent_portfolio_id, purpose_type, is_virtual, sandbox_id, status, modified_by,
    performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-00000000ac00', tenant_id, 0, party_id, 'OFFREP-A', null, 'Risk', false, null, 'Active',
    owner_name, owner_name, 'system.new_record', 'test' from t_ctx;
insert into ores_refdata_portfolio_rights_tbl (id, tenant_id, version, account_id,
    portfolio_id, right_code, modified_by, performed_by, change_reason_code,
    change_commentary)
select gen_random_uuid(), tenant_id, 0, owner_id, '00000000-0000-0000-0000-00000000ac00', 'open_sandbox', owner_name,
    owner_name, 'system.new_record', 'test' from t_ctx;
insert into ores_refdata_sandboxes_tbl (id, tenant_id, version, name, purpose,
    anchor_portfolio_id, owner_account_id, visibility, status, review_date,
    modified_by, performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-00000000ac20', tenant_id, 0, 'OFFREP-SANDBOX', 'experiment', '00000000-0000-0000-0000-00000000ac00', owner_id, 'private',
    'open', current_date + 90, owner_name, owner_name, 'system.new_record', 'test'
from t_ctx;
insert into ores_refdata_portfolios_tbl (id, tenant_id, version, party_id, name,
    parent_portfolio_id, purpose_type, is_virtual, sandbox_id, status, modified_by,
    performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-00000000ac10', tenant_id, 0, party_id, 'OFFREP-P', null, 'Risk', false, '00000000-0000-0000-0000-00000000ac20', 'Active',
    owner_name, owner_name, 'system.new_record', 'test' from t_ctx;
insert into ores_refdata_books_tbl (id, tenant_id, version, party_id, name,
    parent_portfolio_id, functional_currency, book_status, regulatory_book_type,
    is_sweepable, rates_centre_code, sandbox_id, modified_by, performed_by,
    change_reason_code, change_commentary)
select v.id, tenant_id, 0, party_id, v.name, v.parent, ccy, 'Active', rbt, false, rc,
    v.sandbox, owner_name, owner_name, 'system.new_record', 'test'
from t_ctx, (values ('00000000-0000-0000-0000-00000000ac30'::uuid, 'OFFREP-VIRTUAL', '00000000-0000-0000-0000-00000000ac10'::uuid, '00000000-0000-0000-0000-00000000ac20'::uuid),
                    ('00000000-0000-0000-0000-00000000ac32'::uuid, 'OFFREP-VIRTUAL-2', '00000000-0000-0000-0000-00000000ac10'::uuid, '00000000-0000-0000-0000-00000000ac20'::uuid),
                    ('00000000-0000-0000-0000-00000000ac31'::uuid, 'OFFREP-OFFICIAL', '00000000-0000-0000-0000-00000000ac00'::uuid, null::uuid))
     v(id, name, parent, sandbox);

-- =============================================================================
-- Official scope
-- =============================================================================

select ok((select is_official from ores_reporting_report_definitions_tbl
           where id = pg_temp.definition_id() and valid_to = ores_utility_infinity_timestamp_fn()),
    'a report definition is official unless set otherwise');
select throws_ok($$select pg_temp.scope_book('00000000-0000-0000-0000-00000000ac30')$$,
    '23514', null, 'an official scope refuses a virtual book');
select throws_ok($$select pg_temp.scope_portfolio('00000000-0000-0000-0000-00000000ac10')$$,
    '23514', null, 'an official scope refuses a sandbox portfolio');
select lives_ok($$select pg_temp.scope_book('00000000-0000-0000-0000-00000000ac31')$$,
    'an official scope accepts an official book');

-- =============================================================================
-- The resolver, as a last line of defence
-- =============================================================================

-- The scope trigger and the definition check keep a virtual book out of an
-- official scope, so the resolver's own filter cannot be reached from here;
-- it guards data written before those checks existed.
select ok('00000000-0000-0000-0000-00000000ac31'::uuid in (select pg_temp.resolved()),
    'an official report still resolves its official books');

-- =============================================================================
-- A report that is not official
-- =============================================================================

select lives_ok($$select pg_temp.set_official(false)$$,
    'a definition can stop being official');
select lives_ok($$select pg_temp.scope_book('00000000-0000-0000-0000-00000000ac32')$$,
    'a report that is not official may scope a virtual book');
select ok('00000000-0000-0000-0000-00000000ac32'::uuid in (select pg_temp.resolved()),
    'a report that is not official resolves its virtual books');
select throws_ok($$select pg_temp.set_official(true)$$,
    '23514', null, 'a definition cannot become official while its scope holds a virtual book');

select * from finish();
rollback;
