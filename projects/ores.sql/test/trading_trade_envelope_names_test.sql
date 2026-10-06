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
 * pgTAP tests for the names an ORE envelope gives a booked trade.
 *
 * Tests cover:
 * - The booking's recorded identifiers name the counterparty and netting set
 * - Two aliases of one counterparty each come back as the trade named them
 * - Without a recorded identifier, the entity's first ORE alias names it
 * - A trade with no netting set gets no netting set name
 * - A deleted identifier falls back to the entity's remaining ORE alias
 * - A trade's portfolios come back by name in the order it stated them
 * - A trade is reported only in its own party's portfolios
 * - A second publish of the ORE sample portfolios adds nothing
 * - Neither names function shows a trade of a party the caller cannot see
 * - The booking refuses an identifier of another counterparty or netting set
 *
 * Run with: pg_prove -d <database> test/trading_trade_envelope_names_test.sql
 */

begin;

select plan(11);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'ACMCOR'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       (select username from ores_iam_accounts_tbl
        where account_type = 'service' and valid_to = ores_utility_infinity_timestamp_fn()
        order by username limit 1) as owner_name;

select set_config('app.current_actor', (select owner_name from t_ctx), true);

create or replace function pg_temp.alias(p_scheme_table text, p_value text)
returns uuid as $$
begin
    if p_scheme_table = 'counterparty' then
        return (select id from ores_refdata_counterparty_identifiers_tbl
                where tenant_id = ores_utility_system_tenant_id_fn() and id_scheme = 'ORE'
                  and id_value = p_value and valid_to = ores_utility_infinity_timestamp_fn());
    end if;
    return (select id from ores_refdata_netting_set_identifiers_tbl
            where tenant_id = ores_utility_system_tenant_id_fn() and id_scheme = 'ORE'
              and id_value = p_value and valid_to = ores_utility_infinity_timestamp_fn());
end;
$$ language plpgsql;

create or replace function pg_temp.counterparty_of(p_alias text)
returns uuid as $$
    select counterparty_id from ores_refdata_counterparty_identifiers_tbl
    where id = pg_temp.alias('counterparty', p_alias)
      and valid_to = ores_utility_infinity_timestamp_fn();
$$ language sql;

create or replace function pg_temp.netting_set_of(p_alias text)
returns uuid as $$
    select netting_set_id from ores_refdata_netting_set_identifiers_tbl
    where id = pg_temp.alias('netting_set', p_alias)
      and valid_to = ores_utility_infinity_timestamp_fn();
$$ language sql;

insert into ores_refdata_portfolios_tbl (id, tenant_id, version, party_id, name,
    parent_portfolio_id, purpose_type, is_virtual, status,
    modified_by, performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-00000000e000'::uuid, ores_utility_system_tenant_id_fn(), 0,
    party_id, 'ENVTEST-PORTFOLIO', null, 'Risk', false, 'Active',
    owner_name, owner_name, 'system.new_record', 'test'
from t_ctx;

insert into ores_refdata_books_tbl (id, tenant_id, version, party_id, name,
    parent_portfolio_id, functional_currency, book_status, regulatory_book_type,
    is_sweepable, rates_centre_code, modified_by, performed_by, change_reason_code,
    change_commentary)
select '00000000-0000-0000-0000-00000000e001'::uuid, ores_utility_system_tenant_id_fn(), 0,
    party_id, 'ENVTEST-BOOK', '00000000-0000-0000-0000-00000000e000'::uuid, 'USD',
    'Active', 'Trading', false, 'GBLO', owner_name, owner_name, 'system.new_record', 'test'
from t_ctx;

create or replace function pg_temp.book(p_id uuid, p_counterparty_alias text,
    p_netting_set_alias text, p_record_names boolean)
returns boolean as $$
    select ores_trading_book_trade_fn(p_id, party_id,
        pg_temp.counterparty_of(p_counterparty_alias), 'Swap', 'external', 'actual', 'stp',
        '00000000-0000-0000-0000-00000000e001'::uuid,
        pg_temp.netting_set_of(p_netting_set_alias),
        case when p_record_names then pg_temp.alias('counterparty', p_counterparty_alias) end,
        case when p_record_names then pg_temp.alias('netting_set', p_netting_set_alias) end,
        null, null, 'new_booking', owner_name, 'system.new_record', 'test') is not null
    from t_ctx;
$$ language sql;

select pg_temp.book('00000000-0000-0000-0000-00000000e101', 'CP', 'NS', true);
select pg_temp.book('00000000-0000-0000-0000-00000000e102', 'CPTY', 'NS', true);
select pg_temp.book('00000000-0000-0000-0000-00000000e103', 'CPTY_B', 'CPTY_B', false);
select pg_temp.book('00000000-0000-0000-0000-00000000e104', 'CPTY_A', null, true);

select results_eq(
    $$select counter_party, netting_set_id from ores_trading_trade_envelope_names_fn(
        array['00000000-0000-0000-0000-00000000e101'::uuid])$$,
    $$values ('CP'::text, 'NS'::text)$$,
    'the recorded identifiers name the counterparty and netting set');

select results_eq(
    $$select counter_party from ores_trading_trade_envelope_names_fn(
        array['00000000-0000-0000-0000-00000000e101'::uuid,
              '00000000-0000-0000-0000-00000000e102'::uuid]) order by trade_id$$,
    $$values ('CP'::text), ('CPTY'::text)$$,
    'two aliases of one counterparty each come back as the trade named them');

select results_eq(
    $$select counter_party, netting_set_id from ores_trading_trade_envelope_names_fn(
        array['00000000-0000-0000-0000-00000000e103'::uuid])$$,
    $$values ('CPTY_7'::text, 'CPTY_B'::text)$$,
    'without a recorded identifier, the first ORE alias names the entity');

select results_eq(
    $$select counter_party, netting_set_id from ores_trading_trade_envelope_names_fn(
        array['00000000-0000-0000-0000-00000000e104'::uuid])$$,
    $$values ('CPTY_A'::text, null::text)$$,
    'a trade with no netting set gets no netting set name');

select throws_like(
    $$select ores_trading_book_trade_fn('00000000-0000-0000-0000-00000000e105'::uuid,
        (select party_id from t_ctx), pg_temp.counterparty_of('CPTY_A'), 'Swap', 'external',
        'actual', 'stp', '00000000-0000-0000-0000-00000000e001'::uuid, null,
        pg_temp.alias('counterparty', 'CPTY_B'), null, null, null, 'new_booking',
        (select owner_name from t_ctx), 'system.new_record', 'test')$$,
    '%The identifier must be the trade''s counterparty''s%',
    'the booking refuses an identifier of another counterparty');

select throws_like(
    $$select ores_trading_book_trade_fn('00000000-0000-0000-0000-00000000e106'::uuid,
        (select party_id from t_ctx), pg_temp.counterparty_of('CPTY_B'), 'Swap', 'external',
        'actual', 'stp', '00000000-0000-0000-0000-00000000e001'::uuid,
        pg_temp.netting_set_of('CPTY_B'), null, pg_temp.alias('netting_set', 'CPTY_B_full'),
        null, null, 'new_booking', (select owner_name from t_ctx), 'system.new_record', 'test')$$,
    '%The identifier must be the netting set''s%',
    'the booking refuses an identifier of another netting set');

delete from ores_refdata_counterparty_identifiers_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and id_scheme = 'ORE' and id_value = 'CP';

select results_eq(
    $$select counter_party from ores_trading_trade_envelope_names_fn(
        array['00000000-0000-0000-0000-00000000e101'::uuid])$$,
    $$values ('CPTY'::text)$$,
    'a deleted identifier falls back to the entity''s remaining ORE alias');

create or replace function pg_temp.activity(p_party uuid, p_type text default 'new_booking')
returns uuid as $$
    insert into ores_trading_trade_activities_tbl (id, tenant_id, party_id,
        activity_type_code, actor, occurred_at, comment)
    values (gen_random_uuid(), ores_utility_system_tenant_id_fn(), p_party, p_type, 'test',
        now(), 'test')
    returning id;
$$ language sql;

create or replace function pg_temp.report_in(p_trade uuid, p_sequence int, p_portfolio uuid)
returns void as $$
    insert into ores_trading_trade_portfolios_tbl (trade_id, trade_activity_id, sequence_number,
        tenant_id, version, party_id, portfolio_id, modified_by, performed_by,
        change_reason_code, change_commentary)
    select p_trade, pg_temp.activity(party_id), p_sequence, ores_utility_system_tenant_id_fn(), 0,
        party_id, p_portfolio,
        owner_name, owner_name, 'system.new_record', 'test'
    from t_ctx;
$$ language sql;

create or replace function pg_temp.portfolio_named(p_name text)
returns uuid as $$
    select id from ores_refdata_portfolios_tbl
    where tenant_id = ores_utility_system_tenant_id_fn()
      and party_id = (select party_id from t_ctx)
      and name = p_name
      and valid_to = ores_utility_infinity_timestamp_fn();
$$ language sql;

select pg_temp.report_in('00000000-0000-0000-0000-00000000e102', 1, pg_temp.portfolio_named('PF2'));
select pg_temp.report_in('00000000-0000-0000-0000-00000000e102', 2, pg_temp.portfolio_named('PF1'));

select results_eq(
    $$select name from ores_trading_trade_portfolio_names_fn(
        array['00000000-0000-0000-0000-00000000e102'::uuid])$$,
    $$values ('PF2'::text), ('PF1'::text)$$,
    'a trade''s portfolios come back by name in the order it stated them');

insert into ores_refdata_portfolios_tbl (id, tenant_id, version, party_id, name,
    parent_portfolio_id, purpose_type, is_virtual, status,
    modified_by, performed_by, change_reason_code, change_commentary)
select '00000000-0000-0000-0000-00000000e002'::uuid, ores_utility_system_tenant_id_fn(), 0,
    (select id from ores_refdata_parties_tbl
     where tenant_id = ores_utility_system_tenant_id_fn() and id <> (select party_id from t_ctx)
       and valid_to = ores_utility_infinity_timestamp_fn() order by id limit 1),
    'ENVTEST-OTHER-PARTY', null, 'Risk', false, 'Active',
    owner_name, owner_name, 'system.new_record', 'test'
from t_ctx;

select throws_like(
    $$select pg_temp.report_in('00000000-0000-0000-0000-00000000e102', 3,
        '00000000-0000-0000-0000-00000000e002'::uuid)$$,
    '%The portfolio must be the trade''s party''s%',
    'a trade is reported only in its own party''s portfolios');

select results_eq(
    $$select action, record_count from ores_refdata_publish_named_portfolios_from_dq_fn(
        (select id from ores_dq_datasets_tbl where code = 'ore.sample_portfolios'
           and valid_to = ores_utility_infinity_timestamp_fn()),
        ores_utility_system_tenant_id_fn())$$,
    $$values ('skipped'::text, 2::bigint)$$,
    'a second publish of the ORE sample portfolios adds nothing');

select set_config('app.visible_party_ids',
    (select '{' || id::text || '}' from ores_refdata_parties_tbl
     where tenant_id = ores_utility_system_tenant_id_fn() and id <> (select party_id from t_ctx)
       and valid_to = ores_utility_infinity_timestamp_fn() order by id limit 1), true);

select is(
    (select count(*) from ores_trading_trade_envelope_names_fn(
        array['00000000-0000-0000-0000-00000000e101'::uuid,
              '00000000-0000-0000-0000-00000000e102'::uuid])) +
    (select count(*) from ores_trading_trade_portfolio_names_fn(
        array['00000000-0000-0000-0000-00000000e102'::uuid])),
    0::bigint,
    'neither names function shows a trade of a party the caller cannot see');

select * from finish();

rollback;
