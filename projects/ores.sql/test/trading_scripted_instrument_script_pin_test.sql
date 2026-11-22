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
 * pgTAP tests for the script a scripted trade was booked on.
 *
 * Tests cover:
 * - A trade that names a library script records the system tenant and the
 *   script's current version
 * - A trade that states its script inline records the SHA-256 of the text and
 *   no library version
 * - An amendment that states no version keeps the version the trade was
 *   booked on after the library moves on
 * - A new trade books on the library's new version
 * - A stated version the library never had is refused
 * - A name no library holds is recorded with no version
 * - A trade whose latest row named another script books on the current version
 *
 * Run with: pg_prove -d <database> test/trading_scripted_instrument_script_pin_test.sql
 */

begin;

select plan(9);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       (select id from ores_dq_datasets_tbl
        where code = 'ore.script_library'
          and valid_to = ores_utility_infinity_timestamp_fn()) as dataset_id,
       (select version from ores_trading_scripts_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and name = 'Accumulator01'
          and valid_to = ores_utility_infinity_timestamp_fn()) as booked_version;

select set_config('app.current_party_id', (select party_id::text from t_ctx), true);
select set_config('app.visible_party_ids', '{' || (select party_id::text from t_ctx) || '}',
                  true);

create or replace function pg_temp.activity()
returns uuid as $$
    insert into ores_trading_trade_activities_tbl (id, tenant_id, party_id,
        activity_type_code, actor, occurred_at, comment,
        modified_by, performed_by, change_reason_code, change_commentary)
    select gen_random_uuid(), ores_utility_system_tenant_id_fn(), party_id, 'new_booking',
        'test', now(), 'test',
        current_user, current_user, 'system.new_record', 'Script pin pgTAP fixture'
    from t_ctx
    returning id;
$$ language sql;

create or replace function pg_temp.trade(p_id uuid)
returns void as $$
    insert into ores_trading_trades_tbl (id, tenant_id, party_id, counterparty_id,
        trade_type, counterparty_scope, booking_nature, entry_channel, origin,
        external_version, modified_by, performed_by, change_reason_code, change_commentary)
    select p_id, ores_utility_system_tenant_id_fn(), party_id, null, 'ScriptedTrade',
        'intra_entity', 'actual', 'manual', 'api', 0,
        current_user, current_user, 'system.new_record', 'Script pin pgTAP fixture'
    from t_ctx;
$$ language sql;

create or replace function pg_temp.instrument(p_trade uuid, p_version int, p_name text,
    p_body text default null, p_script_tenant uuid default null,
    p_script_version int default null)
returns void as $$
    insert into ores_trading_scripted_instruments_tbl (trade_type_code, party_id, trade_id,
        trade_activity_id, tenant_id, version, script_name, script_body, script_npv,
        description, data_element, script_tenant_id, script_version, script_hash,
        modified_by, performed_by, change_reason_code, change_commentary)
    select 'ScriptedTrade', party_id, p_trade, pg_temp.activity(),
        ores_utility_system_tenant_id_fn(), p_version, p_name, p_body,
        case when p_body is null then null else 'Option' end, null, null,
        p_script_tenant, p_script_version, null,
        current_user, current_user, 'system.new_record', 'Script pin pgTAP fixture'
    from t_ctx;
$$ language sql;

create temp view t_pin as
select trade_id, script_tenant_id, script_version, script_hash
from ores_trading_scripted_instruments_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();

select pg_temp.trade('00000000-0000-0000-0000-00000005c001');
select pg_temp.instrument('00000000-0000-0000-0000-00000005c001', 0, 'Accumulator01');

select is(
    (select script_tenant_id from t_pin
     where trade_id = '00000000-0000-0000-0000-00000005c001'),
    ores_utility_system_tenant_id_fn(),
    'a trade that names a library script records the tenant that holds it');

select is(
    (select script_version from t_pin
     where trade_id = '00000000-0000-0000-0000-00000005c001'),
    (select booked_version from t_ctx),
    'a trade that names a library script records its current version');

select pg_temp.trade('00000000-0000-0000-0000-00000005c002');
select pg_temp.instrument('00000000-0000-0000-0000-00000005c002', 0, 'MyPayoff',
    'Option = PAY(1, Expiry, Expiry, PayCcy);');

select is(
    (select row(script_tenant_id, script_version, script_hash)::text from t_pin
     where trade_id = '00000000-0000-0000-0000-00000005c002'),
    row(null::uuid, null::int,
        encode(sha256(convert_to('Option = PAY(1, Expiry, Expiry, PayCcy);', 'UTF8')),
               'hex'))::text,
    'an inline script records the SHA-256 of its text and no library version');

update ores_dq_script_inputs_artefact_tbl set description = 'changed for the test'
where dataset_id = (select dataset_id from t_ctx)
  and script_name = 'Accumulator01' and sequence_number = 1;
select ores_trading_publish_script_library_from_dq_fn(
    (select dataset_id from t_ctx), ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

select is(
    (select version from ores_trading_scripts_tbl
     where tenant_id = ores_utility_system_tenant_id_fn() and name = 'Accumulator01'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select booked_version + 1 from t_ctx),
    'the library moves on to a new version of the script');

select pg_temp.instrument('00000000-0000-0000-0000-00000005c001', 1, 'Accumulator01');

select is(
    (select script_version from t_pin
     where trade_id = '00000000-0000-0000-0000-00000005c001'),
    (select booked_version from t_ctx),
    'an amendment that states no version keeps the version the trade was booked on');

select pg_temp.trade('00000000-0000-0000-0000-00000005c003');
select pg_temp.instrument('00000000-0000-0000-0000-00000005c003', 0, 'Accumulator01');

select is(
    (select script_version from t_pin
     where trade_id = '00000000-0000-0000-0000-00000005c003'),
    (select booked_version + 1 from t_ctx),
    'a new trade books on the library''s new version');

select pg_temp.trade('00000000-0000-0000-0000-00000005c004');

select throws_ok(
    $$select pg_temp.instrument('00000000-0000-0000-0000-00000005c004', 0, 'Accumulator01',
        null, ores_utility_system_tenant_id_fn(), 999999)$$,
    '23503',
    null,
    'a stated version the library never had is refused');

select pg_temp.instrument('00000000-0000-0000-0000-00000005c004', 0, 'NoSuchScript');

select is(
    (select row(script_tenant_id, script_version)::text from t_pin
     where trade_id = '00000000-0000-0000-0000-00000005c004'),
    row(null::uuid, null::int)::text,
    'a name no library holds is recorded with no version');

select pg_temp.instrument('00000000-0000-0000-0000-00000005c004', 1, 'Accumulator01');

select is(
    (select script_version from t_pin
     where trade_id = '00000000-0000-0000-0000-00000005c004'),
    (select booked_version + 1 from t_ctx),
    'a trade whose latest row named another script books on the current version');

select * from finish();
rollback;
