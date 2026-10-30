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
 * A session's settings must match its adapter's configuration schema.
 *
 * The schema is the catalogue's: Frankfurter states url (url), backfill_days
 * (int) and poll_minutes (int). A key outside it, a value of the wrong type or
 * a value that is not a JSON object is refused; a matching object is saved and
 * read back.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/marketdata_session_settings_validation_test.sql
 */

begin;

select plan(7);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_adapters on commit drop as
select id from ores_dq_datasets_tbl
where code = 'marketdata.adapters'
  and valid_to = ores_utility_infinity_timestamp_fn();

create temporary table t_suppliers on commit drop as
select id from ores_dq_datasets_tbl
where code = 'marketdata.suppliers'
  and valid_to = ores_utility_infinity_timestamp_fn();

select * from ores_marketdata_publish_adapters_from_dq_fn(
    (select id from t_adapters), ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);
select * from ores_marketdata_publish_suppliers_from_dq_fn(
    (select id from t_suppliers), ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

create or replace function pg_temp.session_insert(p_code text, p_settings text, p_adapter text default 'frankfurter')
returns void as $$
begin
    insert into ores_marketdata_sessions_tbl (
        code, tenant_id, version, name, supplier_code, adapter_code, environment,
        endpoint, credential_ref, settings, enabled, description,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        p_code, ores_utility_system_tenant_id_fn(), 0, 'Session ' || p_code,
        'frankfurter', p_adapter, 'production', 'https://api.frankfurter.dev',
        '', p_settings, true, 'pgTAP settings validation',
        current_user, current_user, 'system.initial_load', 'pgTAP settings validation'
    );
end;
$$ language plpgsql;

select lives_ok(
    $$select pg_temp.session_insert('pgtap-session-ok',
        '{"url":"https://api.frankfurter.dev","backfill_days":400,"poll_minutes":60}')$$,
    'settings that match the adapter schema are saved'
);

select throws_ok(
    $$select pg_temp.session_insert('pgtap-session-unknown',
        '{"url":"https://api.frankfurter.dev","nope":1}')$$,
    '23514',
    null,
    'a key the schema does not state is refused'
);

select throws_ok(
    $$select pg_temp.session_insert('pgtap-session-type',
        '{"backfill_days":"many"}')$$,
    '23514',
    null,
    'a value of the wrong type is refused'
);

select throws_ok(
    $$select pg_temp.session_insert('pgtap-session-json', 'not json')$$,
    '23514',
    null,
    'settings that are not JSON are refused'
);

select throws_ok(
    $$select pg_temp.session_insert('pgtap-session-array', '[1, 2]')$$,
    '23514',
    null,
    'settings that are not a JSON object are refused'
);

select throws_ok(
    $$select pg_temp.session_insert('pgtap-session-adapter', '{}', 'no-such-adapter')$$,
    '23503',
    null,
    'an adapter outside the catalogue is refused'
);

select is(
    (select settings::jsonb
     from ores_marketdata_sessions_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and code = 'pgtap-session-ok'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    '{"url":"https://api.frankfurter.dev","backfill_days":400,"poll_minutes":60}'::jsonb,
    'the session reads back the settings it was saved with'
);

select * from finish();

rollback;
