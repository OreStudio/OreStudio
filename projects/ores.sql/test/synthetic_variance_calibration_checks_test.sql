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
 * What one row of a variance calibration can show is checked on write.
 *
 * An input's window and counts are sane; a point that is used has a return and
 * no reason, and a point that is not used says why; a calibration's method and
 * input agree, its shape is required and admissible unless it is rejected, a
 * missing value or NaN fails a bound, a decision carries a reason, and one
 * approved calibration of a pair is current. The policy's variance window
 * defaults to five years.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/synthetic_variance_calibration_checks_test.sql
 */

begin;

select plan(29);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

insert into ores_refdata_parties_tbl (
    id, tenant_id, full_name, short_code, party_category, party_type,
    business_center_code, parent_party_id, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    ('00000000-0000-0000-0000-00000000ca01'::uuid, ores_utility_system_tenant_id_fn(),
     'Variance Calibration Test', 'VCTEST', 'Operational', 'CorporateGroup', 'WRLD', null, 'Active',
     current_user, current_user, 'system.test', 'variance calibration fixture');

select set_config('app.visible_party_ids', '{00000000-0000-0000-0000-00000000ca01}', true);

create or replace function pg_temp.add_input(p_id uuid, p_base text, p_quote text,
    p_window_days int, p_points int, p_used int)
returns void as $$
    insert into ores_synthetic_variance_calibration_inputs_tbl (id, tenant_id, version, party_id,
        base_currency_code, quote_currency_code, fixing_uri, provider_code, inverted, policy_id,
        policy_version, window_end, window_days, point_count, used_count,
        modified_by, performed_by, change_reason_code, change_commentary)
    values (p_id, ores_utility_system_tenant_id_fn(), 0,
        '00000000-0000-0000-0000-00000000ca01'::uuid, p_base, p_quote,
        'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=' || p_quote || '&n=' || p_id::text,
        'ecb', false, gen_random_uuid(), 1, date '2026-11-02', p_window_days, p_points, p_used,
        current_user, current_user, 'system.new_record', 'test');
$$ language sql;

create or replace function pg_temp.add_point(p_input uuid, p_date date, p_value double precision,
    p_return double precision, p_used boolean, p_reason text)
returns void as $$
    insert into ores_synthetic_variance_calibration_input_points_tbl (id, tenant_id, version,
        party_id, input_id, fixing_date, fixing_text, fixing_value, log_return, used,
        exclusion_reason, modified_by, performed_by, change_reason_code, change_commentary)
    values (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
        '00000000-0000-0000-0000-00000000ca01'::uuid, p_input, p_date,
        coalesce(p_value::text, 'n/a'), p_value, p_return, p_used, p_reason,
        current_user, current_user, 'system.new_record', 'test');
$$ language sql;

create or replace function pg_temp.add_calibration(p_quote text, p_method text, p_input uuid,
    p_alpha double precision, p_persistence double precision, p_state text, p_reason text)
returns void as $$
    insert into ores_synthetic_variance_calibrations_tbl (id, tenant_id, version, party_id,
        base_currency_code, quote_currency_code, input_id, method, method_version,
        alpha, gamma, beta, persistence, long_run_variance, starting_variance_ratio,
        log_likelihood, returns_used, starts, starts_agreed, state, state_reason,
        modified_by, performed_by, change_reason_code, change_commentary)
    values (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
        '00000000-0000-0000-0000-00000000ca01'::uuid, 'EUR', p_quote, p_input, p_method, 1,
        p_alpha, 0.02, case when p_alpha is null then null else 0.9 end, p_persistence,
        case when p_alpha is null then null else 0.00004 end,
        case when p_alpha is null then null else 1.0 end,
        case when p_method = 'authored' or p_state = 'rejected' then null else -1234.5 end,
        case when p_method = 'authored' or p_state = 'rejected' then 0 else 1250 end,
        case when p_method = 'authored' or p_state = 'rejected' then 0 else 4 end,
        p_method <> 'authored' and p_state <> 'rejected', p_state, p_reason,
        current_user, current_user, 'system.new_record', 'test');
$$ language sql;

select lives_ok($$select pg_temp.add_input('00000000-0000-0000-0000-00000000cb01', 'EUR', 'USD', 1826, 3, 2)$$,
    'an input over a five-year window is kept');
select throws_like($$select pg_temp.add_input(gen_random_uuid(), 'EUR', 'EUR', 1826, 3, 2)$$,
    '%violates check constraint%', 'an input whose pair is one currency twice is refused');
select throws_like($$select pg_temp.add_input(gen_random_uuid(), 'EUR', 'USD', 20, 3, 2)$$,
    '%violates check constraint%', 'an input over a window shorter than thirty days is refused');
select throws_like($$select pg_temp.add_input(gen_random_uuid(), 'EUR', 'USD', 1826, 3, 4)$$,
    '%violates check constraint%', 'an input that uses more points than it holds is refused');

select lives_ok($$select pg_temp.add_point('00000000-0000-0000-0000-00000000cb01', date '2026-10-30', 1.08, null, false, 'the first point has no return')$$,
    'the first point is kept, left out with its reason');
select lives_ok($$select pg_temp.add_point('00000000-0000-0000-0000-00000000cb01', date '2026-10-31', 1.09, 0.0092, true, '')$$,
    'a used point with its return is kept');
select lives_ok($$select pg_temp.add_point('00000000-0000-0000-0000-00000000cb01', date '2026-11-02', null, null, false, 'the value is not a number')$$,
    'a point whose value cannot be read is kept, left out with its reason');
select throws_like($$select pg_temp.add_point('00000000-0000-0000-0000-00000000cb01', date '2026-09-01', 1.08, 0.001, false, '')$$,
    '%violates check constraint%', 'a point left out with no reason is refused');
select throws_like($$select pg_temp.add_point('00000000-0000-0000-0000-00000000cb01', date '2026-09-02', 1.08, 0.001, true, 'why')$$,
    '%violates check constraint%', 'a used point that carries a reason is refused');
select throws_like($$select pg_temp.add_point('00000000-0000-0000-0000-00000000cb01', date '2026-09-03', 1.08, null, true, '')$$,
    '%violates check constraint%', 'a used point with no return is refused');
select throws_like($$select pg_temp.add_point(gen_random_uuid(), date '2026-09-04', 1.08, 0.001, true, '')$$,
    '%No active variance_calibration_input found%', 'a point of an input that does not exist is refused');

select lives_ok($$select pg_temp.add_calibration('USD', 'gjr_garch_qmle', '00000000-0000-0000-0000-00000000cb01', 0.05, 0.96, 'proposed', '')$$,
    'a proposed fit of a stored input is kept');
select lives_ok($$select pg_temp.add_calibration('JPY', 'authored', null, 0.05, 0.96, 'approved', 'the authored default')$$,
    'an approved authored shape that names no input is kept');
select throws_like($$select pg_temp.add_calibration('USD', 'authored', '00000000-0000-0000-0000-00000000cb01', 0.05, 0.96, 'proposed', '')$$,
    '%violates check constraint%', 'an authored shape that names an input is refused');
select throws_like($$select pg_temp.add_calibration('USD', 'gjr_garch_qmle', null, 0.05, 0.96, 'proposed', '')$$,
    '%violates check constraint%', 'a fit that names no input is refused');
select throws_like($$select pg_temp.add_calibration('USD', 'gjr_garch_qmle', '00000000-0000-0000-0000-00000000cb01', null, 0.96, 'proposed', '')$$,
    '%violates check constraint%', 'a proposed calibration with no shape is refused');
select throws_like($$select pg_temp.add_calibration('USD', 'gjr_garch_qmle', '00000000-0000-0000-0000-00000000cb01', 'NaN'::double precision, 0.96, 'proposed', '')$$,
    '%violates check constraint%', 'a NaN alpha is refused');
select throws_like($$select pg_temp.add_calibration('USD', 'gjr_garch_qmle', '00000000-0000-0000-0000-00000000cb01', 0.05, 1.0, 'proposed', '')$$,
    '%violates check constraint%', 'a shape at the stationarity bound is refused unless rejected');
select lives_ok($$select pg_temp.add_calibration('USD', 'gjr_garch_qmle', '00000000-0000-0000-0000-00000000cb01', null, null, 'rejected', 'too few returns to fit')$$,
    'a rejected attempt with no shape is kept with its reason');
select throws_like($$select pg_temp.add_calibration('USD', 'gjr_garch_qmle', '00000000-0000-0000-0000-00000000cb01', null, null, 'rejected', '')$$,
    '%violates check constraint%', 'a rejection with no reason is refused');
select throws_like($$select pg_temp.add_calibration('USD', 'gjr_garch_qmle', gen_random_uuid(), 0.05, 0.96, 'proposed', '')$$,
    '%No active variance_calibration_input found%', 'a fit of an input that does not exist is refused');
select throws_like($$select pg_temp.add_calibration('USD', 'gjr_garch_qmle', '00000000-0000-0000-0000-00000000cb01', 'NaN'::double precision, null, 'rejected', 'the fit diverged')$$,
    '%violates check constraint%', 'a NaN kept in a rejected attempt is refused');
select throws_like($$update ores_synthetic_variance_calibrations_tbl set starts = 3
    where quote_currency_code = 'JPY' and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '%violates check constraint%', 'an authored shape that claims simplex starts is refused');
select throws_like($$update ores_synthetic_variance_calibrations_tbl set starts = 0, starts_agreed = true
    where quote_currency_code = 'USD' and state = 'proposed' and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '%violates check constraint%', 'starts that agree with no start run are refused');
select throws_like($$update ores_synthetic_variance_calibrations_tbl set log_likelihood = null
    where quote_currency_code = 'USD' and state = 'proposed' and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '%violates check constraint%', 'a fit that is not rejected without its likelihood is refused');
select lives_ok($$select pg_temp.add_calibration('USD', 'gjr_garch_qmle', '00000000-0000-0000-0000-00000000cb01', 0.05, 0.96, 'approved', 'reviewed')$$,
    'an approved fit of the pair is kept');
select throws_like($$select pg_temp.add_calibration('USD', 'authored', null, 0.05, 0.96, 'approved', 'a second approval')$$,
    '%duplicate key%', 'a second current approval of the pair is refused, whatever its method');

select is(
    (select variance_window_days from ores_synthetic_calibration_policies_tbl
     where asset_class_code = 'fx' and valid_to = ores_utility_infinity_timestamp_fn()),
    1826,
    'the FX policy reads five years of fixings for a variance calibration');
select throws_like($$update ores_synthetic_calibration_policies_tbl set variance_window_days = 20
    where asset_class_code = 'fx' and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '%violates check constraint%', 'a variance window shorter than thirty days is refused');

select * from finish();

rollback;
