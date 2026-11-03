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
 * What one row of an FX volatility surface recipe can show is checked on write.
 *
 * A blueprint's pair is two different currencies; a pillar's strike label is
 * ATM, or a delta from 1 to 99 followed by RR, BF, C or P; a source's weight is
 * positive and finite, which also refuses NaN, because PostgreSQL orders NaN
 * above every number.
 *
 * Run with: pg_prove -d <database> test/refdata_fx_vol_surface_recipe_checks_test.sql
 */

begin;

select plan(16);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

insert into ores_refdata_parties_tbl (
    id, tenant_id, full_name, short_code, party_category, party_type,
    business_center_code, parent_party_id, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    ('00000000-0000-0000-0000-0000000f5a01'::uuid, ores_utility_system_tenant_id_fn(),
     'Surface Recipe Test', 'FVSTST', 'Operational', 'CorporateGroup', 'WRLD', null, 'Active',
     current_user, current_user, 'system.test', 'surface recipe fixture');

select set_config('app.visible_party_ids', '{00000000-0000-0000-0000-0000000f5a01}', true);

create or replace function pg_temp.add_blueprint(p_id uuid, p_base text, p_quote text)
returns void as $$
    insert into ores_refdata_fx_vol_surface_blueprints_tbl (id, tenant_id, version,
        output_series_id, party_id, name, base_currency_code, quote_currency_code,
        fx_option_convention_id, smile_model, expiry_interpolation, output_expiries,
        output_strikes, modified_by, performed_by, change_reason_code, change_commentary)
    values (p_id, ores_utility_system_tenant_id_fn(), 0, gen_random_uuid(),
        '00000000-0000-0000-0000-0000000f5a01'::uuid, 'FVS-' || p_id::text, p_base, p_quote,
        'EUR-USD-FXOPTION', 'ssvi', 'linear_total_variance', '1M,1Y', 'ATM',
        current_user, current_user, 'system.new_record', 'test');
$$ language sql;

create or replace function pg_temp.add_pillar(p_id uuid, p_label text)
returns void as $$
    insert into ores_refdata_fx_vol_surface_pillars_tbl (id, tenant_id, version, party_id,
        blueprint_id, expiry_code, strike_label, modified_by, performed_by,
        change_reason_code, change_commentary)
    values (p_id, ores_utility_system_tenant_id_fn(), 0,
        '00000000-0000-0000-0000-0000000f5a01'::uuid,
        '00000000-0000-0000-0000-0000000f5b01'::uuid, '1M', p_label,
        current_user, current_user, 'system.new_record', 'test');
$$ language sql;

create or replace function pg_temp.add_source(p_weight double precision)
returns void as $$
    insert into ores_refdata_fx_vol_surface_pillar_sources_tbl (id, tenant_id, version,
        party_id, pillar_id, market_series_id, contributor_code, weight, modified_by,
        performed_by, change_reason_code, change_commentary)
    values (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
        '00000000-0000-0000-0000-0000000f5a01'::uuid,
        '00000000-0000-0000-0000-0000000f5c01'::uuid, gen_random_uuid(), 'derived', p_weight,
        current_user, current_user, 'system.new_record', 'test');
$$ language sql;

select lives_ok($$select pg_temp.add_blueprint('00000000-0000-0000-0000-0000000f5b01', 'EUR', 'USD')$$,
    'a blueprint on two different currencies is kept');
select throws_like($$select pg_temp.add_blueprint(gen_random_uuid(), 'EUR', 'EUR')$$,
    '%violates check constraint%', 'a blueprint whose pair is one currency twice is refused');

select lives_ok($$select pg_temp.add_pillar('00000000-0000-0000-0000-0000000f5c01', 'ATM')$$,
    'an ATM pillar is kept');
select lives_ok($$select pg_temp.add_pillar(gen_random_uuid(), '25RR')$$, 'a 25RR pillar is kept');
select lives_ok($$select pg_temp.add_pillar(gen_random_uuid(), '10BF')$$, 'a 10BF pillar is kept');
select lives_ok($$select pg_temp.add_pillar(gen_random_uuid(), '5C')$$, 'a 5C pillar is kept');
select lives_ok($$select pg_temp.add_pillar(gen_random_uuid(), '99P')$$, 'a 99P pillar is kept');
select throws_like($$select pg_temp.add_pillar(gen_random_uuid(), '0C')$$,
    '%violates check constraint%', 'a zero delta names no option and is refused');
select throws_like($$select pg_temp.add_pillar(gen_random_uuid(), '05P')$$,
    '%violates check constraint%', 'a delta with a leading zero is refused');
select throws_like($$select pg_temp.add_pillar(gen_random_uuid(), '25D')$$,
    '%violates check constraint%', 'a strike label of an unknown kind is refused');
select throws_like($$select pg_temp.add_pillar(gen_random_uuid(), '100RR')$$,
    '%violates check constraint%', 'a delta of three digits is refused');

select lives_ok($$select pg_temp.add_source(0.5)$$, 'a positive weight is kept');
select throws_like($$select pg_temp.add_source(0)$$,
    '%violates check constraint%', 'a zero weight is refused');
select throws_like($$select pg_temp.add_source(-1)$$,
    '%violates check constraint%', 'a negative weight is refused');
select throws_like($$select pg_temp.add_source('NaN'::double precision)$$,
    '%violates check constraint%', 'a NaN weight is refused');
select throws_like($$select pg_temp.add_source('Infinity'::double precision)$$,
    '%violates check constraint%', 'an infinite weight is refused');

select * from finish();

rollback;
