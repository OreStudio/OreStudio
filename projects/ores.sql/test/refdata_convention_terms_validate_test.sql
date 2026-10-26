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
 * A convention value is checked against the value set of the term it holds, by
 * the trigger the term generator writes on every convention entity.
 */

begin;

select plan(4);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

insert into ores_refdata_parties_tbl (
    id, tenant_id, version, full_name, short_code, party_category, party_type,
    parent_party_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-00000000c0df'::uuid,
    ores_utility_system_tenant_id_fn(), 0, 'pgTAP convention terms party', 'PGTAP-TERMS',
    'Operational', 'Bank', null, 'WRLD', 'Active',
    current_user, current_user, 'system.test', 'pgTAP convention terms fixture'
);

select set_config('app.visible_party_ids', '{a0000000-0000-0000-0000-00000000c0df}', true);

create or replace function pg_temp.swap(p_id text, p_frequency text, p_convention text, p_day_counter text,
                             p_float_frequency text)
returns void language sql as $$
    insert into ores_refdata_swap_conventions_tbl (
        id, tenant_id, version, party_id, fixed_calendar, fixed_frequency, fixed_convention,
        fixed_day_count_fraction, index, float_frequency, sub_periods_coupon_type,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        p_id, ores_utility_system_tenant_id_fn(), 0, 'a0000000-0000-0000-0000-00000000c0df'::uuid,
        'TARGET', p_frequency, p_convention, p_day_counter, 'EUR-EURIBOR-6M', p_float_frequency, null,
        current_user, current_user, 'system.test', 'pgTAP convention terms fixture'
    );
$$;

select lives_ok(
    $$select pg_temp.swap('PGTAP-GOOD-SWAP', 'Annual', 'ModifiedFollowing', '30/360', null)$$,
    'a swap whose terms are in their sets is written');

select throws_like(
    $$select pg_temp.swap('PGTAP-BAD-FREQUENCY', 'Fortnightly', 'ModifiedFollowing', '30/360', null)$$,
    '%Invalid value Fortnightly for Swap.FixedFrequency%',
    'a frequency the ORE parser does not accept is refused, naming the term');

select throws_like(
    $$select pg_temp.swap('PGTAP-BAD-CONVENTION', 'Annual', 'Sideways', '30/360', null)$$,
    '%Invalid value Sideways for Swap.FixedConvention%',
    'a business day convention outside the catalogue is refused');

select throws_like(
    $$select pg_temp.swap('PGTAP-BAD-DAY-COUNTER', 'Annual', 'ModifiedFollowing', 'ACT/999', null)$$,
    '%Invalid value ACT/999 for Swap.FixedDayCounter%',
    'a day counter outside the catalogue is refused');

select * from finish();

rollback;
