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
 * Payment Frequencies Population Script
 *
 * Populates ORE's canonical frequencyType enumeration
 * (external/ore/xsd/ore_types.xsd) -- the long-form codes only (ORE also
 * accepts single-letter aliases Z/A/S/Q/B/M/L/W/D, not modelled here).
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Payment Frequencies ---'

insert into ores_refdata_payment_frequencies_tbl (
    tenant_id, code, version, name, description, period_unit, period_multiplier, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'Once', 0, 'Once',
     'A single payment at termination; no periodic schedule.',
     'NONE', null, 1, current_user, current_user, 'system.initial_load', 'Initial population of payment frequencies'),
    (ores_utility_system_tenant_id_fn(), 'Annual', 0, 'Annual',
     'Once per year.',
     'YEAR', 1, 2, current_user, current_user, 'system.initial_load', 'Initial population of payment frequencies'),
    (ores_utility_system_tenant_id_fn(), 'Semiannual', 0, 'Semiannual',
     'Twice per year.',
     'MONTH', 6, 3, current_user, current_user, 'system.initial_load', 'Initial population of payment frequencies'),
    (ores_utility_system_tenant_id_fn(), 'Quarterly', 0, 'Quarterly',
     'Four times per year.',
     'MONTH', 3, 4, current_user, current_user, 'system.initial_load', 'Initial population of payment frequencies'),
    (ores_utility_system_tenant_id_fn(), 'Bimonthly', 0, 'Bimonthly',
     'Every two months.',
     'MONTH', 2, 5, current_user, current_user, 'system.initial_load', 'Initial population of payment frequencies'),
    (ores_utility_system_tenant_id_fn(), 'Monthly', 0, 'Monthly',
     'Twelve times per year.',
     'MONTH', 1, 6, current_user, current_user, 'system.initial_load', 'Initial population of payment frequencies'),
    (ores_utility_system_tenant_id_fn(), 'Lunarmonth', 0, 'Lunar Month',
     'Every 28 days.',
     'DAY', 28, 7, current_user, current_user, 'system.initial_load', 'Initial population of payment frequencies'),
    (ores_utility_system_tenant_id_fn(), 'Weekly', 0, 'Weekly',
     'Once per week.',
     'WEEK', 1, 8, current_user, current_user, 'system.initial_load', 'Initial population of payment frequencies'),
    (ores_utility_system_tenant_id_fn(), 'Daily', 0, 'Daily',
     'Once per business day.',
     'DAY', 1, 9, current_user, current_user, 'system.initial_load', 'Initial population of payment frequencies')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_payment_frequencies' as entity, count(*) as count
from ores_refdata_payment_frequencies_tbl;
