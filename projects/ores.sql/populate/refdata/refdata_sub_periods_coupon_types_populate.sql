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
 * Sub-Periods Coupon Types Population Script
 *
 * Populates how a floating leg's sub-period coupons accrue --
 * compounded or averaged -- mutually exclusive with each other.
 *
 * The two values are the ones the swap and tenor basis swap
 * conventions' own sub_periods_coupon_type column descriptions name.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Sub-Periods Coupon Types ---'

insert into ores_refdata_sub_periods_coupon_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'Compounding', 0, 'Compounding',
     'Each sub-period rate compounds over the accrual, so the coupon is the product of the sub-period growth factors.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of sub-periods coupon types'),
    (ores_utility_system_tenant_id_fn(), 'Averaging', 0, 'Averaging',
     'The sub-period rates are averaged over the accrual, so the coupon is their arithmetic mean.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of sub-periods coupon types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_sub_periods_coupon_types' as entity, count(*) as count
from ores_refdata_sub_periods_coupon_types_tbl;
