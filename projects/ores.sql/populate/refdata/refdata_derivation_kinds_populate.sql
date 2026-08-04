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
 * Derivation Kinds Population Script
 *
 * Populates the valid market_series.derivation_kind values: the OBSERVED
 * sentinel (a directly published tick, the default case) plus a named
 * value per derivation mechanism (IR_CURVE_BOOTSTRAP, CRM_DERIVATION).
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Derivation Kinds ---'

insert into ores_refdata_derivation_kinds_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'OBSERVED', 0, 'Observed',
     'Sentinel for a directly published series -- no derivation involved. The default for every existing producer (synthetic, vendor feeds).',
     1, current_user, current_user, 'system.initial_load', 'Initial population of derivation kinds'),
    (ores_utility_system_tenant_id_fn(), 'IR_CURVE_BOOTSTRAP', 0, 'IR Curve Bootstrap',
     'Series produced by bootstrapping a raw instrument grid into a discount/zero curve.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of derivation kinds'),
    (ores_utility_system_tenant_id_fn(), 'CRM_DERIVATION', 0, 'CRM Derivation',
     'Series produced by the cross-rates matrix triangulating a derived rate from driver rates.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of derivation kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_derivation_kinds' as entity, count(*) as count
from ores_refdata_derivation_kinds_tbl;
