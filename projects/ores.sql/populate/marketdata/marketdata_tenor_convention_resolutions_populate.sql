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
 * Tenor Convention Resolutions Population Script
 *
 * Seeds which tenors belong to which convention's set, and the per-tenor
 * anchor/offset overrides SPECIAL tenors need (see doc/knowledge/domain/
 * tenor.org's "Tenor conventions by curve type" section).
 *
 * RATES_SPOT_FORWARD gets every standard tenor: O/N, T/N, and S/N are
 * SPECIAL and carry an explicit anchor/offset override; every PERIOD tenor
 * (S/W, SPOT, and the regular nD/nW/nM/nY set) is a membership-only row,
 * since its own unit/multiplier plus the convention's SPOT default anchor
 * is already correct.
 *
 * FX_SWAP_NEAR_LEG only seeds O/N and T/N, matching what doc/knowledge/
 * domain/tenor.org's swap-curve section actually states ("today, not
 * tomorrow" / "tomorrow"); S/N and beyond are not documented for this
 * convention and are deliberately not fabricated here.
 *
 * CREDIT_CDS_IMM has no resolution rows yet: the IMM_ROLL algorithm is not
 * implemented (see the capture), and CDS's roll-quarter-per-tenor mapping
 * is not sourced anywhere in this cluster, so seeding it now would be
 * fabrication rather than population.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Tenor Convention Resolutions ---'

insert into ores_marketdata_tenor_convention_resolutions_tbl (
    tenant_id, convention_code, tenor_code, version, anchor_override, offset_unit, offset_multiplier,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    -- RATES_SPOT_FORWARD: SPECIAL tenors with an explicit override.
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', 'O/N', 0, 'TODAY', 'DAY', 1,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', 'T/N', 0, 'TODAY', 'DAY', 2,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', 'S/N', 0, null, 'DAY', 1,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    -- RATES_SPOT_FORWARD: PERIOD tenors, membership only (own unit/multiplier, convention default anchor).
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', 'S/W', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', 'SPOT', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '1D', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '2D', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '3D', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '1W', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '2W', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '3W', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '4W', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '1M', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '2M', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '3M', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '4M', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '5M', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '6M', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '7M', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '8M', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '9M', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '12M', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '18M', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '2Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '3Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '4Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '5Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '6Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '7Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '8Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '9Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '10Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '12Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '15Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '20Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '25Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', '30Y', 0, null, null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    -- FX_SWAP_NEAR_LEG: only what doc/knowledge/domain/tenor.org states.
    (ores_utility_system_tenant_id_fn(), 'FX_SWAP_NEAR_LEG', 'O/N', 0, 'TODAY', 'DAY', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions'),
    (ores_utility_system_tenant_id_fn(), 'FX_SWAP_NEAR_LEG', 'T/N', 0, 'TODAY', 'DAY', 1,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor convention resolutions')
on conflict (tenant_id, convention_code, tenor_code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    anchor_override = excluded.anchor_override,
    offset_unit = excluded.offset_unit,
    offset_multiplier = excluded.offset_multiplier,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'marketdata_tenor_convention_resolutions' as entity, count(*) as count
from ores_marketdata_tenor_convention_resolutions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
