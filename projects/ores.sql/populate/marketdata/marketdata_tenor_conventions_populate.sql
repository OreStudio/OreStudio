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
 * Tenor Conventions Population Script
 *
 * Seeds the three resolution schemes described in doc/knowledge/domain/
 * tenor.org's "Tenor conventions by curve type" section. CREDIT_CDS_IMM's
 * IMM_ROLL algorithm is not implemented yet (see the runtime resolver's
 * documented gap and the corresponding capture); the row is seeded here so
 * the model already accommodates it, with no schema change needed later.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Tenor Conventions ---'

insert into ores_marketdata_tenor_conventions_tbl (
    tenant_id, code, version, description, measured_from, resolution_algorithm,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'RATES_SPOT_FORWARD', 0,
     'Standard convention used by most FX and interest rate curves: regular tenors measured from spot.',
     'SPOT', 'ANCHOR_OFFSET',
     current_user, current_user, 'system.initial_load', 'Initial population of tenor conventions'),
    (ores_utility_system_tenant_id_fn(), 'FX_SWAP_NEAR_LEG', 0,
     'Short-dated FX swap quotation, in terms of the near leg. Tenor set typically runs up to S/N.',
     null, 'ANCHOR_OFFSET',
     current_user, current_user, 'system.initial_load', 'Initial population of tenor conventions'),
    (ores_utility_system_tenant_id_fn(), 'CREDIT_CDS_IMM', 0,
     'Credit/CDS curves: tenors resolve to the first business day following the appropriate IMM roll date, not a fixed offset from an anchor.',
     null, 'IMM_ROLL',
     current_user, current_user, 'system.initial_load', 'Initial population of tenor conventions')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    description = excluded.description,
    measured_from = excluded.measured_from,
    resolution_algorithm = excluded.resolution_algorithm,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'marketdata_tenor_conventions' as entity, count(*) as count
from ores_marketdata_tenor_conventions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
