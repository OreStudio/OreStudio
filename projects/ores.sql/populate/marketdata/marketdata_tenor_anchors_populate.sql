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
 * Tenor Anchors Population Script
 *
 * Seeds the reference points a tenor convention resolves tenors from. See
 * doc/knowledge/domain/tenor.org and doc/knowledge/domain/out_of_convention.org.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Tenor Anchors ---'

insert into ores_marketdata_tenor_anchors_tbl (
    tenant_id, code, version, description,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'SPOT', 0,
     'Horizon date plus spot days (currency-pair specific).',
     current_user, current_user, 'system.initial_load', 'Initial population of tenor anchors'),
    (ores_utility_system_tenant_id_fn(), 'TODAY', 0,
     'The horizon date itself.',
     current_user, current_user, 'system.initial_load', 'Initial population of tenor anchors'),
    (ores_utility_system_tenant_id_fn(), 'TOMORROW', 0,
     'Horizon date plus one calendar day.',
     current_user, current_user, 'system.initial_load', 'Initial population of tenor anchors'),
    (ores_utility_system_tenant_id_fn(), 'NEAR_LEG', 0,
     'The near-leg date of a short-dated FX swap; not a fixed offset from the horizon date.',
     current_user, current_user, 'system.initial_load', 'Initial population of tenor anchors'),
    (ores_utility_system_tenant_id_fn(), 'IMM_ROLL', 0,
     'The nearest IMM quarterly roll date; used by the IMM_ROLL resolution algorithm, not a fixed-offset anchor.',
     current_user, current_user, 'system.initial_load', 'Initial population of tenor anchors')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    description = excluded.description,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'marketdata_tenor_anchors' as entity, count(*) as count
from ores_marketdata_tenor_anchors_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
