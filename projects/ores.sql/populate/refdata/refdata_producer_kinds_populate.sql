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
 * Producer Kinds Population Script
 *
 * Populates the valid feed_binding.producer_kind and
 * market_series.producer_kind values: VENDOR for a real feed, SYNTHETIC
 * for a generated one. The two values are the whole vocabulary; a new
 * kind is a new row here, not a code change.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Producer Kinds ---'

insert into ores_refdata_producer_kinds_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'VENDOR', 0, 'Vendor',
     'A real feed: a vendor or other external producer whose data was observed rather than generated. The default for every existing producer.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of producer kinds'),
    (ores_utility_system_tenant_id_fn(), 'SYNTHETIC', 0, 'Synthetic',
     'A generated producer: a feed whose data was produced by the system rather than observed from a real market.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of producer kinds')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_producer_kinds' as entity, count(*) as count
from ores_refdata_producer_kinds_tbl;
