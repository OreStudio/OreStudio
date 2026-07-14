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
 * Calendar Types Population Script
 *
 * Populates valid calendar type classifications.
 * Classifies calendar rows by what they represent (public holiday,
 * central bank meeting, financial centre, etc.).
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE so that
 * description text is kept up-to-date in a single atomic statement.
 */

\echo '--- Calendar Types ---'

insert into ores_refdata_calendar_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'public_holiday', 0, 'Public Holiday',
     'A business-day/holiday calendar for a country, currency, or supranational region',
     1, current_user, current_user, 'system.initial_load', 'Initial population of calendar types'),
    (ores_utility_system_tenant_id_fn(), 'central_bank_meeting', 0, 'Central Bank Meeting',
     'A calendar of a central bank''s scheduled policy-meeting dates',
     2, current_user, current_user, 'system.initial_load', 'Initial population of calendar types'),
    (ores_utility_system_tenant_id_fn(), 'financial_centre', 0, 'Financial Centre',
     'A business-day calendar for a specific financial centre or exchange (e.g. NYSE, TSX)',
     3, current_user, current_user, 'system.initial_load', 'Initial population of calendar types'),
    (ores_utility_system_tenant_id_fn(), 'data_release', 0, 'Data Release',
     'A calendar of scheduled macroeconomic data release dates',
     4, current_user, current_user, 'system.initial_load', 'Initial population of calendar types'),
    (ores_utility_system_tenant_id_fn(), 'other', 0, 'Other',
     'A calendar not classified under any other calendar type',
     5, current_user, current_user, 'system.initial_load', 'Initial population of calendar types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    name = excluded.name,
    description = excluded.description,
    display_order = excluded.display_order,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_calendar_types' as entity, count(*) as count
from ores_refdata_calendar_types_tbl;
