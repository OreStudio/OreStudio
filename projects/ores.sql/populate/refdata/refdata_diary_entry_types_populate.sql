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
 * Diary Entry Types Population Script
 *
 * Populates the open-ended vocabulary classifying calendar_event
 * entries (story: FOMC-dated OIS short end, Decision D1). The set is
 * open-ended: consumers key off specific codes (e.g.
 * central_bank_meeting in the FOMC_MEETING event-lookup schedule), and
 * new codes can be added without schema change.
 *
 * holiday is deliberately present even though its physical home is
 * the calendar_rules/calendar_exceptions/calendar_date machinery -- the
 * whole classification lives in one vocabulary.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE so that
 * description text is kept up-to-date in a single atomic statement.
 */

\echo '--- Diary Entry Types ---'

insert into ores_refdata_diary_entry_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'holiday', 0, 'Holiday',
     'A non-business day; physically modelled by calendar_rules/calendar_exceptions, kept here for classification completeness',
     1, current_user, current_user, 'system.initial_load', 'Initial population of diary entry types'),
    (ores_utility_system_tenant_id_fn(), 'central_bank_meeting', 0, 'Central Bank Meeting',
     'A scheduled policy-meeting date of a central bank (e.g. an FOMC meeting)',
     2, current_user, current_user, 'system.initial_load', 'Initial population of diary entry types'),
    (ores_utility_system_tenant_id_fn(), 'data_release', 0, 'Data Release',
     'A scheduled macroeconomic data release date',
     3, current_user, current_user, 'system.initial_load', 'Initial population of diary entry types'),
    (ores_utility_system_tenant_id_fn(), 'other', 0, 'Other',
     'An open-ended diary entry not classified under any other type',
     4, current_user, current_user, 'system.initial_load', 'Initial population of diary entry types')
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
select 'refdata_diary_entry_types' as entity, count(*) as count
from ores_refdata_diary_entry_types_tbl;
