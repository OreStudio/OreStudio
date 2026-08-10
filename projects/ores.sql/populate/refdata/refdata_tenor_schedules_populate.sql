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
 * Tenor Schedules Population Script
 *
 * Seeds the named schedule axes a SCHEDULE_STEP tenor walks along (story
 * Decision D2, task unified-tenor-resolution):
 *
 * - ROLL_QUARTER: the closed-form IMM quarterly rule (first business day
 *   after the 20th of March/June/September/December), computed code-side;
 *   no event store involved.
 * - FOMC_MEETING: the central_bank_meeting diary events on US.FOMC
 *   (seeded by task calendar-seeding), walked as data.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Tenor Schedules ---'

insert into ores_refdata_tenor_schedules_tbl (
    tenant_id, code, version, name, description, display_order, schedule_source,
    calendar_code, diary_entry_type,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'ROLL_QUARTER', 0, 'IMM Roll Quarter',
     'Closed-form IMM quarterly rule: the first business day after the 20th of March, June, September, and December (doc/knowledge/domain/imm_dates.org).',
     1, 'CLOSED_FORM', null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenor schedules'),
    (ores_utility_system_tenant_id_fn(), 'FOMC_MEETING', 0, 'FOMC Meeting Schedule',
     'Event-lookup schedule: the central_bank_meeting diary events on US.FOMC, seeded from federalreserve.gov (task calendar-seeding).',
     2, 'EVENT_LOOKUP', 'US.FOMC', 'central_bank_meeting',
     current_user, current_user, 'system.initial_load', 'Initial population of tenor schedules')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    name = excluded.name,
    description = excluded.description,
    display_order = excluded.display_order,
    schedule_source = excluded.schedule_source,
    calendar_code = excluded.calendar_code,
    diary_entry_type = excluded.diary_entry_type,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_tenor_schedules' as entity, count(*) as count
from ores_refdata_tenor_schedules_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
