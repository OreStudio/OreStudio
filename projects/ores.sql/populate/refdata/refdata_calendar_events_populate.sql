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
 * Calendar Events Population Script
 *
 * Seeds the 2025-2027 FOMC meeting window as calendar_event rows on the
 * US.FOMC calendar (story: FOMC-dated OIS short end, Decision D4). The
 * dates are transcribed from the Fed's published meeting calendar
 * (federalreserve.gov/monetarypolicy/fomccalendars.htm): eight
 * regularly scheduled meetings per year, each row dated the *second
 * day* of the Fed's published two-day range -- the statement/decision
 * day, usually a Wednesday (settled 2026-08-10). The August 2025
 * one-day notation vote is not a two-day meeting and is not seeded.
 *
 * The 2026 second days are the same dates the unified-tenor-resolution
 * tests resolve against; the FOMC_MEETING schedule lookup (Decision D2)
 * walks these rows when production consumption arrives (Decision D3).
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE on the
 * natural key (tenant, calendar, date, entry type).
 */

\echo '--- Calendar Events ---'

insert into ores_refdata_calendar_events_tbl (
    id, tenant_id, calendar_code, version, event_date, diary_entry_type,
    name, description, source,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    -- 2025: second day of each published two-day range.
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2025-01-29', 'central_bank_meeting',
     'FOMC Meeting Jan 2025', 'FOMC policy meeting 28-29 Jan 2025; statement on 29 Jan 2025', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2025-03-19', 'central_bank_meeting',
     'FOMC Meeting Mar 2025', 'FOMC policy meeting 18-19 Mar 2025; statement on 19 Mar 2025', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2025-05-07', 'central_bank_meeting',
     'FOMC Meeting May 2025', 'FOMC policy meeting 6-7 May 2025; statement on 7 May 2025', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2025-06-18', 'central_bank_meeting',
     'FOMC Meeting Jun 2025', 'FOMC policy meeting 17-18 Jun 2025; statement on 18 Jun 2025', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2025-07-30', 'central_bank_meeting',
     'FOMC Meeting Jul 2025', 'FOMC policy meeting 29-30 Jul 2025; statement on 30 Jul 2025', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2025-09-17', 'central_bank_meeting',
     'FOMC Meeting Sep 2025', 'FOMC policy meeting 16-17 Sep 2025; statement on 17 Sep 2025', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2025-10-29', 'central_bank_meeting',
     'FOMC Meeting Oct 2025', 'FOMC policy meeting 28-29 Oct 2025; statement on 29 Oct 2025', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2025-12-10', 'central_bank_meeting',
     'FOMC Meeting Dec 2025', 'FOMC policy meeting 9-10 Dec 2025; statement on 10 Dec 2025', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    -- 2026: second day of each published two-day range.
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2026-01-28', 'central_bank_meeting',
     'FOMC Meeting Jan 2026', 'FOMC policy meeting 27-28 Jan 2026; statement on 28 Jan 2026', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2026-03-18', 'central_bank_meeting',
     'FOMC Meeting Mar 2026', 'FOMC policy meeting 17-18 Mar 2026; statement on 18 Mar 2026', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2026-04-29', 'central_bank_meeting',
     'FOMC Meeting Apr 2026', 'FOMC policy meeting 28-29 Apr 2026; statement on 29 Apr 2026', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2026-06-17', 'central_bank_meeting',
     'FOMC Meeting Jun 2026', 'FOMC policy meeting 16-17 Jun 2026; statement on 17 Jun 2026', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2026-07-29', 'central_bank_meeting',
     'FOMC Meeting Jul 2026', 'FOMC policy meeting 28-29 Jul 2026; statement on 29 Jul 2026', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2026-09-16', 'central_bank_meeting',
     'FOMC Meeting Sep 2026', 'FOMC policy meeting 15-16 Sep 2026; statement on 16 Sep 2026', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2026-10-28', 'central_bank_meeting',
     'FOMC Meeting Oct 2026', 'FOMC policy meeting 27-28 Oct 2026; statement on 28 Oct 2026', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2026-12-09', 'central_bank_meeting',
     'FOMC Meeting Dec 2026', 'FOMC policy meeting 8-9 Dec 2026; statement on 9 Dec 2026', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    -- 2027: second day of each published two-day range (Fed release 2025-09-05).
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2027-01-27', 'central_bank_meeting',
     'FOMC Meeting Jan 2027', 'FOMC policy meeting 26-27 Jan 2027; statement on 27 Jan 2027', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2027-03-17', 'central_bank_meeting',
     'FOMC Meeting Mar 2027', 'FOMC policy meeting 16-17 Mar 2027; statement on 17 Mar 2027', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2027-04-28', 'central_bank_meeting',
     'FOMC Meeting Apr 2027', 'FOMC policy meeting 27-28 Apr 2027; statement on 28 Apr 2027', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2027-06-09', 'central_bank_meeting',
     'FOMC Meeting Jun 2027', 'FOMC policy meeting 8-9 Jun 2027; statement on 9 Jun 2027', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2027-07-28', 'central_bank_meeting',
     'FOMC Meeting Jul 2027', 'FOMC policy meeting 27-28 Jul 2027; statement on 28 Jul 2027', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2027-09-15', 'central_bank_meeting',
     'FOMC Meeting Sep 2027', 'FOMC policy meeting 14-15 Sep 2027; statement on 15 Sep 2027', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2027-10-27', 'central_bank_meeting',
     'FOMC Meeting Oct 2027', 'FOMC policy meeting 26-27 Oct 2027; statement on 27 Oct 2027', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events'),
    (gen_random_uuid(), ores_utility_system_tenant_id_fn(), 'US.FOMC', 0, '2027-12-08', 'central_bank_meeting',
     'FOMC Meeting Dec 2027', 'FOMC policy meeting 7-8 Dec 2027; statement on 8 Dec 2027', 'federalreserve.gov',
     current_user, current_user, 'system.initial_load', 'Initial population of calendar events')
on conflict (tenant_id, calendar_code, event_date, diary_entry_type)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    name = excluded.name,
    description = excluded.description,
    source = excluded.source,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary: per-year counts must match the Fed's published schedule (8
-- regularly scheduled meetings per year).
select 'calendar_events 2025' as entity, count(*) as count
from ores_refdata_calendar_events_tbl
where calendar_code = 'US.FOMC'
  and diary_entry_type = 'central_bank_meeting'
  and extract(year from event_date) = 2025
  and valid_to = ores_utility_infinity_timestamp_fn();

select 'calendar_events 2026' as entity, count(*) as count
from ores_refdata_calendar_events_tbl
where calendar_code = 'US.FOMC'
  and diary_entry_type = 'central_bank_meeting'
  and extract(year from event_date) = 2026
  and valid_to = ores_utility_infinity_timestamp_fn();

select 'calendar_events 2027' as entity, count(*) as count
from ores_refdata_calendar_events_tbl
where calendar_code = 'US.FOMC'
  and diary_entry_type = 'central_bank_meeting'
  and extract(year from event_date) = 2027
  and valid_to = ores_utility_infinity_timestamp_fn();
