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
 * Calendar Events Seed Population Script
 *
 * Registers the refdata.calendar_events dataset and seeds its DQ artefact
 * table with the 2025-2027 FOMC meeting window: the second day of each
 * published two-day range, transcribed from the Fed's meeting calendar
 * (federalreserve.gov/monetarypolicy/fomccalendars.htm). The dataset is the
 * one writer of ores_refdata_calendar_events_tbl: the bring-up publishes it
 * into the system tenant, and no populate script inserts the rows.
 *
 * An event's id is not carried to a tenant; the publish matches events on
 * (calendar_code, event_date, diary_entry_type). See
 * ores_refdata_publish_calendar_events_from_dq_fn().
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.calendar_events',
        'Calendar Event Reference Data',
        'Calendars',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Calendar Events',
        'Dated diary entries on calendars, such as the FOMC policy meeting dates.',
        'ORESTUDIO',
        'Seed data for the calendar events Librarian bundle',
        current_date,
        'Internal Use Only',
        'calendar_events'
    );

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.calendar_events',
        'refdata.calendars',
        'calendar_reference'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

DO $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_count integer := 0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.calendar_events'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.calendar_events';
    end if;

    delete from ores_dq_calendar_events_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_calendar_events_artefact_tbl (
        dataset_id, tenant_id, id, version, calendar_code, event_date,
        diary_entry_type, name, description, source
    )
    values
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2025-01-29',
         'central_bank_meeting', 'FOMC Meeting Jan 2025',
         'FOMC policy meeting 28-29 Jan 2025; statement on 29 Jan 2025', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2025-03-19',
         'central_bank_meeting', 'FOMC Meeting Mar 2025',
         'FOMC policy meeting 18-19 Mar 2025; statement on 19 Mar 2025', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2025-05-07',
         'central_bank_meeting', 'FOMC Meeting May 2025',
         'FOMC policy meeting 6-7 May 2025; statement on 7 May 2025', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2025-06-18',
         'central_bank_meeting', 'FOMC Meeting Jun 2025',
         'FOMC policy meeting 17-18 Jun 2025; statement on 18 Jun 2025', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2025-07-30',
         'central_bank_meeting', 'FOMC Meeting Jul 2025',
         'FOMC policy meeting 29-30 Jul 2025; statement on 30 Jul 2025', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2025-09-17',
         'central_bank_meeting', 'FOMC Meeting Sep 2025',
         'FOMC policy meeting 16-17 Sep 2025; statement on 17 Sep 2025', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2025-10-29',
         'central_bank_meeting', 'FOMC Meeting Oct 2025',
         'FOMC policy meeting 28-29 Oct 2025; statement on 29 Oct 2025', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2025-12-10',
         'central_bank_meeting', 'FOMC Meeting Dec 2025',
         'FOMC policy meeting 9-10 Dec 2025; statement on 10 Dec 2025', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2026-01-28',
         'central_bank_meeting', 'FOMC Meeting Jan 2026',
         'FOMC policy meeting 27-28 Jan 2026; statement on 28 Jan 2026', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2026-03-18',
         'central_bank_meeting', 'FOMC Meeting Mar 2026',
         'FOMC policy meeting 17-18 Mar 2026; statement on 18 Mar 2026', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2026-04-29',
         'central_bank_meeting', 'FOMC Meeting Apr 2026',
         'FOMC policy meeting 28-29 Apr 2026; statement on 29 Apr 2026', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2026-06-17',
         'central_bank_meeting', 'FOMC Meeting Jun 2026',
         'FOMC policy meeting 16-17 Jun 2026; statement on 17 Jun 2026', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2026-07-29',
         'central_bank_meeting', 'FOMC Meeting Jul 2026',
         'FOMC policy meeting 28-29 Jul 2026; statement on 29 Jul 2026', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2026-09-16',
         'central_bank_meeting', 'FOMC Meeting Sep 2026',
         'FOMC policy meeting 15-16 Sep 2026; statement on 16 Sep 2026', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2026-10-28',
         'central_bank_meeting', 'FOMC Meeting Oct 2026',
         'FOMC policy meeting 27-28 Oct 2026; statement on 28 Oct 2026', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2026-12-09',
         'central_bank_meeting', 'FOMC Meeting Dec 2026',
         'FOMC policy meeting 8-9 Dec 2026; statement on 9 Dec 2026', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2027-01-27',
         'central_bank_meeting', 'FOMC Meeting Jan 2027',
         'FOMC policy meeting 26-27 Jan 2027; statement on 27 Jan 2027', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2027-03-17',
         'central_bank_meeting', 'FOMC Meeting Mar 2027',
         'FOMC policy meeting 16-17 Mar 2027; statement on 17 Mar 2027', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2027-04-28',
         'central_bank_meeting', 'FOMC Meeting Apr 2027',
         'FOMC policy meeting 27-28 Apr 2027; statement on 28 Apr 2027', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2027-06-09',
         'central_bank_meeting', 'FOMC Meeting Jun 2027',
         'FOMC policy meeting 8-9 Jun 2027; statement on 9 Jun 2027', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2027-07-28',
         'central_bank_meeting', 'FOMC Meeting Jul 2027',
         'FOMC policy meeting 27-28 Jul 2027; statement on 28 Jul 2027', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2027-09-15',
         'central_bank_meeting', 'FOMC Meeting Sep 2027',
         'FOMC policy meeting 14-15 Sep 2027; statement on 15 Sep 2027', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2027-10-27',
         'central_bank_meeting', 'FOMC Meeting Oct 2027',
         'FOMC policy meeting 26-27 Oct 2027; statement on 27 Oct 2027', 'federalreserve.gov'),
        (v_dataset_id, v_tenant_id, gen_random_uuid(), 0, 'US.FOMC', '2027-12-08',
         'central_bank_meeting', 'FOMC Meeting Dec 2027',
         'FOMC policy meeting 7-8 Dec 2027; statement on 8 Dec 2027', 'federalreserve.gov');

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % calendar events for dataset: refdata.calendar_events', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Calendar Events Summary ---'

select 'Total DQ Calendar Events' as metric, count(*) as count
from ores_dq_calendar_events_artefact_tbl;
