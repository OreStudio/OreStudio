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
 * table from the system tenant's own calendar events, which
 * refdata_calendar_events_populate.sql loads during the foundation step:
 * today, the FOMC meeting dates on US.FOMC. Copying them keeps one list of
 * meeting dates.
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
    select
        v_dataset_id, v_tenant_id, e.id, 0, e.calendar_code, e.event_date,
        e.diary_entry_type, e.name, e.description, e.source
    from ores_refdata_calendar_events_tbl e
    where e.tenant_id = v_tenant_id
      and e.valid_to = ores_utility_infinity_timestamp_fn();

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
