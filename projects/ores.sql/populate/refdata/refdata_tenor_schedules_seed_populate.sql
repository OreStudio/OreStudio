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
 * Tenor Schedules Seed Population Script
 *
 * Registers the refdata.tenor_schedules dataset and seeds its DQ artefact
 * table from the system tenant's own tenor schedules, which
 * refdata_tenor_schedules_populate.sql loads during the foundation step.
 * Copying them keeps one list of schedules.
 *
 * A tenant gets its schedules from this dataset rather than from
 * provisioning, because FOMC_MEETING names the US.FOMC calendar, which a
 * tenant only has once refdata.calendars is published. Publishing this
 * dataset also sets the schedule columns of the tenant's tenor convention
 * resolutions; see ores_refdata_publish_tenor_schedules_from_dq_fn().
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.tenor_schedules',
        'Tenor Schedule Reference Data',
        'Calendars',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Tenor Schedules',
        'The schedule axes tenors resolve along: the IMM roll quarter and the FOMC meeting schedule.',
        'ORESTUDIO',
        'Seed data for the tenor schedules Librarian bundle',
        current_date,
        'Internal Use Only',
        'tenor_schedules'
    );

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.tenor_schedules',
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
      and code = 'refdata.tenor_schedules'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.tenor_schedules';
    end if;

    delete from ores_dq_tenor_schedules_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_tenor_schedules_artefact_tbl (
        dataset_id, tenant_id, code, version, name, description, display_order,
        schedule_source, calendar_code, diary_entry_type
    )
    select
        v_dataset_id, v_tenant_id, t.code, 0, t.name, t.description, t.display_order,
        t.schedule_source, t.calendar_code, t.diary_entry_type
    from ores_refdata_tenor_schedules_tbl t
    where t.tenant_id = v_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % tenor schedules for dataset: refdata.tenor_schedules', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Tenor Schedules Summary ---'

select 'Total DQ Tenor Schedules' as metric, count(*) as count
from ores_dq_tenor_schedules_artefact_tbl;
