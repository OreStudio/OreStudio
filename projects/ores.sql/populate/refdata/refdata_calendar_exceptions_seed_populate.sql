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
 * Calendar Exceptions Seed Population Script
 *
 * Registers the refdata.calendar_exceptions dataset and seeds its DQ artefact
 * table from the system tenant's own exceptions, which
 * refdata_calendar_exceptions_populate.sql generates from QuantLib.
 *
 * An exception's id is not carried to a tenant. The publish matches
 * exceptions on (calendar_code, exception_date).
 *
 * This script is idempotent.
 */

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.calendar_exceptions',
        'Calendar Exception Reference Data',
        'Calendars',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Calendar Exceptions',
        'Days on which a calendar differs from a Monday to Friday week, generated from QuantLib.',
        'QUANTLIB',
        'Seed data for the calendar exceptions Librarian bundle',
        current_date,
        'Internal Use Only',
        'calendar_exceptions'
    );

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.calendar_exceptions',
        'refdata.calendars',
        'calendar_reference'
    );
END $$;

DO $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.calendar_exceptions'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.calendar_exceptions';
    end if;

    delete from ores_dq_calendar_exceptions_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_calendar_exceptions_artefact_tbl (
        dataset_id, tenant_id, id, version, calendar_code, exception_date,
        is_business_day, description
    )
    select
        v_dataset_id, v_tenant_id, e.id, 0, e.calendar_code, e.exception_date,
        e.is_business_day, e.description
    from ores_refdata_calendar_exceptions_tbl e
    where e.tenant_id = v_tenant_id
      and e.valid_to = ores_utility_infinity_timestamp_fn();
end $$;

\echo ''
\echo '--- DQ Calendar Exceptions Summary ---'

select 'Total DQ Calendar Exceptions' as metric, count(*) as count
from ores_dq_calendar_exceptions_artefact_tbl;
