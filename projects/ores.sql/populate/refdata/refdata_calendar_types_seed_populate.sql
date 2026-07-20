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
 * Calendar Types Seed Population Script
 *
 * Registers the refdata.calendar_types dataset and seeds its DQ artefact
 * table with the same classification set already loaded straight into
 * ores_refdata_calendar_types_tbl by refdata_calendar_types_populate.sql
 * (kept in lockstep by hand -- the two exist for different purposes: that
 * one is the system tenant's own live rows, this one is the Librarian-
 * publishable dataset a party bundle-Applies to get its own copy).
 *
 * Execution order: no dependency on other populate scripts.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Catalog Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_catalogs_upsert_fn(ores_utility_system_tenant_id_fn(),
        'Calendar Reference Data',
        'ORE/QuantLib calendar reference data: calendar type classifications and business-day/holiday calendars.',
        'OreStudio Development Team'
    );
END $$;

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.calendar_types',
        'Calendar Reference Data',
        'Calendars',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Calendar Types',
        'Calendar type classification (public holiday, central bank meeting, financial centre, data release, other).',
        'ORESTUDIO',
        'Seed data for the calendar types Librarian bundle',
        current_date,
        'Internal Use Only',
        'calendar_types'
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
      and code = 'refdata.calendar_types'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.calendar_types';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_calendar_types_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_calendar_types_artefact_tbl (
        dataset_id, tenant_id, code, version, name, description, display_order
    )
    values
        (v_dataset_id, v_tenant_id, 'public_holiday', 0, 'Public Holiday',
         'A business-day/holiday calendar for a country, currency, or supranational region', 1),
        (v_dataset_id, v_tenant_id, 'central_bank_meeting', 0, 'Central Bank Meeting',
         'A calendar of a central bank''s scheduled policy-meeting dates', 2),
        (v_dataset_id, v_tenant_id, 'financial_centre', 0, 'Financial Centre',
         'A business-day calendar for a specific financial centre or exchange (e.g. NYSE, TSX)', 3),
        (v_dataset_id, v_tenant_id, 'data_release', 0, 'Data Release',
         'A calendar of scheduled macroeconomic data release dates', 4),
        (v_dataset_id, v_tenant_id, 'other', 0, 'Other',
         'A calendar not classified under any other calendar type', 5);

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % calendar types for dataset: refdata.calendar_types', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Calendar Types Summary ---'

select 'Total DQ Calendar Types' as metric, count(*) as count
from ores_dq_calendar_types_artefact_tbl;
