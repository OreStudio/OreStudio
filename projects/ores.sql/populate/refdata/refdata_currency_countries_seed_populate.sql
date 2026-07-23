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
 * Currency Countries Seed Population Script
 *
 * Registers the refdata.currency_countries dataset and mirrors the
 * already-seeded ores_refdata_currency_countries_tbl (foundation layer,
 * see refdata_currency_countries_populate.sql) into its DQ artefact
 * table, so the currency-to-issuing-country mapping becomes a
 * Librarian-publishable dataset like every other reference-data table
 * in this story -- currency_countries itself is the source of truth
 * here, not derived data, unlike refdata.currency_calendars.
 *
 * Execution order: depends on the foundation layer (which seeds
 * ores_refdata_currency_countries_tbl) having already run.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_countries',
        'Calendar Reference Data',
        'Calendars',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Currency Countries',
        'Currency-to-country issuance assignments (a currency can be issued by more than one country, e.g. EUR).',
        'ORESTUDIO',
        'Seed data for the currency countries Librarian bundle',
        current_date,
        'Internal Use Only',
        'currency_countries'
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
      and code = 'refdata.currency_countries'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.currency_countries';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_currency_countries_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_currency_countries_artefact_tbl (
        dataset_id, tenant_id, currency_iso_code, country_alpha2_code, version
    )
    select v_dataset_id, v_tenant_id, currency_iso_code, country_alpha2_code, 0
    from ores_refdata_currency_countries_tbl
    where tenant_id = v_tenant_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % currency countries for dataset: refdata.currency_countries', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Currency Countries Summary ---'

select 'Total DQ Currency Countries' as metric, count(*) as count
from ores_dq_currency_countries_artefact_tbl;
