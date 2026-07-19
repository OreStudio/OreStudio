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
 * Calendars Seed Population Script
 *
 * Registers the refdata.calendars dataset and seeds its DQ artefact table
 * with the same QuantLib calendar set already loaded straight into
 * ores_refdata_calendars_tbl by refdata_calendars_populate.sql (kept in
 * lockstep by hand -- the two exist for different purposes: that one is
 * the system tenant's own live rows, this one is the Librarian-publishable
 * dataset a party bundle-Applies to get its own copy).
 *
 * Execution order: depends on refdata.calendar_types being registered
 * first (soft dependency only -- calendar_type/country_code are free-text
 * columns here, not FKs; the target table's own insert trigger validates
 * them once published).
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.calendars',
        'Calendar Reference Data',
        'Calendars',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Calendars',
        'ORE/QuantLib calendar reference data: business-day/holiday calendars, financial-centre calendars, and central-bank meeting calendars.',
        'ORESTUDIO',
        'Seed data for the calendars Librarian bundle',
        current_date,
        'Internal Use Only',
        'calendars'
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
      and code = 'refdata.calendars'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.calendars';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_calendars_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_calendars_artefact_tbl (
        dataset_id, tenant_id, code, version, name, calendar_type, country_code
    )
    values
        -- Supranational
        (v_dataset_id, v_tenant_id, 'TARGET', 0, 'TARGET (Euro area)', 'public_holiday', 'ZZ'),
        (v_dataset_id, v_tenant_id, 'WeekendsOnly', 0, 'Weekends Only', 'public_holiday', 'ZZ'),
        -- Single-market national calendars
        (v_dataset_id, v_tenant_id, 'Argentina', 0, 'Argentina', 'public_holiday', 'AR'),
        (v_dataset_id, v_tenant_id, 'Australia', 0, 'Australia', 'public_holiday', 'AU'),
        (v_dataset_id, v_tenant_id, 'Austria', 0, 'Austria', 'public_holiday', 'AT'),
        (v_dataset_id, v_tenant_id, 'Botswana', 0, 'Botswana', 'public_holiday', 'BW'),
        (v_dataset_id, v_tenant_id, 'Brazil', 0, 'Brazil', 'public_holiday', 'BR'),
        (v_dataset_id, v_tenant_id, 'Chile', 0, 'Chile', 'public_holiday', 'CL'),
        (v_dataset_id, v_tenant_id, 'CzechRepublic', 0, 'Czech Republic', 'public_holiday', 'CZ'),
        (v_dataset_id, v_tenant_id, 'Denmark', 0, 'Denmark', 'public_holiday', 'DK'),
        (v_dataset_id, v_tenant_id, 'Finland', 0, 'Finland', 'public_holiday', 'FI'),
        (v_dataset_id, v_tenant_id, 'France', 0, 'France', 'public_holiday', 'FR'),
        (v_dataset_id, v_tenant_id, 'HongKong', 0, 'Hong Kong', 'public_holiday', 'HK'),
        (v_dataset_id, v_tenant_id, 'Hungary', 0, 'Hungary', 'public_holiday', 'HU'),
        (v_dataset_id, v_tenant_id, 'Iceland', 0, 'Iceland', 'public_holiday', 'IS'),
        (v_dataset_id, v_tenant_id, 'India', 0, 'India (National Stock Exchange)', 'financial_centre', 'IN'),
        (v_dataset_id, v_tenant_id, 'Italy', 0, 'Italy', 'public_holiday', 'IT'),
        (v_dataset_id, v_tenant_id, 'Japan', 0, 'Japan', 'public_holiday', 'JP'),
        (v_dataset_id, v_tenant_id, 'Mexico', 0, 'Mexico', 'public_holiday', 'MX'),
        (v_dataset_id, v_tenant_id, 'NewZealand', 0, 'New Zealand', 'public_holiday', 'NZ'),
        (v_dataset_id, v_tenant_id, 'Norway', 0, 'Norway', 'public_holiday', 'NO'),
        (v_dataset_id, v_tenant_id, 'Poland', 0, 'Poland', 'public_holiday', 'PL'),
        (v_dataset_id, v_tenant_id, 'Romania', 0, 'Romania', 'public_holiday', 'RO'),
        (v_dataset_id, v_tenant_id, 'Russia', 0, 'Russia', 'public_holiday', 'RU'),
        (v_dataset_id, v_tenant_id, 'SaudiArabia', 0, 'Saudi Arabia', 'public_holiday', 'SA'),
        (v_dataset_id, v_tenant_id, 'Singapore', 0, 'Singapore', 'public_holiday', 'SG'),
        (v_dataset_id, v_tenant_id, 'Slovakia', 0, 'Slovakia', 'public_holiday', 'SK'),
        (v_dataset_id, v_tenant_id, 'SouthAfrica', 0, 'South Africa', 'public_holiday', 'ZA'),
        (v_dataset_id, v_tenant_id, 'Sweden', 0, 'Sweden', 'public_holiday', 'SE'),
        (v_dataset_id, v_tenant_id, 'Switzerland', 0, 'Switzerland', 'public_holiday', 'CH'),
        (v_dataset_id, v_tenant_id, 'Taiwan', 0, 'Taiwan', 'public_holiday', 'TW'),
        (v_dataset_id, v_tenant_id, 'Thailand', 0, 'Thailand', 'public_holiday', 'TH'),
        (v_dataset_id, v_tenant_id, 'Turkey', 0, 'Turkey', 'public_holiday', 'TR'),
        (v_dataset_id, v_tenant_id, 'Ukraine', 0, 'Ukraine', 'public_holiday', 'UA'),
        -- United States (Market enum)
        (v_dataset_id, v_tenant_id, 'UnitedStates.Settlement', 0, 'United States (Settlement)', 'public_holiday', 'US'),
        (v_dataset_id, v_tenant_id, 'UnitedStates.NYSE', 0, 'United States (NYSE)', 'financial_centre', 'US'),
        (v_dataset_id, v_tenant_id, 'UnitedStates.GovernmentBond', 0, 'United States (Government Bond)', 'financial_centre', 'US'),
        (v_dataset_id, v_tenant_id, 'UnitedStates.NERC', 0, 'United States (NERC)', 'financial_centre', 'US'),
        (v_dataset_id, v_tenant_id, 'UnitedStates.LiborImpact', 0, 'United States (Libor Impact)', 'financial_centre', 'US'),
        (v_dataset_id, v_tenant_id, 'UnitedStates.FederalReserve', 0, 'United States (Federal Reserve)', 'financial_centre', 'US'),
        (v_dataset_id, v_tenant_id, 'UnitedStates.SOFR', 0, 'United States (SOFR)', 'financial_centre', 'US'),
        -- United Kingdom (Market enum)
        (v_dataset_id, v_tenant_id, 'UnitedKingdom.Settlement', 0, 'United Kingdom (Settlement)', 'public_holiday', 'GB'),
        (v_dataset_id, v_tenant_id, 'UnitedKingdom.Exchange', 0, 'United Kingdom (Exchange)', 'financial_centre', 'GB'),
        (v_dataset_id, v_tenant_id, 'UnitedKingdom.Metals', 0, 'United Kingdom (Metals)', 'financial_centre', 'GB'),
        -- China (Market enum)
        (v_dataset_id, v_tenant_id, 'China.SSE', 0, 'China (Shanghai Stock Exchange)', 'financial_centre', 'CN'),
        (v_dataset_id, v_tenant_id, 'China.IB', 0, 'China (Interbank)', 'financial_centre', 'CN'),
        -- Germany (Market enum)
        (v_dataset_id, v_tenant_id, 'Germany.Settlement', 0, 'Germany (Settlement)', 'public_holiday', 'DE'),
        (v_dataset_id, v_tenant_id, 'Germany.FrankfurtStockExchange', 0, 'Germany (Frankfurt Stock Exchange)', 'financial_centre', 'DE'),
        (v_dataset_id, v_tenant_id, 'Germany.Xetra', 0, 'Germany (Xetra)', 'financial_centre', 'DE'),
        (v_dataset_id, v_tenant_id, 'Germany.Eurex', 0, 'Germany (Eurex)', 'financial_centre', 'DE'),
        (v_dataset_id, v_tenant_id, 'Germany.Euwax', 0, 'Germany (Euwax)', 'financial_centre', 'DE'),
        -- Canada (Market enum)
        (v_dataset_id, v_tenant_id, 'Canada.Settlement', 0, 'Canada (Settlement)', 'public_holiday', 'CA'),
        (v_dataset_id, v_tenant_id, 'Canada.TSX', 0, 'Canada (Toronto Stock Exchange)', 'financial_centre', 'CA'),
        -- South Korea (Market enum)
        (v_dataset_id, v_tenant_id, 'SouthKorea.Settlement', 0, 'South Korea (Settlement)', 'public_holiday', 'KR'),
        (v_dataset_id, v_tenant_id, 'SouthKorea.KRX', 0, 'South Korea (Korea Exchange)', 'financial_centre', 'KR'),
        -- Indonesia (Market enum)
        (v_dataset_id, v_tenant_id, 'Indonesia.BEJ', 0, 'Indonesia (Jakarta SE, legacy BEJ)', 'financial_centre', 'ID'),
        (v_dataset_id, v_tenant_id, 'Indonesia.JSX', 0, 'Indonesia (Jakarta SE, legacy JSX)', 'financial_centre', 'ID'),
        (v_dataset_id, v_tenant_id, 'Indonesia.IDX', 0, 'Indonesia (Indonesia Stock Exchange)', 'financial_centre', 'ID'),
        -- Israel (Market enum)
        (v_dataset_id, v_tenant_id, 'Israel.Settlement', 0, 'Israel (Settlement)', 'public_holiday', 'IL'),
        (v_dataset_id, v_tenant_id, 'Israel.TASE', 0, 'Israel (Tel-Aviv Stock Exchange)', 'financial_centre', 'IL');

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % calendars for dataset: refdata.calendars', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Calendars Summary ---'

select 'Total DQ Calendars' as metric, count(*) as count
from ores_dq_calendars_artefact_tbl;
