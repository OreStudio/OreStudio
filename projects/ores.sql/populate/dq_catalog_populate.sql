/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
 * Data Quality Catalog Population Script
 *
 * Seeds the database with catalog values for grouping related datasets.
 * This script is idempotent.
 *
 * Catalogs:
 * - ISO Standards: International standards for currencies, countries
 * - Cryptocurrency: Digital asset reference data and visual assets
 * - FpML Standards: FpML coding schemes for OTC derivatives
 */

set schema 'ores';

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- Helper function to insert a catalog if it doesn't exist
create or replace function ores.upsert_dq_catalog(
    p_name text,
    p_description text,
    p_owner text default null
) returns void as $$
begin
    if not exists (
        select 1 from ores.dq_catalog_tbl
        where name = p_name and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_catalog_tbl (
            name, version, description, owner,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_name, 0, p_description, p_owner,
            'system', 'system.new_record', 'System seed data - data quality catalog',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality catalog: %', p_name;
    else
        raise notice 'Data quality catalog already exists: %', p_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality Catalogs
-- =============================================================================

\echo '--- Data Quality Catalogs ---'

select ores.upsert_dq_catalog(
    'ISO Standards',
    'International Organization for Standardization (ISO) reference data including ISO 3166 country codes, ISO 4217 currency codes, and associated flag imagery.',
    'Reference Data Team'
);

select ores.upsert_dq_catalog(
    'Cryptocurrency',
    'Digital asset reference data including cryptocurrency symbols, names, and icon imagery from community-maintained repositories.',
    'Digital Assets Team'
);

select ores.upsert_dq_catalog(
    'FpML Standards',
    'Financial products Markup Language (FpML) coding schemes and reference data for OTC derivatives trading. Includes non-ISO currencies, business centers, and other FpML-defined code lists.',
    'Reference Data Team'
);

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_catalog(text, text, text);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Catalogs' as entity, count(*) as count
from ores.dq_catalog_tbl where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;
