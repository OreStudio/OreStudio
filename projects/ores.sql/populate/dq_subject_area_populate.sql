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
 * Data Quality Subject Area Population Script
 *
 * Seeds the database with subject area values for data quality classification.
 * This script is idempotent.
 *
 * Subject Areas:
 * - Currencies: Currency reference data
 * - Countries: Country reference data
 * - Country Flags: Flag image data associated with countries
 * - IP Address to Country maps: IP geolocation mapping data
 * - Cryptocurrencies: Cryptocurrency reference data including icons
 */

set schema 'ores';

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- Helper function to insert a data quality subject area if it doesn't exist
create or replace function ores.upsert_dq_subject_area(
    p_domain_name text,
    p_name text,
    p_description text
) returns void as $$
begin
    if not exists (
        select 1 from ores.dq_subject_area_tbl
        where name = p_name and domain_name = p_domain_name and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_subject_area_tbl (
            name, version, domain_name, description,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_name, 0, p_domain_name, p_description,
            'system', 'system.new_record', 'System seed data - data quality subject area',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality subject area: % in domain %', p_name, p_domain_name;
    else
        raise notice 'Data quality subject area already exists: % in domain %', p_name, p_domain_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality Subject Areas
-- =============================================================================

\echo '--- Data Quality Subject Areas ---'

select ores.upsert_dq_subject_area(
    'Reference Data',
    'Currencies',
    'Currency reference data.'
);

select ores.upsert_dq_subject_area(
    'Reference Data',
    'Countries',
    'Country reference data.'
);

select ores.upsert_dq_subject_area(
    'Reference Data',
    'Country Flags',
    'Flag image data associated with countries.'
);

select ores.upsert_dq_subject_area(
    'Reference Data',
    'IP Address to Country maps',
    'IP geolocation mapping data.'
);

select ores.upsert_dq_subject_area(
    'Reference Data',
    'Cryptocurrencies',
    'Cryptocurrency reference data including icons and metadata.'
);

select ores.upsert_dq_subject_area(
    'Reference Data',
    'Parties',
    'Party identification schemes and reference data for legal entities and financial institutions.'
);

select ores.upsert_dq_subject_area(
    'Reference Data',
    'General',
    'Cross-cutting reference data not specific to a particular domain.'
);

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_subject_area(text, text, text);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Subject Areas' as entity, count(*) as count
from ores.dq_subject_area_tbl where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;