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
 * Data Quality Nature Dimension Population Script
 *
 * Seeds the database with nature dimension values for data quality classification.
 * This script is idempotent.
 *
 * Natures:
 * - Actual: Real-world data (replaces "Real")
 * - Synthetic: Artificially generated data for testing/modeling
 * - Mock: Static, hand-written data for unit tests
 */

set schema 'ores';

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- Helper function to insert a data quality nature dimension if it doesn't exist
create or replace function ores.upsert_dq_nature_dimension(
    p_code text,
    p_name text,
    p_description text
) returns void as $$
begin
    if not exists (
        select 1 from ores.dq_nature_dimension_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_nature_dimension_tbl (
            code, version, name, description,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_code, 0, p_name, p_description,
            'system', 'system.new_record', 'System seed data - data quality nature dimension',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality nature: %', p_code;
    else
        raise notice 'Data quality nature already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality Nature Dimensions
-- =============================================================================

\echo '--- Data Quality Nature Dimensions ---'

select ores.upsert_dq_nature_dimension(
    'Actual',
    'Actual Data',
    'Real-world data (replaces "Real").'
);

select ores.upsert_dq_nature_dimension(
    'Synthetic',
    'Synthetic Data',
    'Artificially generated data for testing/modeling.'
);

select ores.upsert_dq_nature_dimension(
    'Mock',
    'Mock Data',
    'Static, hand-written data for unit tests.'
);

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_nature_dimension(text, text, text);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Nature Dimensions' as entity, count(*) as count
from ores.dq_nature_dimension_tbl where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;