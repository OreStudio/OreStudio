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
 * Data Quality Data Domain Population Script
 *
 * Seeds the database with data domain values for data quality classification.
 * This script is idempotent.
 *
 * Domains:
 * - Reference Data: Standardized data used across the system
 * - Trade Data: Transaction and position data
 * - Market Data: Pricing and market information
 */

set schema 'ores';

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- Helper function to insert a data quality data domain if it doesn't exist
create or replace function ores.upsert_dq_data_domain(
    p_name text,
    p_description text
) returns void as $$
begin
    if not exists (
        select 1 from ores.dq_data_domain_tbl
        where name = p_name and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_data_domain_tbl (
            id, version, name, description,
            modified_by, change_commentary, valid_from, valid_to
        )
        values (
            gen_random_uuid(), 0, p_name, p_description,
            'system', 'System seed data - data quality data domain',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality data domain: %', p_name;
    else
        raise notice 'Data quality data domain already exists: %', p_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality Data Domains
-- =============================================================================

\echo '--- Data Quality Data Domains ---'

select ores.upsert_dq_data_domain(
    'Reference Data',
    'Standardized data used across the system.'
);

select ores.upsert_dq_data_domain(
    'Trade Data',
    'Transaction and position data.'
);

select ores.upsert_dq_data_domain(
    'Market Data',
    'Pricing and market information.'
);

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_data_domain(text, text);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Data Domains' as entity, count(*) as count
from ores.dq_data_domain_tbl where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;