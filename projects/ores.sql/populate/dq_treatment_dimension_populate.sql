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
 * Data Quality Treatment Dimension Population Script
 *
 * Seeds the database with treatment dimension values for data quality classification.
 * This script is idempotent.
 *
 * Treatments:
 * - Raw: Untouched, identifiable data
 * - Masked: PII has been redacted or obfuscated (replaces "Obfuscated")
 * - Anonymized: Irreversibly altered to prevent re-identification
 */

set schema 'ores';

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- Helper function to insert a data quality treatment dimension if it doesn't exist
create or replace function ores.upsert_dq_treatment_dimension(
    p_code text,
    p_name text,
    p_description text
) returns void as $$
begin
    if not exists (
        select 1 from ores.dq_treatment_dimension_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_treatment_dimension_tbl (
            code, version, name, description,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_code, 0, p_name, p_description,
            'system', 'system.new_record', 'System seed data - data quality treatment dimension',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality treatment: %', p_code;
    else
        raise notice 'Data quality treatment already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality Treatment Dimensions
-- =============================================================================

\echo '--- Data Quality Treatment Dimensions ---'

select ores.upsert_dq_treatment_dimension(
    'Raw',
    'Raw Data',
    'Untouched, identifiable data.'
);

select ores.upsert_dq_treatment_dimension(
    'Masked',
    'Masked Data',
    'PII has been redacted or obfuscated (replaces "Obfuscated").'
);

select ores.upsert_dq_treatment_dimension(
    'Anonymized',
    'Anonymized Data',
    'Irreversibly altered to prevent re-identification.'
);

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_treatment_dimension(text, text, text);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Treatment Dimensions' as entity, count(*) as count
from ores.dq_treatment_dimension_tbl where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;