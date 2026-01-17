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
 * Coding Scheme Authority Type Population Script
 *
 * Seeds the database with authority type values for classifying coding schemes.
 * This script is idempotent.
 *
 * Authority Types:
 * - official: Standards body (ISO, IEEE, GLEIF)
 * - industry: De facto market standards (SWIFT, DTCC, FINRA)
 * - internal: Proprietary/organization-specific
 */

set schema 'ores';

-- =============================================================================
-- Helper Functions
-- =============================================================================

create or replace function ores.upsert_dq_coding_scheme_authority_type(
    p_code text,
    p_name text,
    p_description text
) returns void as $$
begin
    if not exists (
        select 1 from ores.dq_coding_scheme_authority_types_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_coding_scheme_authority_types_tbl (
            code, version, name, description,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_code, 0, p_name, p_description,
            'system', 'system.new_record', 'System seed data - coding scheme authority type',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created coding scheme authority type: %', p_code;
    else
        raise notice 'Coding scheme authority type already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Coding Scheme Authority Types
-- =============================================================================

\echo '--- Coding Scheme Authority Types ---'

select ores.upsert_dq_coding_scheme_authority_type(
    'official',
    'Official Standard',
    'Formal standard published by a recognized standards body such as ISO, IEEE, or a regulatory authority. These schemes have official governance, versioning, and maintenance processes.'
);

select ores.upsert_dq_coding_scheme_authority_type(
    'industry',
    'Industry Standard',
    'De facto standard widely adopted in the financial industry but not published by a formal standards body. Typically maintained by industry consortiums, trade associations, or market infrastructure providers such as SWIFT, DTCC, or FINRA.'
);

select ores.upsert_dq_coding_scheme_authority_type(
    'internal',
    'Internal/Proprietary',
    'Proprietary or organization-specific identifier scheme. Used for internal systems, client identifiers, or custom classifications that are not standardized outside the organization.'
);

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_coding_scheme_authority_type(text, text, text);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Coding Scheme Authority Types' as entity, count(*) as count
from ores.dq_coding_scheme_authority_types_tbl where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;
