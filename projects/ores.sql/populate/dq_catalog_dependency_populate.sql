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
 * Data Quality Catalog Dependency Population Script
 *
 * Seeds the database with catalog dependencies. These declare which catalogs
 * must be loaded before others.
 *
 * Dependencies:
 * - FpML Standards depends on ISO Standards (uses flag images for currencies)
 */

set schema 'ores';

-- =============================================================================
-- Helper Functions
-- =============================================================================

create or replace function ores.upsert_dq_catalog_dependency(
    p_catalog_name text,
    p_dependency_name text
) returns void as $$
begin
    if not exists (
        select 1 from ores.dq_catalog_dependencies_tbl
        where catalog_name = p_catalog_name
          and dependency_name = p_dependency_name
          and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_catalog_dependencies_tbl (
            catalog_name, dependency_name,
            recorded_by, change_reason_code, change_commentary,
            valid_from, valid_to
        )
        values (
            p_catalog_name, p_dependency_name,
            'system', 'system.new_record', 'System seed data - catalog dependency',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created catalog dependency: % depends on %', p_catalog_name, p_dependency_name;
    else
        raise notice 'Catalog dependency already exists: % depends on %', p_catalog_name, p_dependency_name;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Seed Data
-- =============================================================================

\echo '--- Catalog Dependencies ---'

-- FpML Standards uses flag images from ISO Standards for currency display
select ores.upsert_dq_catalog_dependency(
    'FpML Standards',
    'ISO Standards'
);

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_catalog_dependency(text, text);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select catalog_name, dependency_name
from ores.dq_catalog_dependencies_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
order by catalog_name, dependency_name;
