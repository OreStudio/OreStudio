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
/*
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_dataset_dependency_populate.mustache
 * To modify, update the template and regenerate.
 */

-- =============================================================================
-- Data Quality Slovaris Dataset Dependencies
-- =============================================================================

\echo '--- Data Quality Slovaris Dataset Dependencies ---'

select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'slovaris.countries',
    'slovaris.country_flags',
    'visual_assets'
);
select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'slovaris.currencies',
    'slovaris.country_flags',
    'visual_assets'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select dataset_code, dependency_code, role
from ores_dq_dataset_dependencies_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
  and dataset_code like 'slovaris.%'
order by dataset_code, dependency_code;
