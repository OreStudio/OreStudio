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
 * Flag Icons Dataset Tags
 *
 * Tags for flag icon datasets.
 * Auto-generated from external/flags/manifest.json
 * Must be run after flags_dataset_populate.sql.
 */

-- =============================================================================
-- Flag Icons Dataset Tags
-- =============================================================================

\echo '--- Flag Icons Dataset Tags ---'

select ores_dq_tags_upsert_fn(ores_iam_system_tenant_id_fn(),
    'Country Flag Images',
    'Country Flags',
    'Reference Data',
    'flag',
    'Country and region flag images'
);

