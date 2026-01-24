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
 * Flag Icons Dataset Population Script
 *
 * Creates the dataset entries for flag icon reference data.
 * Auto-generated from external/flags/manifest.json
 * This must be run before populating the artefact tables.
 */

set schema 'ores';

-- =============================================================================
-- Flag Icons Datasets
-- =============================================================================

\echo '--- Flag Icons Datasets ---'

-- Country Flag Images
select ores.upsert_dq_datasets(
    'assets.country_flags',
    'Visual Assets',
    'Country Flags',
    'Reference Data',
    'ISO_3166_1_ALPHA_2',
    'Primary',
    'Actual',
    'Raw',
    'GitHub Flag Icons Download',
    'Country Flag Images',
    'SVG flag images for each ISO 3166-1 country.',
    'lipis/flag-icons',
    'Visual assets for countries',
    '2024-12-29'::date,
    'MIT License',
    'images',
    'refdata_images_tbl',
    'dq_populate_images'
);

