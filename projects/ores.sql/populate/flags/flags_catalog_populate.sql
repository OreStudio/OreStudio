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
 * Visual Assets Catalog Population Script
 *
 * Auto-generated from external/flags/manifest.json
 * This script is idempotent.
 */

set schema 'metadata';

-- =============================================================================
-- Visual Assets Catalog
-- =============================================================================

\echo '--- Visual Assets Catalog ---'

select public.upsert_dq_catalogs(
    'Visual Assets',
    'Visual media assets including country flag images, cryptocurrency icons, and other imagery used to enrich reference data displays.',
    'Reference Data Team'
);
