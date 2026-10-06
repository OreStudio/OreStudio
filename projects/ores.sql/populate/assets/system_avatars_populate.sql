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
 * System Avatar Images Master Include File
 *
 * Includes the system avatar SQL files in dependency order. Publication into
 * the system tenant's live table is a runtime act: assets.system_avatars is a
 * member of the base bundle, so the provisioning publishes it with the rest.
 */

-- =============================================================================
-- System Avatar Methodology
-- =============================================================================

\echo '--- System Avatar Methodology ---'
\ir system_avatars_methodology_populate.sql

-- =============================================================================
-- System Avatar Datasets
-- =============================================================================

\echo '--- System Avatar Datasets ---'
\ir system_avatars_dataset_populate.sql

-- =============================================================================
-- System Avatar Dataset Tags
-- =============================================================================

\echo '--- System Avatar Dataset Tags ---'
\ir system_avatars_dataset_tag_populate.sql

-- =============================================================================
-- System Avatar Images
-- =============================================================================

\echo '--- System Avatar Images ---'
\ir system_avatars_images_artefact_populate.sql
