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
 * Includes the system avatar SQL files in dependency order, then publishes
 * them into the system tenant's live table. The super administrator's account
 * belongs to the system tenant, and the provisioning attach step copies each
 * account's picture from a system-tenant template, so the rows must exist
 * before any tenant is set up. assets.system_avatars is also a base bundle
 * member, which puts a copy in every tenant; that copy is not the one
 * attach_photos reads for a system-tenant account.
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

-- =============================================================================
-- Publish to the System Tenant
-- =============================================================================

\echo '--- Publishing System Avatars ---'
select * from ores_assets_publish_images_from_dq_fn(
    (select id from ores_dq_datasets_tbl where code = 'assets.system_avatars' and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_utility_system_tenant_id_fn(),
    'upsert'
);
