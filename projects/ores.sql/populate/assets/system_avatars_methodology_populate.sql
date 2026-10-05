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
 * System Avatar Methodology Population Script
 *
 * Defines how the default system avatar artwork is sourced and maintained.
 * Must be run before system_avatars_dataset_populate.sql, which names it.
 * This script is idempotent.
 */

DO $$
BEGIN
    -- =============================================================================
    -- System Avatar Data Sourcing Methodologies
    -- =============================================================================

    PERFORM ores_dq_methodologies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'OreStudio System Avatar Artwork',
        'Default administrator avatar artwork vendored with the platform',
        null,
        'Data Sourcing and Generation Steps:

    1. ARTWORK
       The PNG files are product artwork provided by the project owner
       (no external download). The delivered files are vendored as they
       are, with no re-encoding and no cropping.

    2. SAVE TO REPOSITORY
       Target directory: external/avatars/
       The key is the filename without extension (super_admin_avatar,
       tenant_admin_avatar). The provisioning handler attaches the
       pictures by those codes, so a rename changes what it looks up.

    3. GENERATE SQL POPULATE SCRIPT
       Script: projects/ores.codegen/src/images_generate_sql.py
       Command: python3 images_generate_sql.py --config system_avatars
       Output: projects/ores.sql/populate/assets/system_avatars_images_artefact_populate.sql

    4. COMMIT GENERATED SQL
       git add projects/ores.sql/populate/assets/
       git commit -m "[sql] Regenerate system avatars populate script"'
    );
END $$;
