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
        'Default administrator and service avatars vendored with the platform',
        'https://github.com/microsoft/fluentui-system-icons',
        'Data Sourcing and Generation Steps:

    1. ARTWORK
       Fluent UI System Icons (Microsoft, MIT licence): the 24px regular
       SVG of each chosen icon, copied unchanged. external/avatars/
       methodology.txt lists which icon each code takes.

    2. SAVE TO REPOSITORY
       Target directory: external/avatars/
       The key is the filename without extension. The provisioning handler
       attaches the administrator pictures by their codes
       (super_admin_avatar, tenant_admin_avatar), and each service account
       takes the code of its registry name with the dots replaced by
       underscores (ores_iam_service), so a rename changes what is looked
       up.

    3. GENERATE SQL POPULATE SCRIPT
       Script: projects/ores.codegen/src/images_generate_sql.py
       Command: python3 images_generate_sql.py --config system_avatars
       Output: projects/ores.sql/populate/assets/system_avatars_images_artefact_populate.sql

    4. COMMIT GENERATED SQL
       git add projects/ores.sql/populate/assets/
       git commit -m "[sql] Regenerate system avatars populate script"'
    );
END $$;
