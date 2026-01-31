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
 * Drop Upsert Functions
 *
 * Drops all helper functions used by population scripts.
 * This script is safe to run even if functions don't exist (uses IF EXISTS).
 *
 * Naming convention: ores_{component}_{entities}_{action}_fn
 */

-- Validation helper
drop function if exists ores_seed_validate_not_empty_fn;

-- DQ upsert functions
drop function if exists ores_dq_data_domains_upsert_fn;
drop function if exists ores_dq_subject_areas_upsert_fn;
drop function if exists ores_dq_catalogs_upsert_fn;
drop function if exists ores_dq_dataset_dependencies_upsert_fn;
drop function if exists ores_dq_origin_dimensions_upsert_fn;
drop function if exists ores_dq_nature_dimensions_upsert_fn;
drop function if exists ores_dq_treatment_dimensions_upsert_fn;
drop function if exists ores_dq_change_reason_categories_upsert_fn;
drop function if exists ores_dq_change_reasons_upsert_fn;
drop function if exists ores_dq_coding_scheme_authority_types_upsert_fn;
drop function if exists ores_dq_coding_schemes_upsert_fn;
drop function if exists ores_dq_methodologies_upsert_fn;
drop function if exists ores_dq_datasets_upsert_fn;
drop function if exists ores_dq_tags_upsert_fn;
drop function if exists ores_dq_dataset_bundles_upsert_fn;
drop function if exists ores_dq_dataset_bundle_members_upsert_fn;

-- IAM upsert functions
drop function if exists ores_iam_permissions_upsert_fn;
drop function if exists ores_iam_roles_upsert_fn;
drop function if exists ores_iam_role_permissions_assign_fn;

-- Variability upsert functions
drop function if exists ores_variability_feature_flags_upsert_fn;
