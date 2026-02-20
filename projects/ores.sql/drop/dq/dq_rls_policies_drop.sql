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

-- =============================================================================
-- Drop Row-Level Security Policies for Data Quality Governance Tables
-- =============================================================================
-- Must be dropped before the corresponding tables are dropped.

\ir ./dq_fsm_rls_policies_drop.sql

-- Dataset Bundle Members
drop policy if exists ores_dq_dataset_bundle_members_modification_policy on "ores_dq_dataset_bundle_members_tbl";
drop policy if exists ores_dq_dataset_bundle_members_read_policy on "ores_dq_dataset_bundle_members_tbl";

-- Dataset Bundles
drop policy if exists ores_dq_dataset_bundles_modification_policy on "ores_dq_dataset_bundles_tbl";
drop policy if exists ores_dq_dataset_bundles_read_policy on "ores_dq_dataset_bundles_tbl";

-- Dataset Dependencies
drop policy if exists ores_dq_dataset_dependencies_modification_policy on "ores_dq_dataset_dependencies_tbl";
drop policy if exists ores_dq_dataset_dependencies_read_policy on "ores_dq_dataset_dependencies_tbl";

-- Treatment Dimensions
drop policy if exists ores_dq_treatment_dimensions_modification_policy on "ores_dq_treatment_dimensions_tbl";
drop policy if exists ores_dq_treatment_dimensions_read_policy on "ores_dq_treatment_dimensions_tbl";

-- Nature Dimensions
drop policy if exists ores_dq_nature_dimensions_modification_policy on "ores_dq_nature_dimensions_tbl";
drop policy if exists ores_dq_nature_dimensions_read_policy on "ores_dq_nature_dimensions_tbl";

-- Origin Dimensions
drop policy if exists ores_dq_origin_dimensions_modification_policy on "ores_dq_origin_dimensions_tbl";
drop policy if exists ores_dq_origin_dimensions_read_policy on "ores_dq_origin_dimensions_tbl";

-- Change Reasons
drop policy if exists ores_dq_change_reasons_modification_policy on "ores_dq_change_reasons_tbl";
drop policy if exists ores_dq_change_reasons_read_policy on "ores_dq_change_reasons_tbl";

-- Change Reason Categories
drop policy if exists ores_dq_change_reason_categories_modification_policy on "ores_dq_change_reason_categories_tbl";
drop policy if exists ores_dq_change_reason_categories_read_policy on "ores_dq_change_reason_categories_tbl";

-- Coding Scheme Authority Types
drop policy if exists ores_dq_coding_scheme_authority_types_modification_policy on "ores_dq_coding_scheme_authority_types_tbl";
drop policy if exists ores_dq_coding_scheme_authority_types_read_policy on "ores_dq_coding_scheme_authority_types_tbl";

-- Coding Schemes
drop policy if exists ores_dq_coding_schemes_modification_policy on "ores_dq_coding_schemes_tbl";
drop policy if exists ores_dq_coding_schemes_read_policy on "ores_dq_coding_schemes_tbl";

-- Data Domains
drop policy if exists ores_dq_data_domains_modification_policy on "ores_dq_data_domains_tbl";
drop policy if exists ores_dq_data_domains_read_policy on "ores_dq_data_domains_tbl";

-- Subject Areas
drop policy if exists ores_dq_subject_areas_modification_policy on "ores_dq_subject_areas_tbl";
drop policy if exists ores_dq_subject_areas_read_policy on "ores_dq_subject_areas_tbl";

-- Methodologies
drop policy if exists ores_dq_methodologies_modification_policy on "ores_dq_methodologies_tbl";
drop policy if exists ores_dq_methodologies_read_policy on "ores_dq_methodologies_tbl";

-- Datasets
drop policy if exists ores_dq_datasets_modification_policy on "ores_dq_datasets_tbl";
drop policy if exists ores_dq_datasets_read_policy on "ores_dq_datasets_tbl";

-- Catalogs
drop policy if exists ores_dq_catalogs_modification_policy on "ores_dq_catalogs_tbl";
drop policy if exists ores_dq_catalogs_read_policy on "ores_dq_catalogs_tbl";
