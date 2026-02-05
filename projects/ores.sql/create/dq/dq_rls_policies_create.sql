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

-- =============================================================================
-- Row-Level Security Policies for Data Quality Governance Tables
-- =============================================================================
-- DQ governance data (catalogs, datasets, methodologies, etc.) uses a shared
-- data pattern where system tenant data is readable by all tenants.
--
-- Pattern:
-- - SELECT: Own tenant data OR system tenant data (shared reference data)
-- - INSERT/UPDATE/DELETE: Own tenant data only

-- -----------------------------------------------------------------------------
-- Catalogs
-- -----------------------------------------------------------------------------
alter table ores_dq_catalogs_tbl enable row level security;

create policy ores_dq_catalogs_read_policy on ores_dq_catalogs_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_catalogs_modification_policy on ores_dq_catalogs_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Datasets
-- -----------------------------------------------------------------------------
alter table ores_dq_datasets_tbl enable row level security;

create policy ores_dq_datasets_read_policy on ores_dq_datasets_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_datasets_modification_policy on ores_dq_datasets_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Methodologies
-- -----------------------------------------------------------------------------
alter table ores_dq_methodologies_tbl enable row level security;

create policy ores_dq_methodologies_read_policy on ores_dq_methodologies_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_methodologies_modification_policy on ores_dq_methodologies_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Subject Areas
-- -----------------------------------------------------------------------------
alter table ores_dq_subject_areas_tbl enable row level security;

create policy ores_dq_subject_areas_read_policy on ores_dq_subject_areas_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_subject_areas_modification_policy on ores_dq_subject_areas_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Data Domains
-- -----------------------------------------------------------------------------
alter table ores_dq_data_domains_tbl enable row level security;

create policy ores_dq_data_domains_read_policy on ores_dq_data_domains_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_data_domains_modification_policy on ores_dq_data_domains_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Coding Schemes
-- -----------------------------------------------------------------------------
alter table ores_dq_coding_schemes_tbl enable row level security;

create policy ores_dq_coding_schemes_read_policy on ores_dq_coding_schemes_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_coding_schemes_modification_policy on ores_dq_coding_schemes_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Coding Scheme Authority Types
-- -----------------------------------------------------------------------------
alter table ores_dq_coding_scheme_authority_types_tbl enable row level security;

create policy ores_dq_coding_scheme_authority_types_read_policy on ores_dq_coding_scheme_authority_types_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_coding_scheme_authority_types_modification_policy on ores_dq_coding_scheme_authority_types_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Change Reason Categories
-- -----------------------------------------------------------------------------
alter table ores_dq_change_reason_categories_tbl enable row level security;

create policy ores_dq_change_reason_categories_read_policy on ores_dq_change_reason_categories_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_change_reason_categories_modification_policy on ores_dq_change_reason_categories_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Change Reasons
-- -----------------------------------------------------------------------------
alter table ores_dq_change_reasons_tbl enable row level security;

create policy ores_dq_change_reasons_read_policy on ores_dq_change_reasons_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_change_reasons_modification_policy on ores_dq_change_reasons_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Origin Dimensions
-- -----------------------------------------------------------------------------
alter table ores_dq_origin_dimensions_tbl enable row level security;

create policy ores_dq_origin_dimensions_read_policy on ores_dq_origin_dimensions_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_origin_dimensions_modification_policy on ores_dq_origin_dimensions_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Nature Dimensions
-- -----------------------------------------------------------------------------
alter table ores_dq_nature_dimensions_tbl enable row level security;

create policy ores_dq_nature_dimensions_read_policy on ores_dq_nature_dimensions_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_nature_dimensions_modification_policy on ores_dq_nature_dimensions_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Treatment Dimensions
-- -----------------------------------------------------------------------------
alter table ores_dq_treatment_dimensions_tbl enable row level security;

create policy ores_dq_treatment_dimensions_read_policy on ores_dq_treatment_dimensions_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_treatment_dimensions_modification_policy on ores_dq_treatment_dimensions_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Dataset Dependencies
-- -----------------------------------------------------------------------------
alter table ores_dq_dataset_dependencies_tbl enable row level security;

create policy ores_dq_dataset_dependencies_read_policy on ores_dq_dataset_dependencies_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_dataset_dependencies_modification_policy on ores_dq_dataset_dependencies_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Dataset Bundles
-- -----------------------------------------------------------------------------
alter table ores_dq_dataset_bundles_tbl enable row level security;

create policy ores_dq_dataset_bundles_read_policy on ores_dq_dataset_bundles_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_dataset_bundles_modification_policy on ores_dq_dataset_bundles_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Dataset Bundle Members
-- -----------------------------------------------------------------------------
alter table ores_dq_dataset_bundle_members_tbl enable row level security;

create policy ores_dq_dataset_bundle_members_read_policy on ores_dq_dataset_bundle_members_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()
);

create policy ores_dq_dataset_bundle_members_modification_policy on ores_dq_dataset_bundle_members_tbl
for all
using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());
