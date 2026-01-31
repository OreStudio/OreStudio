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

-- DQ Functions
\ir ./dq_functions_drop.sql

-- Publications (bundle before individual)
\ir ./dq_bundle_publication_drop.sql
\ir ./dq_publication_drop.sql

-- Artefacts
\ir ./dq_ip2country_artefact_drop.sql
\ir ./dq_countries_artefact_drop.sql

-- Datasets
\ir ./dq_datasets_notify_trigger_drop.sql
\ir ./dq_dataset_drop.sql

-- Artefact Types (after datasets due to FK)
\ir ./dq_artefact_types_drop.sql

-- Methodologies
\ir ./dq_methodologies_notify_trigger_drop.sql
\ir ./dq_methodology_drop.sql

-- Dimensions
\ir ./dq_treatment_dimensions_notify_trigger_drop.sql
\ir ./dq_treatment_dimension_drop.sql
\ir ./dq_nature_dimensions_notify_trigger_drop.sql
\ir ./dq_nature_dimension_drop.sql
\ir ./dq_origin_dimensions_notify_trigger_drop.sql
\ir ./dq_origin_dimension_drop.sql

-- Coding schemes
\ir ./dq_coding_schemes_notify_trigger_drop.sql
\ir ./dq_coding_scheme_drop.sql
\ir ./dq_coding_scheme_authority_types_notify_trigger_drop.sql
\ir ./dq_coding_scheme_authority_type_drop.sql

-- Subject areas
\ir ./dq_subject_areas_notify_trigger_drop.sql
\ir ./dq_subject_area_drop.sql

-- Data domains
\ir ./dq_data_domains_notify_trigger_drop.sql
\ir ./dq_data_domain_drop.sql

-- Dataset bundles (members before bundles)
\ir ./dq_dataset_bundle_member_drop.sql
\ir ./dq_dataset_bundle_drop.sql

-- Dataset dependencies
\ir ./dq_dataset_dependency_drop.sql

-- Catalogs
\ir ./dq_catalogs_notify_trigger_drop.sql
\ir ./dq_catalog_drop.sql

-- Population functions
\ir ./dq_population_functions_drop.sql

-- Change reasons (dropped last - other tables reference these)
\ir ./dq_change_reason_functions_drop.sql
\ir ./dq_change_reasons_notify_trigger_drop.sql
\ir ./dq_change_reasons_drop.sql
\ir ./dq_change_reason_categories_notify_trigger_drop.sql
\ir ./dq_change_reason_categories_drop.sql
