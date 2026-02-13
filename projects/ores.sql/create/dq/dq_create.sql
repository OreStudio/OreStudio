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

-- Change reasons (must be created first - other tables reference these)
\ir ./dq_change_reason_categories_create.sql
\ir ./dq_change_reason_categories_notify_trigger_create.sql
\ir ./dq_change_reasons_create.sql
\ir ./dq_change_reasons_notify_trigger_create.sql
\ir ./dq_change_reason_functions_create.sql

-- Catalogs and dataset dependencies
\ir ./dq_catalog_create.sql
\ir ./dq_catalogs_notify_trigger_create.sql
\ir ./dq_dataset_dependency_create.sql

-- Dataset bundles
\ir ./dq_dataset_bundle_create.sql
\ir ./dq_dataset_bundle_member_create.sql

-- Data domains and subject areas
\ir ./dq_data_domain_create.sql
\ir ./dq_data_domains_notify_trigger_create.sql
\ir ./dq_subject_area_create.sql
\ir ./dq_subject_areas_notify_trigger_create.sql

-- Coding schemes
\ir ./dq_coding_scheme_authority_type_create.sql
\ir ./dq_coding_scheme_authority_types_notify_trigger_create.sql
\ir ./dq_coding_scheme_create.sql
\ir ./dq_coding_schemes_notify_trigger_create.sql
\ir ./dq_coding_schemes_artefact_create.sql

-- Dimensions
\ir ./dq_origin_dimension_create.sql
\ir ./dq_origin_dimensions_notify_trigger_create.sql
\ir ./dq_nature_dimension_create.sql
\ir ./dq_nature_dimensions_notify_trigger_create.sql
\ir ./dq_treatment_dimension_create.sql
\ir ./dq_treatment_dimensions_notify_trigger_create.sql

-- Methodologies
\ir ./dq_methodology_create.sql
\ir ./dq_methodologies_notify_trigger_create.sql

-- Artefact Types (must precede datasets for FK validation)
\ir ./dq_artefact_types_create.sql

-- Datasets
\ir ./dq_dataset_create.sql
\ir ./dq_datasets_notify_trigger_create.sql

-- Core artefact tables
\ir ./dq_countries_artefact_create.sql
\ir ./dq_currencies_artefact_create.sql
\ir ./dq_images_artefact_create.sql
\ir ./dq_tags_artefact_create.sql
\ir ./dq_image_tags_artefact_create.sql
\ir ./dq_ip2country_artefact_create.sql

-- FPML artefact tables
\ir ./dq_account_types_artefact_create.sql
\ir ./dq_asset_classes_artefact_create.sql
\ir ./dq_asset_measures_artefact_create.sql
\ir ./dq_benchmark_rates_artefact_create.sql
\ir ./dq_business_centres_artefact_create.sql
\ir ./dq_business_processes_artefact_create.sql
\ir ./dq_cashflow_types_artefact_create.sql
\ir ./dq_entity_classifications_artefact_create.sql
\ir ./dq_local_jurisdictions_artefact_create.sql
\ir ./dq_party_relationships_artefact_create.sql
\ir ./dq_party_roles_artefact_create.sql
\ir ./dq_person_roles_artefact_create.sql
\ir ./dq_regulatory_corporate_sectors_artefact_create.sql
\ir ./dq_reporting_regimes_artefact_create.sql
\ir ./dq_supervisory_bodies_artefact_create.sql

-- GLEIF LEI artefact tables
\ir ./dq_lei_entities_artefact_create.sql
\ir ./dq_lei_relationships_artefact_create.sql
\ir ./dq_lei_bic_artefact_create.sql

-- DQ functions
\ir ./dq_functions_create.sql
\ir ./dq_population_functions_create.sql

-- FPML entity population functions
\ir ./dq_account_types_population_functions_create.sql
\ir ./dq_asset_classes_population_functions_create.sql
\ir ./dq_asset_measures_population_functions_create.sql
\ir ./dq_benchmark_rates_population_functions_create.sql
\ir ./dq_business_centres_population_functions_create.sql
\ir ./dq_business_processes_population_functions_create.sql
\ir ./dq_cashflow_types_population_functions_create.sql
\ir ./dq_entity_classifications_population_functions_create.sql
\ir ./dq_local_jurisdictions_population_functions_create.sql
\ir ./dq_party_relationships_population_functions_create.sql
\ir ./dq_party_roles_population_functions_create.sql
\ir ./dq_person_roles_population_functions_create.sql
\ir ./dq_regulatory_corporate_sectors_population_functions_create.sql
\ir ./dq_reporting_regimes_population_functions_create.sql
\ir ./dq_supervisory_bodies_population_functions_create.sql
\ir ./dq_coding_schemes_population_functions_create.sql

-- Publication
\ir ./dq_publication_create.sql
\ir ./dq_bundle_publication_create.sql
\ir ./dq_lei_counterparties_publish_create.sql
\ir ./dq_lei_parties_publish_create.sql

