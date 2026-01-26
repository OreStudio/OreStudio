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
 * Database Summary Functions
 *
 * Functions for inspecting the current state of the database across
 * all population layers. Useful for verifying database setup and
 * monitoring data completeness.
 *
 * Layers:
 * - Foundation: Essential lookup and configuration data
 * - Governance: Data governance metadata (dimensions, methodologies, artefact types)
 * - Catalogues: Catalogued reference data (catalogs, datasets, artefacts)
 * - Production: Published reference data in production tables
 */

SET search_path TO ores;

-- =============================================================================
-- Summary Result Type
-- =============================================================================

DROP TYPE IF EXISTS ores.layer_summary_row CASCADE;
CREATE TYPE ores.layer_summary_row AS (
    layer text,
    entity text,
    count bigint
);

-- =============================================================================
-- Foundation Layer Summary
-- =============================================================================

CREATE OR REPLACE FUNCTION ores.summary_foundation_layer_fn()
RETURNS SETOF ores.layer_summary_row AS $$
BEGIN
    RETURN QUERY
    SELECT 'Foundation'::text, 'Change Reason Categories'::text, count(*)
    FROM ores.dq_change_reason_categories_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Foundation', 'Change Reasons', count(*)
    FROM ores.dq_change_reasons_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Foundation', 'Rounding Types', count(*)
    FROM ores.refdata_rounding_types_tbl
    UNION ALL
    SELECT 'Foundation', 'Data Domains', count(*)
    FROM ores.dq_data_domains_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Foundation', 'Subject Areas', count(*)
    FROM ores.dq_subject_areas_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Foundation', 'Coding Scheme Authority Types', count(*)
    FROM ores.dq_coding_scheme_authority_types_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Foundation', 'Coding Schemes', count(*)
    FROM ores.dq_coding_schemes_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Foundation', 'Permissions', count(*)
    FROM ores.iam_permissions_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Foundation', 'Roles', count(*)
    FROM ores.iam_roles_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Foundation', 'System Flags', count(*)
    FROM ores.variability_feature_flags_tbl
    WHERE name LIKE 'system.%' AND valid_to = ores.utility_infinity_timestamp_fn()
    ORDER BY 2;
END;
$$ LANGUAGE plpgsql STABLE;

COMMENT ON FUNCTION ores.summary_foundation_layer_fn() IS
'Returns a summary of the Foundation layer: essential lookup and configuration data required for schema integrity.';

-- =============================================================================
-- Data Governance Layer Summary
-- =============================================================================

CREATE OR REPLACE FUNCTION ores.summary_governance_layer_fn()
RETURNS SETOF ores.layer_summary_row AS $$
BEGIN
    RETURN QUERY
    SELECT 'Governance'::text, 'Origin Dimensions'::text, count(*)
    FROM ores.dq_origin_dimensions_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Governance', 'Nature Dimensions', count(*)
    FROM ores.dq_nature_dimensions_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Governance', 'Treatment Dimensions', count(*)
    FROM ores.dq_treatment_dimensions_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Governance', 'Methodologies', count(*)
    FROM ores.dq_methodologies_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Governance', 'Artefact Types', count(*)
    FROM ores.dq_artefact_types_tbl
    ORDER BY 2;
END;
$$ LANGUAGE plpgsql STABLE;

COMMENT ON FUNCTION ores.summary_governance_layer_fn() IS
'Returns a summary of the Data Governance layer: metadata defining rules and classifications for data organization.';

-- =============================================================================
-- Data Catalogues Layer Summary
-- =============================================================================

CREATE OR REPLACE FUNCTION ores.summary_catalogues_layer_fn()
RETURNS SETOF ores.layer_summary_row AS $$
BEGIN
    RETURN QUERY
    SELECT 'Catalogues'::text, 'Catalogs'::text, count(*)
    FROM ores.dq_catalogs_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Catalogues', 'Datasets', count(*)
    FROM ores.dq_datasets_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Catalogues', 'Dataset Dependencies', count(*)
    FROM ores.dq_dataset_dependencies_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Images', count(*)
    FROM ores.dq_images_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Countries', count(*)
    FROM ores.dq_countries_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Currencies', count(*)
    FROM ores.dq_currencies_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: IP Ranges', count(*)
    FROM ores.dq_ip2country_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Account Types', count(*)
    FROM ores.dq_account_types_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Asset Classes', count(*)
    FROM ores.dq_asset_classes_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Asset Measures', count(*)
    FROM ores.dq_asset_measures_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Benchmark Rates', count(*)
    FROM ores.dq_benchmark_rates_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Business Centres', count(*)
    FROM ores.dq_business_centres_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Business Processes', count(*)
    FROM ores.dq_business_processes_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Cashflow Types', count(*)
    FROM ores.dq_cashflow_types_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Entity Classifications', count(*)
    FROM ores.dq_entity_classifications_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Local Jurisdictions', count(*)
    FROM ores.dq_local_jurisdictions_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Party Relationships', count(*)
    FROM ores.dq_party_relationships_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Party Roles', count(*)
    FROM ores.dq_party_roles_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Person Roles', count(*)
    FROM ores.dq_person_roles_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Regulatory Corporate Sectors', count(*)
    FROM ores.dq_regulatory_corporate_sectors_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Reporting Regimes', count(*)
    FROM ores.dq_reporting_regimes_artefact_tbl
    UNION ALL
    SELECT 'Catalogues', 'Artefact: Supervisory Bodies', count(*)
    FROM ores.dq_supervisory_bodies_artefact_tbl
    ORDER BY 2;
END;
$$ LANGUAGE plpgsql STABLE;

COMMENT ON FUNCTION ores.summary_catalogues_layer_fn() IS
'Returns a summary of the Data Catalogues layer: catalogued reference data awaiting publication to production.';

-- =============================================================================
-- Production Layer Summary
-- =============================================================================

CREATE OR REPLACE FUNCTION ores.summary_production_layer_fn()
RETURNS SETOF ores.layer_summary_row AS $$
BEGIN
    RETURN QUERY
    SELECT 'Production'::text, 'Countries'::text, count(*)
    FROM ores.refdata_countries_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Currencies', count(*)
    FROM ores.refdata_currencies_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Images', count(*)
    FROM ores.assets_images_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Account Types', count(*)
    FROM ores.refdata_account_types_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Asset Classes', count(*)
    FROM ores.refdata_asset_classes_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Asset Measures', count(*)
    FROM ores.refdata_asset_measures_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Benchmark Rates', count(*)
    FROM ores.refdata_benchmark_rates_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Business Centres', count(*)
    FROM ores.refdata_business_centres_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Business Processes', count(*)
    FROM ores.refdata_business_processes_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Cashflow Types', count(*)
    FROM ores.refdata_cashflow_types_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Entity Classifications', count(*)
    FROM ores.refdata_entity_classifications_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Local Jurisdictions', count(*)
    FROM ores.refdata_local_jurisdictions_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Party Relationships', count(*)
    FROM ores.refdata_party_relationships_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Party Roles', count(*)
    FROM ores.refdata_party_roles_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Person Roles', count(*)
    FROM ores.refdata_person_roles_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Regulatory Corporate Sectors', count(*)
    FROM ores.refdata_regulatory_corporate_sectors_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Reporting Regimes', count(*)
    FROM ores.refdata_reporting_regimes_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    UNION ALL
    SELECT 'Production', 'Supervisory Bodies', count(*)
    FROM ores.refdata_supervisory_bodies_tbl
    WHERE valid_to = ores.utility_infinity_timestamp_fn()
    ORDER BY 2;
END;
$$ LANGUAGE plpgsql STABLE;

COMMENT ON FUNCTION ores.summary_production_layer_fn() IS
'Returns a summary of the Production layer: published reference data in production tables.';

-- =============================================================================
-- All Layers Summary
-- =============================================================================

CREATE OR REPLACE FUNCTION ores.summary_all_layers_fn()
RETURNS SETOF ores.layer_summary_row AS $$
BEGIN
    RETURN QUERY
    SELECT * FROM ores.summary_foundation_layer_fn()
    UNION ALL
    SELECT * FROM ores.summary_governance_layer_fn()
    UNION ALL
    SELECT * FROM ores.summary_catalogues_layer_fn()
    UNION ALL
    SELECT * FROM ores.summary_production_layer_fn()
    ORDER BY 1, 2;
END;
$$ LANGUAGE plpgsql STABLE;

COMMENT ON FUNCTION ores.summary_all_layers_fn() IS
'Returns a summary of all database layers: Foundation, Governance, Catalogues, and Production.';
