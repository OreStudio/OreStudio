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
 * DQ Artefact Types Population Script
 *
 * Seeds the database with artefact type mappings.
 * This script is idempotent.
 */

set schema 'metadata';

-- =============================================================================
-- DQ Artefact Types
-- =============================================================================

\echo '--- DQ Artefact Types ---'

insert into metadata.dq_artefact_types_tbl (code, name, description, artefact_table, target_table, populate_function, display_order)
values
    ('none', 'None', 'Dataset with no artefacts (metadata only)', null, null, null, 0),
    ('images', 'Images', 'Image assets for visual elements', 'dq_images_artefact_tbl', 'assets_images_tbl', 'dq_populate_images_fn', 1),
    ('countries', 'Countries', 'ISO 3166 country codes', 'dq_countries_artefact_tbl', 'refdata_countries_tbl', 'dq_populate_countries_fn', 2),
    ('currencies', 'Currencies', 'ISO 4217 currency codes', 'dq_currencies_artefact_tbl', 'refdata_currencies_tbl', 'dq_populate_currencies_fn', 3),
    ('ip2country', 'IP to Country', 'IP address to country mappings', 'dq_ip2country_artefact_tbl', 'geo_ip2country_tbl', 'dq_populate_ip2country_fn', 4),
    ('coding_schemes', 'Coding Schemes', 'Coding scheme definitions', 'dq_coding_schemes_artefact_tbl', 'dq_coding_schemes_tbl', 'dq_populate_coding_schemes_fn', 5),
    ('account_types', 'Account Types', 'FpML account type codes', 'dq_account_types_artefact_tbl', 'refdata_account_types_tbl', 'dq_populate_account_types_fn', 10),
    ('asset_classes', 'Asset Classes', 'FpML asset class codes', 'dq_asset_classes_artefact_tbl', 'refdata_asset_classes_tbl', 'dq_populate_asset_classes_fn', 11),
    ('asset_measures', 'Asset Measures', 'FpML asset measure codes', 'dq_asset_measures_artefact_tbl', 'refdata_asset_measures_tbl', 'dq_populate_asset_measures_fn', 12),
    ('benchmark_rates', 'Benchmark Rates', 'FpML benchmark rate codes', 'dq_benchmark_rates_artefact_tbl', 'refdata_benchmark_rates_tbl', 'dq_populate_benchmark_rates_fn', 13),
    ('business_centres', 'Business Centres', 'FpML business centre codes', 'dq_business_centres_artefact_tbl', 'refdata_business_centres_tbl', 'dq_populate_business_centres_fn', 14),
    ('business_processes', 'Business Processes', 'FpML business process codes', 'dq_business_processes_artefact_tbl', 'refdata_business_processes_tbl', 'dq_populate_business_processes_fn', 15),
    ('cashflow_types', 'Cashflow Types', 'FpML cashflow type codes', 'dq_cashflow_types_artefact_tbl', 'refdata_cashflow_types_tbl', 'dq_populate_cashflow_types_fn', 16),
    ('entity_classifications', 'Entity Classifications', 'FpML entity classification codes', 'dq_entity_classifications_artefact_tbl', 'refdata_entity_classifications_tbl', 'dq_populate_entity_classifications_fn', 17),
    ('local_jurisdictions', 'Local Jurisdictions', 'FpML local jurisdiction codes', 'dq_local_jurisdictions_artefact_tbl', 'refdata_local_jurisdictions_tbl', 'dq_populate_local_jurisdictions_fn', 18),
    ('party_relationships', 'Party Relationships', 'FpML party relationship codes', 'dq_party_relationships_artefact_tbl', 'refdata_party_relationships_tbl', 'dq_populate_party_relationships_fn', 19),
    ('party_roles', 'Party Roles', 'FpML party role codes', 'dq_party_roles_artefact_tbl', 'refdata_party_roles_tbl', 'dq_populate_party_roles_fn', 20),
    ('person_roles', 'Person Roles', 'FpML person role codes', 'dq_person_roles_artefact_tbl', 'refdata_person_roles_tbl', 'dq_populate_person_roles_fn', 21),
    ('regulatory_corporate_sectors', 'Regulatory Corporate Sectors', 'FpML regulatory corporate sector codes', 'dq_regulatory_corporate_sectors_artefact_tbl', 'refdata_regulatory_corporate_sectors_tbl', 'dq_populate_regulatory_corporate_sectors_fn', 22),
    ('reporting_regimes', 'Reporting Regimes', 'FpML reporting regime codes', 'dq_reporting_regimes_artefact_tbl', 'refdata_reporting_regimes_tbl', 'dq_populate_reporting_regimes_fn', 23),
    ('supervisory_bodies', 'Supervisory Bodies', 'FpML supervisory body codes', 'dq_supervisory_bodies_artefact_tbl', 'refdata_supervisory_bodies_tbl', 'dq_populate_supervisory_bodies_fn', 24)
on conflict (code) do nothing;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'DQ Artefact Types' as entity, count(*) as count
from metadata.dq_artefact_types_tbl
order by entity;
