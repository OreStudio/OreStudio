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

-- =============================================================================
-- DQ Artefact Types
-- =============================================================================

\echo '--- DQ Artefact Types ---'

insert into ores_dq_artefact_types_tbl (
    tenant_id, code, version, name, description, artefact_table, target_table, target_subject, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'none', 0, 'None', 'Dataset with no artefacts (metadata only)',
     null, null, null, 0, current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'images', 0, 'Images', 'Image assets for visual elements',
     'dq_images_artefact_tbl', 'assets_images_tbl', 'assets.v1.images.publish-from-dq', 1,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'countries', 0, 'Countries', 'ISO 3166 country codes',
     'dq_countries_artefact_tbl', 'refdata_countries_tbl', 'refdata.v1.countries.publish-from-dq', 2,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'currencies', 0, 'Currencies', 'ISO 4217 currency codes',
     'dq_currencies_artefact_tbl', 'refdata_currencies_tbl', 'refdata.v1.currencies.publish-from-dq', 3,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'ip2country', 0, 'IP to Country', 'IP address to country mappings',
     'dq_ip2country_artefact_tbl', 'geo_ip2country_tbl', 'dq.v1.ip2country.publish-from-dq', 4,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'coding_schemes', 0, 'Coding Schemes', 'Coding scheme definitions',
     'dq_coding_schemes_artefact_tbl', 'dq_coding_schemes_tbl', 'dq.v1.coding-schemes.publish-from-dq', 5,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'account_types', 0, 'Account Types', 'FpML account type codes',
     'dq_account_types_artefact_tbl', 'refdata_account_types_tbl', 'refdata.v1.account-types.publish-from-dq', 10,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'asset_classes', 0, 'Asset Classes', 'FpML asset class codes',
     'dq_asset_classes_artefact_tbl', 'refdata_asset_classes_tbl', 'refdata.v1.asset-classes.publish-from-dq', 11,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'asset_measures', 0, 'Asset Measures', 'FpML asset measure codes',
     'dq_asset_measures_artefact_tbl', 'refdata_asset_measures_tbl', 'refdata.v1.asset-measures.publish-from-dq', 12,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'benchmark_rates', 0, 'Benchmark Rates', 'FpML benchmark rate codes',
     'dq_benchmark_rates_artefact_tbl', 'refdata_benchmark_rates_tbl', 'refdata.v1.benchmark-rates.publish-from-dq', 13,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'business_centres', 0, 'Business Centres', 'FpML business centre codes',
     'dq_business_centres_artefact_tbl', 'refdata_business_centres_tbl', 'refdata.v1.business-centres.publish-from-dq', 14,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'business_processes', 0, 'Business Processes', 'FpML business process codes',
     'dq_business_processes_artefact_tbl', 'refdata_business_processes_tbl', 'refdata.v1.business-processes.publish-from-dq', 15,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'cashflow_types', 0, 'Cashflow Types', 'FpML cashflow type codes',
     'dq_cashflow_types_artefact_tbl', 'refdata_cashflow_types_tbl', 'refdata.v1.cashflow-types.publish-from-dq', 16,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'entity_classifications', 0, 'Entity Classifications', 'FpML entity classification codes',
     'dq_entity_classifications_artefact_tbl', 'refdata_entity_classifications_tbl', 'refdata.v1.entity-classifications.publish-from-dq', 17,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'local_jurisdictions', 0, 'Local Jurisdictions', 'FpML local jurisdiction codes',
     'dq_local_jurisdictions_artefact_tbl', 'refdata_local_jurisdictions_tbl', 'refdata.v1.local-jurisdictions.publish-from-dq', 18,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'party_relationships', 0, 'Party Relationships', 'FpML party relationship codes',
     'dq_party_relationships_artefact_tbl', 'refdata_party_relationships_tbl', 'refdata.v1.party-relationships.publish-from-dq', 19,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'party_roles', 0, 'Party Roles', 'FpML party role codes',
     'dq_party_roles_artefact_tbl', 'refdata_party_roles_tbl', 'refdata.v1.party-roles.publish-from-dq', 20,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'person_roles', 0, 'Person Roles', 'FpML person role codes',
     'dq_person_roles_artefact_tbl', 'refdata_person_roles_tbl', 'refdata.v1.person-roles.publish-from-dq', 21,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'regulatory_corporate_sectors', 0, 'Regulatory Corporate Sectors', 'FpML regulatory corporate sector codes',
     'dq_regulatory_corporate_sectors_artefact_tbl', 'refdata_regulatory_corporate_sectors_tbl', 'refdata.v1.regulatory-corporate-sectors.publish-from-dq', 22,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'reporting_regimes', 0, 'Reporting Regimes', 'FpML reporting regime codes',
     'dq_reporting_regimes_artefact_tbl', 'refdata_reporting_regimes_tbl', 'refdata.v1.reporting-regimes.publish-from-dq', 23,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'supervisory_bodies', 0, 'Supervisory Bodies', 'FpML supervisory body codes',
     'dq_supervisory_bodies_artefact_tbl', 'refdata_supervisory_bodies_tbl', 'refdata.v1.supervisory-bodies.publish-from-dq', 24,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'lei_entities', 0, 'LEI Entities', 'GLEIF LEI entity master data',
     'dq_lei_entities_artefact_tbl', null, null, 25,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'lei_relationships', 0, 'LEI Relationships', 'GLEIF LEI corporate hierarchy relationships',
     'dq_lei_relationships_artefact_tbl', null, null, 26,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'lei_counterparties', 0, 'LEI Counterparties', 'GLEIF LEI entities published as counterparties',
     'dq_lei_entities_artefact_tbl', 'refdata_counterparties_tbl', 'refdata.v1.lei-counterparties.publish-from-dq', 27,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'lei_parties', 0, 'LEI Parties', 'GLEIF LEI entities published as parties (subtree)',
     'dq_lei_entities_artefact_tbl', 'refdata_parties_tbl', 'refdata.v1.lei-parties.publish-from-dq', 28,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'lei_bic', 0, 'LEI BIC', 'GLEIF LEI to BIC identifier mappings',
     'dq_lei_bic_artefact_tbl', null, null, 29,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'business_units', 0, 'Business Units', 'Organisational business units',
     'dq_business_units_artefact_tbl', 'refdata_business_units_tbl', 'refdata.v1.business-units.publish-from-dq', 30,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'portfolios', 0, 'Portfolios', 'Portfolio hierarchy nodes',
     'dq_portfolios_artefact_tbl', 'refdata_portfolios_tbl', 'refdata.v1.portfolios.publish-from-dq', 31,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'books', 0, 'Books', 'Trading and banking books',
     'dq_books_artefact_tbl', 'refdata_books_tbl', 'refdata.v1.books.publish-from-dq', 32,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'report_definitions', 0, 'Report Definitions', 'ORE analytics report definition templates',
     'dq_report_definitions_artefact_tbl', 'reporting_report_definitions_tbl', 'reporting.v1.report-definitions.publish-from-dq', 33,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'synthetic_fx_spot_configs', 0, 'Synthetic FX Spot Configs', 'Synthetic FX spot generation configs (writes both market_data_generation_configs and fx_spot_generation_configs)',
     'dq_synthetic_fx_spot_configs_artefact_tbl', 'synthetic_market_data_generation_configs_tbl', 'synthetic.v1.fx-spot-configs.publish-from-dq', 34,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'currency_pairs', 0, 'Currency Pairs', 'Standard FX currency pairs (majors, minors, and common EM crosses)',
     'dq_currency_pairs_artefact_tbl', 'refdata_currency_pairs_tbl', 'refdata.v1.currency-pairs.publish-from-dq', 35,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'currency_pair_conventions', 0, 'Currency Pair Conventions', 'Quoting and date conventions (pip factor, tick size, calendars) for standard FX currency pairs',
     'dq_currency_pair_conventions_artefact_tbl', 'refdata_currency_pair_conventions_tbl', 'refdata.v1.currency-pair-conventions.publish-from-dq', 36,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'market_data_observations', 0, 'Market Data Observations', 'Curated FX driver rates (Federal Reserve H.10) for a fixed as-of date',
     'dq_market_data_observations_artefact_tbl', 'marketdata_market_observations_tbl', 'marketdata.v1.market-data-observations.publish-from-dq', 37,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'crm_topology_bundles', 0, 'CRM Topology Bundles', 'Named CRM (Cross-Rates Matrix) topology configs, driver pairs, and curated derived pairs (writes crm_topology_configs, crm_driver_pairs, and crm_enabled_derived_pairs)',
     'dq_crm_topology_bundles_artefact_tbl', 'refdata_crm_topology_configs_tbl', 'refdata.v1.crm-topology-bundles.publish-from-dq', 38,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'calendar_types', 0, 'Calendar Types', 'Calendar type classification (public holiday, central bank meeting, financial centre, data release, other)',
     'dq_calendar_types_artefact_tbl', 'refdata_calendar_types_tbl', 'refdata.v1.calendar-types.publish-from-dq', 39,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'calendars', 0, 'Calendars', 'ORE/QuantLib calendar reference data: business-day/holiday calendars, financial-centre calendars, and central-bank meeting calendars',
     'dq_calendars_artefact_tbl', 'refdata_calendars_tbl', 'refdata.v1.calendars.publish-from-dq', 40,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'currency_calendars', 0, 'Currency Calendars', 'Currency-to-calendar spot/settlement calendar assignments',
     'dq_currency_calendars_artefact_tbl', 'refdata_currency_calendars_tbl', 'refdata.v1.currency-calendars.publish-from-dq', 60,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'currency_pair_convention_calendars', 0, 'Currency Pair Convention Calendars', 'Currency-pair-convention-to-calendar advance-calendar assignments',
     'dq_currency_pair_convention_calendars_artefact_tbl', 'refdata_currency_pair_convention_calendars_tbl', 'refdata.v1.currency-pair-convention-calendars.publish-from-dq', 61,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'payment_frequencies', 0, 'Payment Frequencies', 'ORE''s canonical frequencyType enumeration (Annual, Semiannual, Quarterly, Monthly...)',
     'dq_payment_frequencies_artefact_tbl', 'refdata_payment_frequencies_tbl', 'refdata.v1.payment-frequencies.publish-from-dq', 41,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'badge_severities', 0, 'Badge Severities', 'Badge severity levels (secondary/info/success/warning/danger/primary), self-published into DQ''s own table like coding_schemes',
     'dq_badge_severities_artefact_tbl', 'dq_badge_severities_tbl', 'dq.v1.badge-severities.publish-from-dq', 50,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'badge_definitions', 0, 'Badge Definitions', 'Visual badge definitions (colour, severity, CSS class), self-published into DQ''s own table like coding_schemes/badge_severities',
     'dq_badge_definitions_artefact_tbl', 'dq_badge_definitions_tbl', 'dq.v1.badge-definitions.publish-from-dq', 51,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'code_domains', 0, 'Code Domains', 'Named namespaces for disambiguating enum codes across entity types, self-published into DQ''s own table like coding_schemes/badge_severities/badge_definitions',
     'dq_code_domains_artefact_tbl', 'dq_code_domains_tbl', 'dq.v1.code-domains.publish-from-dq', 52,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'badge_mappings', 0, 'Badge Mappings', '(code_domain, entity_code) -> badge_definition mappings, self-published into DQ''s own table like coding_schemes/badge_severities/badge_definitions/code_domains',
     'dq_badge_mappings_artefact_tbl', 'dq_badge_mappings_tbl', 'dq.v1.badge-mappings.publish-from-dq', 53,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'synthetic_ir_curve_configs', 0, 'Synthetic IR Curve Configs', 'Synthetic IR curve generation configs (writes both market_data_generation_configs and ir_curve_generation_configs)',
     'dq_synthetic_ir_curve_configs_artefact_tbl', 'synthetic_market_data_generation_configs_tbl', 'synthetic.v1.ir-curve-configs.publish-from-dq', 54,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'accounts', 0, 'Accounts', 'Generated staff login accounts (e.g. Acme Bank); target/publish-from-dq wiring lands with the server-side-orchestration follow-up task',
     'dq_accounts_artefact_tbl', 'iam_accounts_tbl', 'iam.v1.accounts.publish-from-dq', 55,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types'),
    (ores_utility_system_tenant_id_fn(), 'account_contact_informations', 0, 'Account Contact Informations', 'Generated staff real names/contact details (e.g. Acme Bank); target/publish-from-dq wiring lands with the server-side-orchestration follow-up task',
     'dq_account_contact_informations_artefact_tbl', 'iam_account_contact_informations_tbl', 'iam.v1.account-contact-informations.publish-from-dq', 56,
     current_user, current_user, 'system.initial_load', 'Initial population of artefact types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- =============================================================================
-- Backfill: correct crm_topology_bundles rows seeded before the refdata
-- reclassification fix (subject/target_table wrongly reverted to
-- marketdata_* by a later, unrelated commit). The insert above is a
-- no-op on any database where this row already exists, so any
-- environment provisioned before this fix landed needs this explicit
-- correction to actually pick it up.
-- =============================================================================

insert into ores_dq_artefact_types_tbl (
    tenant_id, code, version, name, description, artefact_table, target_table, target_subject, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
select
    tenant_id, code, version, name, description, artefact_table,
    'refdata_crm_topology_configs_tbl', 'refdata.v1.crm-topology-bundles.publish-from-dq', display_order,
    current_user, current_user, 'system.admin_reset',
    'Backfill: correct crm_topology_bundles target_table/target_subject reverted to marketdata_* by an unrelated commit'
from ores_dq_artefact_types_tbl
where code = 'crm_topology_bundles'
  and valid_to = ores_utility_infinity_timestamp_fn()
  and target_subject <> 'refdata.v1.crm-topology-bundles.publish-from-dq';

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'DQ Artefact Types' as entity, count(*) as count
from ores_dq_artefact_types_tbl
order by entity;
