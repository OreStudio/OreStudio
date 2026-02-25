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
-- Drop Row-Level Security Policies for Reference Data Tables
-- =============================================================================
-- Must be dropped before the corresponding tables are dropped.

-- Books
drop policy if exists ores_refdata_books_tenant_isolation_policy on "ores_refdata_books_tbl";

-- Portfolios
drop policy if exists ores_refdata_portfolios_tenant_isolation_policy on "ores_refdata_portfolios_tbl";

-- Business Units
drop policy if exists ores_refdata_business_units_tenant_isolation_policy on "ores_refdata_business_units_tbl";

-- Party Counterparties (dual RLS: tenant + party)
drop policy if exists ores_refdata_party_counterparties_party_isolation_policy on "ores_refdata_party_counterparties_tbl";
drop policy if exists ores_refdata_party_counterparties_tenant_isolation_policy on "ores_refdata_party_counterparties_tbl";

-- Counterparty Contact Informations
drop policy if exists ores_refdata_counterparty_contact_informations_tenant_isolation_policy on "ores_refdata_counterparty_contact_informations_tbl";

-- Counterparty Identifiers
drop policy if exists ores_refdata_counterparty_identifiers_tenant_isolation_policy on "ores_refdata_counterparty_identifiers_tbl";

-- Counterparties
drop policy if exists ores_refdata_counterparties_tenant_isolation_policy on "ores_refdata_counterparties_tbl";

-- Party Contact Informations
drop policy if exists ores_refdata_party_contact_informations_tenant_isolation_policy on "ores_refdata_party_contact_informations_tbl";

-- Party Identifiers
drop policy if exists ores_refdata_party_identifiers_tenant_isolation_policy on "ores_refdata_party_identifiers_tbl";

-- Parties
drop policy if exists ores_refdata_parties_tenant_isolation_policy on "ores_refdata_parties_tbl";

-- Supervisory Bodies
drop policy if exists ores_refdata_supervisory_bodies_tenant_isolation_policy on "ores_refdata_supervisory_bodies_tbl";

-- Reporting Regimes
drop policy if exists ores_refdata_reporting_regimes_tenant_isolation_policy on "ores_refdata_reporting_regimes_tbl";

-- Regulatory Corporate Sectors
drop policy if exists ores_refdata_regulatory_corporate_sectors_tenant_isolation_policy on "ores_refdata_regulatory_corporate_sectors_tbl";

-- Person Roles
drop policy if exists ores_refdata_person_roles_tenant_isolation_policy on "ores_refdata_person_roles_tbl";

-- Party Roles
drop policy if exists ores_refdata_party_roles_tenant_isolation_policy on "ores_refdata_party_roles_tbl";

-- Party Relationships
drop policy if exists ores_refdata_party_relationships_tenant_isolation_policy on "ores_refdata_party_relationships_tbl";

-- Local Jurisdictions
drop policy if exists ores_refdata_local_jurisdictions_tenant_isolation_policy on "ores_refdata_local_jurisdictions_tbl";

-- Entity Classifications
drop policy if exists ores_refdata_entity_classifications_tenant_isolation_policy on "ores_refdata_entity_classifications_tbl";

-- Cashflow Types
drop policy if exists ores_refdata_cashflow_types_tenant_isolation_policy on "ores_refdata_cashflow_types_tbl";

-- Business Processes
drop policy if exists ores_refdata_business_processes_tenant_isolation_policy on "ores_refdata_business_processes_tbl";

-- Business Centres
drop policy if exists ores_refdata_business_centres_tenant_isolation_policy on "ores_refdata_business_centres_tbl";

-- Benchmark Rates
drop policy if exists ores_refdata_benchmark_rates_tenant_isolation_policy on "ores_refdata_benchmark_rates_tbl";

-- Asset Measures
drop policy if exists ores_refdata_asset_measures_tenant_isolation_policy on "ores_refdata_asset_measures_tbl";

-- Asset Classes
drop policy if exists ores_refdata_asset_classes_tenant_isolation_policy on "ores_refdata_asset_classes_tbl";

-- Account Types
drop policy if exists ores_refdata_account_types_tenant_isolation_policy on "ores_refdata_account_types_tbl";

-- Countries
drop policy if exists ores_refdata_countries_tenant_isolation_policy on "ores_refdata_countries_tbl";

-- Currencies
drop policy if exists ores_refdata_currencies_tenant_isolation_policy on "ores_refdata_currencies_tbl";

-- Currency Market Tiers
drop policy if exists ores_refdata_currency_market_tiers_tenant_isolation_policy on "ores_refdata_currency_market_tiers_tbl";

-- Monetary Natures
drop policy if exists ores_refdata_monetary_natures_tenant_isolation_policy on "ores_refdata_monetary_natures_tbl";
