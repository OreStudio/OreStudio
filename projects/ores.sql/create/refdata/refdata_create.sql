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

-- Lookup tables (no bitemporal support - static configuration)
\ir ./refdata_rounding_types_create.sql

-- Core reference data tables
\ir ./refdata_currencies_create.sql
\ir ./refdata_currencies_notify_trigger_create.sql
\ir ./refdata_countries_create.sql
\ir ./refdata_countries_notify_trigger_create.sql

-- FPML Reference data tables
\ir ./refdata_account_types_create.sql
\ir ./refdata_account_types_notify_trigger_create.sql
\ir ./refdata_asset_classes_create.sql
\ir ./refdata_asset_classes_notify_trigger_create.sql
\ir ./refdata_asset_measures_create.sql
\ir ./refdata_asset_measures_notify_trigger_create.sql
\ir ./refdata_benchmark_rates_create.sql
\ir ./refdata_benchmark_rates_notify_trigger_create.sql
\ir ./refdata_business_centres_create.sql
\ir ./refdata_business_centres_notify_trigger_create.sql
\ir ./refdata_business_processes_create.sql
\ir ./refdata_business_processes_notify_trigger_create.sql
\ir ./refdata_cashflow_types_create.sql
\ir ./refdata_cashflow_types_notify_trigger_create.sql
\ir ./refdata_entity_classifications_create.sql
\ir ./refdata_entity_classifications_notify_trigger_create.sql
\ir ./refdata_local_jurisdictions_create.sql
\ir ./refdata_local_jurisdictions_notify_trigger_create.sql
\ir ./refdata_party_relationships_create.sql
\ir ./refdata_party_relationships_notify_trigger_create.sql
\ir ./refdata_party_roles_create.sql
\ir ./refdata_party_roles_notify_trigger_create.sql
\ir ./refdata_person_roles_create.sql
\ir ./refdata_person_roles_notify_trigger_create.sql
\ir ./refdata_regulatory_corporate_sectors_create.sql
\ir ./refdata_regulatory_corporate_sectors_notify_trigger_create.sql
\ir ./refdata_reporting_regimes_create.sql
\ir ./refdata_reporting_regimes_notify_trigger_create.sql
\ir ./refdata_supervisory_bodies_create.sql
\ir ./refdata_supervisory_bodies_notify_trigger_create.sql

-- Party reference data tables
\ir ./refdata_party_types_create.sql
\ir ./refdata_party_types_notify_trigger_create.sql
\ir ./refdata_party_statuses_create.sql
\ir ./refdata_party_statuses_notify_trigger_create.sql
\ir ./refdata_party_id_schemes_create.sql
\ir ./refdata_party_id_schemes_notify_trigger_create.sql
\ir ./refdata_contact_types_create.sql
\ir ./refdata_contact_types_notify_trigger_create.sql

-- Party and counterparty tables
\ir ./refdata_parties_create.sql
\ir ./refdata_parties_notify_trigger_create.sql
\ir ./refdata_counterparties_create.sql
\ir ./refdata_counterparties_notify_trigger_create.sql
