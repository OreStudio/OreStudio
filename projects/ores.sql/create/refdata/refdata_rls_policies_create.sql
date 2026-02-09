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
-- Row-Level Security Policies for Reference Data Tables
-- =============================================================================
-- These policies enforce strict tenant isolation for reference data.
-- Each tenant maintains their own copy of reference data (currencies, countries,
-- etc.) and can only see and modify their own records. All tenants are fully
-- isolated, including the system tenant.

-- -----------------------------------------------------------------------------
-- Currencies
-- -----------------------------------------------------------------------------
alter table ores_refdata_currencies_tbl enable row level security;

create policy ores_refdata_currencies_tenant_isolation_policy on ores_refdata_currencies_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Countries
-- -----------------------------------------------------------------------------
alter table ores_refdata_countries_tbl enable row level security;

create policy ores_refdata_countries_tenant_isolation_policy on ores_refdata_countries_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Account Types
-- -----------------------------------------------------------------------------
alter table ores_refdata_account_types_tbl enable row level security;

create policy ores_refdata_account_types_tenant_isolation_policy on ores_refdata_account_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Asset Classes
-- -----------------------------------------------------------------------------
alter table ores_refdata_asset_classes_tbl enable row level security;

create policy ores_refdata_asset_classes_tenant_isolation_policy on ores_refdata_asset_classes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Asset Measures
-- -----------------------------------------------------------------------------
alter table ores_refdata_asset_measures_tbl enable row level security;

create policy ores_refdata_asset_measures_tenant_isolation_policy on ores_refdata_asset_measures_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Benchmark Rates
-- -----------------------------------------------------------------------------
alter table ores_refdata_benchmark_rates_tbl enable row level security;

create policy ores_refdata_benchmark_rates_tenant_isolation_policy on ores_refdata_benchmark_rates_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Business Centres
-- -----------------------------------------------------------------------------
alter table ores_refdata_business_centres_tbl enable row level security;

create policy ores_refdata_business_centres_tenant_isolation_policy on ores_refdata_business_centres_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Business Processes
-- -----------------------------------------------------------------------------
alter table ores_refdata_business_processes_tbl enable row level security;

create policy ores_refdata_business_processes_tenant_isolation_policy on ores_refdata_business_processes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Cashflow Types
-- -----------------------------------------------------------------------------
alter table ores_refdata_cashflow_types_tbl enable row level security;

create policy ores_refdata_cashflow_types_tenant_isolation_policy on ores_refdata_cashflow_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Entity Classifications
-- -----------------------------------------------------------------------------
alter table ores_refdata_entity_classifications_tbl enable row level security;

create policy ores_refdata_entity_classifications_tenant_isolation_policy on ores_refdata_entity_classifications_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Local Jurisdictions
-- -----------------------------------------------------------------------------
alter table ores_refdata_local_jurisdictions_tbl enable row level security;

create policy ores_refdata_local_jurisdictions_tenant_isolation_policy on ores_refdata_local_jurisdictions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Party Relationships
-- -----------------------------------------------------------------------------
alter table ores_refdata_party_relationships_tbl enable row level security;

create policy ores_refdata_party_relationships_tenant_isolation_policy on ores_refdata_party_relationships_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Party Roles
-- -----------------------------------------------------------------------------
alter table ores_refdata_party_roles_tbl enable row level security;

create policy ores_refdata_party_roles_tenant_isolation_policy on ores_refdata_party_roles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Person Roles
-- -----------------------------------------------------------------------------
alter table ores_refdata_person_roles_tbl enable row level security;

create policy ores_refdata_person_roles_tenant_isolation_policy on ores_refdata_person_roles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Regulatory Corporate Sectors
-- -----------------------------------------------------------------------------
alter table ores_refdata_regulatory_corporate_sectors_tbl enable row level security;

create policy ores_refdata_regulatory_corporate_sectors_tenant_isolation_policy on ores_refdata_regulatory_corporate_sectors_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Reporting Regimes
-- -----------------------------------------------------------------------------
alter table ores_refdata_reporting_regimes_tbl enable row level security;

create policy ores_refdata_reporting_regimes_tenant_isolation_policy on ores_refdata_reporting_regimes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Supervisory Bodies
-- -----------------------------------------------------------------------------
alter table ores_refdata_supervisory_bodies_tbl enable row level security;

create policy ores_refdata_supervisory_bodies_tenant_isolation_policy on ores_refdata_supervisory_bodies_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Parties
-- -----------------------------------------------------------------------------
alter table ores_refdata_parties_tbl enable row level security;

create policy ores_refdata_parties_tenant_isolation_policy on ores_refdata_parties_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Party Identifiers
-- -----------------------------------------------------------------------------
alter table ores_refdata_party_identifiers_tbl enable row level security;

create policy ores_refdata_party_identifiers_tenant_isolation_policy on ores_refdata_party_identifiers_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Party Contact Informations
-- -----------------------------------------------------------------------------
alter table ores_refdata_party_contact_informations_tbl enable row level security;

create policy ores_refdata_party_contact_informations_tenant_isolation_policy on ores_refdata_party_contact_informations_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Counterparties
-- -----------------------------------------------------------------------------
alter table ores_refdata_counterparties_tbl enable row level security;

create policy ores_refdata_counterparties_tenant_isolation_policy on ores_refdata_counterparties_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Counterparty Identifiers
-- -----------------------------------------------------------------------------
alter table ores_refdata_counterparty_identifiers_tbl enable row level security;

create policy ores_refdata_counterparty_identifiers_tenant_isolation_policy on ores_refdata_counterparty_identifiers_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Counterparty Contact Informations
-- -----------------------------------------------------------------------------
alter table ores_refdata_counterparty_contact_informations_tbl enable row level security;

create policy ores_refdata_counterparty_contact_informations_tenant_isolation_policy on ores_refdata_counterparty_contact_informations_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
