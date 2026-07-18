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
-- Monetary Natures
-- -----------------------------------------------------------------------------
alter table ores_refdata_monetary_natures_tbl enable row level security;

create policy monetary_natures_tenant_isolation_policy on ores_refdata_monetary_natures_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Currency Market Tiers
-- -----------------------------------------------------------------------------
alter table ores_refdata_currency_market_tiers_tbl enable row level security;

create policy currency_market_tiers_tenant_isolation_policy on ores_refdata_currency_market_tiers_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Zero Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_zero_conventions_tbl enable row level security;

create policy zero_conventions_tenant_isolation_policy on ores_refdata_zero_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Deposit Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_deposit_conventions_tbl enable row level security;

create policy deposit_conventions_tenant_isolation_policy on ores_refdata_deposit_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Swap Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_swap_conventions_tbl enable row level security;

create policy swap_conventions_tenant_isolation_policy on ores_refdata_swap_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- OIS Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_ois_conventions_tbl enable row level security;

create policy ois_conventions_tenant_isolation_policy on ores_refdata_ois_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- FRA Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_fra_conventions_tbl enable row level security;

create policy fra_conventions_tenant_isolation_policy on ores_refdata_fra_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- IBOR Index Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_ibor_index_conventions_tbl enable row level security;

create policy ibor_index_conventions_tenant_isolation_policy on ores_refdata_ibor_index_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Overnight Index Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_overnight_index_conventions_tbl enable row level security;

create policy overnight_index_conventions_tenant_isolation_policy on ores_refdata_overnight_index_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Currency Pair Classifications
-- -----------------------------------------------------------------------------
alter table ores_refdata_currency_pair_classifications_tbl enable row level security;

create policy currency_pair_classifications_tenant_isolation_policy
on ores_refdata_currency_pair_classifications_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Currency Groups
-- -----------------------------------------------------------------------------
alter table ores_refdata_currency_groups_tbl enable row level security;

create policy currency_groups_tenant_isolation_policy on ores_refdata_currency_groups_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Currency Currency Groups (junction)
-- -----------------------------------------------------------------------------
alter table ores_refdata_currency_currency_groups_tbl enable row level security;

create policy currency_currency_groups_tenant_isolation_policy
on ores_refdata_currency_currency_groups_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Currency Pairs
-- -----------------------------------------------------------------------------
alter table ores_refdata_currency_pairs_tbl enable row level security;

create policy currency_pairs_tenant_isolation_policy on ores_refdata_currency_pairs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Currency Pair Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_currency_pair_conventions_tbl enable row level security;

create policy currency_pair_conventions_tenant_isolation_policy
on ores_refdata_currency_pair_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- CDS Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_cds_conventions_tbl enable row level security;

create policy cds_conventions_tenant_isolation_policy on ores_refdata_cds_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Currencies
-- -----------------------------------------------------------------------------
alter table ores_refdata_currencies_tbl enable row level security;

create policy currencies_tenant_isolation_policy on ores_refdata_currencies_tbl
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

create policy countries_tenant_isolation_policy on ores_refdata_countries_tbl
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

create policy account_types_tenant_isolation_policy on ores_refdata_account_types_tbl
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

create policy asset_classes_tenant_isolation_policy on ores_refdata_asset_classes_tbl
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

create policy asset_measures_tenant_isolation_policy on ores_refdata_asset_measures_tbl
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

create policy benchmark_rates_tenant_isolation_policy on ores_refdata_benchmark_rates_tbl
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

create policy business_centres_tenant_isolation_policy on ores_refdata_business_centres_tbl
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

create policy business_processes_tenant_isolation_policy on ores_refdata_business_processes_tbl
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

create policy cashflow_types_tenant_isolation_policy on ores_refdata_cashflow_types_tbl
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

create policy entity_classifications_tenant_isolation_policy on ores_refdata_entity_classifications_tbl
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

create policy local_jurisdictions_tenant_isolation_policy on ores_refdata_local_jurisdictions_tbl
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

create policy party_relationships_tenant_isolation_policy on ores_refdata_party_relationships_tbl
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

create policy party_roles_tenant_isolation_policy on ores_refdata_party_roles_tbl
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

create policy person_roles_tenant_isolation_policy on ores_refdata_person_roles_tbl
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

create policy regulatory_corporate_sectors_tenant_isolation_policy on ores_refdata_regulatory_corporate_sectors_tbl
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

create policy reporting_regimes_tenant_isolation_policy on ores_refdata_reporting_regimes_tbl
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

create policy supervisory_bodies_tenant_isolation_policy on ores_refdata_supervisory_bodies_tbl
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

create policy parties_tenant_isolation_policy on ores_refdata_parties_tbl
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

create policy party_identifiers_tenant_isolation_policy on ores_refdata_party_identifiers_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement — no party context means no rows visible.
-- FOR SELECT only: party_id FK validated by trigger; WITH CHECK would block
-- bulk inserts from the publisher.
create policy party_identifiers_party_isolation_policy
on ores_refdata_party_identifiers_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Party Contact Informations
-- -----------------------------------------------------------------------------
alter table ores_refdata_party_contact_informations_tbl enable row level security;

create policy party_contact_informations_tenant_isolation_policy on ores_refdata_party_contact_informations_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement — no party context means no rows visible.
-- FOR SELECT only: party_id FK validated by trigger; WITH CHECK would block
-- bulk inserts from the publisher.
create policy party_contact_informations_party_isolation_policy
on ores_refdata_party_contact_informations_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Counterparties
-- -----------------------------------------------------------------------------
alter table ores_refdata_counterparties_tbl enable row level security;

create policy counterparties_tenant_isolation_policy on ores_refdata_counterparties_tbl
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

create policy counterparty_identifiers_tenant_isolation_policy on ores_refdata_counterparty_identifiers_tbl
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

create policy counterparty_contact_informations_tenant_isolation_policy on ores_refdata_counterparty_contact_informations_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Business Units
-- -----------------------------------------------------------------------------
alter table ores_refdata_business_units_tbl enable row level security;

create policy business_units_tenant_isolation_policy on ores_refdata_business_units_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement — no party context means no rows visible.
-- FOR SELECT only: party_id FK validated by trigger; WITH CHECK would block
-- bulk inserts from the publisher.
create policy business_units_party_isolation_policy
on ores_refdata_business_units_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Party Countries
-- -----------------------------------------------------------------------------
alter table ores_refdata_party_countries_tbl enable row level security;

create policy party_countries_tenant_isolation_policy on ores_refdata_party_countries_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement — no party context means no rows visible.
-- FOR SELECT only: party_id is part of the PK; WITH CHECK would block bulk
-- inserts from the publisher.
create policy party_countries_party_isolation_policy
on ores_refdata_party_countries_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Party Currencies
-- -----------------------------------------------------------------------------
alter table ores_refdata_party_currencies_tbl enable row level security;

create policy party_currencies_tenant_isolation_policy on ores_refdata_party_currencies_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement — no party context means no rows visible.
-- FOR SELECT only: party_id is part of the PK; WITH CHECK would block bulk
-- inserts from the publisher.
create policy party_currencies_party_isolation_policy
on ores_refdata_party_currencies_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Portfolios
-- -----------------------------------------------------------------------------
alter table ores_refdata_portfolios_tbl enable row level security;

create policy portfolios_tenant_isolation_policy on ores_refdata_portfolios_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement — no party context means no rows visible.
-- x = ANY(NULL) evaluates to NULL (falsy) when no party context is set.
-- FOR SELECT only: the trigger validates party_id FK on INSERT/UPDATE, so
-- WITH CHECK is not needed and would block bulk inserts from the publisher.
create policy portfolios_party_isolation_policy
on ores_refdata_portfolios_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Books
-- -----------------------------------------------------------------------------
alter table ores_refdata_books_tbl enable row level security;

create policy books_tenant_isolation_policy on ores_refdata_books_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: strict enforcement — no party context means no rows visible.
-- FOR SELECT only: the trigger validates party_id FK on INSERT/UPDATE, so
-- WITH CHECK is not needed and would block bulk inserts from the publisher.
create policy books_party_isolation_policy
on ores_refdata_books_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Party Counterparties (dual RLS: tenant + party isolation)
-- -----------------------------------------------------------------------------
alter table ores_refdata_party_counterparties_tbl enable row level security;

-- Tenant isolation (standard pattern)
create policy party_counterparties_tenant_isolation_policy
on ores_refdata_party_counterparties_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation (RESTRICTIVE — ANDed with the permissive tenant policy).
-- When no party context is set (visible_party_ids is NULL), the policy
-- passes through, preserving backward compatibility. When party context IS
-- set, only rows matching the visible party set are accessible.
create policy party_counterparties_party_isolation_policy
on ores_refdata_party_counterparties_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Book Purpose Types
-- -----------------------------------------------------------------------------
alter table ores_refdata_book_purpose_types_tbl enable row level security;

create policy book_purpose_types_tenant_isolation_policy on ores_refdata_book_purpose_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Ledger Feed Types
-- -----------------------------------------------------------------------------
alter table ores_refdata_ledger_feed_types_tbl enable row level security;

create policy ledger_feed_types_tenant_isolation_policy on ores_refdata_ledger_feed_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Calendar Types
-- -----------------------------------------------------------------------------
alter table ores_refdata_calendar_types_tbl enable row level security;

create policy calendar_types_tenant_isolation_policy on ores_refdata_calendar_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Calendars
-- -----------------------------------------------------------------------------
alter table ores_refdata_calendars_tbl enable row level security;

create policy calendars_tenant_isolation_policy on ores_refdata_calendars_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Currency Countries
-- -----------------------------------------------------------------------------
alter table ores_refdata_currency_countries_tbl enable row level security;

create policy currency_countries_tenant_isolation_policy on ores_refdata_currency_countries_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Currency Calendars
-- -----------------------------------------------------------------------------
alter table ores_refdata_currency_calendars_tbl enable row level security;

create policy currency_calendars_tenant_isolation_policy on ores_refdata_currency_calendars_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Currency Pair Convention Calendars
-- -----------------------------------------------------------------------------
alter table ores_refdata_currency_pair_convention_calendars_tbl enable row level security;

create policy currency_pair_convention_calendars_tenant_isolation_policy
on ores_refdata_currency_pair_convention_calendars_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- CRM topology configs (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_crm_topology_configs_tbl enable row level security;

create policy crm_topology_configs_tbl_tenant_isolation_policy
on ores_refdata_crm_topology_configs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- ores.marketdata.service's crm_ingest_bridge builds one rate_engine per
-- (tenant, party) at startup/refresh via read_latest_all_tenants(), which
-- has no single tenant session context to scope to -- a second, permissive,
-- SELECT-only policy for that role's read-only role is OR'd with the
-- tenant-isolation policy above, so it can see every tenant's rows without
-- weakening the default-deny-cross-tenant behaviour for any other role.
create policy crm_topology_configs_tbl_marketdata_cross_tenant_read_policy
on ores_refdata_crm_topology_configs_tbl
for select
to :marketdata_service_user
using (true);

-- Party isolation: strict enforcement — no party context means no rows
-- visible. Scoped to refdata_service_user (the normal party-scoped
-- consumer for CRUD reads/writes on this table) so it does not also
-- restrict marketdata_service_user's documented cross-tenant, cross-party
-- read above. FOR SELECT only: party_id FK validated by trigger; WITH
-- CHECK would block bulk inserts from the publisher.
create policy crm_topology_configs_tbl_party_isolation_policy
on ores_refdata_crm_topology_configs_tbl
as restrictive
for select
to :refdata_service_user
using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- CRM driver pairs (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_crm_driver_pairs_tbl enable row level security;

create policy crm_driver_pairs_tbl_tenant_isolation_policy
on ores_refdata_crm_driver_pairs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- See crm_topology_configs_tbl_marketdata_cross_tenant_read_policy above.
create policy crm_driver_pairs_tbl_marketdata_cross_tenant_read_policy
on ores_refdata_crm_driver_pairs_tbl
for select
to :marketdata_service_user
using (true);

-- See crm_topology_configs_tbl_party_isolation_policy above.
create policy crm_driver_pairs_tbl_party_isolation_policy
on ores_refdata_crm_driver_pairs_tbl
as restrictive
for select
to :refdata_service_user
using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Asset Class Codes (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_asset_class_codes_tbl enable row level security;

create policy asset_class_codes_tbl_tenant_isolation_policy
on ores_refdata_asset_class_codes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Instrument Codes (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_instrument_codes_tbl enable row level security;

create policy instrument_codes_tbl_tenant_isolation_policy
on ores_refdata_instrument_codes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenor Anchors (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_tenor_anchors_tbl enable row level security;

create policy tenor_anchors_tbl_tenant_isolation_policy
on ores_refdata_tenor_anchors_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenor Kinds (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_tenor_kinds_tbl enable row level security;

create policy tenor_kinds_tbl_tenant_isolation_policy
on ores_refdata_tenor_kinds_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenor Units (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_tenor_units_tbl enable row level security;

create policy tenor_units_tbl_tenant_isolation_policy
on ores_refdata_tenor_units_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenor Resolution Algorithms (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_tenor_resolution_algorithms_tbl enable row level security;

create policy tenor_resolution_algorithms_tbl_tenant_isolation_policy
on ores_refdata_tenor_resolution_algorithms_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Payment Frequencies (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_payment_frequencies_tbl enable row level security;

create policy payment_frequencies_tbl_tenant_isolation_policy
on ores_refdata_payment_frequencies_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenors (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_tenors_tbl enable row level security;

create policy tenors_tbl_tenant_isolation_policy
on ores_refdata_tenors_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenor Conventions (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_tenor_conventions_tbl enable row level security;

create policy tenor_conventions_tbl_tenant_isolation_policy
on ores_refdata_tenor_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenor Convention Resolutions (hand-authored junction table -- codegen does not yet generate SQL for junctions)
-- -----------------------------------------------------------------------------
alter table ores_refdata_tenor_convention_resolutions_tbl enable row level security;

create policy tenor_convention_resolutions_tbl_tenant_isolation_policy
on ores_refdata_tenor_convention_resolutions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- CRM enabled derived pairs (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_crm_enabled_derived_pairs_tbl enable row level security;

create policy crm_enabled_derived_pairs_tbl_tenant_isolation_policy
on ores_refdata_crm_enabled_derived_pairs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- See crm_topology_configs_tbl_marketdata_cross_tenant_read_policy above.
create policy crm_enabled_derived_pairs_tbl_marketdata_cross_tenant_read_policy
on ores_refdata_crm_enabled_derived_pairs_tbl
for select
to :marketdata_service_user
using (true);

-- See crm_topology_configs_tbl_party_isolation_policy above.
create policy crm_enabled_derived_pairs_tbl_party_isolation_policy
on ores_refdata_crm_enabled_derived_pairs_tbl
as restrictive
for select
to :refdata_service_user
using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Curve Roles
-- -----------------------------------------------------------------------------
alter table ores_refdata_curve_roles_tbl enable row level security;

create policy curve_roles_tbl_tenant_isolation_policy on ores_refdata_curve_roles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Floating Index Types (moved from ores.trading)
-- -----------------------------------------------------------------------------
alter table ores_refdata_floating_index_types_tbl enable row level security;

create policy floating_index_types_tbl_tenant_isolation_policy
on ores_refdata_floating_index_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Leg Types (moved from ores.trading)
-- -----------------------------------------------------------------------------
alter table ores_refdata_leg_types_tbl enable row level security;

create policy leg_types_tbl_tenant_isolation_policy on ores_refdata_leg_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
