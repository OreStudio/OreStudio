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

drop policy if exists monetary_natures_tenant_isolation_policy
    on ores_refdata_monetary_natures_tbl;

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

drop policy if exists currency_market_tiers_tenant_isolation_policy
    on ores_refdata_currency_market_tiers_tbl;

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

drop policy if exists zero_conventions_tenant_isolation_policy
    on ores_refdata_zero_conventions_tbl;

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

drop policy if exists deposit_conventions_tenant_isolation_policy
    on ores_refdata_deposit_conventions_tbl;

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

drop policy if exists swap_conventions_tenant_isolation_policy
    on ores_refdata_swap_conventions_tbl;

create policy swap_conventions_tenant_isolation_policy on ores_refdata_swap_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Swap Index Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_swap_index_conventions_tbl enable row level security;

drop policy if exists swap_index_conventions_tenant_isolation_policy
    on ores_refdata_swap_index_conventions_tbl;

create policy swap_index_conventions_tenant_isolation_policy
    on ores_refdata_swap_index_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Future Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_future_conventions_tbl enable row level security;

drop policy if exists future_conventions_tenant_isolation_policy
    on ores_refdata_future_conventions_tbl;

create policy future_conventions_tenant_isolation_policy
    on ores_refdata_future_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- FX Option Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_fx_option_conventions_tbl enable row level security;

drop policy if exists fx_option_conventions_tenant_isolation_policy
    on ores_refdata_fx_option_conventions_tbl;

create policy fx_option_conventions_tenant_isolation_policy
    on ores_refdata_fx_option_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Averaging OIS Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_average_ois_conventions_tbl enable row level security;

drop policy if exists average_ois_conventions_tenant_isolation_policy
    on ores_refdata_average_ois_conventions_tbl;

create policy average_ois_conventions_tenant_isolation_policy
    on ores_refdata_average_ois_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Cross-Currency Basis Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_cross_currency_basis_conventions_tbl enable row level security;

drop policy if exists cross_currency_basis_conventions_tenant_isolation_policy
    on ores_refdata_cross_currency_basis_conventions_tbl;

create policy cross_currency_basis_conventions_tenant_isolation_policy
    on ores_refdata_cross_currency_basis_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Two-Tenor Basis Swap Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_tenor_basis_two_swap_conventions_tbl enable row level security;

drop policy if exists tenor_basis_two_swap_conventions_tenant_isolation_policy
    on ores_refdata_tenor_basis_two_swap_conventions_tbl;

create policy tenor_basis_two_swap_conventions_tenant_isolation_policy
    on ores_refdata_tenor_basis_two_swap_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenor Basis Swap Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_tenor_basis_swap_conventions_tbl enable row level security;

drop policy if exists tenor_basis_swap_conventions_tenant_isolation_policy
    on ores_refdata_tenor_basis_swap_conventions_tbl;

create policy tenor_basis_swap_conventions_tenant_isolation_policy
    on ores_refdata_tenor_basis_swap_conventions_tbl
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

drop policy if exists ois_conventions_tenant_isolation_policy
    on ores_refdata_ois_conventions_tbl;

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

drop policy if exists fra_conventions_tenant_isolation_policy
    on ores_refdata_fra_conventions_tbl;

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

drop policy if exists ibor_index_conventions_tenant_isolation_policy
    on ores_refdata_ibor_index_conventions_tbl;

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

drop policy if exists overnight_index_conventions_tenant_isolation_policy
    on ores_refdata_overnight_index_conventions_tbl;

create policy overnight_index_conventions_tenant_isolation_policy on ores_refdata_overnight_index_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Zero Inflation Index Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_zero_inflation_index_conventions_tbl enable row level security;

drop policy if exists zero_inflation_index_conventions_tenant_isolation_policy
    on ores_refdata_zero_inflation_index_conventions_tbl;

create policy zero_inflation_index_conventions_tenant_isolation_policy on ores_refdata_zero_inflation_index_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- BMA Basis Swap Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_bma_basis_swap_conventions_tbl enable row level security;

drop policy if exists bma_basis_swap_conventions_tenant_isolation_policy
    on ores_refdata_bma_basis_swap_conventions_tbl;

create policy bma_basis_swap_conventions_tenant_isolation_policy on ores_refdata_bma_basis_swap_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Inflation Swap Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_inflation_swap_conventions_tbl enable row level security;

drop policy if exists inflation_swap_conventions_tenant_isolation_policy
    on ores_refdata_inflation_swap_conventions_tbl;

create policy inflation_swap_conventions_tenant_isolation_policy on ores_refdata_inflation_swap_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Cross-Currency Fix-Float Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_cross_currency_fix_float_conventions_tbl enable row level security;

drop policy if exists cross_currency_fix_float_conventions_tenant_isolation_policy
    on ores_refdata_cross_currency_fix_float_conventions_tbl;

create policy cross_currency_fix_float_conventions_tenant_isolation_policy on ores_refdata_cross_currency_fix_float_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- CMS Spread Option Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_cms_spread_option_conventions_tbl enable row level security;

drop policy if exists cms_spread_option_conventions_tenant_isolation_policy
    on ores_refdata_cms_spread_option_conventions_tbl;

create policy cms_spread_option_conventions_tenant_isolation_policy on ores_refdata_cms_spread_option_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Commodity Future Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_commodity_future_conventions_tbl enable row level security;

drop policy if exists commodity_future_conventions_tenant_isolation_policy
    on ores_refdata_commodity_future_conventions_tbl;

create policy commodity_future_conventions_tenant_isolation_policy on ores_refdata_commodity_future_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Commodity Forward Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_commodity_forward_conventions_tbl enable row level security;

drop policy if exists commodity_forward_conventions_tenant_isolation_policy
    on ores_refdata_commodity_forward_conventions_tbl;

create policy commodity_forward_conventions_tenant_isolation_policy on ores_refdata_commodity_forward_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Intraday Power Load Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_intraday_power_load_conventions_tbl enable row level security;

drop policy if exists intraday_power_load_conventions_tenant_isolation_policy
    on ores_refdata_intraday_power_load_conventions_tbl;

create policy intraday_power_load_conventions_tenant_isolation_policy on ores_refdata_intraday_power_load_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Bond Yield Conventions
-- -----------------------------------------------------------------------------
alter table ores_refdata_bond_yield_conventions_tbl enable row level security;

drop policy if exists bond_yield_conventions_tenant_isolation_policy on ores_refdata_bond_yield_conventions_tbl;

create policy bond_yield_conventions_tenant_isolation_policy on ores_refdata_bond_yield_conventions_tbl
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

drop policy if exists currency_pair_classifications_tenant_isolation_policy
    on ores_refdata_currency_pair_classifications_tbl;

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

drop policy if exists currency_groups_tenant_isolation_policy
    on ores_refdata_currency_groups_tbl;

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

drop policy if exists currency_currency_groups_tenant_isolation_policy
    on ores_refdata_currency_currency_groups_tbl;

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

drop policy if exists currency_pairs_tenant_isolation_policy
    on ores_refdata_currency_pairs_tbl;

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

drop policy if exists currency_pair_conventions_tenant_isolation_policy
    on ores_refdata_currency_pair_conventions_tbl;

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

drop policy if exists cds_conventions_tenant_isolation_policy
    on ores_refdata_cds_conventions_tbl;

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

drop policy if exists currencies_tenant_isolation_policy
    on ores_refdata_currencies_tbl;

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

drop policy if exists countries_tenant_isolation_policy
    on ores_refdata_countries_tbl;

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

drop policy if exists account_types_tenant_isolation_policy
    on ores_refdata_account_types_tbl;

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

drop policy if exists asset_classes_tenant_isolation_policy
    on ores_refdata_asset_classes_tbl;

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

drop policy if exists asset_measures_tenant_isolation_policy
    on ores_refdata_asset_measures_tbl;

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

drop policy if exists benchmark_rates_tenant_isolation_policy
    on ores_refdata_benchmark_rates_tbl;

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

drop policy if exists business_centres_tenant_isolation_policy
    on ores_refdata_business_centres_tbl;

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

drop policy if exists business_processes_tenant_isolation_policy
    on ores_refdata_business_processes_tbl;

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

drop policy if exists cashflow_types_tenant_isolation_policy
    on ores_refdata_cashflow_types_tbl;

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

drop policy if exists entity_classifications_tenant_isolation_policy
    on ores_refdata_entity_classifications_tbl;

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

drop policy if exists local_jurisdictions_tenant_isolation_policy
    on ores_refdata_local_jurisdictions_tbl;

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

drop policy if exists party_relationships_tenant_isolation_policy
    on ores_refdata_party_relationships_tbl;

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

drop policy if exists party_roles_tenant_isolation_policy
    on ores_refdata_party_roles_tbl;

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

drop policy if exists person_roles_tenant_isolation_policy
    on ores_refdata_person_roles_tbl;

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

drop policy if exists regulatory_corporate_sectors_tenant_isolation_policy
    on ores_refdata_regulatory_corporate_sectors_tbl;

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

drop policy if exists reporting_regimes_tenant_isolation_policy
    on ores_refdata_reporting_regimes_tbl;

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

drop policy if exists supervisory_bodies_tenant_isolation_policy
    on ores_refdata_supervisory_bodies_tbl;

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

-- The system tenant reads parties too. IAM resolves a party's name while
-- authenticating -- the chooser a caller picks from -- and it authenticates as
-- its own service account, which lives in the system tenant. A party row is
-- owned by the tenant that created it, so without the widening below the read
-- admits none of them and every name comes back empty. T2, the same form
-- ores_marketdata_feed_bindings_tbl states and for the same reason:
-- infrastructure reading rows it does not own.
drop policy if exists parties_tenant_isolation_policy
    on ores_refdata_parties_tbl;

create policy parties_tenant_isolation_policy on ores_refdata_parties_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Party Identifiers
-- -----------------------------------------------------------------------------
alter table ores_refdata_party_identifiers_tbl enable row level security;

drop policy if exists party_identifiers_tenant_isolation_policy
    on ores_refdata_party_identifiers_tbl;

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
drop policy if exists party_identifiers_party_isolation_policy
    on ores_refdata_party_identifiers_tbl;

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

drop policy if exists party_contact_informations_tenant_isolation_policy
    on ores_refdata_party_contact_informations_tbl;

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
drop policy if exists party_contact_informations_party_isolation_policy
    on ores_refdata_party_contact_informations_tbl;

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

drop policy if exists counterparties_tenant_isolation_policy
    on ores_refdata_counterparties_tbl;

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

drop policy if exists counterparty_identifiers_tenant_isolation_policy
    on ores_refdata_counterparty_identifiers_tbl;

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

drop policy if exists counterparty_contact_informations_tenant_isolation_policy
    on ores_refdata_counterparty_contact_informations_tbl;

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

drop policy if exists business_units_tenant_isolation_policy
    on ores_refdata_business_units_tbl;

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
drop policy if exists business_units_party_isolation_policy
    on ores_refdata_business_units_tbl;

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

drop policy if exists party_countries_tenant_isolation_policy
    on ores_refdata_party_countries_tbl;

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
drop policy if exists party_countries_party_isolation_policy
    on ores_refdata_party_countries_tbl;

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

drop policy if exists party_currencies_tenant_isolation_policy
    on ores_refdata_party_currencies_tbl;

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
drop policy if exists party_currencies_party_isolation_policy
    on ores_refdata_party_currencies_tbl;

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

drop policy if exists portfolios_tenant_isolation_policy
    on ores_refdata_portfolios_tbl;

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
drop policy if exists portfolios_party_isolation_policy
    on ores_refdata_portfolios_tbl;

create policy portfolios_party_isolation_policy
on ores_refdata_portfolios_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- Sandbox isolation: a sandbox's portfolios are read and written only by
-- those who may see the sandbox, so they never reach an official read, a
-- report run by a system process, or a writer the sandbox is not shared
-- with. Official portfolios have no sandbox and pass. FOR ALL with a check,
-- unlike party isolation: official bulk inserts carry no sandbox, so the
-- check does not touch them.
drop policy if exists portfolios_sandbox_isolation_policy
    on ores_refdata_portfolios_tbl;

create policy portfolios_sandbox_isolation_policy
on ores_refdata_portfolios_tbl
as restrictive
for all using (
    sandbox_id is null
    or ores_refdata_actor_sees_sandbox_fn(tenant_id, sandbox_id)
)
with check (
    sandbox_id is null
    or ores_refdata_actor_sees_sandbox_fn(tenant_id, sandbox_id)
);

-- -----------------------------------------------------------------------------
-- Books
-- -----------------------------------------------------------------------------
alter table ores_refdata_books_tbl enable row level security;

drop policy if exists books_tenant_isolation_policy
    on ores_refdata_books_tbl;

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
drop policy if exists books_party_isolation_policy
    on ores_refdata_books_tbl;

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
drop policy if exists party_counterparties_tenant_isolation_policy
    on ores_refdata_party_counterparties_tbl;

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
drop policy if exists party_counterparties_party_isolation_policy
    on ores_refdata_party_counterparties_tbl;

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

drop policy if exists book_purpose_types_tenant_isolation_policy
    on ores_refdata_book_purpose_types_tbl;

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

drop policy if exists ledger_feed_types_tenant_isolation_policy
    on ores_refdata_ledger_feed_types_tbl;

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

drop policy if exists calendar_types_tenant_isolation_policy
    on ores_refdata_calendar_types_tbl;

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

drop policy if exists calendars_tenant_isolation_policy
    on ores_refdata_calendars_tbl;

create policy calendars_tenant_isolation_policy on ores_refdata_calendars_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Calendar Dates
-- -----------------------------------------------------------------------------
alter table ores_refdata_calendar_dates_tbl enable row level security;

drop policy if exists calendar_dates_tenant_isolation_policy
    on ores_refdata_calendar_dates_tbl;

create policy calendar_dates_tenant_isolation_policy on ores_refdata_calendar_dates_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Calendar Exceptions
-- -----------------------------------------------------------------------------
alter table ores_refdata_calendar_exceptions_tbl enable row level security;

drop policy if exists calendar_exceptions_tenant_isolation_policy
    on ores_refdata_calendar_exceptions_tbl;

create policy calendar_exceptions_tenant_isolation_policy on ores_refdata_calendar_exceptions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Calendar Rules
-- -----------------------------------------------------------------------------
alter table ores_refdata_calendar_rules_tbl enable row level security;

drop policy if exists calendar_rules_tenant_isolation_policy
    on ores_refdata_calendar_rules_tbl;

create policy calendar_rules_tenant_isolation_policy on ores_refdata_calendar_rules_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Diary Entry Types (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_diary_entry_types_tbl enable row level security;

drop policy if exists diary_entry_types_tenant_isolation_policy
    on ores_refdata_diary_entry_types_tbl;

create policy diary_entry_types_tenant_isolation_policy on ores_refdata_diary_entry_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Calendar Events (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_calendar_events_tbl enable row level security;

drop policy if exists calendar_events_tenant_isolation_policy
    on ores_refdata_calendar_events_tbl;

create policy calendar_events_tenant_isolation_policy on ores_refdata_calendar_events_tbl
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

drop policy if exists currency_countries_tenant_isolation_policy
    on ores_refdata_currency_countries_tbl;

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

drop policy if exists currency_calendars_tenant_isolation_policy
    on ores_refdata_currency_calendars_tbl;

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

drop policy if exists currency_pair_convention_calendars_tenant_isolation_policy
    on ores_refdata_currency_pair_convention_calendars_tbl;

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

drop policy if exists crm_topology_configs_tbl_tenant_isolation_policy
    on ores_refdata_crm_topology_configs_tbl;

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
drop policy if exists crm_topology_configs_tbl_marketdata_cross_tenant_read_policy
    on ores_refdata_crm_topology_configs_tbl;

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
drop policy if exists crm_topology_configs_tbl_party_isolation_policy
    on ores_refdata_crm_topology_configs_tbl;

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

drop policy if exists crm_driver_pairs_tbl_tenant_isolation_policy
    on ores_refdata_crm_driver_pairs_tbl;

create policy crm_driver_pairs_tbl_tenant_isolation_policy
on ores_refdata_crm_driver_pairs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- See crm_topology_configs_tbl_marketdata_cross_tenant_read_policy above.
drop policy if exists crm_driver_pairs_tbl_marketdata_cross_tenant_read_policy
    on ores_refdata_crm_driver_pairs_tbl;

create policy crm_driver_pairs_tbl_marketdata_cross_tenant_read_policy
on ores_refdata_crm_driver_pairs_tbl
for select
to :marketdata_service_user
using (true);

-- See crm_topology_configs_tbl_party_isolation_policy above.
drop policy if exists crm_driver_pairs_tbl_party_isolation_policy
    on ores_refdata_crm_driver_pairs_tbl;

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

drop policy if exists asset_class_codes_tbl_tenant_isolation_policy
    on ores_refdata_asset_class_codes_tbl;

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

drop policy if exists instrument_codes_tbl_tenant_isolation_policy
    on ores_refdata_instrument_codes_tbl;

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

drop policy if exists tenor_anchors_tbl_tenant_isolation_policy
    on ores_refdata_tenor_anchors_tbl;

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

drop policy if exists tenor_kinds_tbl_tenant_isolation_policy
    on ores_refdata_tenor_kinds_tbl;

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

drop policy if exists tenor_units_tbl_tenant_isolation_policy
    on ores_refdata_tenor_units_tbl;

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

drop policy if exists tenor_resolution_algorithms_tbl_tenant_isolation_policy
    on ores_refdata_tenor_resolution_algorithms_tbl;

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

drop policy if exists payment_frequencies_tbl_tenant_isolation_policy
    on ores_refdata_payment_frequencies_tbl;

create policy payment_frequencies_tbl_tenant_isolation_policy
on ores_refdata_payment_frequencies_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenor Schedules (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_refdata_tenor_schedules_tbl enable row level security;

drop policy if exists tenor_schedules_tenant_isolation_policy
    on ores_refdata_tenor_schedules_tbl;

create policy tenor_schedules_tenant_isolation_policy on ores_refdata_tenor_schedules_tbl
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

drop policy if exists tenors_tbl_tenant_isolation_policy
    on ores_refdata_tenors_tbl;

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

drop policy if exists tenor_conventions_tbl_tenant_isolation_policy
    on ores_refdata_tenor_conventions_tbl;

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

drop policy if exists tenor_convention_resolutions_tbl_tenant_isolation_policy
    on ores_refdata_tenor_convention_resolutions_tbl;

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

drop policy if exists crm_enabled_derived_pairs_tbl_tenant_isolation_policy
    on ores_refdata_crm_enabled_derived_pairs_tbl;

create policy crm_enabled_derived_pairs_tbl_tenant_isolation_policy
on ores_refdata_crm_enabled_derived_pairs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- See crm_topology_configs_tbl_marketdata_cross_tenant_read_policy above.
drop policy if exists crm_enabled_derived_pairs_tbl_marketdata_cross_tenant_read_policy
    on ores_refdata_crm_enabled_derived_pairs_tbl;

create policy crm_enabled_derived_pairs_tbl_marketdata_cross_tenant_read_policy
on ores_refdata_crm_enabled_derived_pairs_tbl
for select
to :marketdata_service_user
using (true);

-- See crm_topology_configs_tbl_party_isolation_policy above.
drop policy if exists crm_enabled_derived_pairs_tbl_party_isolation_policy
    on ores_refdata_crm_enabled_derived_pairs_tbl;

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

drop policy if exists curve_roles_tbl_tenant_isolation_policy
    on ores_refdata_curve_roles_tbl;

create policy curve_roles_tbl_tenant_isolation_policy on ores_refdata_curve_roles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Derivation Kinds
-- -----------------------------------------------------------------------------
alter table ores_refdata_derivation_kinds_tbl enable row level security;

drop policy if exists derivation_kinds_tbl_tenant_isolation_policy
    on ores_refdata_derivation_kinds_tbl;

create policy derivation_kinds_tbl_tenant_isolation_policy on ores_refdata_derivation_kinds_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Series Subclass Codes
-- -----------------------------------------------------------------------------
alter table ores_refdata_series_subclass_codes_tbl enable row level security;

drop policy if exists series_subclass_codes_tbl_tenant_isolation_policy
    on ores_refdata_series_subclass_codes_tbl;

create policy series_subclass_codes_tbl_tenant_isolation_policy
on ores_refdata_series_subclass_codes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- IR Curve Bootstrap Configs (dual RLS: tenant + party isolation)
-- -----------------------------------------------------------------------------
alter table ores_refdata_ir_curve_bootstrap_configs_tbl enable row level security;

drop policy if exists ir_curve_bootstrap_configs_tenant_isolation_policy
    on ores_refdata_ir_curve_bootstrap_configs_tbl;

create policy ir_curve_bootstrap_configs_tenant_isolation_policy
on ores_refdata_ir_curve_bootstrap_configs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists ir_curve_bootstrap_configs_party_isolation_policy
    on ores_refdata_ir_curve_bootstrap_configs_tbl;

create policy ir_curve_bootstrap_configs_party_isolation_policy
on ores_refdata_ir_curve_bootstrap_configs_tbl
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
-- IR Curve Bootstrap Pillars (dual RLS: tenant + party isolation)
-- -----------------------------------------------------------------------------
alter table ores_refdata_ir_curve_bootstrap_pillars_tbl enable row level security;

drop policy if exists ir_curve_bootstrap_pillars_tenant_isolation_policy
    on ores_refdata_ir_curve_bootstrap_pillars_tbl;

create policy ir_curve_bootstrap_pillars_tenant_isolation_policy
on ores_refdata_ir_curve_bootstrap_pillars_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists ir_curve_bootstrap_pillars_party_isolation_policy
    on ores_refdata_ir_curve_bootstrap_pillars_tbl;

create policy ir_curve_bootstrap_pillars_party_isolation_policy
on ores_refdata_ir_curve_bootstrap_pillars_tbl
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
-- Floating Index Types (moved from ores.trading)
-- -----------------------------------------------------------------------------
alter table ores_refdata_floating_index_types_tbl enable row level security;

drop policy if exists floating_index_types_tbl_tenant_isolation_policy
    on ores_refdata_floating_index_types_tbl;

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

drop policy if exists leg_types_tbl_tenant_isolation_policy
    on ores_refdata_leg_types_tbl;

create policy leg_types_tbl_tenant_isolation_policy on ores_refdata_leg_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Curve Sections
-- -----------------------------------------------------------------------------
alter table ores_refdata_curve_sections_tbl enable row level security;

drop policy if exists curve_sections_tbl_tenant_isolation_policy
    on ores_refdata_curve_sections_tbl;

create policy curve_sections_tbl_tenant_isolation_policy
on ores_refdata_curve_sections_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Curve Segment Types
-- -----------------------------------------------------------------------------
alter table ores_refdata_curve_segment_types_tbl enable row level security;

drop policy if exists curve_segment_types_tbl_tenant_isolation_policy
    on ores_refdata_curve_segment_types_tbl;

create policy curve_segment_types_tbl_tenant_isolation_policy
on ores_refdata_curve_segment_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Day Counters
-- -----------------------------------------------------------------------------
alter table ores_refdata_day_counters_tbl enable row level security;

drop policy if exists day_counters_tbl_tenant_isolation_policy
    on ores_refdata_day_counters_tbl;

create policy day_counters_tbl_tenant_isolation_policy
on ores_refdata_day_counters_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Calendar Names
-- -----------------------------------------------------------------------------
alter table ores_refdata_calendar_names_tbl enable row level security;

drop policy if exists calendar_names_tbl_tenant_isolation_policy
    on ores_refdata_calendar_names_tbl;

create policy calendar_names_tbl_tenant_isolation_policy
on ores_refdata_calendar_names_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Curve Securities
-- -----------------------------------------------------------------------------
alter table ores_refdata_curve_securities_tbl enable row level security;

drop policy if exists curve_securities_tbl_tenant_isolation_policy
    on ores_refdata_curve_securities_tbl;

create policy curve_securities_tbl_tenant_isolation_policy
on ores_refdata_curve_securities_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Intraday Power Curves
-- -----------------------------------------------------------------------------
alter table ores_refdata_intraday_power_curves_tbl enable row level security;

drop policy if exists intraday_power_curves_tbl_tenant_isolation_policy
    on ores_refdata_intraday_power_curves_tbl;

create policy intraday_power_curves_tbl_tenant_isolation_policy
on ores_refdata_intraday_power_curves_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Equity Curves
-- -----------------------------------------------------------------------------
alter table ores_refdata_equity_curves_tbl enable row level security;

drop policy if exists equity_curves_tbl_tenant_isolation_policy
    on ores_refdata_equity_curves_tbl;

create policy equity_curves_tbl_tenant_isolation_policy
on ores_refdata_equity_curves_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Inflation Curves
-- -----------------------------------------------------------------------------
alter table ores_refdata_inflation_curves_tbl enable row level security;

drop policy if exists inflation_curves_tbl_tenant_isolation_policy
    on ores_refdata_inflation_curves_tbl;

create policy inflation_curves_tbl_tenant_isolation_policy
on ores_refdata_inflation_curves_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Inflation Seasonality Factors
-- -----------------------------------------------------------------------------
alter table ores_refdata_inflation_seasonality_factors_tbl enable row level security;

drop policy if exists inflation_seasonality_factors_tbl_tenant_isolation_policy
    on ores_refdata_inflation_seasonality_factors_tbl;

create policy inflation_seasonality_factors_tbl_tenant_isolation_policy
on ores_refdata_inflation_seasonality_factors_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Default Curves
-- -----------------------------------------------------------------------------
alter table ores_refdata_default_curves_tbl enable row level security;

drop policy if exists default_curves_tbl_tenant_isolation_policy
    on ores_refdata_default_curves_tbl;

create policy default_curves_tbl_tenant_isolation_policy
on ores_refdata_default_curves_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Default Curve Configurations
-- -----------------------------------------------------------------------------
alter table ores_refdata_default_curve_configurations_tbl enable row level security;

drop policy if exists default_curve_configurations_tbl_tenant_isolation_policy
    on ores_refdata_default_curve_configurations_tbl;

create policy default_curve_configurations_tbl_tenant_isolation_policy
on ores_refdata_default_curve_configurations_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Commodity Curves
-- -----------------------------------------------------------------------------
alter table ores_refdata_commodity_curves_tbl enable row level security;

drop policy if exists commodity_curves_tbl_tenant_isolation_policy
    on ores_refdata_commodity_curves_tbl;

create policy commodity_curves_tbl_tenant_isolation_policy
on ores_refdata_commodity_curves_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Commodity Price Segments
-- -----------------------------------------------------------------------------
alter table ores_refdata_commodity_price_segments_tbl enable row level security;

drop policy if exists commodity_price_segments_tbl_tenant_isolation_policy
    on ores_refdata_commodity_price_segments_tbl;

create policy commodity_price_segments_tbl_tenant_isolation_policy
on ores_refdata_commodity_price_segments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Curve Configurations
-- -----------------------------------------------------------------------------
alter table ores_refdata_curve_configurations_tbl enable row level security;

drop policy if exists curve_configurations_tbl_tenant_isolation_policy
    on ores_refdata_curve_configurations_tbl;

create policy curve_configurations_tbl_tenant_isolation_policy
on ores_refdata_curve_configurations_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Curve Configuration Sections
-- -----------------------------------------------------------------------------
alter table ores_refdata_curve_configuration_sections_tbl enable row level security;

drop policy if exists curve_configuration_sections_tbl_tenant_isolation_policy
    on ores_refdata_curve_configuration_sections_tbl;

create policy curve_configuration_sections_tbl_tenant_isolation_policy
on ores_refdata_curve_configuration_sections_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Yield Curves
-- -----------------------------------------------------------------------------
alter table ores_refdata_yield_curves_tbl enable row level security;

drop policy if exists yield_curves_tbl_tenant_isolation_policy
    on ores_refdata_yield_curves_tbl;

create policy yield_curves_tbl_tenant_isolation_policy
on ores_refdata_yield_curves_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Curve Bootstrap Configs
-- -----------------------------------------------------------------------------
alter table ores_refdata_curve_bootstrap_configs_tbl enable row level security;

drop policy if exists curve_bootstrap_configs_tbl_tenant_isolation_policy
    on ores_refdata_curve_bootstrap_configs_tbl;

create policy curve_bootstrap_configs_tbl_tenant_isolation_policy
on ores_refdata_curve_bootstrap_configs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Curve Segment Curves
-- -----------------------------------------------------------------------------
alter table ores_refdata_curve_segment_curves_tbl enable row level security;

drop policy if exists curve_segment_curves_tbl_tenant_isolation_policy
    on ores_refdata_curve_segment_curves_tbl;

create policy curve_segment_curves_tbl_tenant_isolation_policy
on ores_refdata_curve_segment_curves_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Curve Definitions
-- -----------------------------------------------------------------------------
alter table ores_refdata_curve_definitions_tbl enable row level security;

drop policy if exists curve_definitions_tbl_tenant_isolation_policy
    on ores_refdata_curve_definitions_tbl;

create policy curve_definitions_tbl_tenant_isolation_policy
on ores_refdata_curve_definitions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Curve Segments
-- -----------------------------------------------------------------------------
alter table ores_refdata_curve_segments_tbl enable row level security;

drop policy if exists curve_segments_tbl_tenant_isolation_policy
    on ores_refdata_curve_segments_tbl;

create policy curve_segments_tbl_tenant_isolation_policy
on ores_refdata_curve_segments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Curve Quotes
-- -----------------------------------------------------------------------------
alter table ores_refdata_curve_quotes_tbl enable row level security;

drop policy if exists curve_quotes_tbl_tenant_isolation_policy
    on ores_refdata_curve_quotes_tbl;

create policy curve_quotes_tbl_tenant_isolation_policy
on ores_refdata_curve_quotes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Netting agreements
-- -----------------------------------------------------------------------------
alter table ores_refdata_netting_agreements_tbl enable row level security;

drop policy if exists netting_agreements_tenant_isolation_policy
    on ores_refdata_netting_agreements_tbl;

create policy netting_agreements_tenant_isolation_policy on ores_refdata_netting_agreements_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: an agreement is visible to the parties that can see the
-- firm's legal entity that signed it. FOR SELECT only, as for books.
drop policy if exists netting_agreements_party_isolation_policy
    on ores_refdata_netting_agreements_tbl;

create policy netting_agreements_party_isolation_policy
on ores_refdata_netting_agreements_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Netting sets
-- -----------------------------------------------------------------------------
alter table ores_refdata_netting_sets_tbl enable row level security;

drop policy if exists netting_sets_tenant_isolation_policy
    on ores_refdata_netting_sets_tbl;

create policy netting_sets_tenant_isolation_policy on ores_refdata_netting_sets_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation: a set read from an ORE document has no legal entity
-- yet and stays visible across the tenant until one is assigned. FOR SELECT
-- only, as for books.
drop policy if exists netting_sets_party_isolation_policy
    on ores_refdata_netting_sets_tbl;

create policy netting_sets_party_isolation_policy
on ores_refdata_netting_sets_tbl
as restrictive
for select using (
    party_id is null or party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- CSAs
-- -----------------------------------------------------------------------------
alter table ores_refdata_csas_tbl enable row level security;

drop policy if exists csas_tenant_isolation_policy
    on ores_refdata_csas_tbl;

create policy csas_tenant_isolation_policy on ores_refdata_csas_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- CSA eligible currencies
-- -----------------------------------------------------------------------------
alter table ores_refdata_csa_eligible_currencies_tbl enable row level security;

drop policy if exists csa_eligible_currencies_tenant_isolation_policy
    on ores_refdata_csa_eligible_currencies_tbl;

create policy csa_eligible_currencies_tenant_isolation_policy on ores_refdata_csa_eligible_currencies_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Portfolio rights
-- -----------------------------------------------------------------------------
alter table ores_refdata_portfolio_rights_tbl enable row level security;

drop policy if exists portfolio_rights_tenant_isolation_policy
    on ores_refdata_portfolio_rights_tbl;

create policy portfolio_rights_tenant_isolation_policy on ores_refdata_portfolio_rights_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Sandboxes
-- -----------------------------------------------------------------------------
alter table ores_refdata_sandboxes_tbl enable row level security;

drop policy if exists sandboxes_tenant_isolation_policy
    on ores_refdata_sandboxes_tbl;

create policy sandboxes_tenant_isolation_policy on ores_refdata_sandboxes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Sandbox members
-- -----------------------------------------------------------------------------
alter table ores_refdata_sandbox_members_tbl enable row level security;

drop policy if exists sandbox_members_tenant_isolation_policy
    on ores_refdata_sandbox_members_tbl;

create policy sandbox_members_tenant_isolation_policy on ores_refdata_sandbox_members_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
