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

-- A tenant's parties are read inside the tenant, the system tenant included:
-- the system tenant sees its own parties and no others. IAM's party cache
-- reads each tenant's partition with a token it issues itself for that
-- tenant, so it needs no wider policy.
drop policy if exists parties_tenant_isolation_policy
    on ores_refdata_parties_tbl;

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
-- Book changes (the lines of a request that proposes a change to a book)
-- -----------------------------------------------------------------------------
alter table ores_refdata_book_changes_tbl enable row level security;

drop policy if exists book_changes_tenant_isolation_policy
    on ores_refdata_book_changes_tbl;

create policy book_changes_tenant_isolation_policy on ores_refdata_book_changes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
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

-- Sandbox isolation, as for portfolios: a virtual book is read and written
-- only by those who may see its sandbox, so no official read, report or
-- process reaches it.
drop policy if exists books_sandbox_isolation_policy
    on ores_refdata_books_tbl;

create policy books_sandbox_isolation_policy
on ores_refdata_books_tbl
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

-- Party isolation: a set belongs to the legal entity that holds it and is
-- visible to the parties that can see that entity. FOR SELECT only, as for
-- books.
drop policy if exists netting_sets_party_isolation_policy
    on ores_refdata_netting_sets_tbl;

create policy netting_sets_party_isolation_policy
on ores_refdata_netting_sets_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- CSAs
-- -----------------------------------------------------------------------------
-- The tenant policy is generated with the table. Party isolation: a CSA is
-- visible to the parties that can see the legal entity it belongs to. FOR
-- SELECT only, as for netting sets.
drop policy if exists csas_party_isolation_policy
    on ores_refdata_csas_tbl;

create policy csas_party_isolation_policy
on ores_refdata_csas_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Netting set identifiers
-- -----------------------------------------------------------------------------
-- The tenant policy is generated with the table. Party isolation: an
-- identifier is visible to the parties that can see the legal entity it
-- belongs to. FOR SELECT only, as for netting sets.
drop policy if exists netting_set_identifiers_party_isolation_policy
    on ores_refdata_netting_set_identifiers_tbl;

create policy netting_set_identifiers_party_isolation_policy
on ores_refdata_netting_set_identifiers_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);
