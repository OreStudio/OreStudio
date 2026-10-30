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

/**
 * Dataset Bundle Member Population Script
 *
 * Seeds the database with dataset bundle membership definitions.
 * This script is idempotent.
 */

DO $$
BEGIN
    -- =============================================================================
    -- System Core Bundle Members
    -- =============================================================================

    -- --- System Core Bundle Members ---

    -- The datasets the installation itself is read from, published into the
    -- system tenant before any tenant is provisioned. A coding-scheme
    -- validator resolves its scheme against the system tenant, a template
    -- image is read from it across the tenant-isolation policy, and several
    -- reference reads treat its rows as a shared overlay on the tenant's own
    -- ("tenant_id in (p_tenant_id, system)"). None of these rides a profile,
    -- because a profile describes one tenant and these rows never belong to
    -- one.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'iso.coding_schemes', 1);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'fpml.coding_schemes', 2);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'assets.country_flags', 3);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'assets.system_avatars', 4);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'iso.countries', 5);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'iso.currencies', 6);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'fpml.asset_class', 7);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'ore.asset_class', 8);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'synthetic.market_data_providers', 9);

    -- The provider feeds: system collections, one for each provider that has feeds of a
    -- kind the platform simulates. They publish after the provider catalogue they name,
    -- into the system tenant under its system party, and run there once.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'synthetic.feeds.lseg', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'synthetic.feeds.ice', 11);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'synthetic.feeds.tradeweb', 12);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'synthetic.feeds.bloomberg', 13);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'synthetic.feeds.uniform_demo', 15);

    -- The ORE parameter documentation. The system tenant's rows are what the
    -- validators of the configuration parameters read, and each tenant's rows
    -- are what its own screens read, so both bundles carry it. The parameters
    -- depend on the value domains, so the dataset dependency orders them.
    -- Two copies of one dataset is a stopgap: the capture "One source for
    -- reference data that the system and each tenant read" tracks the choice
    -- of one source and the rewrite of the readers that follows.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'ore.parameter_value_domains', 16);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'ore.parameters', 17);

    -- The adapter and supplier catalogues. Like the providers, they are
    -- installation data: the system tenant owns them, every tenant reads them,
    -- and nobody adds a row from a screen. A session configures one adapter at
    -- one supplier, so the two publish before anything that names them.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'marketdata.adapters', 18);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'system_core', 'marketdata.suppliers', 19);

    -- =============================================================================
    -- Base System Bundle Members
    -- =============================================================================


    -- --- Base System Bundle Members ---

    -- Visual assets
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'assets.country_flags', 1);
    -- Default administrator pictures; the attach step binds them by code.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'assets.system_avatars', 3);

    -- Geolocation
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'geo.ip2country', 2);

    -- ISO Standards
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'iso.countries', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'iso.currencies', 11);
    -- The equity underlyings a population draws. A trade that named one no
    -- catalogue holds is refused when it is written, so the catalogue has to
    -- publish before the population runs.
    -- The commodity vocabulary the same population draws on, with the same
    -- reason for publishing it before a population runs.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.commodities', 12);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.equity_underlyings', 12);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.currency_pairs', 12);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.currency_pair_conventions', 13);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.calendar_types', 14);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.calendars', 15);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.currency_calendars', 16);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.currency_pair_convention_calendars', 17);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.currency_countries', 18);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.tenor_schedules', 19);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.calendar_events', 20);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.currency_groups', 21);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.currency_currency_groups', 22);

    -- FpML Standards
    -- NOTE: iso.coding_schemes/fpml.coding_schemes are deliberately NOT members
    -- of this bundle. They are the system tenant's, published by the
    -- system_core bundle before any tenant is provisioned, and every
    -- coding-scheme validator checks the system tenant's rows. Publishing them
    -- here as well produced dead, RLS-visible duplicate rows (every code
    -- appearing twice in tenant-scoped combos) with no validator ever
    -- consulting the tenant-owned copy.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.account_type', 101);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.asset_class', 102);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.asset_measure', 103);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.benchmark_rate', 104);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.business_center', 105);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.business_process', 106);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.cashflow_type', 107);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.cftc_entity_classification', 108);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.cftc_organization_type', 109);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.entity_type', 110);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.hkma_rewrite_party_relationship_type', 111);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.hkma_rewrite_regulatory_corporate_sector', 112);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.local_jurisdiction', 113);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.non_iso_currency', 114);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.party_relationship_type', 115);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.party_role', 116);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.party_role_type', 117);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.person_role', 118);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.regulatory_corporate_sector', 119);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.reporting_regime', 120);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.supervisory_body', 121);

    -- GLEIF LEI. Every rung is opt-in: a tenant asks for the slice it needs.
    -- Publishing base must not force a thirteen-thousand-entity corpus on a
    -- tenant that wants a handful, which is what the two mandatory members
    -- here used to do. Tiny covers every region and is what the demo tenants
    -- want; small and large are for work where the volume is the point.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_entities.tiny', 210, true);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_relationships.tiny', 211, true);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_counterparties.tiny', 212, true);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_parties.tiny', 213, true);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_entities.small', 200, true);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_relationships.small', 201, true);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_counterparties.small', 202, true);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_parties.small', 203, true);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_counterparties.large', 204, true);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_parties.large', 205, true);
    -- The ORE samples' counterparty names, as aliases of GLEIF counterparties.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'ore.counterparty_aliases', 206, true);

    -- Badge system (self-published, DQ -> DQ; unlike coding_schemes, badge
    -- severities genuinely need a per-tenant copy, since BadgeCache/admin
    -- CRUD windows read tenant-scoped -- see the badge-colour-scheme-support
    -- story's tenant-scoping migration task for the full rationale)
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'ore.badge_severities', 300);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'ore.badge_definitions', 301);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'ore.code_domains', 302);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'ore.badge_mappings', 303);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'ore.parameter_value_domains', 304);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'ore.parameters', 305);

    -- =============================================================================
    -- Crypto Bundle Members
    -- =============================================================================


    -- --- Crypto Bundle Members ---

    -- Visual assets
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'assets.country_flags', 1);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'assets.crypto_icons', 2);

    -- Geolocation
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'geo.ip2country', 3);

    -- ISO Standards
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'iso.countries', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'iso.currencies', 11);

    -- Cryptocurrency
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'crypto.small', 20);

    -- FpML Standards
    -- NOTE: iso.coding_schemes/fpml.coding_schemes deliberately omitted; see
    -- the 'base' bundle above for why.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.account_type', 101);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.asset_class', 102);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.asset_measure', 103);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.benchmark_rate', 104);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.business_center', 105);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.business_process', 106);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.cashflow_type', 107);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.cftc_entity_classification', 108);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.cftc_organization_type', 109);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.entity_type', 110);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.hkma_rewrite_party_relationship_type', 111);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.hkma_rewrite_regulatory_corporate_sector', 112);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.local_jurisdiction', 113);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.non_iso_currency', 114);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.party_relationship_type', 115);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.party_role', 116);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.party_role_type', 117);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.person_role', 118);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.regulatory_corporate_sector', 119);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.reporting_regime', 120);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.supervisory_body', 121);

    -- =============================================================================
    -- Risk Management Bundle Members
    -- =============================================================================


    -- --- Risk Management Bundle Members ---
    -- Reports reference the book/portfolio tree, so both live together: a
    -- new party-scoped dataset in this bundle never requires a client code
    -- change, only a new row here. The risk report configs ride with them:
    -- a config resolves a report definition and scopes to the root
    -- portfolio, so it needs the definitions and the organisation data in
    -- the same publish.

    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'risk_management', 'testdata.business_units', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'risk_management', 'testdata.portfolios', 20);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'risk_management', 'testdata.books', 30);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'risk_management', 'ore.report_definitions', 40);

    -- --- CRM Cross-Rates Matrix Topology Bundle Members ---

    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crm_topology', 'refdata.crm_topology_bundles', 10);

    -- --- GLEIF LEI Hierarchy Import Bundle Members ---

    -- One member: the publish trigger. Its entity/relationship siblings
    -- (gleif.lei_entities.small, gleif.lei_relationships.small) are read by
    -- ores_refdata_publish_lei_parties_from_dq_fn but carry no publish
    -- target of their own, and the dependency graph declared in
    -- lei_dataset_dependency_populate.sql already orders them; listing them
    -- here would add members that never dispatch. Same shape as
    -- acme_lei_import, which lists only acme.lei_parties.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'lei_hierarchy', 'gleif.lei_parties.small', 10);

    -- --- Party Essentials Bundle Members ---

    -- The datasets every party gets: its instrument conventions
    -- (ore.conventions), then its CRM topology (refdata.crm_topology_bundles,
    -- the crm_topology bundle's only member). Both are party-scoped: the CRM
    -- publish requires params.party_id.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'party_essentials', 'ore.conventions', 5);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'party_essentials', 'refdata.crm_topology_bundles', 20);

    -- The generic report set, for a party with no report design of its own: its
    -- report definitions (ore.report_definitions). The report publish resolves the
    -- party from params.party_id (or the tenant root). It runs with the party
    -- stage, once the party is there, and after party_essentials, because it
    -- reads the conventions.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'party_reporting', 'ore.report_definitions', 10);

    -- The ORE samples, in publish order. Party-scoped like party_essentials.
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'ore_samples', 'ore.sample_netting_agreements', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'ore_samples', 'ore.sample_netting_sets', 20);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'ore_samples', 'ore.sample_csas', 30);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'ore_samples', 'ore.netting_set_aliases', 40);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'ore_samples', 'ore.sample_portfolios', 50);
END $$;

