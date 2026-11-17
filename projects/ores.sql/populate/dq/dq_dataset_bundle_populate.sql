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
 * Dataset Bundle Population Script
 *
 * Seeds the database with dataset bundle definitions.
 * Bundles are named collections of datasets designed to work together.
 * This script is idempotent.
 */

DO $$
BEGIN
    -- =============================================================================
    -- Dataset Bundles
    -- =============================================================================

    -- --- Dataset Bundles ---

    -- The platform catalogue: the rows the platform defines in order to
    -- function. Published once, into the system tenant, at database bring-up.
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'system_core',
        'System Core',
        'The rows the platform defines in order to function: coding schemes, the template images, the provider, adapter and supplier catalogues, the system tenant''s feeds, the ORE parameters and their value domains, and the FX vol surface blueprints.'
    );

    -- The world catalogue: the facts every installation holds identically,
    -- adopted from an outside standard. Published once, into the system tenant,
    -- at database bring-up. A tenant reads these rows where they lie and holds
    -- no copy, so no bundle of this level is ever published into a tenant.
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'world',
        'World',
        'Reference data adopted from an outside standard: ISO countries and currencies, the FpML vocabularies, calendars, currency pairs, commodities and equity underlyings. Held in the system tenant, and read by every tenant where it lies.'
    );

    -- A new tenant's own starting configuration: the rows it may change, and
    -- nothing else. Published once when the tenant is created.
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'tenant_defaults',
        'Tenant Defaults',
        'The configuration a tenant starts with and may change: its badges, its badge mappings and its code domains.'
    );

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'crypto',
        'Crypto',
        'The cryptocurrency slice: the symbols, and the icons a screen draws beside them. Published into the system tenant beside the world catalogue, which carries every other dataset a crypto installation needs.'
    );

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'risk_management',
        'Risk Management',
        'Sample organisational and risk-reporting data for development and testing. Includes business units, portfolios, trading books, and the standard set of risk report definitions (NPV, VaR, CVA, XVA, etc.) -- reports reference the book/portfolio tree, so both live in one bundle.'
    );

    -- The CRM Cross-Rates Matrix's topology config (which crosses/tiers
    -- exist -- majors/exotics/scandies) is deliberately its own bundle,
    -- independent of any synthetic feeds dataset: a party gets a
    -- working CRM window regardless of which dataset's feed (if any) is
    -- currently running, and adding/retiring a vintage bundle never has to
    -- remember to carry CRM topology along with it.
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'crm_topology',
        'CRM Cross-Rates Matrix Topology',
        'Named CRM topology configs (majors/exotics/scandies tiers) for the Cross-Rates Matrix window. Published per party, independent of which synthetic feeds dataset (if any) is providing live rates -- the CRM shows static/stale prices until a feed starts ticking the underlying driver pairs.'
    );

    -- The generic LEI hierarchy import: the production counterpart of
    -- acme_lei_import, carrying no organisation's data. Its required member
    -- derives the parties of a GLEIF legal-entity subtree from the root LEI
    -- the run supplies, reading the GLEIF entity and relationship staging
    -- rows that the dataset dependency graph orders. The profile's
    -- import_lei_hierarchy step names this bundle and passes root_lei.
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'lei_hierarchy',
        'GLEIF LEI Hierarchy Import',
        'Imports the GLEIF legal-entity hierarchy under a root LEI as the tenant''s parties. Published tenant-wide with the {"lei_parties": {"root_lei": "..."}} param, before any party under the root exists. The root LEI is the run''s, so the bundle holds no organisation''s data.'
    );

    -- The GLEIF records a tenant trades against, as the tenant's own rows.
    -- Published tenant-wide, on request, with the rungs the caller opts into.
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'lei_counterparties',
        'GLEIF Counterparties',
        'The GLEIF records a tenant trades against: the counterparties it draws from the GLEIF golden copy, and the aliases that name them. Published tenant-wide on request, with each rung opted into by name. The LEI record is a world fact; the counterparty and the alias are the tenant''s own rows.'
    );

    -- The reference data one party needs and nothing more: its instrument
    -- conventions and its CRM topology. Published once per party with a
    -- {"party_id": "..."} param. Deliberately excludes the organisation
    -- structure, staff accounts and demonstration data that the ACME office
    -- bundles and risk_management carry, so a production tenant's parties
    -- get reference data and no test data.
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'party_essentials',
        'Party Essentials',
        'The reference data a single party needs: its instrument conventions and its CRM (Cross-Rates Matrix) topology. Published once per party with a {"party_id": "..."} param. Carries no organisation structure, staff accounts or demonstration data -- those belong to the ACME demo bundles.'
    );

    -- The generic report set for a party that has no report design of its
    -- own: the tiered ORE report definitions and the run settings they
    -- resolve. A profile that gives each party its own report definitions,
    -- as acme_demo does through the ACME office bundles, does not name it.
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'party_reporting',
        'Party Reporting',
        'The generic report set for a party: the tiered ORE report definitions and the run settings they resolve. Published once per party with a {"party_id": "..."} param, after party_essentials. A profile that gives each party its own report definitions does not publish it.'
    );

    -- What the ORE sample documents import against. Demonstration data for
    -- one party, so a profile names the party it belongs to; acme_demo gives
    -- it to Acme Corporation Plc, the group's root.
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore_samples',
        'ORE Samples',
        'ORE sample data, apart from the official data: what the ORE sample documents import against: master agreements, netting sets, CSAs and netting set aliases for the netting set ids they use, built from the samples'' own netting definitions, and the portfolios their PortfolioIds name, which the publish places in a sample sandbox. Published once per party with a {"party_id": "..."} param, against the GLEIF banks the ORE counterparty aliases map onto.'
    );
END $$;

