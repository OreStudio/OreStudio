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

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'base',
        'Base System',
        'Industry-standard reference data (ISO + FpML) for production use. Includes country codes, currency codes, and financial market standards.'
    );

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'crypto',
        'Crypto',
        'Base System plus cryptocurrency reference data. Extends the production dataset with cryptocurrency symbols and icons.'
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
    -- acme_lei_import, carrying no organisation's data. Its one member
    -- derives the parties of a GLEIF legal-entity subtree from the root LEI
    -- the run supplies, reading the GLEIF entity/relationship staging rows
    -- that the base bundle's gleif.* datasets carry. The profile's
    -- import_lei_hierarchy step names this bundle and passes root_lei.
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'lei_hierarchy',
        'GLEIF LEI Hierarchy Import',
        'Imports the GLEIF legal-entity hierarchy under a root LEI as the tenant''s parties. Published tenant-wide with the {"lei_parties": {"root_lei": "..."}} param, before any party under the root exists. The root LEI is the run''s, so the bundle holds no organisation''s data.'
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

