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
-- Acme Corporation Provisioning Bundles
--
-- The data-driven holding-group spec the internal-impersonation orchestrator
-- (ores.iam.core's Acme provisioner) drives via dq.v1.bundles.publish,
-- instead of a hardcoded SQL VALUES list of companies/datasets -- see
-- "Rework Acme provisioning: simulate real per-party setup via internal
-- actor impersonation". Adding a fifth office later is a new bundle
-- registration here plus the corresponding dataset rows in
-- acme_dataset_populate.sql, not a C++/SQL orchestrator change.
--
-- Unlike risk_management (whose members are the generic 'testdata.*'
-- datasets, identical filler content published to whichever party the
-- caller names), each Acme office bundle here references that office's own
-- distinct dataset codes -- Acme's four legal entities are differentiated
-- content, not the same synthetic filler four times.
-- =============================================================================

DO $$
BEGIN
    -- --- LEI hierarchy + business centres (published once, tenant-wide,
    -- before any operating-company party exists to scope a bundle to) ---
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme_lei_import',
        'Acme Corporation LEI Import',
        'The four-party ACME Corporation LEI hierarchy plus the business centres LEI-imported parties require. Published tenant-wide with the {"root_lei": "9695ACMEGROUP0000030"} param, before any operating-company party exists.'
    );
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_lei_import', 'fpml.business_center', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_lei_import', 'acme.lei_parties', 20);

    -- --- Real GLEIF counterparties (small), tenant-wide, so every ACME
    -- Corporation party can trade against a realistic counterparty set ---
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme_gleif_counterparties',
        'Acme Corporation GLEIF Counterparties',
        'Real GLEIF counterparties (small), published tenant-wide with no params, so every ACME Corporation party can trade against a realistic counterparty set.'
    );
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_gleif_counterparties', 'gleif.lei_counterparties.small', 10);

    -- --- Per-office business units, portfolios, books, accounts, and
    -- account contact informations -- published once each operating
    -- company's party exists, with a {"party_id": "<company party id>"}
    -- param scoping every member to that party ---
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme_uk',
        'Acme Corporation UK',
        'ACME Corporation UK plc''s business units, portfolios, books, accounts, and account contact informations. Publish with a {"party_id": "..."} param scoping every member to the UK party.'
    );
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_uk', 'acme.acme_uk.business_units', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_uk', 'acme.acme_uk.portfolios', 20);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_uk', 'acme.acme_uk.books', 30);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_uk', 'acme.acme_uk.accounts', 40);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_uk', 'acme.acme_uk.account_contact_informations', 50);

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme_us',
        'Acme Corporation US',
        'ACME Corporation US Inc''s business units, portfolios, books, accounts, and account contact informations. Publish with a {"party_id": "..."} param scoping every member to the US party.'
    );
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_us', 'acme.acme_us.business_units', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_us', 'acme.acme_us.portfolios', 20);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_us', 'acme.acme_us.books', 30);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_us', 'acme.acme_us.accounts', 40);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_us', 'acme.acme_us.account_contact_informations', 50);

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'acme_hk',
        'Acme Corporation HK',
        'ACME Corporation HK Ltd''s business units, portfolios, books, accounts, and account contact informations. Publish with a {"party_id": "..."} param scoping every member to the HK party.'
    );
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_hk', 'acme.acme_hk.business_units', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_hk', 'acme.acme_hk.portfolios', 20);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_hk', 'acme.acme_hk.books', 30);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_hk', 'acme.acme_hk.accounts', 40);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'acme_hk', 'acme.acme_hk.account_contact_informations', 50);
END $$;
