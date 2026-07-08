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
    -- Solvaris Bundle Members
    -- =============================================================================

    -- --- Solvaris Bundle Members ---

    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'solvaris', 'geo.ip2country', 1);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'solvaris', 'slovaris.country_flags', 2);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'solvaris', 'slovaris.countries', 3);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'solvaris', 'slovaris.currencies', 4);

    -- =============================================================================
    -- Base System Bundle Members
    -- =============================================================================


    -- --- Base System Bundle Members ---

    -- Visual assets
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'assets.country_flags', 1);

    -- Geolocation
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'geo.ip2country', 2);

    -- ISO Standards
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'iso.countries', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'iso.currencies', 11);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.currency_pairs', 12);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'refdata.currency_pair_conventions', 13);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'iso.coding_schemes', 14);

    -- FpML Standards
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'fpml.coding_schemes', 100);
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

    -- GLEIF LEI
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_entities.small', 200);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_relationships.small', 201);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_counterparties.small', 202, true);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_parties.small', 203, true);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_counterparties.large', 204, true);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'base', 'gleif.lei_parties.large', 205, true);

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
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'iso.coding_schemes', 12);

    -- Cryptocurrency
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'crypto.small', 20);

    -- FpML Standards
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'crypto', 'fpml.coding_schemes', 100);
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
    -- Organisation Bundle Members
    -- =============================================================================


    -- --- Organisation Bundle Members ---

    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'organisation', 'testdata.business_units', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'organisation', 'testdata.portfolios', 20);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'organisation', 'testdata.books', 30);

    -- --- ORE Analytics Bundle Members ---

    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'ore_analytics', 'ore.report_definitions', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'ore_analytics', 'synthetic.fx_spot_configs', 20);
END $$;

