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

-- =============================================================================
-- Solvaris Bundle Members
-- =============================================================================

\echo '--- Solvaris Bundle Members ---'

select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'solvaris', 'geo.ip2country', 1);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'solvaris', 'slovaris.country_flags', 2);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'solvaris', 'slovaris.countries', 3);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'solvaris', 'slovaris.currencies', 4);

-- =============================================================================
-- Base System Bundle Members
-- =============================================================================

\echo ''
\echo '--- Base System Bundle Members ---'

-- Visual assets
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'assets.country_flags', 1);

-- Geolocation
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'geo.ip2country', 2);

-- ISO Standards
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'iso.countries', 10);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'iso.currencies', 11);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'iso.coding_schemes', 12);

-- FpML Standards
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.coding_schemes', 100);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.account_type', 101);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.asset_class', 102);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.asset_measure', 103);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.benchmark_rate', 104);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.business_center', 105);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.business_process', 106);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.cashflow_type', 107);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.cftc_entity_classification', 108);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.cftc_organization_type', 109);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.entity_type', 110);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.hkma_rewrite_party_relationship_type', 111);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.hkma_rewrite_regulatory_corporate_sector', 112);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.local_jurisdiction', 113);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.non_iso_currency', 114);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.party_relationship_type', 115);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.party_role', 116);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.party_role_type', 117);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.person_role', 118);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.regulatory_corporate_sector', 119);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.reporting_regime', 120);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'fpml.supervisory_body', 121);

-- GLEIF LEI
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'gleif.lei_entities.small', 200);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'gleif.lei_relationships.small', 201);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'gleif.lei_counterparties.small', 202, true);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'base', 'gleif.lei_parties.small', 203, true);

-- =============================================================================
-- Crypto Bundle Members
-- =============================================================================

\echo ''
\echo '--- Crypto Bundle Members ---'

-- Visual assets
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'assets.country_flags', 1);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'assets.crypto_icons', 2);

-- Geolocation
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'geo.ip2country', 3);

-- ISO Standards
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'iso.countries', 10);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'iso.currencies', 11);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'iso.coding_schemes', 12);

-- Cryptocurrency
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'crypto.small', 20);

-- FpML Standards
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.coding_schemes', 100);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.account_type', 101);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.asset_class', 102);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.asset_measure', 103);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.benchmark_rate', 104);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.business_center', 105);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.business_process', 106);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.cashflow_type', 107);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.cftc_entity_classification', 108);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.cftc_organization_type', 109);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.entity_type', 110);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.hkma_rewrite_party_relationship_type', 111);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.hkma_rewrite_regulatory_corporate_sector', 112);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.local_jurisdiction', 113);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.non_iso_currency', 114);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.party_relationship_type', 115);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.party_role', 116);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.party_role_type', 117);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.person_role', 118);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.regulatory_corporate_sector', 119);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.reporting_regime', 120);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'crypto', 'fpml.supervisory_body', 121);

-- =============================================================================
-- Organisation Bundle Members
-- =============================================================================

\echo ''
\echo '--- Organisation Bundle Members ---'

select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'organisation', 'testdata.business_units', 10);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'organisation', 'testdata.portfolios', 20);
select ores_dq_dataset_bundle_members_upsert_fn(ores_iam_system_tenant_id_fn(), 'organisation', 'testdata.books', 30);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select b.code as bundle, count(*) as dataset_count
from ores_dq_dataset_bundles_tbl b
join ores_dq_dataset_bundle_members_tbl m on b.code = m.bundle_code
where b.valid_to = ores_utility_infinity_timestamp_fn()
and m.valid_to = ores_utility_infinity_timestamp_fn()
group by b.code
order by b.code;
