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

set schema 'metadata';

-- =============================================================================
-- Solvaris Bundle Members
-- =============================================================================

\echo '--- Solvaris Bundle Members ---'

select metadata.upsert_dq_dataset_bundle_member('solvaris', 'geo.ip2country', 1);
select metadata.upsert_dq_dataset_bundle_member('solvaris', 'slovaris.country_flags', 2);
select metadata.upsert_dq_dataset_bundle_member('solvaris', 'slovaris.countries', 3);
select metadata.upsert_dq_dataset_bundle_member('solvaris', 'slovaris.currencies', 4);

-- =============================================================================
-- Base System Bundle Members
-- =============================================================================

\echo ''
\echo '--- Base System Bundle Members ---'

-- Visual assets
select metadata.upsert_dq_dataset_bundle_member('base', 'assets.country_flags', 1);

-- Geolocation
select metadata.upsert_dq_dataset_bundle_member('base', 'geo.ip2country', 2);

-- ISO Standards
select metadata.upsert_dq_dataset_bundle_member('base', 'iso.countries', 10);
select metadata.upsert_dq_dataset_bundle_member('base', 'iso.currencies', 11);
select metadata.upsert_dq_dataset_bundle_member('base', 'iso.coding_schemes', 12);

-- FpML Standards
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.coding_schemes', 100);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.account_type', 101);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.asset_class', 102);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.asset_measure', 103);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.benchmark_rate', 104);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.business_center', 105);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.business_process', 106);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.cashflow_type', 107);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.cftc_entity_classification', 108);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.cftc_organization_type', 109);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.entity_type', 110);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.hkma_rewrite_party_relationship_type', 111);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.hkma_rewrite_regulatory_corporate_sector', 112);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.local_jurisdiction', 113);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.non_iso_currency', 114);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.party_relationship_type', 115);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.party_role', 116);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.party_role_type', 117);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.person_role', 118);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.regulatory_corporate_sector', 119);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.reporting_regime', 120);
select metadata.upsert_dq_dataset_bundle_member('base', 'fpml.supervisory_body', 121);

-- =============================================================================
-- Crypto Bundle Members
-- =============================================================================

\echo ''
\echo '--- Crypto Bundle Members ---'

-- Visual assets
select metadata.upsert_dq_dataset_bundle_member('crypto', 'assets.country_flags', 1);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'assets.crypto_icons', 2);

-- Geolocation
select metadata.upsert_dq_dataset_bundle_member('crypto', 'geo.ip2country', 3);

-- ISO Standards
select metadata.upsert_dq_dataset_bundle_member('crypto', 'iso.countries', 10);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'iso.currencies', 11);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'iso.coding_schemes', 12);

-- Cryptocurrency
select metadata.upsert_dq_dataset_bundle_member('crypto', 'crypto.small', 20);

-- FpML Standards
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.coding_schemes', 100);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.account_type', 101);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.asset_class', 102);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.asset_measure', 103);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.benchmark_rate', 104);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.business_center', 105);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.business_process', 106);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.cashflow_type', 107);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.cftc_entity_classification', 108);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.cftc_organization_type', 109);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.entity_type', 110);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.hkma_rewrite_party_relationship_type', 111);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.hkma_rewrite_regulatory_corporate_sector', 112);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.local_jurisdiction', 113);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.non_iso_currency', 114);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.party_relationship_type', 115);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.party_role', 116);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.party_role_type', 117);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.person_role', 118);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.regulatory_corporate_sector', 119);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.reporting_regime', 120);
select metadata.upsert_dq_dataset_bundle_member('crypto', 'fpml.supervisory_body', 121);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select b.code as bundle, count(*) as dataset_count
from metadata.dq_dataset_bundles_tbl b
join metadata.dq_dataset_bundle_members_tbl m on b.code = m.bundle_code
where b.valid_to = public.utility_infinity_timestamp_fn()
and m.valid_to = public.utility_infinity_timestamp_fn()
group by b.code
order by b.code;
