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
 * GLEIF LEI Dataset Dependencies
 *
 * Auto-generated from external/lei/manifest.json
 * This script is idempotent.
 */

-- =============================================================================
-- GLEIF LEI Dataset Dependencies
-- =============================================================================

\echo '--- GLEIF LEI Dataset Dependencies ---'

select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_relationships.small',
    'gleif.lei_entities.small',
    'entity_reference'
);

select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_relationships.large',
    'gleif.lei_entities.large',
    'entity_reference'
);

-- lei_parties depends on business_center, lei_entities, lei_relationships
select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_parties.small', 'fpml.business_center', 'business_centres');
select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_parties.small', 'gleif.lei_entities.small', 'entity_reference');
select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_parties.small', 'gleif.lei_relationships.small', 'hierarchy');

-- lei_counterparties depends on all of the above plus lei_parties
select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_counterparties.small', 'fpml.business_center', 'business_centres');
select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_counterparties.small', 'gleif.lei_entities.small', 'entity_reference');
select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_counterparties.small', 'gleif.lei_relationships.small', 'hierarchy');
select ores_dq_dataset_dependencies_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_counterparties.small', 'gleif.lei_parties.small', 'party_reference');

