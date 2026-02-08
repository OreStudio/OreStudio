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
 * GLEIF LEI Dataset Population Script
 *
 * Auto-generated from external/lei/manifest.json
 * This script is idempotent.
 */

-- =============================================================================
-- GLEIF LEI Datasets
-- =============================================================================

\echo '--- GLEIF LEI Datasets ---'

-- GLEIF LEI Entities (Small)
select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_entities.small',
    'GLEIF Standards',
    'Parties',
    'Reference Data',
    'LEI',
    'Primary',
    'Actual',
    'Raw',
    'GLEIF Golden Copy Extraction',
    'GLEIF LEI Entities (Small)',
    '',
    'GLEIF',
    'GLEIF golden copy data',
    '2026-01-23'::date,
    'Open Data',
    'lei_entities'
);

-- GLEIF LEI Entities (Large)
select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_entities.large',
    'GLEIF Standards',
    'Parties',
    'Reference Data',
    'LEI',
    'Primary',
    'Actual',
    'Raw',
    'GLEIF Golden Copy Extraction',
    'GLEIF LEI Entities (Large)',
    '',
    'GLEIF',
    'GLEIF golden copy data',
    '2026-01-23'::date,
    'Open Data',
    'lei_entities'
);

-- GLEIF LEI Relationships (Small)
select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_relationships.small',
    'GLEIF Standards',
    'Parties',
    'Reference Data',
    'NONE',
    'Primary',
    'Actual',
    'Raw',
    'GLEIF Golden Copy Extraction',
    'GLEIF LEI Relationships (Small)',
    '',
    'GLEIF',
    'GLEIF golden copy data',
    '2026-01-23'::date,
    'Open Data',
    'lei_relationships'
);

-- GLEIF LEI Relationships (Large)
select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    'gleif.lei_relationships.large',
    'GLEIF Standards',
    'Parties',
    'Reference Data',
    'NONE',
    'Primary',
    'Actual',
    'Raw',
    'GLEIF Golden Copy Extraction',
    'GLEIF LEI Relationships (Large)',
    '',
    'GLEIF',
    'GLEIF golden copy data',
    '2026-01-23'::date,
    'Open Data',
    'lei_relationships'
);

