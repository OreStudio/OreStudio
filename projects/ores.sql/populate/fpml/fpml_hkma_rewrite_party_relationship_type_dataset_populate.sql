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
 * FpML Hkma Rewrite Party Relationship Type Dataset Population Script
 *
 * Creates the dataset entry for fpml.hkma_rewrite_party_relationship_type.
 * Source version: 1-0
 * This must be run before populating the artefact table.
 */


-- =============================================================================
-- FpML Hkma Rewrite Party Relationship Type Dataset
-- =============================================================================

\echo '--- FpML Hkma Rewrite Party Relationship Type Dataset ---'

select ores_dq_datasets_upsert_fn(
    'fpml.hkma_rewrite_party_relationship_type',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_HKMA_REWRITE_PARTY_RELATIONSHIP_TYPE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Hkma Rewrite Party Relationship Type',
    'Indicates the relationship between two parties as defined by Hong Kong Monetary Authority (HKMA) Rewrite field 189 - Intragroup.',
    'FPML',
    'Reference data for FpML Hkma Rewrite Party Relationship Type (version 1-0)',
    '2025-04-25'::date,
    'FpML Public License 2.0',
    'party_relationships'
);
