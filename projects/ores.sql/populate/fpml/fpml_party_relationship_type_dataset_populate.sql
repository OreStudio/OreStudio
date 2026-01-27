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
 * FpML Party Relationship Type Dataset Population Script
 *
 * Creates the dataset entry for fpml.party_relationship_type.
 * Source version: 1-1
 * This must be run before populating the artefact table.
 */

set schema 'metadata';

-- =============================================================================
-- FpML Party Relationship Type Dataset
-- =============================================================================

\echo '--- FpML Party Relationship Type Dataset ---'

select public.upsert_dq_datasets(
    'fpml.party_relationship_type',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_PARTY_RELATIONSHIP_TYPE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Party Relationship Type',
    'A type is containing a code representing how two parties are related, e.g. Affiliated, Intragroup.',
    'FPML',
    'Reference data for FpML Party Relationship Type (version 1-1)',
    '2015-05-10'::date,
    'FpML Public License 2.0',
    'party_relationships'
);
