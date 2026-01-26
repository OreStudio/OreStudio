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
 * ISO Coding Schemes Dataset Population Script
 *
 * Auto-generated from external/iso/manifest.json
 * This must be run before other datasets that reference these coding schemes.
 */

set schema 'metadata';

-- =============================================================================
-- ISO Coding Schemes Dataset
-- =============================================================================

\echo '--- ISO Coding Schemes Dataset ---'

-- ISO Coding Schemes
select public.upsert_dq_datasets(
    'iso.coding_schemes',
    'ISO Standards',
    'General',
    'Reference Data',
    'NONE',
    'Primary',
    'Actual',
    'Raw',
    'Wikipedia ISO 3166 Extraction',
    'ISO Coding Schemes',
    'ISO coding scheme definitions for countries (ISO 3166) and currencies (ISO 4217).',
    'WIKIPEDIA',
    'Coding scheme metadata for ISO standards',
    current_date,
    'CC BY-SA 3.0',
    'coding_schemes'
);

