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
 * FpML Person Role Dataset Population Script
 *
 * Creates the dataset entry for fpml.person_role.
 * Source version: 1-3
 * This must be run before populating the artefact table.
 */

set schema 'metadata';

-- =============================================================================
-- FpML Person Role Dataset
-- =============================================================================

\echo '--- FpML Person Role Dataset ---'

select public.upsert_dq_datasets(
    'fpml.person_role',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_PERSON_ROLE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Person Role',
    'Indicates the role of a person in a transaction.',
    'FPML',
    'Reference data for FpML Person Role (version 1-3)',
    '2019-10-31'::date,
    'FpML Public License 2.0',
    'person_roles'
);
