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
 * FpML Party Role Type Dataset Population Script
 *
 * Creates the dataset entry for fpml.party_role_type.
 * Source version: 1-0
 * This must be run before populating the artefact table.
 */

-- =============================================================================
-- FpML Party Role Type Dataset
-- =============================================================================

\echo '--- FpML Party Role Type Dataset ---'

select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    'fpml.party_role_type',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_PARTY_ROLE_TYPE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Party Role Type',
    'Contains a code representing a related party role type. A type refining the role a role played by a party in one or more transactions. This can be extended to provide custom types.',
    'FPML',
    'Reference data for FpML Party Role Type (version 1-0)',
    '2011-04-24'::date,
    'FpML Public License 2.0',
    'party_roles'
);
