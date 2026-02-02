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
 * FpML Cftc Organization Type Dataset Population Script
 *
 * Creates the dataset entry for fpml.cftc_organization_type.
 * Source version: 2-0
 * This must be run before populating the artefact table.
 */

-- =============================================================================
-- FpML Cftc Organization Type Dataset
-- =============================================================================

\echo '--- FpML Cftc Organization Type Dataset ---'

select ores_dq_datasets_upsert_fn(ores_iam_system_tenant_id_fn(),
    'fpml.cftc_organization_type',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_CFTC_ORGANIZATION_TYPE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Cftc Organization Type',
    'Indicates whether a counterparty is an entity established pursuant to a U.S. federal law, including CFTC Amendments to Part 45 (2020).',
    'FPML',
    'Reference data for FpML Cftc Organization Type (version 2-0)',
    '2022-11-18'::date,
    'FpML Public License 2.0',
    'entity_classifications'
);
