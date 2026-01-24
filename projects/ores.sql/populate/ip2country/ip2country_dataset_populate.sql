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
 * IP Geolocation Dataset Population Script
 *
 * Creates the dataset entries for IP geolocation reference data.
 * Auto-generated from external/ip2country/manifest.json
 * This must be run before populating the artefact tables.
 */

set schema 'ores';

-- =============================================================================
-- IP Geolocation Datasets
-- =============================================================================

\echo '--- IP Geolocation Datasets ---'

-- IP to Country IPv4 Ranges
select ores.upsert_dq_datasets(
    'geo.ip2country',
    'IP Geolocation',
    'IP Address to Country maps',
    'Reference Data',
    'ISO_3166_1_ALPHA_2',
    'Primary',
    'Actual',
    'Raw',
    'iptoasn.com IP to Country Database',
    'IP to Country IPv4 Ranges',
    'IPv4 address ranges mapped to ISO 3166-1 alpha-2 country codes.',
    'iptoasn.com',
    'Geographic IP lookup for network analysis',
    '2025-01-21'::date,
    'PDDL v1.0',
    'ip_ranges',
    'refdata_ip2country_tbl',
    'dq_populate_ip2country'
);

