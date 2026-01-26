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
 * IP Geolocation Methodology Population Script
 *
 * Auto-generated from external/ip2country/manifest.json
 * This script is idempotent.
 */

set schema 'metadata';

-- =============================================================================
-- IP Geolocation Data Sourcing Methodologies
-- =============================================================================

\echo '--- IP Geolocation Methodologies ---'

select public.upsert_dq_methodologies(
    'iptoasn.com IP to Country Database',
    'IPv4 ranges mapped to country codes from iptoasn.com',
    'https://iptoasn.com/',
    'Last Download: 2025-01-21

# IP to Country Mapping - Methodology

## Data Source

The IP to country mapping data is sourced from iptoasn.com, which provides
free IP address to ASN (Autonomous System Number) and country mappings
derived from BGP routing data.

Website: https://iptoasn.com/
License: PDDL v1.0 (Public Domain Dedication and License)

## Data Format

The data is provided as a TSV file with IPv4 ranges expressed as unsigned
32-bit integers:

- range_start: First IP address in the range (as u32)
- range_end: Last IP address in the range (as u32)
- country_code: ISO 3166-1 alpha-2 code, or "None" for unrouted ranges

## Update Frequency

iptoasn.com updates their data regularly based on BGP routing changes.
For most use cases, monthly updates are sufficient.

## Download Instructions

1. Visit https://iptoasn.com/
2. Download "ip2country-v4-u32.tsv.gz"
3. Extract the gzip file
4. Replace the existing ip2country-v4-u32.tsv file

## Data Quality Notes

- Some IP ranges map to "None" (unrouted or reserved)
- Country codes follow ISO 3166-1 alpha-2 standard
- Data accuracy depends on BGP routing information'
);

