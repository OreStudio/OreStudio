/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
 * ip2country Data Import Script
 *
 * Imports IP-to-country mapping data from iptoasn.com.
 * Supports periodic updates by truncating and reloading the table.
 *
 * Prerequisites:
 * 1. Download ip2country-v4-u32.tsv from https://iptoasn.com/
 * 2. Extract to a directory accessible by the psql client
 * 3. Set the :data_file variable to the full path of the TSV file
 *
 * Usage:
 *   psql -d your_database \
 *        -v data_file='/path/to/ip2country-v4-u32.tsv' \
 *        -f geolocation_import.sql
 *
 * For periodic updates, simply run this script again with the new data file.
 * The script will truncate and reload all data atomically within a transaction.
 */

\echo 'Starting ip2country import...'
\echo 'Data file: ' :data_file

begin;

-- Clear existing data for fresh import
\echo 'Clearing existing data...'
truncate table ores.ip2country;

-- Create temporary staging table for TSV import
-- Columns: range_start (bigint), range_end (bigint), country_code (text)
create temp table staging_ip2country (
    range_start bigint,
    range_end bigint,
    country_code text
) on commit drop;

-- Import TSV file (tab-separated, no header)
\echo 'Importing TSV data...'
\copy staging_ip2country from :'data_file' with (format text, delimiter E'\t')

-- Insert into final table, converting start/end to int8range
-- Use '[)' bounds: inclusive start, exclusive end+1 for proper range semantics
\echo 'Converting to int8range and inserting...'
insert into ores.ip2country (ip_range, country_code)
select
    int8range(range_start, range_end + 1, '[)'),
    country_code
from staging_ip2country;

-- Staging table is automatically dropped on commit

commit;

-- Analyze table for query optimization
\echo 'Analyzing table...'
analyze ores.ip2country;

-- Show statistics
\echo 'Import complete. Statistics:'
select
    count(*) as total_ranges,
    count(distinct country_code) as unique_countries,
    count(*) filter (where country_code = 'None') as unrouted_ranges
from ores.ip2country;

-- Test lookup for well-known IPs
\echo 'Testing lookups:'
\echo '  8.8.8.8 (Google DNS, expected: US):'
select * from ores.geoip_lookup('8.8.8.8'::inet);

\echo '  1.1.1.1 (Cloudflare, expected: US):'
select * from ores.geoip_lookup('1.1.1.1'::inet);
