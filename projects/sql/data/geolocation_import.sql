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
 * MaxMind GeoLite2-City Data Import Script
 *
 * Prerequisites:
 * 1. Download GeoLite2-City-CSV.zip from MaxMind
 *    https://dev.maxmind.com/geoip/geolite2-free-geolocation-data
 * 2. Extract to a directory accessible by PostgreSQL
 * 3. Set the :data_dir variable to the extraction path
 *
 * Usage:
 *   psql -d your_database -v data_dir='/path/to/GeoLite2-City-CSV' -f geolocation_import.sql
 *
 * Note: This script uses the \copy meta-command, which requires psql to
 * be used for execution. It reads files from the client-side.
 */

-- Clear existing data
truncate table ores.geoip_locations cascade;
truncate table ores.geoip_blocks_ipv4;
truncate table ores.geoip_blocks_ipv6;

-- Create temporary staging tables for CSV import
-- (CSV has different column names and some columns we don't need)

create temp table staging_locations (
    geoname_id integer,
    locale_code text,
    continent_code text,
    continent_name text,
    country_iso_code text,
    country_name text,
    subdivision_1_iso_code text,
    subdivision_1_name text,
    subdivision_2_iso_code text,
    subdivision_2_name text,
    city_name text,
    metro_code text,
    time_zone text,
    is_in_european_union integer
);

create temp table staging_blocks_ipv4 (
    network text,
    geoname_id integer,
    registered_country_geoname_id integer,
    represented_country_geoname_id integer,
    is_anonymous_proxy integer,
    is_satellite_provider integer,
    postal_code text,
    latitude double precision,
    longitude double precision,
    accuracy_radius integer
);

create temp table staging_blocks_ipv6 (
    network text,
    geoname_id integer,
    registered_country_geoname_id integer,
    represented_country_geoname_id integer,
    is_anonymous_proxy integer,
    is_satellite_provider integer,
    postal_code text,
    latitude double precision,
    longitude double precision,
    accuracy_radius integer
);

-- Import locations (English)
\echo 'Importing locations...'
\copy staging_locations from :'data_dir'/GeoLite2-City-Locations-en.csv with (format csv, header true, null '')

-- Import IPv4 blocks
\echo 'Importing IPv4 blocks...'
\copy staging_blocks_ipv4 from :'data_dir'/GeoLite2-City-Blocks-IPv4.csv with (format csv, header true, null '')

-- Import IPv6 blocks
\echo 'Importing IPv6 blocks...'
\copy staging_blocks_ipv6 from :'data_dir'/GeoLite2-City-Blocks-IPv6.csv with (format csv, header true, null '')

-- Insert into final tables with proper type conversions
\echo 'Inserting locations...'
insert into ores.geoip_locations (
    geoname_id, continent_code, continent_name,
    country_iso_code, country_name,
    subdivision_1_iso_code, subdivision_1_name,
    subdivision_2_iso_code, subdivision_2_name,
    city_name, metro_code, time_zone, is_in_european_union
)
select
    geoname_id,
    coalesce(continent_code, ''),
    coalesce(continent_name, ''),
    coalesce(country_iso_code, ''),
    coalesce(country_name, ''),
    coalesce(subdivision_1_iso_code, ''),
    coalesce(subdivision_1_name, ''),
    coalesce(subdivision_2_iso_code, ''),
    coalesce(subdivision_2_name, ''),
    coalesce(city_name, ''),
    coalesce(metro_code, ''),
    coalesce(time_zone, ''),
    coalesce(is_in_european_union = 1, false)
from staging_locations
where geoname_id is not null;

\echo 'Inserting IPv4 blocks...'
insert into ores.geoip_blocks_ipv4 (
    network, geoname_id, registered_country_geoname_id,
    represented_country_geoname_id, is_anonymous_proxy,
    is_satellite_provider, postal_code, latitude, longitude,
    accuracy_radius
)
select
    network::inet,
    geoname_id,
    registered_country_geoname_id,
    represented_country_geoname_id,
    coalesce(is_anonymous_proxy = 1, false),
    coalesce(is_satellite_provider = 1, false),
    coalesce(postal_code, ''),
    latitude,
    longitude,
    accuracy_radius
from staging_blocks_ipv4
where network is not null;

\echo 'Inserting IPv6 blocks...'
insert into ores.geoip_blocks_ipv6 (
    network, geoname_id, registered_country_geoname_id,
    represented_country_geoname_id, is_anonymous_proxy,
    is_satellite_provider, postal_code, latitude, longitude,
    accuracy_radius
)
select
    network::inet,
    geoname_id,
    registered_country_geoname_id,
    represented_country_geoname_id,
    coalesce(is_anonymous_proxy = 1, false),
    coalesce(is_satellite_provider = 1, false),
    coalesce(postal_code, ''),
    latitude,
    longitude,
    accuracy_radius
from staging_blocks_ipv6
where network is not null;

-- Cleanup staging tables
drop table staging_locations;
drop table staging_blocks_ipv4;
drop table staging_blocks_ipv6;

-- Analyze tables for query optimization
\echo 'Analyzing tables...'
analyze ores.geoip_locations;
analyze ores.geoip_blocks_ipv4;
analyze ores.geoip_blocks_ipv6;

-- Show statistics
\echo 'Import complete. Statistics:'
select 'Locations' as table_name, count(*) as row_count from ores.geoip_locations
union all
select 'IPv4 Blocks', count(*) from ores.geoip_blocks_ipv4
union all
select 'IPv6 Blocks', count(*) from ores.geoip_blocks_ipv6;

-- Test lookup
\echo 'Testing lookup for 8.8.8.8 (Google DNS):'
select * from ores.geoip_lookup('8.8.8.8'::inet);
