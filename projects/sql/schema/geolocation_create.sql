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
 * Geolocation Schema for IP-to-Location Lookups
 *
 * Stores MaxMind GeoLite2-City data in PostgreSQL for efficient IP lookups.
 * Uses PostgreSQL's native inet type with GiST indexes for range queries.
 *
 * Data source: MaxMind GeoLite2-City (CSV format)
 * Download from: https://dev.maxmind.com/geoip/geolite2-free-geolocation-data
 *
 * Tables:
 * - geoip_locations: Location data (countries, cities, coordinates)
 * - geoip_blocks_ipv4: IPv4 CIDR blocks mapped to locations
 * - geoip_blocks_ipv6: IPv6 CIDR blocks mapped to locations
 */

set schema 'ores';

--
-- Location data from GeoLite2-City-Locations-en.csv
-- Contains country/city names and coordinates
--
create table if not exists "ores"."geoip_locations" (
    -- MaxMind geoname_id (primary key)
    "geoname_id" integer primary key,

    -- Continent
    "continent_code" text not null default '',
    "continent_name" text not null default '',

    -- Country
    "country_iso_code" text not null default '',
    "country_name" text not null default '',

    -- Subdivision (state/province)
    "subdivision_1_iso_code" text not null default '',
    "subdivision_1_name" text not null default '',
    "subdivision_2_iso_code" text not null default '',
    "subdivision_2_name" text not null default '',

    -- City
    "city_name" text not null default '',

    -- Metro code (US only)
    "metro_code" text not null default '',

    -- Timezone
    "time_zone" text not null default '',

    -- Is in European Union
    "is_in_european_union" boolean not null default false
);

-- Index on country code for filtering
create index if not exists geoip_locations_country_idx
on "ores"."geoip_locations" (country_iso_code);

--
-- IPv4 blocks from GeoLite2-City-Blocks-IPv4.csv
-- Maps CIDR ranges to geoname_id
--
create table if not exists "ores"."geoip_blocks_ipv4" (
    -- IP network in CIDR notation (e.g., 192.168.1.0/24)
    "network" inet not null,

    -- Foreign key to geoip_locations
    "geoname_id" integer,

    -- Registered country (may differ from actual location)
    "registered_country_geoname_id" integer,

    -- Represented country (for proxies/VPNs)
    "represented_country_geoname_id" integer,

    -- Anonymous proxy flag
    "is_anonymous_proxy" boolean not null default false,

    -- Satellite provider flag
    "is_satellite_provider" boolean not null default false,

    -- Postal code
    "postal_code" text not null default '',

    -- Coordinates (more precise than city-level)
    "latitude" double precision,
    "longitude" double precision,

    -- Accuracy radius in km
    "accuracy_radius" integer
);

-- GiST index for efficient IP range lookups
-- This is the key to fast IP lookups
create index if not exists geoip_blocks_ipv4_network_idx
on "ores"."geoip_blocks_ipv4" using gist (network inet_ops);

--
-- IPv6 blocks from GeoLite2-City-Blocks-IPv6.csv
-- Same structure as IPv4
--
create table if not exists "ores"."geoip_blocks_ipv6" (
    "network" inet not null,
    "geoname_id" integer,
    "registered_country_geoname_id" integer,
    "represented_country_geoname_id" integer,
    "is_anonymous_proxy" boolean not null default false,
    "is_satellite_provider" boolean not null default false,
    "postal_code" text not null default '',
    "latitude" double precision,
    "longitude" double precision,
    "accuracy_radius" integer
);

create index if not exists geoip_blocks_ipv6_network_idx
on "ores"."geoip_blocks_ipv6" using gist (network inet_ops);

--
-- Function to lookup IP address and return location data
-- Works with both IPv4 and IPv6 addresses
--
create or replace function ores.geoip_lookup(ip_address inet)
returns table (
    country_code text,
    country_name text,
    city_name text,
    latitude double precision,
    longitude double precision,
    accuracy_radius integer
) as $$
begin
    -- Try IPv4 first (more common)
    return query
    select
        l.country_iso_code,
        l.country_name,
        l.city_name,
        coalesce(b.latitude, 0.0),
        coalesce(b.longitude, 0.0),
        coalesce(b.accuracy_radius, 0)
    from ores.geoip_blocks_ipv4 b
    left join ores.geoip_locations l on l.geoname_id = b.geoname_id
    where b.network >> ip_address
    order by masklen(b.network) desc
    limit 1;

    if found then
        return;
    end if;

    -- Try IPv6
    return query
    select
        l.country_iso_code,
        l.country_name,
        l.city_name,
        coalesce(b.latitude, 0.0),
        coalesce(b.longitude, 0.0),
        coalesce(b.accuracy_radius, 0)
    from ores.geoip_blocks_ipv6 b
    left join ores.geoip_locations l on l.geoname_id = b.geoname_id
    where b.network >> ip_address
    order by masklen(b.network) desc
    limit 1;
end;
$$ language plpgsql stable;

-- Comment on the lookup function
comment on function ores.geoip_lookup(inet) is
'Looks up geolocation data for an IP address. Returns country, city, and coordinates.';
