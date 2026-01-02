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
 * Geolocation Schema for IP-to-Country Lookups
 *
 * Stores ip2country data in PostgreSQL for efficient IP lookups.
 * Uses PostgreSQL's int8range type with GiST indexes for fast range queries.
 *
 * Data source: ip2country (https://iptoasn.com/)
 * Format: TSV with 32-bit unsigned integer IP ranges
 *
 * Tables:
 * - ip2country: IPv4 ranges mapped to country codes
 */

set schema 'ores';

--
-- IP to country mapping from ip2country-v4-u32.tsv
-- Uses int8range for efficient range containment queries
--
create table if not exists "ores"."ip2country" (
    -- IP range as int8range (start and end as 32-bit unsigned integers)
    "ip_range" int8range not null,

    -- ISO 3166-1 alpha-2 country code (or 'None' for unrouted)
    "country_code" text not null
);

-- GiST index for efficient IP range containment queries
-- This enables fast lookups using the @> operator
create index if not exists ip2country_range_idx
on "ores"."ip2country" using gist (ip_range);

--
-- Helper function to convert IPv4 address to bigint
-- IPv4 addresses are 32-bit unsigned integers (0 to 4294967295)
-- Returns NULL for IPv6 addresses (not supported by ip2country data)
--
create or replace function ores.inet_to_bigint(ip_address inet)
returns bigint as $$
begin
    -- Only IPv4 is supported; return NULL for IPv6
    if family(ip_address) <> 4 then
        return null;
    end if;

    -- Extract the 32-bit integer representation of the IPv4 address
    -- PostgreSQL's inet stores IPv4 as network byte order
    return (
        (get_byte(inet_send(ip_address), 0)::bigint << 24) +
        (get_byte(inet_send(ip_address), 1)::bigint << 16) +
        (get_byte(inet_send(ip_address), 2)::bigint << 8) +
        (get_byte(inet_send(ip_address), 3)::bigint)
    );
end;
$$ language plpgsql immutable strict;

comment on function ores.inet_to_bigint(inet) is
'Converts an IPv4 address to its 32-bit unsigned integer representation as bigint. Returns NULL for IPv6.';

--
-- Function to lookup country code for an IP address
-- Returns the country code or empty result if not found
-- Note: IPv6 addresses always return empty result (not supported by ip2country)
--
create or replace function ores.geoip_lookup(ip_address inet)
returns table (
    country_code text
) as $$
declare
    ip_as_bigint bigint;
begin
    -- Convert IP to bigint for range lookup (returns NULL for IPv6)
    ip_as_bigint := ores.inet_to_bigint(ip_address);

    -- If NULL (IPv6 or invalid), return empty result
    if ip_as_bigint is null then
        return;
    end if;

    -- Find the range containing this IP
    return query
    select c.country_code
    from ores.ip2country c
    where c.ip_range @> ip_as_bigint
    limit 1;
end;
$$ language plpgsql stable;

comment on function ores.geoip_lookup(inet) is
'Looks up the country code for an IPv4 address. Returns empty result for IPv6 or if not found.';
