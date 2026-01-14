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
set schema 'ores';

create table if not exists "ores"."geo_ip2country_tbl" (
    "ip_range" int8range not null,
    "country_code" text not null
);

create index if not exists geo_ip2country_range_idx
on "ores"."geo_ip2country_tbl" using gist (ip_range);

create or replace function ores.geo_inet_to_bigint_fn(ip_address inet)
returns bigint as $$
begin
    if family(ip_address) <> 4 then
        return null;
    end if;

    return (
        (get_byte(inet_send(ip_address), 0)::bigint << 24) +
        (get_byte(inet_send(ip_address), 1)::bigint << 16) +
        (get_byte(inet_send(ip_address), 2)::bigint << 8) +
        (get_byte(inet_send(ip_address), 3)::bigint)
    );
end;
$$ language plpgsql immutable strict;

create or replace function ores.geo_ip_lookup_fn(ip_address inet)
returns table (
    country_code text
) as $$
declare
    ip_as_bigint bigint;
begin
    ip_as_bigint := ores.geo_inet_to_bigint_fn(ip_address);

    if ip_as_bigint is null then
        return;
    end if;

    return query
    select c.country_code
    from ores.geo_ip2country_tbl c
    where c.ip_range @> ip_as_bigint
    limit 1;
end;
$$ language plpgsql stable;
