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
-- =============================================================================
-- IP to country lookup.
--
-- The store is generated from ores.geo.ip2country. These two functions are not:
-- a model states a table's shape, not the query a caller runs against it.
-- =============================================================================

-- The ranges are counted in the integers IPv4 addresses are. An address that is
-- not IPv4 has no such integer and is answered with nothing rather than an
-- error, so a caller asking about an IPv6 address gets no country.
create or replace function ores_geo_inet_to_bigint_fn(ip_address inet)
returns bigint as $$
begin
    if family(ip_address) <> 4 then
        return null;
    end if;

    -- The inet difference operator yields the address as an integer, which is
    -- the space the ip2country ranges are stored in.
    return ip_address - '0.0.0.0'::inet;
end;
$$ language plpgsql immutable strict;

-- The ranges are published per tenant, so the lookup is per tenant too: the
-- same address resolves to a country in the tenant whose ranges cover it,
-- and to nothing in a tenant that has published none.
create or replace function ores_geo_ip2country_lookup_fn(
    p_tenant_id uuid,
    p_ip_address inet
)
returns table (
    country_code text
) as $$
declare
    ip_as_bigint bigint;
begin
    if p_tenant_id is null then
        return;
    end if;

    ip_as_bigint := ores_geo_inet_to_bigint_fn(p_ip_address);

    if ip_as_bigint is null then
        return;
    end if;

    return query
    select c.country_code
    from ores_geo_ip2country_tbl c
    where c.tenant_id = p_tenant_id
      and c.ip_range @> ip_as_bigint
    limit 1;
end;
$$ language plpgsql stable;
