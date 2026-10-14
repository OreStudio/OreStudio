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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_create.mustache
 * To modify, update the template and regenerate.
 *
 * IP to Country Table
 *
 * An IPv4 address range and the country code it resolves to, one row per range
 * per tenant. The ranges are found by containment -- ip_range @> address -- so
 * the range is the key and the index over it is a GiST index; a btree cannot
 * answer a containment test.
 *
 * The store holds no history. A publish truncates it and loads the ranges the
 * dataset carries, because ip2country reissues its whole table rather than
 * amending rows.
 */

create table if not exists "ores_geo_ip2country_tbl" (
    "ip_range" int8range not null,
    "tenant_id" uuid not null,
    "country_code" text not null,
    primary key (ip_range)
);



create index if not exists ip2country_range_idx
on "ores_geo_ip2country_tbl" using gist (ip_range);

create index if not exists ip2country_tenant_idx
on "ores_geo_ip2country_tbl" (tenant_id);

create or replace function ores_geo_ip2country_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_geo_ip2country_insert_trg
before insert on "ores_geo_ip2country_tbl"
for each row execute function ores_geo_ip2country_insert_fn();

