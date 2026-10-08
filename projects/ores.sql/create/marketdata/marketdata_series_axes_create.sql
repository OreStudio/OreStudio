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
 * Series Axis Table
 *
 * The axes a composite market series varies over. A term structure varies over one
 * axis and a surface over two. Each row names one field the series' instrument type
 * marks as a coordinate, so the object's shape is a declared thing rather than an
 * inference from whatever points happen to be stored.
 *
 * The rows are ordered. The sequence column holds the position of each axis among
 * the object's coordinates, so a reader walks the axes in the order the type's key
 * writes them and a hole in a grid is a hole rather than an absent row.
 *
 * The table is a current state, not a history: the series row is already temporal
 * and the shape of a series is written again whenever it is built. A series with no
 * rows here declares no shape, and a caller that reads one gets nothing rather than
 * a fabricated default.
 */

create table if not exists "ores_marketdata_series_axes_tbl" (
    "series_id" uuid not null,
    "axis_field" text not null,
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "sequence" integer not null default 0,
    primary key (series_id, axis_field),
    check ("series_id" <> ores_utility_nil_uuid_fn()),
    check ("axis_field" <> '')
);



create index if not exists series_axes_series_idx
on "ores_marketdata_series_axes_tbl" (tenant_id, series_id);

create or replace function ores_marketdata_series_axes_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_series_axes_insert_trg
before insert on "ores_marketdata_series_axes_tbl"
for each row execute function ores_marketdata_series_axes_insert_fn();


-- =============================================================================
-- Row-level security: tenant isolation for Series Axis
-- =============================================================================
alter table ores_marketdata_series_axes_tbl enable row level security;

drop policy if exists series_axes_tbl_tenant_isolation_policy
    on ores_marketdata_series_axes_tbl;

create policy series_axes_tbl_tenant_isolation_policy
on ores_marketdata_series_axes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
