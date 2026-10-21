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
 * Tick Coordinate Dictionary
 *
 * A tick row stores the coordinate of its datum as a small number, not as the URI text. This
 * table says which datum each number means within one series. Coordinate 0 is the datum of a
 * series without coordinates, and it has no row here. The numbers of a series start at 1 and
 * stay for as long as the series exists, because a tick row refers to them.
 *
 * The ingest assigns a number the first time it sees a datum, so the table is small and cold:
 * one row for each distinct datum of a series. The ingest context names no party, so the
 * table has the tenant policy only, as the observation table has, and a reader states the
 * party in its query.
 */

create table if not exists "ores_marketdata_tick_coords_tbl" (
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "series_id" uuid not null,
    "coord" smallint not null,
    "oresmd_uri" text not null,
    primary key (series_id, coord),
    unique (series_id, oresmd_uri),
    check ("coord" > 0),
    check ("oresmd_uri" <> '')
);

alter table ores_marketdata_tick_coords_tbl enable row level security;

drop policy if exists tick_coords_tbl_tenant_isolation_policy on ores_marketdata_tick_coords_tbl;

create policy tick_coords_tbl_tenant_isolation_policy
on ores_marketdata_tick_coords_tbl
for all using (
    tenant_id = (select nullif(current_setting('app.current_tenant_id', true), '')::uuid)
)
with check (
    tenant_id = (select nullif(current_setting('app.current_tenant_id', true), '')::uuid)
);
