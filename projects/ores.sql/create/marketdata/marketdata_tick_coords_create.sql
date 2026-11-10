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
 * Tick Coordinate Table
 *
 * A tick row stores its datum as a small number, not as the URI text. This table
 * says which datum each number means within one stream. A series without
 * coordinates has one datum, and so one row. The numbers of a stream start at 1 and
 * stay for as long as the stream exists, because a tick row refers to them. The URI
 * is the one the feed sent, which is the URI of the observation rows too.
 *
 * The stream is shared by every party, so this table is shared too: one row for each
 * distinct datum of a stream, not one for each party of it. Every party of the
 * stream therefore agrees on the coordinate order, and the read joins the table by
 * the stream id alone.
 *
 * The ingest assigns a number the first time it sees a datum. The table belongs to
 * no tenant and no party and carries no row-level security, as the tick table
 * does.
 *
 * Variables: :marketdata_writer_user
 */

create table if not exists "ores_marketdata_tick_coords_tbl" (
    "stream_id" uuid not null,
    "coord" smallint not null,
    "oresmd_uri" text not null,
    primary key (stream_id, coord),
    check ("stream_id" <> ores_utility_nil_uuid_fn())
);



create unique index if not exists marketdata_tick_coords_stream_uri_idx
on "ores_marketdata_tick_coords_tbl" (stream_id, oresmd_uri);

create or replace function ores_marketdata_tick_coords_insert_fn()
returns trigger as $$
declare
begin


    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_tick_coords_insert_trg
before insert on "ores_marketdata_tick_coords_tbl"
for each row execute function ores_marketdata_tick_coords_insert_fn();


grant insert on ores_marketdata_tick_coords_tbl to :marketdata_writer_user;
