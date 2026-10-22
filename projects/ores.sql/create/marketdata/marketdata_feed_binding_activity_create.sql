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
 * Feed Binding Activity
 *
 * What each feed binding last delivered: the series its source writes and the time of its
 * newest tick. A tick has no source in the tick table, because the source is a fact about the
 * binding and not about each tick, so the ingest records it here. It writes one row for each
 * binding at most every few seconds, so the row trails the newest tick by that wait.
 *
 * The ingest context names no party, so the table has the tenant policy only, as the
 * observation table has, and a reader states the party in its query.
 */

create table if not exists "ores_marketdata_feed_binding_activity_tbl" (
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "source_name" text not null,
    "series_id" uuid not null,
    "last_tick_at" timestamp with time zone not null,
    primary key (tenant_id, party_id, source_name),
    check ("source_name" <> '')
);

alter table ores_marketdata_feed_binding_activity_tbl enable row level security;

drop policy if exists feed_binding_activity_tbl_tenant_isolation_policy
    on ores_marketdata_feed_binding_activity_tbl;

create policy feed_binding_activity_tbl_tenant_isolation_policy
on ores_marketdata_feed_binding_activity_tbl
for all using (
    tenant_id = (select nullif(current_setting('app.current_tenant_id', true), '')::uuid)
)
with check (
    tenant_id = (select nullif(current_setting('app.current_tenant_id', true), '')::uuid)
);
