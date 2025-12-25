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

-- Remove retention policy
select remove_retention_policy('ores.sessions', if_exists => true);

-- Remove compression policy
select remove_compression_policy('ores.sessions', if_exists => true);

-- Drop continuous aggregates that depend on sessions table
drop materialized view if exists "ores"."session_stats_daily" cascade;
drop materialized view if exists "ores"."session_stats_hourly" cascade;
drop materialized view if exists "ores"."session_stats_aggregate_daily" cascade;

-- Drop sessions table (this also drops the hypertable and all chunks)
drop table if exists "ores"."sessions" cascade;
