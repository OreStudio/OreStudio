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
 * DQ IP to Country Artefact Table
 *
 * Staging table for IPv4 to country mapping data from iptoasn.com.
 * Data is imported here first, then promoted to geo_ip2country_tbl.
 */

set schema 'metadata';

create table if not exists "metadata"."dq_ip2country_artefact_tbl" (
    "dataset_id" uuid not null,
    "range_start" bigint not null,
    "range_end" bigint not null,
    "country_code" text not null
);

create index if not exists dq_ip2country_artefact_dataset_idx
on "metadata"."dq_ip2country_artefact_tbl" (dataset_id);

create index if not exists dq_ip2country_artefact_range_idx
on "metadata"."dq_ip2country_artefact_tbl" using gist (int8range(range_start, range_end + 1, '[)'));
