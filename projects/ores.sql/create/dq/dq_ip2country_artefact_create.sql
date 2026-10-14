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
/*
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_artefact_create.mustache
 * To modify, update the template and regenerate.
 */

-- =============================================================================
-- An IPv4 address range and the country code it resolves to, one row per range per tenant. The ranges are found by containment -- ip_range @> address -- so the range is the key and the index over it is a GiST index; a btree cannot answer a containment test. The store holds no history. A publish truncates it and loads the ranges the dataset carries, because ip2country reissues its whole table rather than amending rows. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_ip2country_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "range_start" bigint not null,
    "range_end" bigint not null,
    "country_code" text not null
);

create index if not exists dq_ip2country_artefact_dataset_idx
on ores_dq_ip2country_artefact_tbl (dataset_id);

create index if not exists dq_ip2country_artefact_tenant_idx
on ores_dq_ip2country_artefact_tbl (tenant_id);

create index if not exists dq_ip2country_artefact_range_start_idx
on ores_dq_ip2country_artefact_tbl (range_start);

create index if not exists dq_ip2country_artefact_range_idx
on ores_dq_ip2country_artefact_tbl using gist (int8range(range_start, range_end + 1, '[)'));
