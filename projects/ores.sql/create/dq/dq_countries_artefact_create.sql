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
create table if not exists "ores_dq_countries_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "alpha2_code" text not null,
    "version" integer not null,
    "alpha3_code" text not null,
    "numeric_code" text not null,
    "name" text not null,
    "official_name" text not null,
    "image_id" uuid
);

create index if not exists ores_dq_countries_artefact_dataset_idx
on "ores_dq_countries_artefact_tbl" (dataset_id);

create index if not exists ores_dq_countries_artefact_tenant_idx
on "ores_dq_countries_artefact_tbl" (tenant_id);

create index if not exists ores_dq_countries_artefact_alpha2_idx
on "ores_dq_countries_artefact_tbl" (alpha2_code);

create index if not exists ores_dq_countries_artefact_alpha3_idx
on "ores_dq_countries_artefact_tbl" (alpha3_code);

create index if not exists ores_dq_countries_artefact_numeric_idx
on "ores_dq_countries_artefact_tbl" (numeric_code);

-- Function to insert countries into the artifact table
create or replace function ores_dq_countries_artefact_insert_fn(
    p_dataset_id uuid,
    p_tenant_id uuid,
    p_alpha2_code text,
    p_version integer,
    p_alpha3_code text,
    p_numeric_code text,
    p_name text,
    p_official_name text,
    p_image_id uuid default null
) returns void as $$
begin
    insert into ores_dq_countries_artefact_tbl (
        dataset_id, tenant_id, alpha2_code, version, alpha3_code, numeric_code, name, official_name, image_id
    )
    values (
        p_dataset_id, p_tenant_id, p_alpha2_code, p_version, p_alpha3_code, p_numeric_code, p_name, p_official_name, p_image_id
    );
end;
$$ language plpgsql;