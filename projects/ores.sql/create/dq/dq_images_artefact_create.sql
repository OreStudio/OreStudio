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
create table if not exists "ores_dq_images_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "image_id" uuid not null,
    "version" integer not null,
    "key" text not null,
    "description" text not null,
    "svg_data" text not null
);

create index if not exists ores_dq_images_artefact_dataset_idx
on "ores_dq_images_artefact_tbl" (dataset_id);

create index if not exists ores_dq_images_artefact_tenant_idx
on "ores_dq_images_artefact_tbl" (tenant_id);

create index if not exists ores_dq_images_artefact_image_idx
on "ores_dq_images_artefact_tbl" (image_id);

create index if not exists ores_dq_images_artefact_key_idx
on "ores_dq_images_artefact_tbl" (key);

-- Function to insert images into the artifact table
create or replace function ores_dq_images_artefact_insert_fn(
    p_dataset_id uuid,
    p_tenant_id uuid,
    p_image_id uuid,
    p_version integer,
    p_key text,
    p_description text,
    p_svg_data text
) returns void as $$
begin
    insert into ores_dq_images_artefact_tbl (
        dataset_id, tenant_id, image_id, version, key, description, svg_data
    )
    values (
        p_dataset_id, p_tenant_id, p_image_id, p_version, p_key, p_description, p_svg_data
    );
end;
$$ language plpgsql;
