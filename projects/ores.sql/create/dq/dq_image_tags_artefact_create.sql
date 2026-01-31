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
create table if not exists "ores_dq_image_tags_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "image_id" uuid not null,
    "tag_id" uuid not null
);

create index if not exists ores_dq_image_tags_artefact_dataset_idx
on "ores_dq_image_tags_artefact_tbl" (dataset_id);

create index if not exists ores_dq_image_tags_artefact_tenant_idx
on "ores_dq_image_tags_artefact_tbl" (tenant_id);

create index if not exists ores_dq_image_tags_artefact_image_idx
on "ores_dq_image_tags_artefact_tbl" (image_id);

create index if not exists ores_dq_image_tags_artefact_tag_idx
on "ores_dq_image_tags_artefact_tbl" (tag_id);

-- Function to insert image tags into the artifact table
create or replace function ores_dq_image_tags_artefact_insert_fn(
    p_dataset_id uuid,
    p_tenant_id uuid,
    p_image_id uuid,
    p_tag_id uuid
) returns void as $$
begin
    insert into ores_dq_image_tags_artefact_tbl (
        dataset_id, tenant_id, image_id, tag_id
    )
    values (
        p_dataset_id, p_tenant_id, p_image_id, p_tag_id
    );
end;
$$ language plpgsql;
