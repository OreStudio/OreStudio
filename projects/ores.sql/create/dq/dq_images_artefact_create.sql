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
-- An image is a named document the platform renders, such as a currency flag or a commodity icon. The bytes are format-agnostic: an SVG document, a JPEG, or any other media type mime_type names. The database stores them base64-encoded in a text column, and the domain type carries the raw bytes. Images carry tags through the image_tag junction, and an image arrives either by upload or from the data-quality publish path. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_images_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "image_id" uuid not null,
    "version" integer not null,
    "key" text not null,
    "description" text not null,
    "mime_type" text not null,
    "data" text not null
);

create index if not exists dq_images_artefact_dataset_idx
on ores_dq_images_artefact_tbl (dataset_id);

create index if not exists dq_images_artefact_tenant_idx
on ores_dq_images_artefact_tbl (tenant_id);

create index if not exists dq_images_artefact_image_id_idx
on ores_dq_images_artefact_tbl (image_id);
