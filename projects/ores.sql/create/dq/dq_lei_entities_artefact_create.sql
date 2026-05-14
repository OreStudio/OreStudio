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
 * Template: sql_schema_artefact_create.mustache
 * To modify, update the template and regenerate.
 */

-- =============================================================================
-- GLEIF LEI entity master data from LEI2 golden copy files - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_lei_entities_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "lei" text not null,
    "version" integer not null,
    "entity_legal_name" text not null,
    "entity_entity_category" text not null,
    "entity_entity_sub_category" text null,
    "entity_entity_status" text not null,
    "entity_legal_form_entity_legal_form_code" text null,
    "entity_legal_form_other_legal_form" text null,
    "entity_legal_jurisdiction" text null,
    "entity_legal_address_first_address_line" text null,
    "entity_legal_address_city" text null,
    "entity_legal_address_region" text null,
    "entity_legal_address_country" text not null,
    "entity_legal_address_postal_code" text null,
    "entity_headquarters_address_first_address_line" text null,
    "entity_headquarters_address_city" text null,
    "entity_headquarters_address_region" text null,
    "entity_headquarters_address_country" text null,
    "entity_headquarters_address_postal_code" text null,
    "entity_entity_creation_date" timestamp with time zone null,
    "registration_initial_registration_date" timestamp with time zone null,
    "registration_last_update_date" timestamp with time zone null,
    "registration_next_renewal_date" timestamp with time zone null,
    "registration_registration_status" text null
);

create index if not exists dq_lei_entities_artefact_dataset_idx
on ores_dq_lei_entities_artefact_tbl (dataset_id);

create index if not exists dq_lei_entities_artefact_tenant_idx
on ores_dq_lei_entities_artefact_tbl (tenant_id);

create index if not exists dq_lei_entities_artefact_lei_idx
on ores_dq_lei_entities_artefact_tbl (lei);

create index if not exists dq_lei_entities_artefact_entity_legal_address_country_idx
on ores_dq_lei_entities_artefact_tbl (entity_legal_address_country);

create index if not exists dq_lei_entities_artefact_entity_entity_category_idx
on ores_dq_lei_entities_artefact_tbl (entity_entity_category);

-- Function to insert lei_entities into the artefact table
create or replace function ores_dq_lei_entities_artefact_insert_fn(
    p_dataset_id uuid,
    p_tenant_id uuid,
    p_lei text,
    p_version integer,
    p_entity_legal_name text,
    p_entity_entity_category text,
    p_entity_entity_sub_category text,
    p_entity_entity_status text,
    p_entity_legal_form_entity_legal_form_code text,
    p_entity_legal_form_other_legal_form text,
    p_entity_legal_jurisdiction text,
    p_entity_legal_address_first_address_line text,
    p_entity_legal_address_city text,
    p_entity_legal_address_region text,
    p_entity_legal_address_country text,
    p_entity_legal_address_postal_code text,
    p_entity_headquarters_address_first_address_line text,
    p_entity_headquarters_address_city text,
    p_entity_headquarters_address_region text,
    p_entity_headquarters_address_country text,
    p_entity_headquarters_address_postal_code text,
    p_entity_entity_creation_date timestamp with time zone,
    p_registration_initial_registration_date timestamp with time zone,
    p_registration_last_update_date timestamp with time zone,
    p_registration_next_renewal_date timestamp with time zone,
    p_registration_registration_status text
) returns void as $$
begin
    insert into ores_dq_lei_entities_artefact_tbl (
        dataset_id, tenant_id, lei, version,
        entity_legal_name,
        entity_entity_category,
        entity_entity_sub_category,
        entity_entity_status,
        entity_legal_form_entity_legal_form_code,
        entity_legal_form_other_legal_form,
        entity_legal_jurisdiction,
        entity_legal_address_first_address_line,
        entity_legal_address_city,
        entity_legal_address_region,
        entity_legal_address_country,
        entity_legal_address_postal_code,
        entity_headquarters_address_first_address_line,
        entity_headquarters_address_city,
        entity_headquarters_address_region,
        entity_headquarters_address_country,
        entity_headquarters_address_postal_code,
        entity_entity_creation_date,
        registration_initial_registration_date,
        registration_last_update_date,
        registration_next_renewal_date,
        registration_registration_status
    )
    values (
        p_dataset_id, p_tenant_id, p_lei, p_version,
        p_entity_legal_name,
        p_entity_entity_category,
        p_entity_entity_sub_category,
        p_entity_entity_status,
        p_entity_legal_form_entity_legal_form_code,
        p_entity_legal_form_other_legal_form,
        p_entity_legal_jurisdiction,
        p_entity_legal_address_first_address_line,
        p_entity_legal_address_city,
        p_entity_legal_address_region,
        p_entity_legal_address_country,
        p_entity_legal_address_postal_code,
        p_entity_headquarters_address_first_address_line,
        p_entity_headquarters_address_city,
        p_entity_headquarters_address_region,
        p_entity_headquarters_address_country,
        p_entity_headquarters_address_postal_code,
        p_entity_entity_creation_date,
        p_registration_initial_registration_date,
        p_registration_last_update_date,
        p_registration_next_renewal_date,
        p_registration_registration_status
    );
end;
$$ language plpgsql;
