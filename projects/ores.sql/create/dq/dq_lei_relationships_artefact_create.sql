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
-- GLEIF LEI corporate hierarchy relationships from RR golden copy files - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_lei_relationships_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "relationship_start_node_node_id" text not null,
    "version" integer not null,
    "relationship_start_node_node_id_type" text not null,
    "relationship_end_node_node_id" text not null,
    "relationship_end_node_node_id_type" text not null,
    "relationship_relationship_type" text not null,
    "relationship_relationship_status" text not null,
    "relationship_period_1_start_date" timestamp with time zone null,
    "relationship_period_1_end_date" timestamp with time zone null,
    "registration_initial_registration_date" timestamp with time zone null,
    "registration_last_update_date" timestamp with time zone null,
    "registration_registration_status" text null,
    "registration_validation_sources" text null
);

create index if not exists dq_lei_relationships_artefact_dataset_idx
on ores_dq_lei_relationships_artefact_tbl (dataset_id);

create index if not exists dq_lei_relationships_artefact_tenant_idx
on ores_dq_lei_relationships_artefact_tbl (tenant_id);

create index if not exists dq_lei_relationships_artefact_relationship_start_node_node_id_idx
on ores_dq_lei_relationships_artefact_tbl (relationship_start_node_node_id);

create index if not exists dq_lei_relationships_artefact_relationship_end_node_node_id_idx
on ores_dq_lei_relationships_artefact_tbl (relationship_end_node_node_id);

create index if not exists dq_lei_relationships_artefact_relationship_relationship_type_idx
on ores_dq_lei_relationships_artefact_tbl (relationship_relationship_type);

-- Function to insert lei_relationships into the artefact table
create or replace function ores_dq_lei_relationships_artefact_insert_fn(
    p_dataset_id uuid,
    p_tenant_id uuid,
    p_relationship_start_node_node_id text,
    p_version integer,
    p_relationship_start_node_node_id_type text,
    p_relationship_end_node_node_id text,
    p_relationship_end_node_node_id_type text,
    p_relationship_relationship_type text,
    p_relationship_relationship_status text,
    p_relationship_period_1_start_date timestamp with time zone,
    p_relationship_period_1_end_date timestamp with time zone,
    p_registration_initial_registration_date timestamp with time zone,
    p_registration_last_update_date timestamp with time zone,
    p_registration_registration_status text,
    p_registration_validation_sources text
) returns void as $$
begin
    insert into ores_dq_lei_relationships_artefact_tbl (
        dataset_id, tenant_id, relationship_start_node_node_id, version,
        relationship_start_node_node_id_type,
        relationship_end_node_node_id,
        relationship_end_node_node_id_type,
        relationship_relationship_type,
        relationship_relationship_status,
        relationship_period_1_start_date,
        relationship_period_1_end_date,
        registration_initial_registration_date,
        registration_last_update_date,
        registration_registration_status,
        registration_validation_sources
    )
    values (
        p_dataset_id, p_tenant_id, p_relationship_start_node_node_id, p_version,
        p_relationship_start_node_node_id_type,
        p_relationship_end_node_node_id,
        p_relationship_end_node_node_id_type,
        p_relationship_relationship_type,
        p_relationship_relationship_status,
        p_relationship_period_1_start_date,
        p_relationship_period_1_end_date,
        p_registration_initial_registration_date,
        p_registration_last_update_date,
        p_registration_registration_status,
        p_registration_validation_sources
    );
end;
$$ language plpgsql;
