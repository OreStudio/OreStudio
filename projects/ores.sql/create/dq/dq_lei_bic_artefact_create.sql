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

-- =============================================================================
-- GLEIF LEI to BIC mapping data - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_lei_bic_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "lei" text not null,
    "version" integer not null,
    "bic" text not null
);

create index if not exists dq_lei_bic_artefact_dataset_idx
on ores_dq_lei_bic_artefact_tbl (dataset_id);

create index if not exists dq_lei_bic_artefact_tenant_idx
on ores_dq_lei_bic_artefact_tbl (tenant_id);

create index if not exists dq_lei_bic_artefact_lei_idx
on ores_dq_lei_bic_artefact_tbl (lei);

create index if not exists dq_lei_bic_artefact_bic_idx
on ores_dq_lei_bic_artefact_tbl (bic);

-- Function to insert lei_bic into the artefact table
create or replace function ores_dq_lei_bic_artefact_insert_fn(
    p_dataset_id uuid,
    p_tenant_id uuid,
    p_lei text,
    p_version integer,
    p_bic text
) returns void as $$
begin
    insert into ores_dq_lei_bic_artefact_tbl (
        dataset_id, tenant_id, lei, version, bic
    )
    values (
        p_dataset_id, p_tenant_id, p_lei, p_version, p_bic
    );
end;
$$ language plpgsql;
