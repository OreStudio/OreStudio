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

/**
 * Report configuration bindings: the staging table
 *
 * A report definition binds the configuration documents it reads, one per
 * configuration type its report type requires. A dataset that stages definitions
 * stages their bindings in this table beside them. A binding names its definition
 * and its configuration by name, never by id, because the ids of both are made
 * when they are published. The definitions publish resolves each name for the
 * target party, so the documents are published before the definitions.
 */

create table if not exists "ores_dq_report_configurations_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "report_name" text not null,
    "configuration_type_code" text not null,
    "configuration_name" text not null
);

create index if not exists dq_report_configurations_artefact_dataset_idx
on ores_dq_report_configurations_artefact_tbl (dataset_id);

create index if not exists dq_report_configurations_artefact_tenant_idx
on ores_dq_report_configurations_artefact_tbl (tenant_id);
