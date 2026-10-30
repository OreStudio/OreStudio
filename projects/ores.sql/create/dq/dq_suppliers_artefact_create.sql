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
-- The catalogue of delivery parties. A publisher owns the data; a supplier is who we connect to for it. The release holds two: Frankfurter, an aggregator that delivers many publishers' rates over one endpoint, and the synthetic simulator the platform runs itself. A supplier has sessions, and a session is one configured adapter at that supplier. The system tenant owns the rows. Every tenant reads them and only the system tenant writes them. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_suppliers_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "name" text not null,
    "kind" text not null,
    "description" text not null,
    "contract_reference" text not null default '',
    "support" text not null default '',
    "display_order" integer not null default 0
);

create index if not exists dq_suppliers_artefact_dataset_idx
on ores_dq_suppliers_artefact_tbl (dataset_id);

create index if not exists dq_suppliers_artefact_tenant_idx
on ores_dq_suppliers_artefact_tbl (tenant_id);

create index if not exists dq_suppliers_artefact_code_idx
on ores_dq_suppliers_artefact_tbl (code);
