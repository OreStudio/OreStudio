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
-- A retention policy declares one bounded store: what it targets, what action is taken, and the period past which the action applies. The target is named by shape rather than by a particular table, so the policy survives a table being replaced. A policy is a row of the installation, held by the system tenant: the classification that governs published rows makes the platform the author of the stores it bounds, and one copy in the system tenant is what keeps two installations' policies from disagreeing. The column is not null for that reason: a policy with no tenant has no owner to be published to. A policy is data, not a schema constant, so a period changes without a rebuild. The scheduler fires one sweep, which applies the enabled policies that are due, including where the storage engine runs no retention job of its own. A policy is due once its cadence has passed since the sweep last applied it, so the sweep's own schedule sets the finest cadence the installation can express. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_retention_policies_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "target_kind" text not null,
    "target" text not null,
    "store_code" text not null,
    "action" text not null,
    "retention_days" integer not null,
    "cadence_hours" integer not null,
    "enabled" boolean not null,
    "description" text not null
);

create index if not exists dq_retention_policies_artefact_dataset_idx
on ores_dq_retention_policies_artefact_tbl (dataset_id);

create index if not exists dq_retention_policies_artefact_tenant_idx
on ores_dq_retention_policies_artefact_tbl (tenant_id);

create index if not exists dq_retention_policies_artefact_code_idx
on ores_dq_retention_policies_artefact_tbl (code);
