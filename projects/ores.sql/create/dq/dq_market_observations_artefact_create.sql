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
-- A single market data observation: the value of a series at a given observation_datetime and oresmd_uri (the datum's coordinate). observation_datetime is the financial valid-time (UTC); valid_from/valid_to is the transaction time. Corrections replace the previous value via the soft-update trigger. TimescaleDB hypertable partitioned by observation_datetime with 30-day chunks; GIST exclusion and DELETE RULEs are incompatible with hypertables — uniqueness is enforced via partial unique index and the soft-update/soft-delete trigger pair. No audit trail columns (version, modified_by, performed_by, change_reason_code, change_commentary) — tick-level data volumes make these impractical. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_market_observations_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "oresmd_uri" text not null,
    "key" text not null,
    "datum_uri" text not null,
    "observation_date" date not null,
    "value" numeric not null,
    "source" text not null,
    "source_url" text null,
    "retrieved_at" timestamp with time zone null
);

create index if not exists dq_market_observations_artefact_dataset_idx
on ores_dq_market_observations_artefact_tbl (dataset_id);

create index if not exists dq_market_observations_artefact_tenant_idx
on ores_dq_market_observations_artefact_tbl (tenant_id);

create index if not exists dq_market_observations_artefact_oresmd_uri_idx
on ores_dq_market_observations_artefact_tbl (oresmd_uri);
