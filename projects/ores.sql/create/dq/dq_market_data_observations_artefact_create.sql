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
-- Generic across market-data shapes (FX spot today; rates curves, vol
-- surfaces, credit spreads, equity spots, ... later) — oresmd_uri/key/point_id
-- mirror market_observation's own identity and key columns, not anything
-- FX-specific, so a new asset class is new dataset rows here, never a new
-- artefact table. source_url/retrieved_at are per-row (not on the shared
-- methodology row) so every future value, whatever its source, carries its own
-- citation independently.
--
-- oresmd_uri is the series the row belongs to and key is the observation's own
-- ORE key, and both are carried rather than derived: the projection from a key
-- to an identity is the C++ grammar's, and SQL has no way back to it. A seed
-- therefore states the two facts its own source names, the way the market-data
-- import does when it reads a file.
create table if not exists "ores_dq_market_data_observations_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "oresmd_uri" text not null,
    "key" text not null,
    "point_id" text not null,
    "observation_date" date not null,
    "value" numeric not null,
    "source" text not null,
    "source_url" text,
    "retrieved_at" timestamp with time zone
);

create index if not exists market_data_observations_artefact_dataset_idx
on "ores_dq_market_data_observations_artefact_tbl" (dataset_id);

create index if not exists market_data_observations_artefact_tenant_idx
on "ores_dq_market_data_observations_artefact_tbl" (tenant_id);

create index if not exists market_data_observations_artefact_series_idx
on "ores_dq_market_data_observations_artefact_tbl" (oresmd_uri);
