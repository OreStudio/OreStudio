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
 * Market data observations artefact: the identity and the key
 *
 * The artefact carried only the registry's decomposition of each row's key,
 * which the publish function turned into a series and an observation. The series
 * is keyed by its oresmd identity and the observation keeps its own ORE key now,
 * so the dataset names both: the projection from a key to an identity is the C++
 * grammar's, and SQL has no way back to it.
 *
 * The existing rows cannot name either, and they are re-derived from the populate
 * scripts, so they are cleared rather than carried.
 *
 * The schema is applied by recreation, so this migration is for a database built
 * before the change: a rebuild has the columns already.
 */

\echo '--- DQ market data observations artefact: identity and key ---'

delete from ores_dq_market_data_observations_artefact_tbl;

alter table ores_dq_market_data_observations_artefact_tbl
    add column if not exists oresmd_uri text not null,
    add column if not exists "key" text not null;

create index if not exists market_data_observations_artefact_series_idx
on "ores_dq_market_data_observations_artefact_tbl" (oresmd_uri);
