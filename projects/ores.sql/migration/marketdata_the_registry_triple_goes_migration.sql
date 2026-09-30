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
 * The registry's triple leaves the series and the DQ artefact
 *
 * seriestype, metric and qualifier were the registry's decomposition of a
 * series' key, and the identity -- the oresmd URI -- replaced them: it is the
 * natural key with party_id, every writer supplies one, and no reader asks by
 * the triple any more. A consumer that wants the key's tokens decomposes the key
 * itself, through the ORE shapes registry, rather than reading them off a row.
 *
 * The series classification rule keeps its own series_type and metric: it
 * classifies the tokens of a key, never a series row, so it is not touched here.
 *
 * The DQ artefact's rows name the series identity and the observation key now,
 * so its three columns go with the series row's.
 *
 * A consumer that wants the triple back decomposes the key, which is where its
 * tokens come from in the first place. The migration assumes every row already
 * carries a non-empty identity, which the column has required since it became the
 * natural key.
 *
 * The schema is applied by recreation, so this migration is for a database built
 * before the change: a rebuild does not have the columns at all.
 */

\echo '--- market_series and the DQ artefact: the registry triple goes ---'

alter table ores_marketdata_market_series_tbl
    drop column if exists series_type,
    drop column if exists metric,
    drop column if exists qualifier;

alter table ores_dq_market_data_observations_artefact_tbl
    drop column if exists series_type,
    drop column if exists metric,
    drop column if exists qualifier;

drop index if exists market_data_observations_artefact_qualifier_idx;
