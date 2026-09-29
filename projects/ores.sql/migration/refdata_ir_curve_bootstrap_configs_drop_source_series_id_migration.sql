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
 * One-shot migration: the config stops naming the grid it bootstraps from
 *
 * projects/ores.refdata/modeling/ores.refdata.ir_curve_bootstrap_config.org
 * declared source_series_id as the raw RATES/YIELD market_series the config
 * bootstrapped. That grid is no longer written: the synthetic FOMC feed
 * publishes one series per pillar, keyed as the meeting-dated OIS quote it
 * simulates, and curve_republish_service reads each pillar from the series
 * that pillar's own key names. The column therefore names a row nothing
 * writes, and it goes.
 *
 * The check that kept source_series_id apart from output_series_id is named
 * by the order Postgres happened to create it in, so the drop is cascading
 * rather than guessing at the name; nothing else depends on the column.
 *
 * On a freshly recreated database the create script no longer emits the
 * column and this migration is unnecessary. It exists for databases created
 * before the change.
 *
 * Such a database also keeps the RATES/YIELD grid series and the observations
 * under it. Nothing reads them now: the feed writes one series per pillar and
 * the reader asks for those, so the old rows are inert. They are left in place
 * rather than deleted here, because market_series and its observations are
 * temporal and the seed's own row history is not this migration's to rewrite;
 * a rebuilt database has no grid row at all, since the populate script no
 * longer writes one.
 */

alter table ores_refdata_ir_curve_bootstrap_configs_tbl
    drop column if exists "source_series_id" cascade;
