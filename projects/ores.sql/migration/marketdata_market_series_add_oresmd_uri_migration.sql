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
 * One-shot migration: the market series' oresmd identity
 *
 * projects/ores.marketdata/modeling/ores.marketdata.market_series.org gains
 * oresmd_uri, the identifier the series is, written as a URI. The import writes
 * it: from_ore_key() for a market-data key, from_index_name() for a fixing's
 * index name.
 *
 * Null is the honest value for three populations: rows no import produced (feed
 * ticks, curve bootstraps, synthetic output), rows whose key the grammar cannot
 * name, and rows written before this column existed. The triple above still keys
 * the row, so a null identity changes no lookup, and the cutover that deletes the
 * triple is the change that makes this column not null.
 *
 * On a freshly recreated database the create script already emits the column and
 * this migration is unnecessary. It exists for databases created before the
 * change.
 */

alter table ores_marketdata_market_series_tbl
    add column if not exists "oresmd_uri" text null;
