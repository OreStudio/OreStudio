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
 * One-shot migration: the observation's producer key
 *
 * The import rewrites a key it can name -- to the canonical spelling the
 * oresmd projection emits, and to the corrected pair when refdata reports an
 * FX/RATE key reversed -- so the series' columns no longer hold the text the
 * file carried, and an export reading them back cannot return that text. The
 * column keeps it.
 *
 * Null is the honest value for two populations: rows no file produced, such
 * as feed ticks and curve bootstraps, which have no producer key at all; and
 * rows imported before this column existed, whose text is not recoverable.
 * An export rebuilds a key for those from the series, which is exact for
 * every row this import did not rewrite.
 *
 * On a freshly recreated database the create script already emits the column
 * and this migration is unnecessary. It exists for databases created before
 * the change.
 */

alter table ores_marketdata_market_observations_tbl
    add column if not exists "key" text null;
