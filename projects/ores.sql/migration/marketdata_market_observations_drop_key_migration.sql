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
 * One-shot migration: an observation stops storing its ORE key
 *
 * projects/ores.marketdata/modeling/ores.marketdata.market_observation.org
 * declared key as the instrument key the producer wrote, which
 * marketdata_market_observations_add_key_migration.sql added. A row's only
 * identity is now its datum URI, and every ORE key is written from it by the
 * ORE key codec, so the column goes.
 *
 * On a freshly recreated database the create script no longer emits the
 * column and this migration is unnecessary. It exists for databases created
 * before the change. Quote URIs changed form in the same story, so such a
 * database should be recreated and re-imported rather than migrated.
 */

alter table ores_marketdata_market_observations_tbl
    drop column if exists "key";
