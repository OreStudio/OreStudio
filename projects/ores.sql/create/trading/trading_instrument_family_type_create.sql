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

-- =============================================================================
-- Instrument Family Enum
--
-- Discriminator used by ores_trading_trades_tbl.instrument_family to route
-- a trade to its product-specific instrument extension table without requiring
-- application-layer decoding of the trade_type string.
-- =============================================================================

do $$ begin
    create type instrument_family_t as enum (
        'swap',
        'fx',
        'bond',
        'credit',
        'equity',
        'commodity',
        'composite',
        'scripted'
    );
exception
    when duplicate_object then null;
end $$;
