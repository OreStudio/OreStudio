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
-- Product Type Enum
--
-- Discriminator used by ores_trading_trades_tbl.product_type to route a trade
-- to its product-specific extension table without requiring application-layer
-- decoding of the trade_type string.
--
-- This is a structural/routing classification, not a risk taxonomy. It answers
-- "what kind of financial product is this?" (FpML: productType), not "what
-- economic risk does it carry?" (FpML: assetClass). A swap, for example, can
-- belong to the rates, credit, equity, or inflation asset class — the product
-- type identifies the structure; the asset_class column identifies the risk.
-- =============================================================================

do $$ begin
    create type product_type_t as enum (
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
