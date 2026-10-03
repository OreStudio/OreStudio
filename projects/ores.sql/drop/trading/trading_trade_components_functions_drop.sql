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
-- Drop trade component helpers
-- =============================================================================

drop function if exists ores_trading_book_trade_fn(uuid, uuid, uuid, text, text, text, text, uuid, uuid, date, timestamptz, text, text, text, text);
drop function if exists ores_trading_trade_booked_virtual_fn(uuid, uuid);
drop function if exists ores_trading_trade_may_be_virtual_fn(uuid, uuid);
drop function if exists ores_trading_status_may_be_virtual_fn(uuid, uuid, uuid);
drop function if exists ores_trading_status_is_draft_fn(uuid);
drop function if exists ores_trading_book_is_virtual_fn(uuid, uuid);
