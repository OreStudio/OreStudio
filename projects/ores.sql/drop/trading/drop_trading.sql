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
-- Drop Trade Component
-- =============================================================================
-- Drop all trade tables in reverse dependency order.

-- ORE trade view (drop first as it depends on junction tables)
\ir ./trading_ore_envelope_view_drop.sql

-- Trade junction tables (depend on trades, drop after view)
\ir ./trading_party_roles_notify_trigger_drop.sql
\ir ./trading_party_roles_drop.sql

\ir ./trading_identifiers_notify_trigger_drop.sql
\ir ./trading_identifiers_drop.sql

-- Trades (depends on reference data, drop after junction tables)
\ir ./trading_trades_notify_trigger_drop.sql
\ir ./trading_trades_drop.sql

-- Trade reference data (no inter-dependencies within reference data)
\ir ./trading_trade_id_types_notify_trigger_drop.sql
\ir ./trading_trade_id_types_drop.sql

\ir ./trading_party_role_types_notify_trigger_drop.sql
\ir ./trading_party_role_types_drop.sql

\ir ./trading_lifecycle_events_notify_trigger_drop.sql
\ir ./trading_lifecycle_events_drop.sql

\ir ./trading_trade_types_notify_trigger_drop.sql
\ir ./trading_trade_types_drop.sql
