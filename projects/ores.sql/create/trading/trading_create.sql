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
-- Trade Component
-- =============================================================================
-- Creates trade reference data tables. The trade component depends on:
-- - ores.fsm (for FSM state references in lifecycle_events)
-- - ores.iam (for tenant validation)
-- - ores.dq (for change reason validation)

-- Trade reference data (no inter-dependencies within reference data)
\ir ./trading_trade_types_create.sql
\ir ./trading_trade_types_notify_trigger_create.sql

\ir ./trading_lifecycle_events_create.sql
\ir ./trading_lifecycle_events_notify_trigger_create.sql

\ir ./trading_party_role_types_create.sql
\ir ./trading_party_role_types_notify_trigger_create.sql

\ir ./trading_trade_id_types_create.sql
\ir ./trading_trade_id_types_notify_trigger_create.sql

-- Trades (depends on reference data above)
\ir ./trading_trades_create.sql
\ir ./trading_trades_notify_trigger_create.sql

-- Trade junction tables (depend on trades)
\ir ./trading_identifiers_create.sql
\ir ./trading_identifiers_notify_trigger_create.sql

\ir ./trading_party_roles_create.sql
\ir ./trading_party_roles_notify_trigger_create.sql

-- ORE envelope view (depends on trades + party roles)
\ir ./trading_ore_envelope_view_create.sql

-- Trade query functions (depend on trades table + refdata tables)
\ir ./trading_trades_functions_create.sql
