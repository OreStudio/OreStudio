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
-- Drop Row-Level Security Policies for Trade Tables
-- =============================================================================
-- Must be dropped before the corresponding tables are dropped.

-- Trade Types
drop policy if exists ores_trading_trade_types_tenant_isolation_policy on "ores_trading_trade_types_tbl";

-- Lifecycle Events
drop policy if exists ores_trading_lifecycle_events_tenant_isolation_policy on "ores_trading_lifecycle_events_tbl";

-- Party Role Types
drop policy if exists ores_trading_party_role_types_tenant_isolation_policy on "ores_trading_party_role_types_tbl";

-- Trade Identifier Types
drop policy if exists ores_trading_trade_id_types_tenant_isolation_policy on "ores_trading_trade_id_types_tbl";

-- Trade Party Roles
drop policy if exists ores_trading_party_roles_tenant_isolation_policy on "ores_trading_party_roles_tbl";

-- Trade Identifiers
drop policy if exists ores_trading_identifiers_tenant_isolation_policy on "ores_trading_identifiers_tbl";

-- Trades
drop policy if exists ores_trading_trades_tenant_isolation_policy on "ores_trading_trades_tbl";
