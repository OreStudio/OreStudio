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

-- Rates instruments (depend on reference data, drop before reference data)
\ir ./trading_swap_legs_notify_trigger_drop.sql
\ir ./trading_swap_legs_drop.sql

\ir ./trading_fra_instruments_drop.sql
\ir ./trading_vanilla_swap_instruments_drop.sql
\ir ./trading_cap_floor_instruments_drop.sql
\ir ./trading_swaption_instruments_drop.sql
\ir ./trading_balance_guaranteed_swap_instruments_drop.sql
\ir ./trading_callable_swap_instruments_drop.sql
\ir ./trading_knock_out_swap_instruments_drop.sql
\ir ./trading_inflation_swap_instruments_drop.sql
\ir ./trading_risk_participation_agreement_instruments_drop.sql

\ir ./trading_instruments_notify_trigger_drop.sql
\ir ./trading_instruments_drop.sql

-- FX instruments (depend on reference data, drop before reference data)
\ir ./trading_fx_instruments_notify_trigger_drop.sql
\ir ./trading_fx_instruments_drop.sql

-- Bond instruments (depend on reference data, drop before reference data)
\ir ./trading_bond_instruments_notify_trigger_drop.sql
\ir ./trading_bond_instruments_drop.sql

-- Credit instruments
\ir ./trading_credit_instruments_notify_trigger_drop.sql
\ir ./trading_credit_instruments_drop.sql

-- Equity instruments
\ir ./trading_equity_instruments_notify_trigger_drop.sql
\ir ./trading_equity_instruments_drop.sql

-- Commodity instruments
\ir ./trading_commodity_instruments_notify_trigger_drop.sql
\ir ./trading_commodity_instruments_drop.sql

-- Composite instruments (drop legs before header)
\ir ./trading_composite_legs_notify_trigger_drop.sql
\ir ./trading_composite_legs_drop.sql
\ir ./trading_composite_instruments_notify_trigger_drop.sql
\ir ./trading_composite_instruments_drop.sql

-- Scripted instruments
\ir ./trading_scripted_instruments_notify_trigger_drop.sql
\ir ./trading_scripted_instruments_drop.sql

-- Trade helper functions (drop before the table they query)
\ir ./trading_trades_bu_functions_drop.sql
\ir ./trading_trades_functions_drop.sql

-- Trades (depends on reference data, drop after junction tables)
\ir ./trading_trades_notify_trigger_drop.sql
\ir ./trading_trades_drop.sql

-- Trade reference data (no inter-dependencies within reference data)
\ir ./trading_trade_id_types_notify_trigger_drop.sql
\ir ./trading_trade_id_types_drop.sql

\ir ./trading_party_role_types_notify_trigger_drop.sql
\ir ./trading_party_role_types_drop.sql

\ir ./trading_activity_types_notify_trigger_drop.sql
\ir ./trading_activity_types_drop.sql

\ir ./trading_fpml_event_types_notify_trigger_drop.sql
\ir ./trading_fpml_event_types_drop.sql

\ir ./trading_trade_types_notify_trigger_drop.sql
\ir ./trading_trade_types_drop.sql

-- Trading instrument reference data types
\ir ./trading_day_count_fraction_types_notify_trigger_drop.sql
\ir ./trading_day_count_fraction_types_drop.sql

\ir ./trading_business_day_convention_types_notify_trigger_drop.sql
\ir ./trading_business_day_convention_types_drop.sql

\ir ./trading_floating_index_types_notify_trigger_drop.sql
\ir ./trading_floating_index_types_drop.sql

\ir ./trading_payment_frequency_types_notify_trigger_drop.sql
\ir ./trading_payment_frequency_types_drop.sql

\ir ./trading_leg_types_notify_trigger_drop.sql
\ir ./trading_leg_types_drop.sql
