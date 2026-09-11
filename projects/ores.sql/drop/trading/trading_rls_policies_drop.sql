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
drop policy if exists trade_types_tenant_isolation_policy on "ores_trading_trade_types_tbl";

-- FpML Event Types
drop policy if exists fpml_event_types_tenant_isolation_policy on "ores_trading_fpml_event_types_tbl";

-- Activity Types
drop policy if exists activity_types_tenant_isolation_policy on "ores_trading_activity_types_tbl";

-- Party Role Types
drop policy if exists party_role_types_tenant_isolation_policy on "ores_trading_party_role_types_tbl";

-- Trade Identifier Types
drop policy if exists trade_id_types_tenant_isolation_policy on "ores_trading_trade_id_types_tbl";

-- Trade Party Roles
drop policy if exists party_roles_tenant_isolation_policy on "ores_trading_party_roles_tbl";

-- Trade Identifiers
drop policy if exists identifiers_tenant_isolation_policy on "ores_trading_trade_identifiers_tbl";

-- Trades
drop policy if exists trades_tenant_isolation_policy on "ores_trading_trades_tbl";

-- Bond relational model (pilot), task D7943D7E
drop policy if exists bond_issues_tenant_isolation_policy on "ores_trading_bond_issues_tbl";
drop policy if exists bond_issue_call_dates_tenant_isolation_policy on "ores_trading_bond_issue_call_dates_tbl";
drop policy if exists bond_issue_conversion_targets_tenant_isolation_policy on "ores_trading_bond_issue_conversion_targets_tbl";
drop policy if exists bond_options_tenant_isolation_policy on "ores_trading_bond_options_tbl";
drop policy if exists bond_futures_tenant_isolation_policy on "ores_trading_bond_futures_tbl";
drop policy if exists bond_trs_tenant_isolation_policy on "ores_trading_bond_trs_tbl";
drop policy if exists bond_repos_tenant_isolation_policy on "ores_trading_bond_repos_tbl";
drop policy if exists ascots_tenant_isolation_policy on "ores_trading_ascots_tbl";

-- Shared instrument-keyed tables, task B753AD00, waves A.4 to B.4
drop policy if exists bond_forwards_tenant_isolation_policy on "ores_trading_bond_forwards_tbl";
drop policy if exists bond_future_delivery_baskets_tenant_isolation_policy on "ores_trading_bond_future_delivery_baskets_tbl";
drop policy if exists bond_leg_amortizations_tenant_isolation_policy on "ores_trading_bond_leg_amortizations_tbl";
drop policy if exists bond_leg_amounts_tenant_isolation_policy on "ores_trading_bond_leg_amounts_tbl";
drop policy if exists bond_leg_rates_tenant_isolation_policy on "ores_trading_bond_leg_rates_tbl";
drop policy if exists bond_legs_tenant_isolation_policy on "ores_trading_bond_legs_tbl";
drop policy if exists instrument_option_exercise_fees_tenant_isolation_policy on "ores_trading_instrument_option_exercise_fees_tbl";
drop policy if exists instrument_option_payment_dates_tenant_isolation_policy on "ores_trading_instrument_option_payment_dates_tbl";
drop policy if exists instrument_option_premiums_tenant_isolation_policy on "ores_trading_instrument_option_premiums_tbl";
drop policy if exists instrument_options_tenant_isolation_policy on "ores_trading_instrument_options_tbl";
drop policy if exists instrument_schedule_dates_tenant_isolation_policy on "ores_trading_instrument_schedule_dates_tbl";
drop policy if exists instrument_schedules_tenant_isolation_policy on "ores_trading_instrument_schedules_tbl";
drop policy if exists instrument_strikes_tenant_isolation_policy on "ores_trading_instrument_strikes_tbl";
drop policy if exists trade_envelope_additional_fields_tenant_isolation_policy on "ores_trading_trade_envelope_additional_fields_tbl";
drop policy if exists trade_envelope_portfolio_ids_tenant_isolation_policy on "ores_trading_trade_envelope_portfolio_ids_tbl";
drop policy if exists trade_envelopes_tenant_isolation_policy on "ores_trading_trade_envelopes_tbl";
