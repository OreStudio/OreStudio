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

-- Trade identifiers, party roles and additional fields (depend on the anchor)
\ir ./trading_trade_additional_fields_notify_trigger_drop.sql
\ir ./trading_trade_additional_fields_drop.sql
\ir ./trading_trade_portfolios_notify_trigger_drop.sql
\ir ./trading_trade_portfolios_drop.sql

\ir ./trading_trade_party_roles_notify_trigger_drop.sql
\ir ./trading_trade_party_roles_drop.sql

\ir ./trading_trade_identifiers_notify_trigger_drop.sql
\ir ./trading_trade_identifiers_drop.sql

-- Structures and links, and a swap leg's child rows. Each names a table that
-- is dropped further down, so they go first: a template role before its
-- template, a template before its kind, a leg's rows before the leg.
\ir ./trading_structure_template_roles_notify_trigger_drop.sql
\ir ./trading_structure_template_roles_drop.sql

\ir ./trading_structure_members_notify_trigger_drop.sql
\ir ./trading_structure_members_drop.sql

\ir ./trading_structures_notify_trigger_drop.sql
\ir ./trading_structures_drop.sql

\ir ./trading_structure_templates_notify_trigger_drop.sql
\ir ./trading_structure_templates_drop.sql

\ir ./trading_structure_kinds_notify_trigger_drop.sql
\ir ./trading_structure_kinds_drop.sql

\ir ./trading_trade_links_notify_trigger_drop.sql
\ir ./trading_trade_links_drop.sql

\ir ./trading_trade_link_types_notify_trigger_drop.sql
\ir ./trading_trade_link_types_drop.sql

\ir ./trading_swap_leg_amounts_notify_trigger_drop.sql
\ir ./trading_swap_leg_amounts_drop.sql

\ir ./trading_swap_leg_rates_notify_trigger_drop.sql
\ir ./trading_swap_leg_rates_drop.sql

-- Rates instruments (depend on reference data, drop before reference data)
-- A swap leg's notionals and rates are child rows, so they go before the leg
-- they belong to.
\ir ./trading_swap_leg_rates_notify_trigger_drop.sql
\ir ./trading_swap_leg_rates_drop.sql
\ir ./trading_swap_leg_amounts_notify_trigger_drop.sql
\ir ./trading_swap_leg_amounts_drop.sql
\ir ./trading_swap_legs_notify_trigger_drop.sql
\ir ./trading_swap_legs_drop.sql

\ir ./trading_fra_instruments_notify_trigger_drop.sql
\ir ./trading_fra_instruments_drop.sql
\ir ./trading_vanilla_swap_instruments_notify_trigger_drop.sql
\ir ./trading_vanilla_swap_instruments_drop.sql
\ir ./trading_cap_floor_instruments_notify_trigger_drop.sql
\ir ./trading_cap_floor_instruments_drop.sql
\ir ./trading_swaption_instruments_notify_trigger_drop.sql
\ir ./trading_swaption_instruments_drop.sql
\ir ./trading_balance_guaranteed_swap_instruments_notify_trigger_drop.sql
\ir ./trading_balance_guaranteed_swap_instruments_drop.sql
\ir ./trading_callable_swap_instruments_notify_trigger_drop.sql
\ir ./trading_callable_swap_instruments_drop.sql
\ir ./trading_callable_swap_call_dates_notify_trigger_drop.sql
\ir ./trading_callable_swap_call_dates_drop.sql
\ir ./trading_knock_out_swap_instruments_notify_trigger_drop.sql
\ir ./trading_knock_out_swap_instruments_drop.sql
\ir ./trading_inflation_swap_instruments_notify_trigger_drop.sql
\ir ./trading_inflation_swap_instruments_drop.sql
\ir ./trading_rpa_instruments_notify_trigger_drop.sql
\ir ./trading_rpa_instruments_drop.sql

-- Per-type FX instruments (Phase 2, drop before generic FX table)
\ir ./trading_fx_variance_swap_instruments_notify_trigger_drop.sql
\ir ./trading_fx_variance_swap_instruments_drop.sql
\ir ./trading_fx_accumulator_instruments_notify_trigger_drop.sql
\ir ./trading_fx_accumulator_instruments_drop.sql
\ir ./trading_fx_asian_forward_instruments_notify_trigger_drop.sql
\ir ./trading_fx_asian_forward_instruments_drop.sql
\ir ./trading_fx_digital_option_instruments_notify_trigger_drop.sql
\ir ./trading_fx_digital_option_instruments_drop.sql
\ir ./trading_fx_barrier_option_instruments_notify_trigger_drop.sql
\ir ./trading_fx_barrier_option_instruments_drop.sql
\ir ./trading_fx_vanilla_option_instruments_notify_trigger_drop.sql
\ir ./trading_fx_vanilla_option_instruments_drop.sql
\ir ./trading_fx_forward_instruments_notify_trigger_drop.sql
\ir ./trading_fx_forward_instruments_drop.sql

-- Bond relational model (pilot, task D7943D7E): child rows before the
-- issue they belong to; fact rows before the instrument they reference;
-- the instrument before the issue it references.
\ir ./trading_bond_issue_call_dates_notify_trigger_drop.sql
\ir ./trading_bond_issue_call_dates_drop.sql
\ir ./trading_bond_issue_conversion_targets_notify_trigger_drop.sql
\ir ./trading_bond_issue_conversion_targets_drop.sql
\ir ./trading_bond_options_notify_trigger_drop.sql
\ir ./trading_bond_options_drop.sql
\ir ./trading_bond_futures_notify_trigger_drop.sql
\ir ./trading_bond_futures_drop.sql
\ir ./trading_bond_trs_notify_trigger_drop.sql
\ir ./trading_bond_trs_drop.sql
\ir ./trading_bond_repos_notify_trigger_drop.sql
\ir ./trading_bond_repos_drop.sql
\ir ./trading_ascots_notify_trigger_drop.sql
\ir ./trading_ascots_drop.sql
\ir ./trading_bond_forwards_notify_trigger_drop.sql
\ir ./trading_bond_forwards_drop.sql
\ir ./trading_bond_instruments_notify_trigger_drop.sql
\ir ./trading_bond_instruments_drop.sql
\ir ./trading_bond_issues_notify_trigger_drop.sql
\ir ./trading_bond_issues_drop.sql

-- Shared instrument-keyed tables and the trade envelope (task B753AD00)
\ir ./trading_instrument_strikes_notify_trigger_drop.sql
\ir ./trading_instrument_strikes_drop.sql
\ir ./trading_instrument_option_payment_dates_notify_trigger_drop.sql
\ir ./trading_instrument_option_payment_dates_drop.sql
\ir ./trading_instrument_option_exercise_fees_notify_trigger_drop.sql
\ir ./trading_instrument_option_exercise_fees_drop.sql
\ir ./trading_instrument_option_premiums_notify_trigger_drop.sql
\ir ./trading_instrument_option_premiums_drop.sql
\ir ./trading_instrument_options_notify_trigger_drop.sql
\ir ./trading_instrument_options_drop.sql
\ir ./trading_instrument_schedule_dates_notify_trigger_drop.sql
\ir ./trading_instrument_schedule_dates_drop.sql
\ir ./trading_instrument_schedules_notify_trigger_drop.sql
\ir ./trading_instrument_schedules_drop.sql
\ir ./trading_bond_issue_leg_schedule_dates_notify_trigger_drop.sql
\ir ./trading_bond_issue_leg_schedule_dates_drop.sql
\ir ./trading_bond_issue_leg_schedules_notify_trigger_drop.sql
\ir ./trading_bond_issue_leg_schedules_drop.sql
\ir ./trading_bond_issue_leg_rates_notify_trigger_drop.sql
\ir ./trading_bond_issue_leg_rates_drop.sql
\ir ./trading_bond_issue_leg_amortizations_notify_trigger_drop.sql
\ir ./trading_bond_issue_leg_amortizations_drop.sql
\ir ./trading_bond_issue_leg_amounts_notify_trigger_drop.sql
\ir ./trading_bond_issue_leg_amounts_drop.sql
\ir ./trading_bond_issue_legs_notify_trigger_drop.sql
\ir ./trading_bond_issue_legs_drop.sql
\ir ./trading_bond_leg_rates_notify_trigger_drop.sql
\ir ./trading_bond_leg_rates_drop.sql
\ir ./trading_bond_leg_amortizations_notify_trigger_drop.sql
\ir ./trading_bond_leg_amortizations_drop.sql
\ir ./trading_bond_leg_amounts_notify_trigger_drop.sql
\ir ./trading_bond_leg_amounts_drop.sql
\ir ./trading_bond_legs_notify_trigger_drop.sql
\ir ./trading_bond_legs_drop.sql

-- Credit instruments
\ir ./trading_credit_instruments_notify_trigger_drop.sql
\ir ./trading_credit_instruments_drop.sql

-- Commodity instruments
\ir ./trading_commodity_instruments_notify_trigger_drop.sql
\ir ./trading_commodity_instruments_drop.sql
\ir ./trading_commodity_basket_constituents_notify_trigger_drop.sql
\ir ./trading_commodity_basket_constituents_drop.sql

-- Equity position option underlyings (child rows drop on their own)
\ir ./trading_equity_position_option_underlyings_notify_trigger_drop.sql
\ir ./trading_equity_position_option_underlyings_drop.sql

-- Per-type equity instruments (drop before the generic equity tables)
\ir ./trading_equity_position_instruments_notify_trigger_drop.sql
\ir ./trading_equity_position_instruments_drop.sql
\ir ./trading_equity_accumulator_instruments_notify_trigger_drop.sql
\ir ./trading_equity_accumulator_instruments_drop.sql
\ir ./trading_equity_swap_instruments_notify_trigger_drop.sql
\ir ./trading_equity_swap_instruments_drop.sql
\ir ./trading_equity_variance_swap_instruments_notify_trigger_drop.sql
\ir ./trading_equity_variance_swap_instruments_drop.sql
\ir ./trading_equity_forward_instruments_notify_trigger_drop.sql
\ir ./trading_equity_forward_instruments_drop.sql
\ir ./trading_equity_asian_option_instruments_notify_trigger_drop.sql
\ir ./trading_equity_asian_option_instruments_drop.sql
\ir ./trading_equity_barrier_option_instruments_notify_trigger_drop.sql
\ir ./trading_equity_barrier_option_instruments_drop.sql
\ir ./trading_equity_digital_option_instruments_notify_trigger_drop.sql
\ir ./trading_equity_digital_option_instruments_drop.sql
\ir ./trading_equity_option_instruments_notify_trigger_drop.sql
\ir ./trading_equity_option_instruments_drop.sql

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
\ir ./trading_structures_functions_drop.sql

-- Trades (depends on reference data, drop after junction tables)

-- Trade components (drop before the anchor they reference)
\ir ./trading_trade_states_notify_trigger_drop.sql
\ir ./trading_trade_states_drop.sql
\ir ./trading_trade_bookings_notify_trigger_drop.sql
\ir ./trading_trade_bookings_drop.sql
\ir ./trading_party_visibility_functions_drop.sql

\ir ./trading_trade_components_functions_drop.sql

-- Trades (drop before the classification lookups they reference)
\ir ./trading_trades_notify_trigger_drop.sql
\ir ./trading_trades_drop.sql

-- Trade activities (drop before the activity types they reference)
\ir ./trading_trade_activities_notify_trigger_drop.sql
\ir ./trading_trade_activities_drop.sql

-- Trade reference data (no inter-dependencies within reference data)
\ir ./trading_trade_id_types_notify_trigger_drop.sql
\ir ./trading_trade_id_types_drop.sql

\ir ./trading_party_role_types_notify_trigger_drop.sql
\ir ./trading_party_role_types_drop.sql

\ir ./trading_activity_types_notify_trigger_drop.sql
\ir ./trading_activity_types_drop.sql

\ir ./trading_fpml_event_types_notify_trigger_drop.sql
\ir ./trading_fpml_event_types_drop.sql

\ir ./trading_lifecycle_events_notify_trigger_drop.sql
\ir ./trading_lifecycle_events_drop.sql

\ir ./trading_trade_types_notify_trigger_drop.sql
\ir ./trading_trade_types_drop.sql

-- Closed-set reference data (dropped after the tables that reference it)
\ir ./trading_settlement_types_notify_trigger_drop.sql
\ir ./trading_settlement_types_drop.sql

\ir ./trading_return_types_notify_trigger_drop.sql
\ir ./trading_return_types_drop.sql

\ir ./trading_price_types_notify_trigger_drop.sql
\ir ./trading_price_types_drop.sql

\ir ./trading_payoff_types_notify_trigger_drop.sql
\ir ./trading_payoff_types_drop.sql

\ir ./trading_option_types_notify_trigger_drop.sql
\ir ./trading_option_types_drop.sql

\ir ./trading_moment_types_notify_trigger_drop.sql
\ir ./trading_moment_types_drop.sql

\ir ./trading_long_short_types_notify_trigger_drop.sql
\ir ./trading_long_short_types_drop.sql

\ir ./trading_entry_channel_types_notify_trigger_drop.sql
\ir ./trading_entry_channel_types_drop.sql

\ir ./trading_counterparty_scope_types_notify_trigger_drop.sql
\ir ./trading_counterparty_scope_types_drop.sql

\ir ./trading_booking_nature_types_notify_trigger_drop.sql
\ir ./trading_booking_nature_types_drop.sql

\ir ./trading_exercise_types_notify_trigger_drop.sql
\ir ./trading_exercise_types_drop.sql

\ir ./trading_barrier_types_notify_trigger_drop.sql
\ir ./trading_barrier_types_drop.sql

\ir ./trading_average_types_notify_trigger_drop.sql
\ir ./trading_average_types_drop.sql

\ir ./trading_amortization_types_notify_trigger_drop.sql
\ir ./trading_amortization_types_drop.sql

\ir ./trading_activity_categories_notify_trigger_drop.sql
\ir ./trading_activity_categories_drop.sql

-- Trading instrument reference data types (floating_index_type,
-- leg_type) moved to ores.refdata; dropped there instead.
