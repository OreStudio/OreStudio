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

-- Product type discriminator enum (must precede trade_types and instrument tables)
\ir ./trading_product_type_create.sql

-- Trade reference data (no inter-dependencies within reference data)
\ir ./trading_trade_types_create.sql
\ir ./trading_trade_types_notify_trigger_create.sql

\ir ./trading_fpml_event_types_create.sql
\ir ./trading_fpml_event_types_notify_trigger_create.sql

\ir ./trading_lifecycle_events_create.sql
\ir ./trading_lifecycle_events_notify_trigger_create.sql

\ir ./trading_activity_types_create.sql
\ir ./trading_activity_types_notify_trigger_create.sql

-- Trade activities (depend on the activity types above; every trade-keyed
-- table below names the activity that wrote its version)
\ir ./trading_trade_activities_create.sql
\ir ./trading_trade_activities_notify_trigger_create.sql

\ir ./trading_party_role_types_create.sql
\ir ./trading_party_role_types_notify_trigger_create.sql

\ir ./trading_trade_id_types_create.sql
\ir ./trading_trade_id_types_notify_trigger_create.sql

-- The link type catalogue: the relation vocabularies a trade link names.
\ir ./trading_trade_link_types_create.sql
\ir ./trading_trade_link_types_notify_trigger_create.sql

-- The structure reference data: the rungs of the composition ladder, the
-- templates that shape a rung, and the roles each template allows. The roles
-- name their template and the templates name their kind, so the order is
-- kinds, templates, roles.
\ir ./trading_structure_kinds_create.sql
\ir ./trading_structure_kinds_notify_trigger_create.sql
\ir ./trading_structure_templates_create.sql
\ir ./trading_structure_templates_notify_trigger_create.sql
\ir ./trading_structure_template_roles_create.sql
\ir ./trading_structure_template_roles_notify_trigger_create.sql

-- Structures (depend on the kind and template catalogues above, and on
-- themselves for the parent)
\ir ./trading_structures_create.sql
\ir ./trading_structures_notify_trigger_create.sql

-- Structure members (depend on the structure and on the trade the leg is)
\ir ./trading_structure_members_create.sql
\ir ./trading_structure_members_notify_trigger_create.sql

-- Closed-set reference data. Each set is the ORE simple type of the same
-- name, so the spellings round-trip through the ORE XML unchanged. The
-- instrument tables reference them, so they load before the instruments.
\ir ./trading_activity_categories_create.sql
\ir ./trading_activity_categories_notify_trigger_create.sql

\ir ./trading_amortization_types_create.sql
\ir ./trading_amortization_types_notify_trigger_create.sql

\ir ./trading_average_types_create.sql
\ir ./trading_average_types_notify_trigger_create.sql

\ir ./trading_barrier_types_create.sql
\ir ./trading_barrier_types_notify_trigger_create.sql

\ir ./trading_exercise_types_create.sql
\ir ./trading_exercise_types_notify_trigger_create.sql

\ir ./trading_long_short_types_create.sql
\ir ./trading_long_short_types_notify_trigger_create.sql

\ir ./trading_entry_channel_types_create.sql
\ir ./trading_entry_channel_types_notify_trigger_create.sql

\ir ./trading_counterparty_scope_types_create.sql
\ir ./trading_counterparty_scope_types_notify_trigger_create.sql

\ir ./trading_booking_nature_types_create.sql
\ir ./trading_booking_nature_types_notify_trigger_create.sql

\ir ./trading_moment_types_create.sql
\ir ./trading_moment_types_notify_trigger_create.sql

\ir ./trading_option_types_create.sql
\ir ./trading_option_types_notify_trigger_create.sql

\ir ./trading_payoff_types_create.sql
\ir ./trading_payoff_types_notify_trigger_create.sql

\ir ./trading_price_types_create.sql
\ir ./trading_price_types_notify_trigger_create.sql

\ir ./trading_return_types_create.sql
\ir ./trading_return_types_notify_trigger_create.sql

\ir ./trading_settlement_types_create.sql
\ir ./trading_settlement_types_notify_trigger_create.sql

-- Instrument reference data (floating_index_type, leg_type) moved to
-- ores.refdata; refdata_create.sql loads before this file.

\ir ./trading_swap_legs_create.sql
\ir ./trading_swap_legs_notify_trigger_create.sql

-- A swap leg's notionals and its rates or spreads are child rows, so they
-- come after the leg they belong to.
\ir ./trading_swap_leg_amounts_create.sql
\ir ./trading_swap_leg_amounts_notify_trigger_create.sql
\ir ./trading_swap_leg_rates_create.sql
\ir ./trading_swap_leg_rates_notify_trigger_create.sql

-- Rates instruments (depend on swap_legs and reference data above)
\ir ./trading_fra_instruments_create.sql
\ir ./trading_fra_instruments_notify_trigger_create.sql

\ir ./trading_vanilla_swap_instruments_create.sql
\ir ./trading_vanilla_swap_instruments_notify_trigger_create.sql

\ir ./trading_cap_floor_instruments_create.sql
\ir ./trading_cap_floor_instruments_notify_trigger_create.sql

\ir ./trading_swaption_instruments_create.sql
\ir ./trading_swaption_instruments_notify_trigger_create.sql

\ir ./trading_balance_guaranteed_swap_instruments_create.sql
\ir ./trading_balance_guaranteed_swap_instruments_notify_trigger_create.sql

\ir ./trading_callable_swap_instruments_create.sql
\ir ./trading_callable_swap_instruments_notify_trigger_create.sql

\ir ./trading_callable_swap_call_dates_create.sql
\ir ./trading_callable_swap_call_dates_notify_trigger_create.sql

\ir ./trading_knock_out_swap_instruments_create.sql
\ir ./trading_knock_out_swap_instruments_notify_trigger_create.sql

\ir ./trading_inflation_swap_instruments_create.sql
\ir ./trading_inflation_swap_instruments_notify_trigger_create.sql

\ir ./trading_rpa_instruments_create.sql
\ir ./trading_rpa_instruments_notify_trigger_create.sql

-- Per-type FX instrument tables (depends on reference data above)
\ir ./trading_fx_forward_instruments_create.sql
\ir ./trading_fx_forward_instruments_notify_trigger_create.sql

\ir ./trading_fx_vanilla_option_instruments_create.sql
\ir ./trading_fx_vanilla_option_instruments_notify_trigger_create.sql

\ir ./trading_fx_barrier_option_instruments_create.sql
\ir ./trading_fx_barrier_option_instruments_notify_trigger_create.sql

\ir ./trading_fx_digital_option_instruments_create.sql
\ir ./trading_fx_digital_option_instruments_notify_trigger_create.sql

\ir ./trading_fx_asian_forward_instruments_create.sql
\ir ./trading_fx_asian_forward_instruments_notify_trigger_create.sql

\ir ./trading_fx_accumulator_instruments_create.sql
\ir ./trading_fx_accumulator_instruments_notify_trigger_create.sql

\ir ./trading_fx_variance_swap_instruments_create.sql
\ir ./trading_fx_variance_swap_instruments_notify_trigger_create.sql

-- Bond relational model (pilot, task D7943D7E): the issue table, the
-- instrument table that references it, the per-trade fact tables and
-- the issue-keyed child tables. The issue, fact and child rows are
-- tenant-scoped; the instrument row carries tenant + party.
\ir ./trading_bond_issues_create.sql
\ir ./trading_bond_issues_notify_trigger_create.sql

\ir ./trading_bond_instruments_create.sql
\ir ./trading_bond_instruments_notify_trigger_create.sql

\ir ./trading_bond_issue_call_dates_create.sql
\ir ./trading_bond_issue_call_dates_notify_trigger_create.sql

\ir ./trading_bond_issue_conversion_targets_create.sql
\ir ./trading_bond_issue_conversion_targets_notify_trigger_create.sql

\ir ./trading_bond_options_create.sql
\ir ./trading_bond_options_notify_trigger_create.sql

\ir ./trading_bond_futures_create.sql
\ir ./trading_bond_futures_notify_trigger_create.sql

\ir ./trading_bond_trs_create.sql
\ir ./trading_bond_trs_notify_trigger_create.sql

\ir ./trading_bond_repos_create.sql
\ir ./trading_bond_repos_notify_trigger_create.sql

\ir ./trading_bond_forwards_create.sql
\ir ./trading_bond_forwards_notify_trigger_create.sql

\ir ./trading_ascots_create.sql
\ir ./trading_ascots_notify_trigger_create.sql

\ir ./trading_bond_issue_legs_create.sql
\ir ./trading_bond_issue_legs_notify_trigger_create.sql

\ir ./trading_bond_issue_leg_amounts_create.sql
\ir ./trading_bond_issue_leg_amounts_notify_trigger_create.sql

\ir ./trading_bond_issue_leg_amortizations_create.sql
\ir ./trading_bond_issue_leg_amortizations_notify_trigger_create.sql

\ir ./trading_bond_issue_leg_rates_create.sql
\ir ./trading_bond_issue_leg_rates_notify_trigger_create.sql

\ir ./trading_bond_issue_leg_schedules_create.sql
\ir ./trading_bond_issue_leg_schedules_notify_trigger_create.sql

\ir ./trading_bond_issue_leg_schedule_dates_create.sql
\ir ./trading_bond_issue_leg_schedule_dates_notify_trigger_create.sql

-- Shared instrument-keyed tables (task B753AD00, waves B.1, B.2 and B.3):
-- everything the nine bond tables cannot hold. The leg family carries a
-- leg and its amounts, amortizations and rate group; the schedule tables
-- carry a leg's schedules and their dates; the option tables carry the
-- option block and its premiums, exercise fees and payment dates; the
-- envelope tables carry the block a document wraps around a trade.
\ir ./trading_bond_legs_create.sql
\ir ./trading_bond_legs_notify_trigger_create.sql

\ir ./trading_bond_leg_amounts_create.sql
\ir ./trading_bond_leg_amounts_notify_trigger_create.sql

\ir ./trading_bond_leg_amortizations_create.sql
\ir ./trading_bond_leg_amortizations_notify_trigger_create.sql

\ir ./trading_bond_leg_rates_create.sql
\ir ./trading_bond_leg_rates_notify_trigger_create.sql

\ir ./trading_instrument_schedules_create.sql
\ir ./trading_instrument_schedules_notify_trigger_create.sql

\ir ./trading_instrument_schedule_dates_create.sql
\ir ./trading_instrument_schedule_dates_notify_trigger_create.sql

\ir ./trading_instrument_options_create.sql
\ir ./trading_instrument_options_notify_trigger_create.sql

\ir ./trading_instrument_option_premiums_create.sql
\ir ./trading_instrument_option_premiums_notify_trigger_create.sql

\ir ./trading_instrument_option_exercise_fees_create.sql
\ir ./trading_instrument_option_exercise_fees_notify_trigger_create.sql

\ir ./trading_instrument_option_payment_dates_create.sql
\ir ./trading_instrument_option_payment_dates_notify_trigger_create.sql

\ir ./trading_instrument_strikes_create.sql
\ir ./trading_instrument_strikes_notify_trigger_create.sql

-- Credit instruments (depends on reference data above)
\ir ./trading_credit_instruments_create.sql
\ir ./trading_credit_instruments_notify_trigger_create.sql

-- Per-type equity instruments
\ir ./trading_equity_option_instruments_create.sql
\ir ./trading_equity_option_instruments_notify_trigger_create.sql
\ir ./trading_equity_digital_option_instruments_create.sql
\ir ./trading_equity_digital_option_instruments_notify_trigger_create.sql
\ir ./trading_equity_barrier_option_instruments_create.sql
\ir ./trading_equity_barrier_option_instruments_notify_trigger_create.sql
\ir ./trading_equity_asian_option_instruments_create.sql
\ir ./trading_equity_asian_option_instruments_notify_trigger_create.sql
\ir ./trading_equity_forward_instruments_create.sql
\ir ./trading_equity_forward_instruments_notify_trigger_create.sql
\ir ./trading_equity_variance_swap_instruments_create.sql
\ir ./trading_equity_variance_swap_instruments_notify_trigger_create.sql
\ir ./trading_equity_swap_instruments_create.sql
\ir ./trading_equity_swap_instruments_notify_trigger_create.sql
\ir ./trading_equity_accumulator_instruments_create.sql
\ir ./trading_equity_accumulator_instruments_notify_trigger_create.sql
\ir ./trading_equity_position_instruments_create.sql
\ir ./trading_equity_position_instruments_notify_trigger_create.sql

\ir ./trading_equity_position_option_underlyings_create.sql
\ir ./trading_equity_position_option_underlyings_notify_trigger_create.sql

-- Commodity instruments (depends on reference data above)
\ir ./trading_commodity_instruments_create.sql
\ir ./trading_commodity_instruments_notify_trigger_create.sql

\ir ./trading_commodity_basket_constituents_create.sql
\ir ./trading_commodity_basket_constituents_notify_trigger_create.sql

-- Composite instruments (depends on reference data above)
\ir ./trading_composite_instruments_create.sql
\ir ./trading_composite_instruments_notify_trigger_create.sql
\ir ./trading_composite_legs_create.sql
\ir ./trading_composite_legs_notify_trigger_create.sql

-- Scripted instruments (depends on reference data above)
\ir ./trading_scripted_instruments_create.sql
\ir ./trading_scripted_instruments_notify_trigger_create.sql

-- Trades (depend on the classification lookups above)
\ir ./trading_trades_create.sql
\ir ./trading_trades_notify_trigger_create.sql

-- Trade components (depend on the trade)
\ir ./trading_trade_components_functions_create.sql

\ir ./trading_party_visibility_functions_create.sql
\ir ./trading_trade_bookings_create.sql
\ir ./trading_trade_bookings_notify_trigger_create.sql
\ir ./trading_trade_states_create.sql
\ir ./trading_trade_states_notify_trigger_create.sql

-- Trade identifiers, party roles and additional fields (depend on the trade)
\ir ./trading_trade_identifiers_create.sql
\ir ./trading_trade_identifiers_notify_trigger_create.sql

\ir ./trading_trade_party_roles_create.sql
\ir ./trading_trade_party_roles_notify_trigger_create.sql

\ir ./trading_trade_additional_fields_create.sql
\ir ./trading_trade_additional_fields_notify_trigger_create.sql
\ir ./trading_trade_portfolios_create.sql
\ir ./trading_trade_portfolios_notify_trigger_create.sql

-- Trade links (depend on the trade at both ends and on the link type
-- catalogue above)
\ir ./trading_trade_links_create.sql
\ir ./trading_trade_links_notify_trigger_create.sql

-- Trade query functions (depend on trades table + refdata tables)
\ir ./trading_trades_functions_create.sql
\ir ./trading_trades_bu_functions_create.sql
