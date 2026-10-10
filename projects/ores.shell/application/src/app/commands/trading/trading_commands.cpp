/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_shell_command_aggregator_impl.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.shell/app/commands/trading/trading_commands.hpp"
#include "ores.shell/app/commands/trading/activity_category_commands.hpp"
#include "ores.shell/app/commands/trading/activity_type_commands.hpp"
#include "ores.shell/app/commands/trading/amortization_type_commands.hpp"
#include "ores.shell/app/commands/trading/average_type_commands.hpp"
#include "ores.shell/app/commands/trading/balance_guaranteed_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/barrier_type_commands.hpp"
#include "ores.shell/app/commands/trading/bond_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/bond_issue_commands.hpp"
#include "ores.shell/app/commands/trading/callable_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/cap_floor_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/commodity_basket_constituent_commands.hpp"
#include "ores.shell/app/commands/trading/commodity_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/composite_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/composite_leg_commands.hpp"
#include "ores.shell/app/commands/trading/credit_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_accumulator_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_asian_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_barrier_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_digital_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_forward_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_position_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_position_option_underlying_commands.hpp"
#include "ores.shell/app/commands/trading/equity_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/equity_variance_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/exercise_type_commands.hpp"
#include "ores.shell/app/commands/trading/flexi_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fpml_event_type_commands.hpp"
#include "ores.shell/app/commands/trading/fra_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_accumulator_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_asian_forward_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_barrier_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_digital_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_forward_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_vanilla_option_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/fx_variance_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/inflation_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/knock_out_swap_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/lifecycle_event_commands.hpp"
#include "ores.shell/app/commands/trading/long_short_type_commands.hpp"
#include "ores.shell/app/commands/trading/moment_type_commands.hpp"
#include "ores.shell/app/commands/trading/option_type_commands.hpp"
#include "ores.shell/app/commands/trading/ore_commands.hpp"
#include "ores.shell/app/commands/trading/party_role_type_commands.hpp"
#include "ores.shell/app/commands/trading/payoff_type_commands.hpp"
#include "ores.shell/app/commands/trading/price_type_commands.hpp"
#include "ores.shell/app/commands/trading/rate_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/return_type_commands.hpp"
#include "ores.shell/app/commands/trading/rpa_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/scripted_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/settlement_type_commands.hpp"
#include "ores.shell/app/commands/trading/structure_commands.hpp"
#include "ores.shell/app/commands/trading/structure_kind_commands.hpp"
#include "ores.shell/app/commands/trading/structure_member_commands.hpp"
#include "ores.shell/app/commands/trading/structure_template_commands.hpp"
#include "ores.shell/app/commands/trading/structure_template_role_commands.hpp"
#include "ores.shell/app/commands/trading/swap_leg_commands.hpp"
#include "ores.shell/app/commands/trading/swaption_instrument_commands.hpp"
#include "ores.shell/app/commands/trading/trade_booking_commands.hpp"
#include "ores.shell/app/commands/trading/trade_id_type_commands.hpp"
#include "ores.shell/app/commands/trading/trade_identifier_commands.hpp"
#include "ores.shell/app/commands/trading/trade_link_commands.hpp"
#include "ores.shell/app/commands/trading/trade_link_type_commands.hpp"
#include "ores.shell/app/commands/trading/trade_party_role_commands.hpp"
#include "ores.shell/app/commands/trading/trade_state_commands.hpp"
#include "ores.shell/app/commands/trading/trade_type_commands.hpp"
#include "ores.shell/app/commands/trading/vanilla_swap_instrument_commands.hpp"

namespace ores::shell::app::commands {

using namespace logging;

void trading_commands::register_commands(cli::Menu& root_menu,
                                         ores::nats::service::nats_client& session) {
    BOOST_LOG_SEV(lg(), debug) << "Registering trading command surface.";
    activity_category_commands::register_commands(root_menu, session);
    activity_type_commands::register_commands(root_menu, session);
    amortization_type_commands::register_commands(root_menu, session);
    average_type_commands::register_commands(root_menu, session);
    balance_guaranteed_swap_instrument_commands::register_commands(root_menu, session);
    barrier_type_commands::register_commands(root_menu, session);
    bond_instrument_commands::register_commands(root_menu, session);
    bond_issue_commands::register_commands(root_menu, session);
    callable_swap_instrument_commands::register_commands(root_menu, session);
    cap_floor_instrument_commands::register_commands(root_menu, session);
    commodity_basket_constituent_commands::register_commands(root_menu, session);
    commodity_instrument_commands::register_commands(root_menu, session);
    composite_instrument_commands::register_commands(root_menu, session);
    composite_leg_commands::register_commands(root_menu, session);
    credit_instrument_commands::register_commands(root_menu, session);
    equity_accumulator_instrument_commands::register_commands(root_menu, session);
    equity_asian_option_instrument_commands::register_commands(root_menu, session);
    equity_barrier_option_instrument_commands::register_commands(root_menu, session);
    equity_digital_option_instrument_commands::register_commands(root_menu, session);
    equity_forward_instrument_commands::register_commands(root_menu, session);
    equity_option_instrument_commands::register_commands(root_menu, session);
    equity_position_instrument_commands::register_commands(root_menu, session);
    equity_position_option_underlying_commands::register_commands(root_menu, session);
    equity_swap_instrument_commands::register_commands(root_menu, session);
    equity_variance_swap_instrument_commands::register_commands(root_menu, session);
    exercise_type_commands::register_commands(root_menu, session);
    flexi_swap_instrument_commands::register_commands(root_menu, session);
    fpml_event_type_commands::register_commands(root_menu, session);
    fra_instrument_commands::register_commands(root_menu, session);
    fx_accumulator_instrument_commands::register_commands(root_menu, session);
    fx_asian_forward_instrument_commands::register_commands(root_menu, session);
    fx_barrier_option_instrument_commands::register_commands(root_menu, session);
    fx_digital_option_instrument_commands::register_commands(root_menu, session);
    fx_forward_instrument_commands::register_commands(root_menu, session);
    fx_vanilla_option_instrument_commands::register_commands(root_menu, session);
    fx_variance_swap_instrument_commands::register_commands(root_menu, session);
    inflation_swap_instrument_commands::register_commands(root_menu, session);
    knock_out_swap_instrument_commands::register_commands(root_menu, session);
    lifecycle_event_commands::register_commands(root_menu, session);
    long_short_type_commands::register_commands(root_menu, session);
    moment_type_commands::register_commands(root_menu, session);
    option_type_commands::register_commands(root_menu, session);
    ore_commands::register_commands(root_menu, session);
    party_role_type_commands::register_commands(root_menu, session);
    payoff_type_commands::register_commands(root_menu, session);
    price_type_commands::register_commands(root_menu, session);
    rate_instrument_commands::register_commands(root_menu, session);
    return_type_commands::register_commands(root_menu, session);
    rpa_instrument_commands::register_commands(root_menu, session);
    scripted_instrument_commands::register_commands(root_menu, session);
    settlement_type_commands::register_commands(root_menu, session);
    structure_commands::register_commands(root_menu, session);
    structure_kind_commands::register_commands(root_menu, session);
    structure_member_commands::register_commands(root_menu, session);
    structure_template_commands::register_commands(root_menu, session);
    structure_template_role_commands::register_commands(root_menu, session);
    swap_leg_commands::register_commands(root_menu, session);
    swaption_instrument_commands::register_commands(root_menu, session);
    trade_booking_commands::register_commands(root_menu, session);
    trade_id_type_commands::register_commands(root_menu, session);
    trade_identifier_commands::register_commands(root_menu, session);
    trade_link_commands::register_commands(root_menu, session);
    trade_link_type_commands::register_commands(root_menu, session);
    trade_party_role_commands::register_commands(root_menu, session);
    trade_state_commands::register_commands(root_menu, session);
    trade_type_commands::register_commands(root_menu, session);
    vanilla_swap_instrument_commands::register_commands(root_menu, session);
}

}
